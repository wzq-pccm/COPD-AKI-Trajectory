/*
COPD and Early AKI Trajectory in Critically Ill Patients
Public reproducibility code | eICU-CRD v2.0 | PostgreSQL

09_rrt_events.sql
-----------------
Purpose
  Extract renal replacement therapy (RRT) CANDIDATE EVIDENCE as a long-format
  event table for all ICU +60-hour landmark stays, for the paper's RRT
  exclusion sensitivity analysis (Supplementary Table S9).

Original code and adaptations
  - Investigator revision source:
    04_eICU_RRT_Long_Official_ID_Optimized_8495(1).sql.
  - The same qualifying +60-h cohort as public eICU 02_aki_trajectory.sql
    Sections 1-6 is independently reconstructed using source tables and
    Supplementary Table S10 COPD/AECOPD diagnosis codes. The cohort block is
    copied verbatim except for TEMP-table name prefixes.
  - The original per-stay demographic/hospital mapping is kept with its
    source changed from an embedded stay-ID list to this generated cohort.
  - All RRT event logic, source fields, event flags, timing conversion,
    evidence classification, and FINAL SELECT are retained from the original
    revision script except for temporary-table name prefixes.
  - Private embedded patientunitstayid values and old workbook MD5 checks
    are NOT included. There are no fixed patient/ICU stay identifiers.

Required official source tables
  - eicu_crd.patient
  - eicu_crd.diagnosis
  - eicu_crd.treatment
  - eicu_crd.intakeoutput
  No local 'dictionary' or ID-optimized anchor is needed.

RRT evidence interpretation (critical)
  - Structured treatment records indicate charted treatment. The eICU
    treatmentoffset is an entry time, NOT a verified RRT initiation time.
  - Positive dialysis-specific fluid I/O documents flowsheet volume;
    cumulative dialysistotal changes are NOT new dialysis sessions.
  - Access/filter/ultrafiltration-only patterns remain separately flagged.
  - The source unit's event offset is converted to INDEX ICU-relative minutes:
      source_offset - source_unit.hospitaladmitoffset
                    + index_unit.hospitaladmitoffset
  - Candidate events from all ICU units of the same hospital admission are
    sought, including those occurring AFTER +60 h. This is intentional.
  - The final LEFT JOIN keeps a single no-event row if no candidate RRT
    record exists; this does NOT prove absence of RRT.
  - The downstream R sensitivity analysis must apply its prespecified
    event timing, clinical evidence-strength and exclusion rules.

Execution and validation
  - Standalone PostgreSQL SQL: no prior execution of 01-08 is required.
  - Run in ONE session/connection because intermediate tables are TEMP.
  - The FINAL SELECT is the patient-level result set. Export only this
    result to a restricted-access analysis file; never publish patient data.
  - Expected cohort size for this study: 8,495 unique patientunitstayid,
    not the smaller 7,738-stay primary classifiable sample.
  - This is long format: one stay may have many RRT candidate events.
    A stay with none is retained as one NULL-event row.
  - Validate the patientunitstayid set AND every record/source/timing/flag
    against the original ID-optimized export. Cohort counts alone do not
    establish event-level equivalence.
  - Static source comparison was completed. The SQL has NOT been executed
    against a credentialed eICU database here; local validation is required.
*/

-- 0. Check required tables before starting; this returns no study records.
DO $$
DECLARE rel text;
BEGIN
    FOREACH rel IN ARRAY ARRAY[
        'eicu_crd.patient',
        'eicu_crd.diagnosis',
        'eicu_crd.treatment',
        'eicu_crd.intakeoutput'
    ] LOOP
        IF to_regclass(rel) IS NULL THEN
            RAISE EXCEPTION 'eICU 09: required source relation missing: %', rel;
        END IF;
    END LOOP;
END $$;

-- Drop only 09-module temporary relations to allow repeated runs.
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_rrt_events;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_rrt_io_balance;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_rrt_io_base;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_rrt_related_units;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_rrt_cohort;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_cohort;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_first_qualifying;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_first_unit;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_candidates;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_esrd_hospitalizations;
DROP TABLE IF EXISTS pg_temp.tmp_eicu09_copd_hospitalizations;

-- A-B. Regenerate exactly the +60-hour cohort defined in public eICU 02.
/* =========================================================================
   1. Broad COPD hospital admissions and AECOPD subgroup flag

   Public/reproducible implementation without the local dictionary schema.
   Codes correspond to Supplementary Table S10:
     COPD:   ICD-9-CM 491.20; ICD-10-CM J44.9
     AECOPD: ICD-9-CM 491.21; ICD-10-CM J44.1
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu09_copd_hospitalizations
ON COMMIT PRESERVE ROWS AS
SELECT
    p.patienthealthsystemstayid,

    1::integer AS broad_copd_flag,

    /* COPD: ICD-9-CM 491.20 or ICD-10-CM J44.9 */
    MAX(
        CASE
            WHEN COALESCE(dia.icd9code, '')
                     ~* E'(^|[^0-9A-Z])(491\\.20|J44\\.9)([^0-9A-Z]|$)'
                THEN 1
            ELSE 0
        END
    )::integer AS copd_800070_flag,

    /* Acute COPD exacerbation: ICD-9-CM 491.21 or ICD-10-CM J44.1 */
    MAX(
        CASE
            WHEN COALESCE(dia.icd9code, '')
                     ~* E'(^|[^0-9A-Z])(491\\.21|J44\\.1)([^0-9A-Z]|$)'
                THEN 1
            ELSE 0
        END
    )::integer AS aecopd_800108_flag,

    MAX(
        CASE
            WHEN COALESCE(dia.icd9code, '')
                     ~* E'(^|[^0-9A-Z])(491\\.21|J44\\.1)([^0-9A-Z]|$)'
                THEN 1
            ELSE 0
        END
    )::integer AS aecopd_flag

FROM eicu_crd.diagnosis AS dia

INNER JOIN eicu_crd.patient AS p
    ON p.patientunitstayid = dia.patientunitstayid

WHERE COALESCE(dia.icd9code, '')
          ~* E'(^|[^0-9A-Z])(491\\.20|J44\\.9|491\\.21|J44\\.1)([^0-9A-Z]|$)'

GROUP BY
    p.patienthealthsystemstayid;

CREATE UNIQUE INDEX tmp_eicu09_copd_hospitalizations_idx
    ON tmp_eicu09_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu09_copd_hospitalizations;


/* =========================================================================
   2. ESRD / chronic dialysis at the hospital-admission level
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu09_esrd_hospitalizations
ON COMMIT PRESERVE ROWS AS
SELECT DISTINCT
    p.patienthealthsystemstayid

FROM eicu_crd.diagnosis AS dia

INNER JOIN eicu_crd.patient AS p
    ON p.patientunitstayid = dia.patientunitstayid

WHERE
       COALESCE(dia.icd9code, '')
           ~* E'(^|[^0-9A-Z])(585\\.6|V45\\.11)([^0-9A-Z]|$)'

    OR LOWER(COALESCE(dia.diagnosisstring, ''))
           LIKE '%end stage renal disease%'

    OR LOWER(COALESCE(dia.diagnosisstring, ''))
           LIKE '%end-stage renal disease%'

    OR LOWER(COALESCE(dia.diagnosisstring, ''))
           LIKE '%chronic dialysis%'

    OR LOWER(COALESCE(dia.diagnosisstring, ''))
           LIKE '%dialysis dependence%';

CREATE UNIQUE INDEX tmp_eicu09_esrd_hospitalizations_idx
    ON tmp_eicu09_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu09_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD candidate ICU unit stays
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu09_candidates
ON COMMIT PRESERVE ROWS AS
WITH patient_age_parsed AS (
    SELECT
        p.*,

        CASE
            WHEN TRIM(COALESCE(p.age, '')) ~ '^[0-9]+$'
                THEN TRIM(p.age)::integer

            WHEN REPLACE(
                    TRIM(COALESCE(p.age, '')),
                    ' ',
                    ''
                 ) = '>89'
                THEN 90

            ELSE NULL
        END AS age_numeric,

        COALESCE(
            NULLIF(TRIM(p.uniquepid), ''),
            'stay_' || p.patientunitstayid::text
        ) AS patient_key

    FROM eicu_crd.patient AS p
)

SELECT
    p.patient_key,
    p.uniquepid,
    p.patienthealthsystemstayid,
    p.patientunitstayid,

    p.age_numeric AS age,
    p.gender,
    p.ethnicity,

    p.hospitalid,
    p.wardid,

    p.hospitaldischargeyear,
    p.hospitaladmittime24,
    p.hospitaladmitoffset,
    p.hospitaladmitsource,
    p.hospitaldischargetime24,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,
    p.hospitaldischargelocation,

    p.unitvisitnumber,
    p.unitstaytype,
    p.unittype,
    p.unitadmittime24,
    p.unitadmitsource,
    p.unitdischargetime24,
    p.unitdischargeoffset,
    p.unitdischargestatus,
    p.unitdischargelocation,

    p.apacheadmissiondx,

    c.broad_copd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,
    c.aecopd_flag,

    0::integer AS esrd_chronic_dialysis_flag

FROM patient_age_parsed AS p

INNER JOIN tmp_eicu09_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu09_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu09_candidates_stay_idx
    ON tmp_eicu09_candidates (patientunitstayid);

CREATE INDEX tmp_eicu09_candidates_hospitalization_idx
    ON tmp_eicu09_candidates (patienthealthsystemstayid);

CREATE INDEX tmp_eicu09_candidates_patient_idx
    ON tmp_eicu09_candidates (patient_key);

ANALYZE tmp_eicu09_candidates;


/* =========================================================================
   4. First ICU unit stay within each qualifying hospital admission
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu09_first_unit
ON COMMIT PRESERVE ROWS AS
SELECT
    x.*

FROM (
    SELECT
        c.*,

        ROW_NUMBER() OVER (
            PARTITION BY c.patienthealthsystemstayid
            ORDER BY
                c.unitvisitnumber NULLS LAST,
                c.patientunitstayid
        ) AS hospital_first_unit_rn

    FROM tmp_eicu09_candidates AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu09_first_unit_stay_idx
    ON tmp_eicu09_first_unit (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu09_first_unit_hospitalization_idx
    ON tmp_eicu09_first_unit (patienthealthsystemstayid);

ANALYZE tmp_eicu09_first_unit;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient

   Preserve the deterministic eICU ordering rule used in Stage 1.
   The 60-h landmark is applied only AFTER this selection.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu09_first_qualifying
ON COMMIT PRESERVE ROWS AS
SELECT
    x.*

FROM (
    SELECT
        h.*,

        ROW_NUMBER() OVER (
            PARTITION BY h.patient_key
            ORDER BY
                h.hospitaldischargeyear NULLS LAST,
                h.patienthealthsystemstayid,
                h.patientunitstayid
        ) AS qualifying_icu_rn

    FROM tmp_eicu09_first_unit AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu09_first_qualifying_stay_idx
    ON tmp_eicu09_first_qualifying (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu09_first_qualifying_patient_idx
    ON tmp_eicu09_first_qualifying (patient_key);

ANALYZE tmp_eicu09_first_qualifying;


/* =========================================================================
   6. ICU +60-h landmark cohort and post-landmark in-hospital outcome

   54 h is NOT an outcome landmark. It is only the end of the narrow
   recovery sensitivity window.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu09_cohort
ON COMMIT PRESERVE ROWS AS
WITH status_normalized AS (
    SELECT
        f.*,

        LOWER(
            TRIM(
                COALESCE(
                    f.hospitaldischargestatus,
                    ''
                )
            )
        ) AS hospitaldischargestatus_normalized,

        CASE
            WHEN
                   LOWER(TRIM(COALESCE(f.hospitaldischargestatus, '')))
                       LIKE '%expired%'

                OR LOWER(TRIM(COALESCE(f.hospitaldischargestatus, '')))
                       LIKE '%dead%'

                OR LOWER(TRIM(COALESCE(f.hospitaldischargestatus, '')))
                       LIKE '%deceased%'

                THEN 1

            WHEN NULLIF(
                    TRIM(COALESCE(f.hospitaldischargestatus, '')),
                    ''
                 ) IS NOT NULL
                THEN 0

            ELSE NULL
        END AS hospital_expire_flag

    FROM tmp_eicu09_first_qualifying AS f
),
landmark_variables AS (
    SELECT
        s.*,

        0::integer AS icu_admission_offset,
        1440::integer AS reference_fallback_0_24h_end_offset,
        2160::integer AS recovery_window_36_60_start_offset,
        2520::integer AS recovery_window_42_54_start_offset,
        2880::integer AS target_48h_offset,
        2880::integer AS early_aki_0_48h_end_offset,
        3240::integer AS recovery_window_42_54_end_offset,
        3600::integer AS recovery_window_36_60_end_offset,
        3600::integer AS landmark_60h_offset,

        GREATEST(
            COALESCE(
                s.hospitaladmitoffset,
                -10080
            ),
            -10080
        )::integer AS scr_lookback_start_offset,

        CASE
            WHEN s.hospitaldischargeoffset IS NOT NULL
             AND s.hospitaldischargestatus_normalized <> ''
             AND s.hospitaldischargeoffset > 3600
                THEN 1
            ELSE 0
        END AS eligible_at_60h_flag,

        CASE
            WHEN s.hospital_expire_flag = 1
             AND s.hospitaldischargeoffset > 3600
                THEN 1

            WHEN s.hospital_expire_flag = 0
             AND s.hospitaldischargeoffset > 3600
                THEN 0

            ELSE NULL
        END AS hospital_death_after_60h,

        CASE
            WHEN s.hospitaldischargeoffset > 3600
                THEN (
                    s.hospitaldischargeoffset - 3600
                ) / 1440.0
            ELSE NULL
        END AS followup_days_after_60h,

        CASE
            WHEN s.unitdischargeoffset IS NOT NULL
             AND s.unitdischargeoffset <= 3600
             AND s.hospitaldischargeoffset > 3600
                THEN 1
            ELSE 0
        END AS left_icu_before_60h_but_still_hospitalized_flag

    FROM status_normalized AS s
)

SELECT
    l.*

FROM landmark_variables AS l

WHERE l.eligible_at_60h_flag = 1;

CREATE UNIQUE INDEX tmp_eicu09_cohort_stay_idx
    ON tmp_eicu09_cohort (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu09_cohort_patient_idx
    ON tmp_eicu09_cohort (patient_key);

ANALYZE tmp_eicu09_cohort;

-- C. Rebuild ONLY the original demographic/hospital anchors.
CREATE TEMP TABLE tmp_eicu09_rrt_cohort
ON COMMIT PRESERVE ROWS AS
SELECT
    COALESCE(NULLIF(TRIM(p.uniquepid), ''),
             'stay_' || p.patientunitstayid::text) AS patient_key,
    p.uniquepid,
    p.patienthealthsystemstayid,
    p.patientunitstayid AS index_unit_stay_id,
    p.hospitalid,
    p.hospitaladmitoffset::integer AS index_hospitaladmitoffset,
    p.hospitaldischargeoffset::integer AS index_hospitaldischargeoffset,
    p.hospitaldischargestatus::text AS index_hospitaldischargestatus,
    p.unitdischargeoffset::integer AS index_unitdischargeoffset,
    3600::integer AS landmark_60h_offset_minutes
FROM tmp_eicu09_cohort AS x
INNER JOIN eicu_crd.patient AS p
    ON p.patientunitstayid = x.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu09_rrt_cohort_pu_idx
    ON tmp_eicu09_rrt_cohort (index_unit_stay_id);
CREATE INDEX tmp_eicu09_rrt_cohort_hosp_idx
    ON tmp_eicu09_rrt_cohort (patienthealthsystemstayid);
ANALYZE tmp_eicu09_rrt_cohort;

-- D. Verify cohort size, one-to-one source mapping, landmark and offsets.
--    The study sample size is a reproducibility target, not a fixed-ID
--    criterion; do not modify the cohort to make counts match.
DO $$
DECLARE
    n_landmark integer;
    n_mapped integer;
    n_invalid integer;
    n_mismatch integer;
BEGIN
    SELECT COUNT(*)::integer INTO n_landmark FROM tmp_eicu09_cohort;
    SELECT COUNT(*)::integer INTO n_mapped FROM tmp_eicu09_rrt_cohort;
    IF n_landmark <> 8495 THEN
        RAISE NOTICE
          'eICU 09: reconstructed +60-h cohort N=%, expected 8,495 for the study. Check SQL and database version before comparison.',
          n_landmark;
    END IF;
    IF n_landmark <> n_mapped THEN
        RAISE EXCEPTION
          'eICU 09: cohort-to-patient mapping mismatch: landmark N=%, mapped N=%.',
          n_landmark, n_mapped;
    END IF;

    SELECT COUNT(*)::integer INTO n_mismatch
    FROM tmp_eicu09_cohort AS x
    FULL JOIN tmp_eicu09_rrt_cohort AS c
      ON x.patientunitstayid = c.index_unit_stay_id
    WHERE x.patientunitstayid IS NULL
       OR c.index_unit_stay_id IS NULL
       OR x.patienthealthsystemstayid IS DISTINCT FROM c.patienthealthsystemstayid;
    IF n_mismatch > 0 THEN
        RAISE EXCEPTION
          'eICU 09: % landmark-to-RRT cohort ID or hospitalization mapping discrepancies.',
          n_mismatch;
    END IF;

    SELECT COUNT(*)::integer INTO n_invalid
    FROM tmp_eicu09_rrt_cohort AS c
    JOIN tmp_eicu09_cohort AS x
      ON x.patientunitstayid = c.index_unit_stay_id
    WHERE x.eligible_at_60h_flag IS DISTINCT FROM 1
       OR c.index_hospitaldischargeoffset IS NULL
       OR c.index_hospitaldischargeoffset <= 3600
       OR c.index_hospitaladmitoffset IS NULL
       OR NULLIF(TRIM(COALESCE(c.index_hospitaldischargestatus, '')), '') IS NULL;
    IF n_invalid > 0 THEN
        RAISE EXCEPTION
          'eICU 09: % stays fail the original RRT landmark/offset QC.',
          n_invalid;
    END IF;
END $$;

-- E. Add ICU units from the SAME hospitalization to prevent missing
--    entries recorded after index-unit transfer. Use the official
--    patientHealthSystemStayID; stay conversion needs hospitalAdmitOffset.
CREATE TEMP TABLE tmp_eicu09_rrt_related_units
ON COMMIT PRESERVE ROWS AS
SELECT
    c.index_unit_stay_id,
    p.patientunitstayid AS source_unit_stay_id,
    p.hospitaladmitoffset::integer AS source_unit_hospitaladmitoffset,
    p.hospitalid AS source_hospitalid
FROM tmp_eicu09_rrt_cohort AS c
INNER JOIN eicu_crd.patient AS p
    ON p.patienthealthsystemstayid = c.patienthealthsystemstayid
   AND p.hospitalid = c.hospitalid;

CREATE UNIQUE INDEX tmp_eicu09_rrt_related_source_idx
    ON tmp_eicu09_rrt_related_units (source_unit_stay_id);
CREATE INDEX tmp_eicu09_rrt_related_index_idx
    ON tmp_eicu09_rrt_related_units (index_unit_stay_id);
ANALYZE tmp_eicu09_rrt_related_units;

DO $$
DECLARE
    n_missing integer;
    n_missing_offset integer;
BEGIN
    SELECT COUNT(*)::integer INTO n_missing
    FROM tmp_eicu09_rrt_cohort AS c
    LEFT JOIN tmp_eicu09_rrt_related_units AS r
      ON r.index_unit_stay_id = c.index_unit_stay_id
     AND r.source_unit_stay_id = c.index_unit_stay_id
    WHERE r.source_unit_stay_id IS NULL;
    IF n_missing <> 0 THEN
        RAISE EXCEPTION
          'eICU 04: % anchor units disappeared from related-unit map.',
          n_missing;
    END IF;

    SELECT COUNT(*)::integer INTO n_missing_offset
    FROM tmp_eicu09_rrt_related_units
    WHERE source_unit_hospitaladmitoffset IS NULL;
    IF n_missing_offset > 0 THEN
        RAISE NOTICE
          'eICU 04: % related units lack hospitalAdmitOffset; event times from these units will remain NULL for manual review.',
          n_missing_offset;
    END IF;
END $$;

-- F. Obtain OFFICIAL I/O rows for this restricted set of hospitalizations.
--    Keep zero cumulative totals in the scan: transitions from nonzero
--    to zero and back must be visible when finding true total CHANGES.
--    Only rows with non-NULL dialysistotal OR a dialysis-related path
--    enter the local temporary table; source data are not modified.
CREATE TEMP TABLE tmp_eicu09_rrt_io_base
ON COMMIT PRESERVE ROWS AS
SELECT
    r.index_unit_stay_id,
    r.source_unit_stay_id,
    r.source_unit_hospitaladmitoffset,
    io.intakeoutputid::integer AS source_record_id,
    io.intakeoutputoffset::integer AS source_eventoffset_minutes,
    io.intakeoutputentryoffset::integer AS source_entryoffset_minutes,
    CASE
      WHEN r.source_unit_hospitaladmitoffset IS NOT NULL THEN
        (io.intakeoutputoffset - r.source_unit_hospitaladmitoffset
         + c.index_hospitaladmitoffset)::integer
      ELSE NULL::integer
    END AS event_offset_from_index_icu_minutes,
    io.cellpath::text AS raw_cellpath,
    io.celllabel::text AS raw_celllabel,
    io.cellvaluenumeric::numeric AS raw_cellvaluenumeric,
    io.dialysistotal::numeric AS raw_dialysistotal
FROM tmp_eicu09_rrt_related_units AS r
INNER JOIN tmp_eicu09_rrt_cohort AS c
    ON c.index_unit_stay_id = r.index_unit_stay_id
INNER JOIN eicu_crd.intakeoutput AS io
    ON io.patientunitstayid = r.source_unit_stay_id
WHERE
    (io.dialysistotal IS NOT NULL
     OR COALESCE(io.celllabel, '') ~*
          '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|scuf|renal replacement|ultrafiltrat)'
     OR COALESCE(io.cellpath, '') ~*
          '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|scuf|renal replacement|ultrafiltrat)')
    AND (
      r.source_unit_hospitaladmitoffset IS NULL
      OR (io.intakeoutputoffset - r.source_unit_hospitaladmitoffset
          + c.index_hospitaladmitoffset)
         BETWEEN LEAST(c.index_hospitaladmitoffset, 0)
             AND c.index_hospitaldischargeoffset
    );

CREATE INDEX tmp_eicu09_rrt_io_source_time_idx
    ON tmp_eicu09_rrt_io_base
    (source_unit_stay_id, source_eventoffset_minutes, source_record_id);
CREATE INDEX tmp_eicu09_rrt_io_index_idx
    ON tmp_eicu09_rrt_io_base (index_unit_stay_id);
ANALYZE tmp_eicu09_rrt_io_base;

-- G. Detect CHANGES in dialysisTotal rather than exporting each repeated
--    cumulative tally. At each source-unit/time, use the latest I/O ID
--    as one consistent cumulative snapshot, then compare consecutive
--    snapshots within that source ICU unit. A change is not an RRT start.
CREATE TEMP TABLE tmp_eicu09_rrt_io_balance
ON COMMIT PRESERVE ROWS AS
WITH one_per_time AS (
    SELECT DISTINCT ON (
        source_unit_stay_id, source_eventoffset_minutes
    )
        index_unit_stay_id,
        source_unit_stay_id,
        source_unit_hospitaladmitoffset,
        source_record_id,
        source_eventoffset_minutes,
        source_entryoffset_minutes,
        event_offset_from_index_icu_minutes,
        raw_cellpath,
        raw_celllabel,
        raw_cellvaluenumeric,
        raw_dialysistotal
    FROM tmp_eicu09_rrt_io_base
    WHERE raw_dialysistotal IS NOT NULL
    ORDER BY source_unit_stay_id,
             source_eventoffset_minutes,
             source_record_id DESC
), tally_transitions AS (
    SELECT
        a.*,
        LAG(raw_dialysistotal) OVER (
            PARTITION BY source_unit_stay_id
            ORDER BY source_eventoffset_minutes
        ) AS previous_dialysistotal
    FROM one_per_time AS a
)
SELECT *
FROM tally_transitions
WHERE raw_dialysistotal <> 0
  AND raw_dialysistotal IS DISTINCT FROM previous_dialysistotal;

CREATE INDEX tmp_eicu09_rrt_balance_idx
    ON tmp_eicu09_rrt_io_balance
    (index_unit_stay_id, event_offset_from_index_icu_minutes);
ANALYZE tmp_eicu09_rrt_io_balance;

-- H. Standardized evidence-event table (NOT a claim of exact RRT start).
CREATE TEMP TABLE tmp_eicu09_rrt_events (
    index_unit_stay_id integer NOT NULL,
    source_unit_stay_id integer,
    source_unit_hospitaladmitoffset integer,
    source_record_id text,
    event_source text,
    source_signal text,
    source_eventoffset_minutes integer,
    source_entryoffset_minutes integer,
    event_offset_from_index_icu_minutes integer,
    evidence_class text,
    modality_candidate text,
    event_time_kind text,
    raw_treatment_string text,
    raw_activeupondischarge text,
    raw_io_cellpath text,
    raw_io_celllabel text,
    raw_io_cellvaluenumeric numeric,
    raw_io_dialysistotal numeric,
    previous_io_dialysistotal numeric,
    rrt_treatment_documented_flag integer,
    rrt_dialysis_flow_flag integer,
    rrt_cumulative_balance_change_flag integer,
    auxiliary_or_ambiguous_only_flag integer
) ON COMMIT PRESERVE ROWS;

-- H1. eICU structured TREATMENT entries. Official treatmentOffset is
--     the time the entry was made, not when therapy definitely started.
INSERT INTO tmp_eicu09_rrt_events (
    index_unit_stay_id,
    source_unit_stay_id,
    source_unit_hospitaladmitoffset,
    source_record_id,
    event_source,
    source_signal,
    source_eventoffset_minutes,
    source_entryoffset_minutes,
    event_offset_from_index_icu_minutes,
    evidence_class,
    modality_candidate,
    event_time_kind,
    raw_treatment_string,
    raw_activeupondischarge,
    rrt_treatment_documented_flag,
    rrt_dialysis_flow_flag,
    rrt_cumulative_balance_change_flag,
    auxiliary_or_ambiguous_only_flag
)
SELECT
    r.index_unit_stay_id,
    r.source_unit_stay_id,
    r.source_unit_hospitaladmitoffset,
    t.treatmentid::text,
    'eicu_crd.treatment'::text,
    'structured_treatment'::text,
    t.treatmentoffset::integer,
    t.treatmentoffset::integer,
    CASE WHEN r.source_unit_hospitaladmitoffset IS NOT NULL
      THEN (t.treatmentoffset - r.source_unit_hospitaladmitoffset
            + c.index_hospitaladmitoffset)::integer
      ELSE NULL::integer END,
    CASE
      WHEN t.treatmentstring ~*
           '(catheter|vascular access|vascular line|dialysis line|av fistula|av shunt|filter change|filter exchange|circuit change)'
        THEN 'access_or_filter_only_review'
      WHEN t.treatmentstring ~*
           '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled)'
        THEN 'structured_rrt_treatment_documented'
      ELSE 'ultrafiltration_only_review'
    END::text,
    CASE
      WHEN t.treatmentstring ~* '(scuf)' THEN 'SCUF'
      WHEN t.treatmentstring ~* '(cvvhdf|hemodiafiltrat|haemodiafiltrat)'
        THEN 'CVVHDF'
      WHEN t.treatmentstring ~* '(cvvhd)' THEN 'CVVHD'
      WHEN t.treatmentstring ~* '(cvvh|hemofiltrat|haemofiltrat)'
        THEN 'CVVH/hemofiltration'
      WHEN t.treatmentstring ~* '(peritoneal|capd)' THEN 'Peritoneal dialysis'
      WHEN t.treatmentstring ~* '(crrt|continuous renal replacement)'
        THEN 'CRRT unspecified'
      WHEN t.treatmentstring ~*
           '(hemodialys|haemodialys|intermittent dialysis|sled)'
        THEN 'IHD/HD'
      WHEN t.treatmentstring ~* '(dialys|renal replacement)'
        THEN 'Dialysis unspecified'
      ELSE 'UF or related review'
    END::text,
    'treatment_entry_time_not_initiation'::text,
    t.treatmentstring::text,
    t.activeupondischarge::text,
    CASE
      WHEN t.treatmentstring !~*
           '(catheter|vascular access|vascular line|dialysis line|av fistula|av shunt|filter change|filter exchange|circuit change)'
       AND t.treatmentstring ~*
           '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled)'
        THEN 1 ELSE 0 END,
    0::integer,
    0::integer,
    CASE
      WHEN t.treatmentstring ~*
           '(catheter|vascular access|vascular line|dialysis line|av fistula|av shunt|filter change|filter exchange|circuit change)'
        OR t.treatmentstring !~*
           '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled)'
        THEN 1 ELSE 0 END
FROM tmp_eicu09_rrt_related_units AS r
INNER JOIN tmp_eicu09_rrt_cohort AS c
    ON c.index_unit_stay_id = r.index_unit_stay_id
INNER JOIN eicu_crd.treatment AS t
    ON t.patientunitstayid = r.source_unit_stay_id
WHERE t.treatmentstring ~*
      '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled|ultrafiltrat)'
  AND (
       r.source_unit_hospitaladmitoffset IS NULL
       OR (t.treatmentoffset - r.source_unit_hospitaladmitoffset
           + c.index_hospitaladmitoffset)
          BETWEEN LEAST(c.index_hospitaladmitoffset, 0)
              AND c.index_hospitaldischargeoffset
  );

-- H2. eICU I/O cells documenting dialysis-related actual fluid volume.
--     Positive cellvaluenumeric means the corresponding flowsheet cell
--     records a nonzero volume; this is not dialysis session start time.
INSERT INTO tmp_eicu09_rrt_events (
    index_unit_stay_id,
    source_unit_stay_id,
    source_unit_hospitaladmitoffset,
    source_record_id,
    event_source,
    source_signal,
    source_eventoffset_minutes,
    source_entryoffset_minutes,
    event_offset_from_index_icu_minutes,
    evidence_class,
    modality_candidate,
    event_time_kind,
    raw_io_cellpath,
    raw_io_celllabel,
    raw_io_cellvaluenumeric,
    raw_io_dialysistotal,
    rrt_treatment_documented_flag,
    rrt_dialysis_flow_flag,
    rrt_cumulative_balance_change_flag,
    auxiliary_or_ambiguous_only_flag
)
SELECT
    io.index_unit_stay_id,
    io.source_unit_stay_id,
    io.source_unit_hospitaladmitoffset,
    io.source_record_id::text,
    'eicu_crd.intakeoutput'::text,
    'dialysis_specific_flow_cell'::text,
    io.source_eventoffset_minutes,
    io.source_entryoffset_minutes,
    io.event_offset_from_index_icu_minutes,
    CASE
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~*
           '(catheter|vascular access|vascular line|dialysis line|av fistula|av shunt|filter change|filter exchange|circuit change)'
        THEN 'access_or_filter_only_review'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~*
           '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled)'
        THEN 'dialysis_specific_flow_record'
      ELSE 'ultrafiltration_only_review'
    END::text,
    CASE
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~* '(scuf)'
        THEN 'SCUF'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~*
           '(cvvhdf|hemodiafiltrat|haemodiafiltrat)'
        THEN 'CVVHDF'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~* '(cvvhd)'
        THEN 'CVVHD'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~*
           '(cvvh|hemofiltrat|haemofiltrat)'
        THEN 'CVVH/hemofiltration'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~* '(peritoneal|capd)'
        THEN 'Peritoneal dialysis'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~* '(crrt)'
        THEN 'CRRT unspecified'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~*
           '(hemodialys|haemodialys|intermittent dialysis|sled)'
        THEN 'IHD/HD'
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~* '(dialys)'
        THEN 'Dialysis unspecified'
      ELSE 'UF or related review'
    END::text,
    'io_volume_observation_not_initiation'::text,
    io.raw_cellpath,
    io.raw_celllabel,
    io.raw_cellvaluenumeric,
    io.raw_dialysistotal,
    0::integer,
    CASE
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) !~*
           '(catheter|vascular access|vascular line|dialysis line|av fistula|av shunt|filter change|filter exchange|circuit change)'
       AND (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~*
           '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled)'
        THEN 1 ELSE 0 END,
    0::integer,
    CASE
      WHEN (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) ~*
           '(catheter|vascular access|vascular line|dialysis line|av fistula|av shunt|filter change|filter exchange|circuit change)'
        OR (COALESCE(io.raw_cellpath, '') || ' ' ||
            COALESCE(io.raw_celllabel, '')) !~*
           '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled)'
        THEN 1 ELSE 0 END
FROM tmp_eicu09_rrt_io_base AS io
WHERE io.raw_cellvaluenumeric > 0
  AND (COALESCE(io.raw_celllabel, '') ~*
         '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled|ultrafiltrat)'
    OR COALESCE(io.raw_cellpath, '') ~*
         '(dialys|crrt|cvvh|hemodiafiltrat|haemodiafiltrat|hemofiltrat|haemofiltrat|renal replacement|scuf|sled|ultrafiltrat)');

-- H3. Changed nonzero cumulative dialysisTotal. It is already
--     de-duplicated by source unit + observation offset in step G.
--     Do NOT count each cumulative record as an independent session.
INSERT INTO tmp_eicu09_rrt_events (
    index_unit_stay_id,
    source_unit_stay_id,
    source_unit_hospitaladmitoffset,
    source_record_id,
    event_source,
    source_signal,
    source_eventoffset_minutes,
    source_entryoffset_minutes,
    event_offset_from_index_icu_minutes,
    evidence_class,
    modality_candidate,
    event_time_kind,
    raw_io_cellpath,
    raw_io_celllabel,
    raw_io_cellvaluenumeric,
    raw_io_dialysistotal,
    previous_io_dialysistotal,
    rrt_treatment_documented_flag,
    rrt_dialysis_flow_flag,
    rrt_cumulative_balance_change_flag,
    auxiliary_or_ambiguous_only_flag
)
SELECT
    b.index_unit_stay_id,
    b.source_unit_stay_id,
    b.source_unit_hospitaladmitoffset,
    b.source_record_id::text,
    'eicu_crd.intakeoutput'::text,
    'cumulative_dialysis_total_change'::text,
    b.source_eventoffset_minutes,
    b.source_entryoffset_minutes,
    b.event_offset_from_index_icu_minutes,
    'dialysis_cumulative_total_changed'::text,
    'Dialysis unspecified'::text,
    'cumulative_dialysis_balance_observed_not_initiation'::text,
    b.raw_cellpath,
    b.raw_celllabel,
    b.raw_cellvaluenumeric,
    b.raw_dialysistotal,
    b.previous_dialysistotal,
    0::integer,
    0::integer,
    1::integer,
    0::integer
FROM tmp_eicu09_rrt_io_balance AS b;

CREATE INDEX tmp_eicu09_rrt_event_staytime_idx
    ON tmp_eicu09_rrt_events
    (index_unit_stay_id, event_offset_from_index_icu_minutes);
CREATE INDEX tmp_eicu09_rrt_event_source_idx
    ON tmp_eicu09_rrt_events
    (event_source, source_unit_stay_id, source_record_id);
ANALYZE tmp_eicu09_rrt_events;

-- I. Fail fast on unexpected orphan events or duplicated event signals.
--    Different signals in the SAME official I/O row are expected to
--    overlap and must remain distinguishable for R sensitivity analyses.
DO $$
DECLARE
    n_orphan integer;
    n_duplicate integer;
    n_outside integer;
    n_missing_time integer;
BEGIN
    SELECT COUNT(*)::integer INTO n_orphan
    FROM tmp_eicu09_rrt_events AS e
    LEFT JOIN tmp_eicu09_rrt_cohort AS c
      ON c.index_unit_stay_id = e.index_unit_stay_id
    WHERE c.index_unit_stay_id IS NULL;
    IF n_orphan <> 0 THEN
        RAISE EXCEPTION 'eICU 04: % orphan evidence rows.', n_orphan;
    END IF;

    SELECT COUNT(*)::integer INTO n_duplicate
    FROM (
        SELECT event_source, source_signal, source_unit_stay_id,
               source_record_id
        FROM tmp_eicu09_rrt_events
        GROUP BY event_source, source_signal, source_unit_stay_id,
                 source_record_id
        HAVING COUNT(*) > 1
    ) AS t;
    IF n_duplicate <> 0 THEN
        RAISE EXCEPTION
          'eICU 04: % repeated source/signal keys; inspect joins.',
          n_duplicate;
    END IF;

    SELECT COUNT(*)::integer INTO n_outside
    FROM tmp_eicu09_rrt_events AS e
    JOIN tmp_eicu09_rrt_cohort AS c
      ON c.index_unit_stay_id = e.index_unit_stay_id
    WHERE e.event_offset_from_index_icu_minutes IS NOT NULL
      AND e.event_offset_from_index_icu_minutes NOT BETWEEN
          LEAST(c.index_hospitaladmitoffset, 0)
          AND c.index_hospitaldischargeoffset;
    IF n_outside <> 0 THEN
        RAISE EXCEPTION
          'eICU 04: % evidence events outside index hospitalization.',
          n_outside;
    END IF;

    SELECT COUNT(*)::integer INTO n_missing_time
    FROM tmp_eicu09_rrt_events
    WHERE event_offset_from_index_icu_minutes IS NULL;
    IF n_missing_time > 0 THEN
        RAISE NOTICE
          'eICU 04: % candidate events lack transferable index-ICU time; retain as unknown for review, NOT as early RRT.',
          n_missing_time;
    END IF;
END $$;

-- J. ONLY exported result set. All eligible INDEX stays survive.
--    Meaning of 60h: COHORT eligibility cutoff, not a restriction on
--    source event follow-up (events after 60h are intentionally shown).
SELECT
    c.patient_key,
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.index_unit_stay_id AS patientunitstayid,
    c.index_unit_stay_id,
    c.hospitalid,
    c.index_hospitaladmitoffset,
    c.index_hospitaldischargeoffset,
    c.index_hospitaldischargestatus,
    c.index_unitdischargeoffset,
    c.landmark_60h_offset_minutes,
    e.source_unit_stay_id,
    e.source_unit_hospitaladmitoffset,
    e.source_record_id,
    e.source_eventoffset_minutes,
    e.source_entryoffset_minutes,
    e.event_offset_from_index_icu_minutes,
    CASE WHEN e.event_offset_from_index_icu_minutes IS NULL
         THEN NULL::numeric
         ELSE e.event_offset_from_index_icu_minutes::numeric / 60.0
    END AS event_hours_from_index_icu,
    CASE WHEN e.event_source = 'eicu_crd.intakeoutput'
           AND e.source_entryoffset_minutes IS NOT NULL
           AND e.source_eventoffset_minutes IS NOT NULL
         THEN (e.source_entryoffset_minutes -
               e.source_eventoffset_minutes)::integer
         ELSE NULL::integer
    END AS io_entry_delay_minutes,
    CASE WHEN e.event_offset_from_index_icu_minutes IS NOT NULL
           AND e.event_offset_from_index_icu_minutes < 0
         THEN 1 ELSE 0 END AS event_before_index_icu_flag,
    CASE WHEN e.event_offset_from_index_icu_minutes BETWEEN 0 AND 2880
         THEN 1 ELSE 0 END AS event_in_icu_0_48h_flag,
    CASE WHEN e.event_offset_from_index_icu_minutes BETWEEN 0 AND 3600
         THEN 1 ELSE 0 END AS event_in_icu_0_60h_flag,
    CASE WHEN e.event_offset_from_index_icu_minutes BETWEEN 2160 AND 3600
         THEN 1 ELSE 0 END AS event_in_36_60h_flag,
    CASE WHEN e.event_offset_from_index_icu_minutes IS NOT NULL
           AND e.event_offset_from_index_icu_minutes <= 3600
         THEN 1 ELSE 0 END AS event_at_or_before_landmark_flag,
    CASE WHEN e.event_offset_from_index_icu_minutes IS NOT NULL
           AND e.event_offset_from_index_icu_minutes > 3600
         THEN 1 ELSE 0 END AS event_after_landmark_flag,
    CASE WHEN e.source_unit_stay_id IS NOT NULL
           AND e.source_unit_stay_id = c.index_unit_stay_id
         THEN 1 ELSE 0 END AS originates_in_index_unit_flag,
    CASE WHEN e.event_offset_from_index_icu_minutes IS NOT NULL
           AND e.event_offset_from_index_icu_minutes >=
               c.index_hospitaladmitoffset
           AND e.event_offset_from_index_icu_minutes <=
               c.index_hospitaldischargeoffset
         THEN 1 ELSE 0 END AS event_within_recorded_hospital_offsets_flag,
    e.event_source,
    e.source_signal,
    e.evidence_class,
    e.modality_candidate,
    e.event_time_kind,
    e.raw_treatment_string,
    e.raw_activeupondischarge,
    e.raw_io_cellpath,
    e.raw_io_celllabel,
    e.raw_io_cellvaluenumeric,
    e.raw_io_dialysistotal,
    e.previous_io_dialysistotal,
    COALESCE(e.rrt_treatment_documented_flag, 0)
        AS rrt_treatment_documented_flag,
    COALESCE(e.rrt_dialysis_flow_flag, 0)
        AS rrt_dialysis_flow_flag,
    COALESCE(e.rrt_cumulative_balance_change_flag, 0)
        AS rrt_cumulative_balance_change_flag,
    COALESCE(e.auxiliary_or_ambiguous_only_flag, 0)
        AS auxiliary_or_ambiguous_only_flag,
    CASE WHEN COALESCE(e.rrt_treatment_documented_flag, 0) = 1
            OR COALESCE(e.rrt_dialysis_flow_flag, 0) = 1
            OR COALESCE(e.rrt_cumulative_balance_change_flag, 0) = 1
         THEN 1 ELSE 0 END AS rrt_therapy_or_dialysis_flow_evidence_flag,
    CASE WHEN e.event_source IS NOT NULL THEN 1 ELSE 0 END
        AS has_rrt_candidate_record
FROM tmp_eicu09_rrt_cohort AS c
LEFT JOIN tmp_eicu09_rrt_events AS e
    ON e.index_unit_stay_id = c.index_unit_stay_id
ORDER BY
    c.index_unit_stay_id,
    e.event_offset_from_index_icu_minutes NULLS LAST,
    e.event_source NULLS LAST,
    e.source_signal NULLS LAST,
    e.source_record_id NULLS LAST;
