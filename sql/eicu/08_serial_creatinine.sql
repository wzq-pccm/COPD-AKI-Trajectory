/*
COPD and Early AKI Trajectory in Critically Ill Patients
Public reproducibility code | eICU-CRD v2.0 | PostgreSQL

08_serial_creatinine.sql
------------------------
Purpose
  Extract all measured serum creatinine (sCr) events for the index ICU unit
  stays in the study's +60-hour landmark population. The long-format
  extract is used downstream for measurement-frequency, strictly timed
  KDIGO-pair, and near-resolution-threshold sensitivity analyses.

Sources and fidelity
  - Cohort selection is copied from the public eICU 02_aki_trajectory.sql
    script, Sections 1-6 (through the +60-hour landmark table), with only
    the temporary-table prefix changed. It matches the original study
    population definition and the no-dictionary Table S10 ICD predicates
    used in the separately checked Stage 3A (module 03).
  - The final 19-column sCr event SELECT comes from the investigators'
    02_eICU_sCr_Long_ID_Optimized_8495.sql revision script. All returned
    columns, expression definitions, event filters, window boundaries and
    ORDER BY clauses are unchanged; only its cohort table name is changed.
  - The private embedded stay-ID array and study-workbook MD5 signatures
    have been removed. No hardcoded patient or ICU stay identifiers appear.

Cohort definition
  - Broad COPD or acute COPD exacerbation from diagnosis ICD-9-CM 491.20 /
    491.21 or ICD-10-CM J44.9 / J44.1 (Supplementary Table S10).
  - Adults aged >=18 years, excluding recorded ESRD/chronic dialysis.
  - First ICU unit stay per qualifying hospital admission, then first
    qualifying hospitalization per patient (before applying the landmark).
  - Require a recorded hospital discharge status and hospital discharge
    offset strictly beyond +3600 minutes after index ICU admission.
  - Expected +60-h landmark population: 8,495 distinct patientunitstayid.
    Do NOT restrict to the 7,738 classified primary-analysis stays here.

sCr event definition (unchanged from the revision source)
  - Source: eicu_crd.lab, normalized labname = 'creatinine'.
  - labresultoffset non-NULL; labresult non-NULL and >0; no unit conversion.
  - ICU admission is offset 0 (minutes).
  - Lookback start: GREATEST(COALESCE(hospitaladmitoffset, -10080), -10080).
  - Extract events between LEAST(lookback start, 0) and +3600 min inclusive.
  - Pre-ICU reference flag: lookback start <= offset <0.
  - Additional event flags: ICU 0-24 h (0..1440), ICU 0-48 h (0..2880),
    and assessment 36-60 h (2160..3600), all inclusive.
  - LEFT JOIN to lab preserves each stay with no qualifying sCr as a
    single row with has_scr_event=0 and NULL event fields.
  - Measurements belong to eICU ICU-unit-stay lab records, and therefore
    do not establish access to labs from a separate unit or ward stay.

Execution / verification
  - Standalone SQL: no prior execution of 01-07 is required.
  - Run in one PostgreSQL session because the intermediate tables are TEMP.
  - The FINAL SELECT is the only patient-level result set; export only that
    SELECT to a restricted-access analysis file (not a public repository).
  - Expected output: 19 columns, 8,495 unique patientunitstayid values;
    total event-row count may be greater than 8,495.
  - Compare to the private original export by patientunitstayid AND by each
    event row (labid, labresultoffset, labresult), including null-lab rows.
    Matching only the number of stays is not sufficient for equivalence.
  - This public version has undergone static source review, but has NOT
    been run against a credentialed eICU-CRD database in this environment.
*/

-- 0. Check required source relations; this yields no patient-level results.
DO $$
DECLARE rel text;
BEGIN
    FOREACH rel IN ARRAY ARRAY[
        'eicu_crd.patient',
        'eicu_crd.diagnosis',
        'eicu_crd.lab'
    ] LOOP
        IF to_regclass(rel) IS NULL THEN
            RAISE EXCEPTION 'eICU 08: required relation missing: %', rel;
        END IF;
    END LOOP;
END $$;

-- Clean only this module's temporary tables, allowing repeated runs.
DROP TABLE IF EXISTS pg_temp.tmp_eicu08_cohort;
DROP TABLE IF EXISTS pg_temp.tmp_eicu08_first_qualifying;
DROP TABLE IF EXISTS pg_temp.tmp_eicu08_first_unit;
DROP TABLE IF EXISTS pg_temp.tmp_eicu08_candidates;
DROP TABLE IF EXISTS pg_temp.tmp_eicu08_esrd_hospitalizations;
DROP TABLE IF EXISTS pg_temp.tmp_eicu08_copd_hospitalizations;

-- 1. Reconstruct the identical +60-hour landmark population with
--    public Table S10 ICD criteria, without fixed ID lists.
/* =========================================================================
   1. Broad COPD hospital admissions and AECOPD subgroup flag

   Public/reproducible implementation without the local dictionary schema.
   Codes correspond to Supplementary Table S10:
     COPD:   ICD-9-CM 491.20; ICD-10-CM J44.9
     AECOPD: ICD-9-CM 491.21; ICD-10-CM J44.1
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu08_copd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu08_copd_hospitalizations_idx
    ON tmp_eicu08_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu08_copd_hospitalizations;


/* =========================================================================
   2. ESRD / chronic dialysis at the hospital-admission level
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu08_esrd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu08_esrd_hospitalizations_idx
    ON tmp_eicu08_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu08_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD candidate ICU unit stays
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu08_candidates
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

INNER JOIN tmp_eicu08_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu08_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu08_candidates_stay_idx
    ON tmp_eicu08_candidates (patientunitstayid);

CREATE INDEX tmp_eicu08_candidates_hospitalization_idx
    ON tmp_eicu08_candidates (patienthealthsystemstayid);

CREATE INDEX tmp_eicu08_candidates_patient_idx
    ON tmp_eicu08_candidates (patient_key);

ANALYZE tmp_eicu08_candidates;


/* =========================================================================
   4. First ICU unit stay within each qualifying hospital admission
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu08_first_unit
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

    FROM tmp_eicu08_candidates AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu08_first_unit_stay_idx
    ON tmp_eicu08_first_unit (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu08_first_unit_hospitalization_idx
    ON tmp_eicu08_first_unit (patienthealthsystemstayid);

ANALYZE tmp_eicu08_first_unit;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient

   Preserve the deterministic eICU ordering rule used in Stage 1.
   The 60-h landmark is applied only AFTER this selection.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu08_first_qualifying
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

    FROM tmp_eicu08_first_unit AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu08_first_qualifying_stay_idx
    ON tmp_eicu08_first_qualifying (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu08_first_qualifying_patient_idx
    ON tmp_eicu08_first_qualifying (patient_key);

ANALYZE tmp_eicu08_first_qualifying;


/* =========================================================================
   6. ICU +60-h landmark cohort and post-landmark in-hospital outcome

   54 h is NOT an outcome landmark. It is only the end of the narrow
   recovery sensitivity window.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu08_cohort
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

    FROM tmp_eicu08_first_qualifying AS f
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

CREATE UNIQUE INDEX tmp_eicu08_cohort_stay_idx
    ON tmp_eicu08_cohort (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu08_cohort_patient_idx
    ON tmp_eicu08_cohort (patient_key);

ANALYZE tmp_eicu08_cohort;

-- 2. Quality checks (NOTICE for count discrepancies; EXCEPTION for invalid
--    landmark timing). These do not return patient-level records.
DO $$
DECLARE
    n_stays integer;
    n_bad_landmark integer;
BEGIN
    SELECT COUNT(*)::integer INTO n_stays
    FROM tmp_eicu08_cohort;

    IF n_stays <> 8495 THEN
        RAISE NOTICE
            'eICU 08: landmark stays N=%; expected 8,495 in the paper cohort. Investigate before interpreting the extract.',
            n_stays;
    END IF;

    SELECT COUNT(*)::integer INTO n_bad_landmark
    FROM tmp_eicu08_cohort
    WHERE eligible_at_60h_flag <> 1
       OR eligible_at_60h_flag IS NULL
       OR hospitaldischargeoffset IS NULL
       OR hospitaldischargeoffset <= 3600
       OR NULLIF(TRIM(COALESCE(hospitaldischargestatus, '')), '') IS NULL
       OR scr_lookback_start_offset <> GREATEST(
              COALESCE(hospitaladmitoffset, -10080), -10080
          );

    IF n_bad_landmark > 0 THEN
        RAISE EXCEPTION
            'eICU 08: % cohort rows fail landmark / sCr-lookback validation.',
            n_bad_landmark;
    END IF;
END $$;

-- 3. Unmodified 19-column serum creatinine event SELECT from the
--    investigator revision source, using the regenerated cohort above.
--    Filter only inside the LEFT JOIN ON clause to keep missing-lab stays.
SELECT
    c.patient_key,
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.patientunitstayid,
    c.hospitaladmitoffset,
    c.hospitaldischargeoffset,
    c.hospitaldischargestatus,
    c.hospital_expire_flag,
    c.scr_lookback_start_offset,
    lab.labid,
    lab.labresultoffset::integer AS labresultoffset_minutes_from_index_icu,
    lab.labresult::numeric AS scr_numeric_as_extracted,
    lab.labmeasurenamesystem::text AS scr_unit_system,
    lab.labmeasurenameinterface::text AS scr_unit_interface,
    CASE WHEN lab.labid IS NULL THEN 0 ELSE 1 END AS has_scr_event,
    CASE WHEN lab.labresultoffset >= c.scr_lookback_start_offset
               AND lab.labresultoffset < 0 THEN 1 ELSE 0 END
        AS valid_pre_icu_reference_window,
    CASE WHEN lab.labresultoffset BETWEEN 0 AND 1440 THEN 1 ELSE 0 END
        AS in_icu_0_24h,
    CASE WHEN lab.labresultoffset BETWEEN 0 AND 2880 THEN 1 ELSE 0 END
        AS in_icu_0_48h,
    CASE WHEN lab.labresultoffset BETWEEN 2160 AND 3600 THEN 1 ELSE 0 END
        AS in_36_60h_assessment_window
FROM tmp_eicu08_cohort AS c
LEFT JOIN eicu_crd.lab AS lab
    ON lab.patientunitstayid = c.patientunitstayid
   AND lab.labresultoffset IS NOT NULL
   AND lab.labresultoffset >= LEAST(c.scr_lookback_start_offset, 0)
   AND lab.labresultoffset <= 3600
   AND lab.labresult IS NOT NULL
   AND lab.labresult > 0
   AND LOWER(TRIM(COALESCE(lab.labname, ''))) = 'creatinine'
ORDER BY
    c.patientunitstayid,
    lab.labresultoffset NULLS LAST,
    lab.labid;
