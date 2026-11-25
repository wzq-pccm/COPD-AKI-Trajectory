/*
eICU-CRD v2.0 / PostgreSQL
05_nonrenal_sofa_and_qc.sql
Public reproducibility module: first-day nonrenal SOFA + reviewer component QC

SOURCES
  - eICU_stage3B_revised_nonrenal_SOFA_24h.sql (primary derivation)
  - 06_eICU_nonrenal_SOFA_Components_QC_ID_Optimized_8495.sql
    (independent component-level QC only; fixed study IDs are excluded)

PURPOSE
  1. Reconstruct the original broad COPD, adult, non-ESRD, first
     qualifying ICU stay and ICU +60-hour landmark cohort.
  2. Compute the *unchanged* eICU first-day nonrenal SOFA primary-model
     covariate from the five respiratory, coagulation, liver,
     cardiovascular, and CNS component scores (renal excluded).
  3. Append the reviewer-added independent component availability,
     arithmetic, score-range, and vasoactive-dose normalization QC.

COHORT IDENTIFICATION (PUBLIC, NO DICTIONARY)
  COPD:   ICD-9-CM 491.20 or ICD-10-CM J44.9
  AECOPD: ICD-9-CM 491.21 or ICD-10-CM J44.1
  Search eicu_crd.diagnosis.icd9code (one field may contain more than
  one comma-separated code); join to eicu_crd.patient at the
  hospitalization level. The code expression is copied verbatim from
  public module 03, previously shown to reproduce all 8,495 stays and
  53 Stage 3A variables in the user's two exported workbooks.
  Legacy column aliases copd_800070_flag/aecopd_800108_flag are kept
  for compatibility. There is no dictionary join or fixed stay-ID list.

CRITICAL SOFA MISSINGNESS CONVENTION (ORIGINAL ALGORITHM UNCHANGED)
  Raw scores *_sofa_24h_raw remain NULL if their source data are absent.
  Final scored components and standard_nonrenal_sofa_24h use
  COALESCE(raw,0), exactly as in the original eICU Stage 3B.
  Therefore stage3b_primary_covariate_complete_flag=1 does NOT mean
  all five underlying SOFA components were observed. Distinguish it
  from nonrenal_sofa_all_components_available_flag.
  This eICU method differs from MIMIC's precomputed first_day_sofa
  total-minus-renal method; do not substitute one for the other.

SOURCE TIMING AND DOSING (ORIGINAL ALGORITHM UNCHANGED)
  - Respiratory and CNS: APACHE first-day physiology (not always exactly
    timestamped within ICU +0 to +24 hours).
  - Lab platelets/bilirubin, raw MAP and vasopressor infusions: ICU
    minute offsets 0 to 1440, with the original fallbacks.
  - Vasoactive rates are normalized where possible; unresolved active
    infusion rates are flagged by the original conservative method.

EXECUTION
  - PostgreSQL; run the WHOLE file in ONE database session.
  - Only eICU-CRD official source tables are required: patient,
    diagnosis, apacheapsvar, lab, vitalperiodic, vitalaperiodic,
    infusiondrug. No external dictionary, fixed IDs or SOFA lookup.
  - Output: final SELECT contains ORIGINAL Stage 3B columns first,
    then the original reviewer QC additions and cohort QC summary.
  - Expected landmark population: 8,495 rows and unique
    patientunitstayid; expected reference only, not hard-coded input.
  - Compare original vs public SQL exports by patientunitstayid and
    every original Stage 3B value; then inspect the QC fields.
  - No individual-level exported data should be published publicly.

PUBLIC QC REVISIONS
  - Original study-specific embedded stay IDs and MD5 reference hashes
    were NOT included. Cohort is regenerated from eligibility rules.
  - The reviewer-added QC flags and their arithmetic are retained.
  - Old Excel snapshot comparison flags are omitted because they are
    tied to a restricted, frozen patient-specific extraction; the
    current counts and flags are computed from the public code.

VALIDATION STATUS
  Calculation source and QC formulas are statically checked; this
  consolidated script has NOT been executed on the user's eICU DB.
*/

-- Restrict cleanup of new QC tables to current-session TEMP relations.
DROP TABLE IF EXISTS pg_temp.tmp_eicu05_summary;
DROP TABLE IF EXISTS pg_temp.tmp_eicu05_qc;

/* =========================================================================
   0. Clean up
   ========================================================================= */

DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_final;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_scores;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_components;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_vaso_summary;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_vaso_normalized;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_vaso_raw;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_map;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_labs;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_apache;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_cohort;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_first_qualifying;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_first_unit;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_candidates;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_esrd_hospitalizations;
DROP TABLE IF EXISTS pg_temp.tmp_eicu_stage3b_copd_hospitalizations;


/* =========================================================================
   1. Broad COPD hospitalizations
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_copd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_stage3b_copd_hosp_idx
    ON tmp_eicu_stage3b_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3b_copd_hospitalizations;


/* =========================================================================
   2. ESRD / chronic dialysis exclusion at hospitalization level
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_esrd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_stage3b_esrd_hosp_idx
    ON tmp_eicu_stage3b_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3b_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD candidate ICU unit stays
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_candidates
ON COMMIT PRESERVE ROWS AS
WITH patient_age_parsed AS (
    SELECT
        p.*,

        CASE
            WHEN TRIM(COALESCE(p.age, '')) ~ '^[0-9]+$'
                THEN TRIM(p.age)::integer

            WHEN REPLACE(TRIM(COALESCE(p.age, '')), ' ', '') = '>89'
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
    p.hospitaladmitoffset,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,

    p.unitvisitnumber,
    p.unitstaytype,
    p.unittype,
    p.unitdischargeoffset,

    c.broad_copd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,
    c.aecopd_flag,

    0::integer AS esrd_chronic_dialysis_flag

FROM patient_age_parsed AS p

INNER JOIN tmp_eicu_stage3b_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu_stage3b_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu_stage3b_candidates_stay_idx
    ON tmp_eicu_stage3b_candidates (patientunitstayid);

CREATE INDEX tmp_eicu_stage3b_candidates_hosp_idx
    ON tmp_eicu_stage3b_candidates (patienthealthsystemstayid);

CREATE INDEX tmp_eicu_stage3b_candidates_patient_idx
    ON tmp_eicu_stage3b_candidates (patient_key);

ANALYZE tmp_eicu_stage3b_candidates;


/* =========================================================================
   4. First ICU unit stay within each qualifying hospitalization
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_first_unit
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

    FROM tmp_eicu_stage3b_candidates AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage3b_first_unit_stay_idx
    ON tmp_eicu_stage3b_first_unit (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage3b_first_unit_hosp_idx
    ON tmp_eicu_stage3b_first_unit (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3b_first_unit;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_first_qualifying
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

    FROM tmp_eicu_stage3b_first_unit AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage3b_first_qualifying_stay_idx
    ON tmp_eicu_stage3b_first_qualifying (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage3b_first_qualifying_patient_idx
    ON tmp_eicu_stage3b_first_qualifying (patient_key);

ANALYZE tmp_eicu_stage3b_first_qualifying;


/* =========================================================================
   6. ICU +60-hour landmark cohort
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_cohort
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
        ) AS hospitaldischargestatus_normalized

    FROM tmp_eicu_stage3b_first_qualifying AS f
)

SELECT
    s.*,
    1440::integer AS first_24h_end_offset,
    3600::integer AS landmark_60h_offset

FROM status_normalized AS s

WHERE s.hospitaldischargeoffset IS NOT NULL
  AND s.hospitaldischargestatus_normalized <> ''
  AND s.hospitaldischargeoffset > 3600;

CREATE UNIQUE INDEX tmp_eicu_stage3b_cohort_stay_idx
    ON tmp_eicu_stage3b_cohort (patientunitstayid);

ANALYZE tmp_eicu_stage3b_cohort;


/* =========================================================================
   7. APACHE first-day physiology for respiratory, CNS, liver fallback,
      and MAP fallback

   Important GCS rule:
     meds = 1 means unable to score because of medications and eyes/motor/verbal
     are encoded as 0. Those zeros must NOT be interpreted as a GCS of 0.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_apache
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patientunitstayid,

    ap.apacheapsvarid,

    ap.intubated,
    ap.vent,

    ap.pao2
        AS apache_pao2,

    ap.fio2
        AS apache_fio2_raw,

    CASE
        WHEN ap.fio2 > 1
         AND ap.fio2 <= 100
            THEN ap.fio2 / 100.0

        WHEN ap.fio2 >= 0.20
         AND ap.fio2 <= 1
            THEN ap.fio2

        ELSE NULL
    END AS apache_fio2_fraction,

    CASE
        WHEN ap.pao2 > 0
         AND (
                (ap.fio2 > 1 AND ap.fio2 <= 100)
                OR (ap.fio2 >= 0.20 AND ap.fio2 <= 1)
             )
            THEN ap.pao2
                 / NULLIF(
                       CASE
                           WHEN ap.fio2 > 1
                               THEN ap.fio2 / 100.0
                           ELSE ap.fio2
                       END,
                       0
                   )
        ELSE NULL
    END AS apache_pao2_fio2_ratio,

    ap.eyes,
    ap.motor,
    ap.verbal,
    ap.meds,

    CASE
        WHEN COALESCE(ap.meds, 0) = 0
         AND ap.eyes BETWEEN 1 AND 4
         AND ap.motor BETWEEN 1 AND 6
         AND ap.verbal BETWEEN 1 AND 5
            THEN ap.eyes + ap.motor + ap.verbal
        ELSE NULL
    END::integer AS apache_gcs,

    CASE
        WHEN COALESCE(ap.meds, 0) = 1 THEN 1
        ELSE 0
    END::integer AS apache_gcs_unscorable_due_to_meds_flag,

    CASE
        WHEN ap.bilirubin >= 0
         AND ap.bilirubin <= 100
            THEN ap.bilirubin
        ELSE NULL
    END AS apache_bilirubin,

    CASE
        WHEN ap.meanbp >= 20
         AND ap.meanbp <= 250
            THEN ap.meanbp
        ELSE NULL
    END AS apache_meanbp

FROM tmp_eicu_stage3b_cohort AS c

LEFT JOIN eicu_crd.apacheapsvar AS ap
    ON ap.patientunitstayid = c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3b_apache_stay_idx
    ON tmp_eicu_stage3b_apache (patientunitstayid);

ANALYZE tmp_eicu_stage3b_apache;


/* =========================================================================
   8. First-24-hour laboratory variables

   Standardized eICU lab names used here:
     - platelets x 1000
     - total bilirubin
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_labs
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patientunitstayid,

    COUNT(*) FILTER (
        WHERE LOWER(TRIM(COALESCE(l.labname, ''))) = 'platelets x 1000'
          AND l.labresult > 0
          AND l.labresult <= 2000
    )::integer AS platelet_measurement_n_0_24h,

    MIN(l.labresult) FILTER (
        WHERE LOWER(TRIM(COALESCE(l.labname, ''))) = 'platelets x 1000'
          AND l.labresult > 0
          AND l.labresult <= 2000
    ) AS platelet_min_0_24h,

    COUNT(*) FILTER (
        WHERE LOWER(TRIM(COALESCE(l.labname, ''))) = 'total bilirubin'
          AND l.labresult >= 0
          AND l.labresult <= 100
    )::integer AS bilirubin_measurement_n_0_24h,

    MAX(l.labresult) FILTER (
        WHERE LOWER(TRIM(COALESCE(l.labname, ''))) = 'total bilirubin'
          AND l.labresult >= 0
          AND l.labresult <= 100
    ) AS bilirubin_max_lab_0_24h

FROM tmp_eicu_stage3b_cohort AS c

LEFT JOIN eicu_crd.lab AS l
    ON l.patientunitstayid = c.patientunitstayid
   AND l.labresultoffset BETWEEN 0 AND 1440
   AND (
          LOWER(TRIM(COALESCE(l.labname, ''))) = 'platelets x 1000'
       OR LOWER(TRIM(COALESCE(l.labname, ''))) = 'total bilirubin'
   )

GROUP BY
    c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3b_labs_stay_idx
    ON tmp_eicu_stage3b_labs (patientunitstayid);

ANALYZE tmp_eicu_stage3b_labs;


/* =========================================================================
   9. Minimum MAP during ICU hours 0-24

   Sources:
     - vitalperiodic.systemicmean (invasive)
     - vitalaperiodic.noninvasivemean (non-invasive)

   APACHE meanBP is added later only as a fallback if neither raw source is
   available.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_map
ON COMMIT PRESERVE ROWS AS
WITH invasive_map AS (
    SELECT
        c.patientunitstayid,
        COUNT(vp.systemicmean)::integer
            AS invasive_map_n_0_24h,
        MIN(vp.systemicmean)::numeric
            AS invasive_map_min_0_24h

    FROM tmp_eicu_stage3b_cohort AS c

    LEFT JOIN eicu_crd.vitalperiodic AS vp
        ON vp.patientunitstayid = c.patientunitstayid
       AND vp.observationoffset BETWEEN 0 AND 1440
       AND vp.systemicmean BETWEEN 20 AND 250

    GROUP BY
        c.patientunitstayid
),
noninvasive_map AS (
    SELECT
        c.patientunitstayid,
        COUNT(va.noninvasivemean)::integer
            AS noninvasive_map_n_0_24h,
        MIN(va.noninvasivemean)::numeric
            AS noninvasive_map_min_0_24h

    FROM tmp_eicu_stage3b_cohort AS c

    LEFT JOIN eicu_crd.vitalaperiodic AS va
        ON va.patientunitstayid = c.patientunitstayid
       AND va.observationoffset BETWEEN 0 AND 1440
       AND va.noninvasivemean BETWEEN 20 AND 250

    GROUP BY
        c.patientunitstayid
)

SELECT
    c.patientunitstayid,

    COALESCE(i.invasive_map_n_0_24h, 0)
        AS invasive_map_n_0_24h,

    i.invasive_map_min_0_24h,

    COALESCE(n.noninvasive_map_n_0_24h, 0)
        AS noninvasive_map_n_0_24h,

    n.noninvasive_map_min_0_24h,

    CASE
        WHEN i.invasive_map_min_0_24h IS NOT NULL
         AND n.noninvasive_map_min_0_24h IS NOT NULL
            THEN LEAST(
                    i.invasive_map_min_0_24h,
                    n.noninvasive_map_min_0_24h
                 )

        ELSE COALESCE(
                 i.invasive_map_min_0_24h,
                 n.noninvasive_map_min_0_24h
             )
    END AS raw_map_min_0_24h

FROM tmp_eicu_stage3b_cohort AS c

LEFT JOIN invasive_map AS i
    ON i.patientunitstayid = c.patientunitstayid

LEFT JOIN noninvasive_map AS n
    ON n.patientunitstayid = c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3b_map_stay_idx
    ON tmp_eicu_stage3b_map (patientunitstayid);

ANALYZE tmp_eicu_stage3b_map;


/* =========================================================================
   10. Relevant vasoactive infusion records during ICU hours 0-24

   drugRate is varchar in eICU. Units are normally embedded in drugName.
   We retain the original fields and parse numeric values conservatively.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_vaso_raw
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patientunitstayid,

    id.infusiondrugid,
    id.infusionoffset,
    id.drugname,
    id.drugrate,
    id.infusionrate,
    id.patientweight,

    CASE
        WHEN LOWER(COALESCE(id.drugname, '')) LIKE '%norepinephrine%'
          OR LOWER(COALESCE(id.drugname, '')) LIKE '%noradrenaline%'
          OR LOWER(COALESCE(id.drugname, '')) LIKE '%levophed%'
            THEN 'norepinephrine'

        WHEN (
                 LOWER(COALESCE(id.drugname, '')) LIKE '%epinephrine%'
              OR LOWER(COALESCE(id.drugname, '')) LIKE '%adrenaline%'
             )
         AND LOWER(COALESCE(id.drugname, '')) NOT LIKE '%norepinephrine%'
         AND LOWER(COALESCE(id.drugname, '')) NOT LIKE '%noradrenaline%'
            THEN 'epinephrine'

        WHEN LOWER(COALESCE(id.drugname, '')) LIKE '%dopamine%'
            THEN 'dopamine'

        WHEN LOWER(COALESCE(id.drugname, '')) LIKE '%dobutamine%'
            THEN 'dobutamine'

        ELSE NULL
    END AS sofa_drug,

    CASE
        WHEN TRIM(COALESCE(id.drugrate, ''))
                 ~ '^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$'
            THEN TRIM(id.drugrate)::numeric
        ELSE NULL
    END AS drugrate_numeric,

    CASE
        WHEN TRIM(COALESCE(id.infusionrate, ''))
                 ~ '^[+-]?([0-9]+([.][0-9]*)?|[.][0-9]+)$'
            THEN TRIM(id.infusionrate)::numeric
        ELSE NULL
    END AS infusionrate_numeric,

    CASE
        WHEN TRIM(COALESCE(id.patientweight, ''))
                 ~ '^[+]?[0-9]+([.][0-9]+)?$'
         AND TRIM(id.patientweight)::numeric BETWEEN 20 AND 400
            THEN TRIM(id.patientweight)::numeric
        ELSE NULL
    END AS patientweight_kg

FROM tmp_eicu_stage3b_cohort AS c

INNER JOIN eicu_crd.infusiondrug AS id
    ON id.patientunitstayid = c.patientunitstayid
   AND id.infusionoffset BETWEEN 0 AND 1440

WHERE
       LOWER(COALESCE(id.drugname, '')) LIKE '%norepinephrine%'
    OR LOWER(COALESCE(id.drugname, '')) LIKE '%noradrenaline%'
    OR LOWER(COALESCE(id.drugname, '')) LIKE '%levophed%'
    OR LOWER(COALESCE(id.drugname, '')) LIKE '%epinephrine%'
    OR LOWER(COALESCE(id.drugname, '')) LIKE '%adrenaline%'
    OR LOWER(COALESCE(id.drugname, '')) LIKE '%dopamine%'
    OR LOWER(COALESCE(id.drugname, '')) LIKE '%dobutamine%';

CREATE INDEX tmp_eicu_stage3b_vaso_raw_stay_idx
    ON tmp_eicu_stage3b_vaso_raw (patientunitstayid, infusionoffset);

ANALYZE tmp_eicu_stage3b_vaso_raw;


/* =========================================================================
   11. Normalize vasoactive rates to mcg/kg/min when possible

   Common unit patterns handled from drugName:
     mcg/kg/min
     mcg/min
     mg/kg/min
     mg/min
     mcg/kg/hr or mcg/kg/hour
     mcg/hr or mcg/hour
     mg/kg/hr or mg/kg/hour
     mg/hr or mg/hour

   For non-weight-normalized rates, patientWeight is required.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_vaso_normalized
ON COMMIT PRESERVE ROWS AS
SELECT
    v.*,

    CASE
        WHEN v.sofa_drug IS NULL
            THEN NULL

        WHEN v.drugrate_numeric IS NOT NULL
         AND v.drugrate_numeric > 0
            THEN 1

        WHEN v.drugrate_numeric IS NULL
         AND v.infusionrate_numeric IS NOT NULL
         AND v.infusionrate_numeric > 0
            THEN 1

        ELSE 0
    END::integer AS active_infusion_flag,

    CASE
        WHEN v.drugrate_numeric IS NULL
          OR v.drugrate_numeric <= 0
            THEN NULL

        /* already mcg/kg/min */
        WHEN LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*kg[ ]*/[ ]*(min|minute)'
            THEN v.drugrate_numeric

        /* mcg/min -> divide by kg */
        WHEN LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*(min|minute)'
         AND v.patientweight_kg IS NOT NULL
            THEN v.drugrate_numeric / v.patientweight_kg

        /* mg/kg/min -> mcg/kg/min */
        WHEN LOWER(v.drugname) ~ 'mg[ ]*/[ ]*kg[ ]*/[ ]*(min|minute)'
            THEN v.drugrate_numeric * 1000.0

        /* mg/min -> mcg/kg/min */
        WHEN LOWER(v.drugname) ~ 'mg[ ]*/[ ]*(min|minute)'
         AND v.patientweight_kg IS NOT NULL
            THEN v.drugrate_numeric * 1000.0 / v.patientweight_kg

        /* mcg/kg/hr -> mcg/kg/min */
        WHEN LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*kg[ ]*/[ ]*(hr|hour)'
            THEN v.drugrate_numeric / 60.0

        /* mcg/hr -> mcg/kg/min */
        WHEN LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*(hr|hour)'
         AND v.patientweight_kg IS NOT NULL
            THEN v.drugrate_numeric / 60.0 / v.patientweight_kg

        /* mg/kg/hr -> mcg/kg/min */
        WHEN LOWER(v.drugname) ~ 'mg[ ]*/[ ]*kg[ ]*/[ ]*(hr|hour)'
            THEN v.drugrate_numeric * 1000.0 / 60.0

        /* mg/hr -> mcg/kg/min */
        WHEN LOWER(v.drugname) ~ 'mg[ ]*/[ ]*(hr|hour)'
         AND v.patientweight_kg IS NOT NULL
            THEN v.drugrate_numeric * 1000.0 / 60.0 / v.patientweight_kg

        ELSE NULL
    END AS rate_mcg_kg_min,

    CASE
        WHEN v.drugrate_numeric IS NOT NULL
         AND v.drugrate_numeric > 0
         AND (
                LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*kg[ ]*/[ ]*(min|minute)'
             OR LOWER(v.drugname) ~ 'mg[ ]*/[ ]*kg[ ]*/[ ]*(min|minute)'
             OR LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*kg[ ]*/[ ]*(hr|hour)'
             OR LOWER(v.drugname) ~ 'mg[ ]*/[ ]*kg[ ]*/[ ]*(hr|hour)'
             OR (
                    LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*(min|minute)'
                    AND v.patientweight_kg IS NOT NULL
                )
             OR (
                    LOWER(v.drugname) ~ 'mg[ ]*/[ ]*(min|minute)'
                    AND v.patientweight_kg IS NOT NULL
                )
             OR (
                    LOWER(v.drugname) ~ 'mcg[ ]*/[ ]*(hr|hour)'
                    AND v.patientweight_kg IS NOT NULL
                )
             OR (
                    LOWER(v.drugname) ~ 'mg[ ]*/[ ]*(hr|hour)'
                    AND v.patientweight_kg IS NOT NULL
                )
             )
            THEN 1
        ELSE 0
    END::integer AS rate_normalized_flag

FROM tmp_eicu_stage3b_vaso_raw AS v;

CREATE INDEX tmp_eicu_stage3b_vaso_normalized_stay_idx
    ON tmp_eicu_stage3b_vaso_normalized (patientunitstayid, sofa_drug);

ANALYZE tmp_eicu_stage3b_vaso_normalized;


/* =========================================================================
   12. Collapse vasoactive records to one row per stay
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_vaso_summary
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patientunitstayid,

    COUNT(v.infusiondrugid)::integer
        AS sofa_vaso_record_n_0_24h,

    COUNT(*) FILTER (
        WHERE v.active_infusion_flag = 1
    )::integer AS sofa_vaso_active_record_n_0_24h,

    COUNT(*) FILTER (
        WHERE v.active_infusion_flag = 1
          AND v.rate_normalized_flag = 1
    )::integer AS sofa_vaso_rate_normalized_record_n_0_24h,

    COUNT(*) FILTER (
        WHERE v.active_infusion_flag = 1
          AND COALESCE(v.rate_normalized_flag, 0) = 0
    )::integer AS sofa_vaso_rate_unresolved_active_n_0_24h,

    MAX(
        CASE
            WHEN v.sofa_drug = 'dopamine'
             AND v.active_infusion_flag = 1
                THEN 1 ELSE 0
        END
    )::integer AS dopamine_any_0_24h,

    MAX(
        CASE
            WHEN v.sofa_drug = 'dobutamine'
             AND v.active_infusion_flag = 1
                THEN 1 ELSE 0
        END
    )::integer AS dobutamine_any_0_24h,

    MAX(
        CASE
            WHEN v.sofa_drug = 'epinephrine'
             AND v.active_infusion_flag = 1
                THEN 1 ELSE 0
        END
    )::integer AS epinephrine_any_0_24h,

    MAX(
        CASE
            WHEN v.sofa_drug = 'norepinephrine'
             AND v.active_infusion_flag = 1
                THEN 1 ELSE 0
        END
    )::integer AS norepinephrine_any_0_24h,

    MAX(v.rate_mcg_kg_min) FILTER (
        WHERE v.sofa_drug = 'dopamine'
          AND v.active_infusion_flag = 1
    ) AS dopamine_max_mcg_kg_min_0_24h,

    MAX(v.rate_mcg_kg_min) FILTER (
        WHERE v.sofa_drug = 'dobutamine'
          AND v.active_infusion_flag = 1
    ) AS dobutamine_max_mcg_kg_min_0_24h,

    MAX(v.rate_mcg_kg_min) FILTER (
        WHERE v.sofa_drug = 'epinephrine'
          AND v.active_infusion_flag = 1
    ) AS epinephrine_max_mcg_kg_min_0_24h,

    MAX(v.rate_mcg_kg_min) FILTER (
        WHERE v.sofa_drug = 'norepinephrine'
          AND v.active_infusion_flag = 1
    ) AS norepinephrine_max_mcg_kg_min_0_24h

FROM tmp_eicu_stage3b_cohort AS c

LEFT JOIN tmp_eicu_stage3b_vaso_normalized AS v
    ON v.patientunitstayid = c.patientunitstayid

GROUP BY
    c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3b_vaso_summary_stay_idx
    ON tmp_eicu_stage3b_vaso_summary (patientunitstayid);

ANALYZE tmp_eicu_stage3b_vaso_summary;


/* =========================================================================
   13. Assemble underlying component variables
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_components
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patient_key,
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.patientunitstayid,

    c.hospitalid,
    c.age,
    c.gender,

    1::integer AS eligible_at_60h_flag,
    c.first_24h_end_offset,
    c.landmark_60h_offset,

    /* Respiratory */
    ap.apache_pao2,
    ap.apache_fio2_raw,
    ap.apache_fio2_fraction,
    ap.apache_pao2_fio2_ratio,
    ap.intubated AS apache_intubated_at_worst_abg,

    /* Coagulation */
    COALESCE(l.platelet_measurement_n_0_24h, 0)
        AS platelet_measurement_n_0_24h,
    l.platelet_min_0_24h,

    /* Liver */
    COALESCE(l.bilirubin_measurement_n_0_24h, 0)
        AS bilirubin_measurement_n_0_24h,
    l.bilirubin_max_lab_0_24h,
    ap.apache_bilirubin,

    CASE
        WHEN l.bilirubin_max_lab_0_24h IS NOT NULL
         AND ap.apache_bilirubin IS NOT NULL
            THEN GREATEST(
                    l.bilirubin_max_lab_0_24h,
                    ap.apache_bilirubin
                 )

        ELSE COALESCE(
                 l.bilirubin_max_lab_0_24h,
                 ap.apache_bilirubin
             )
    END AS bilirubin_max_0_24h,

    CASE
        WHEN l.bilirubin_max_lab_0_24h IS NOT NULL
            THEN 'lab_0_24h'
        WHEN ap.apache_bilirubin IS NOT NULL
            THEN 'apacheapsvar_fallback'
        ELSE 'missing'
    END AS bilirubin_source,

    /* Cardiovascular */
    COALESCE(m.invasive_map_n_0_24h, 0)
        AS invasive_map_n_0_24h,
    m.invasive_map_min_0_24h,

    COALESCE(m.noninvasive_map_n_0_24h, 0)
        AS noninvasive_map_n_0_24h,
    m.noninvasive_map_min_0_24h,

    m.raw_map_min_0_24h,
    ap.apache_meanbp,

    CASE
        WHEN m.raw_map_min_0_24h IS NOT NULL
            THEN m.raw_map_min_0_24h
        ELSE ap.apache_meanbp
    END AS map_min_0_24h,

    CASE
        WHEN m.raw_map_min_0_24h IS NOT NULL
            THEN 'raw_vital_0_24h'
        WHEN ap.apache_meanbp IS NOT NULL
            THEN 'apacheapsvar_fallback'
        ELSE 'missing'
    END AS map_source,

    COALESCE(v.sofa_vaso_record_n_0_24h, 0)
        AS sofa_vaso_record_n_0_24h,
    COALESCE(v.sofa_vaso_active_record_n_0_24h, 0)
        AS sofa_vaso_active_record_n_0_24h,
    COALESCE(v.sofa_vaso_rate_normalized_record_n_0_24h, 0)
        AS sofa_vaso_rate_normalized_record_n_0_24h,
    COALESCE(v.sofa_vaso_rate_unresolved_active_n_0_24h, 0)
        AS sofa_vaso_rate_unresolved_active_n_0_24h,

    COALESCE(v.dopamine_any_0_24h, 0)
        AS dopamine_any_0_24h,
    COALESCE(v.dobutamine_any_0_24h, 0)
        AS dobutamine_any_0_24h,
    COALESCE(v.epinephrine_any_0_24h, 0)
        AS epinephrine_any_0_24h,
    COALESCE(v.norepinephrine_any_0_24h, 0)
        AS norepinephrine_any_0_24h,

    v.dopamine_max_mcg_kg_min_0_24h,
    v.dobutamine_max_mcg_kg_min_0_24h,
    v.epinephrine_max_mcg_kg_min_0_24h,
    v.norepinephrine_max_mcg_kg_min_0_24h,

    /* CNS */
    ap.eyes AS apache_gcs_eyes,
    ap.motor AS apache_gcs_motor,
    ap.verbal AS apache_gcs_verbal,
    ap.meds AS apache_gcs_meds,
    ap.apache_gcs,
    ap.apache_gcs_unscorable_due_to_meds_flag,

    /* APACHE table availability */
    CASE
        WHEN ap.apacheapsvarid IS NOT NULL THEN 1
        ELSE 0
    END::integer AS apacheapsvar_available_flag

FROM tmp_eicu_stage3b_cohort AS c

LEFT JOIN tmp_eicu_stage3b_apache AS ap
    ON ap.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage3b_labs AS l
    ON l.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage3b_map AS m
    ON m.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage3b_vaso_summary AS v
    ON v.patientunitstayid = c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3b_components_stay_idx
    ON tmp_eicu_stage3b_components (patientunitstayid);

ANALYZE tmp_eicu_stage3b_components;


/* =========================================================================
   14. Calculate raw SOFA component scores

   Raw scores remain NULL if component data are unavailable.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_scores
ON COMMIT PRESERVE ROWS AS
SELECT
    x.*,

    /* -------------------------------------------------------------
       Respiratory SOFA
       ------------------------------------------------------------- */
    CASE
        WHEN x.apache_pao2_fio2_ratio IS NULL
            THEN NULL

        WHEN COALESCE(x.apache_intubated_at_worst_abg, 0) = 1
         AND x.apache_pao2_fio2_ratio < 100
            THEN 4

        WHEN COALESCE(x.apache_intubated_at_worst_abg, 0) = 1
         AND x.apache_pao2_fio2_ratio < 200
            THEN 3

        WHEN x.apache_pao2_fio2_ratio < 300
            THEN 2

        WHEN x.apache_pao2_fio2_ratio < 400
            THEN 1

        ELSE 0
    END::integer AS respiratory_sofa_24h_raw,

    /* -------------------------------------------------------------
       Coagulation SOFA
       ------------------------------------------------------------- */
    CASE
        WHEN x.platelet_min_0_24h IS NULL
            THEN NULL
        WHEN x.platelet_min_0_24h < 20
            THEN 4
        WHEN x.platelet_min_0_24h < 50
            THEN 3
        WHEN x.platelet_min_0_24h < 100
            THEN 2
        WHEN x.platelet_min_0_24h < 150
            THEN 1
        ELSE 0
    END::integer AS coagulation_sofa_24h_raw,

    /* -------------------------------------------------------------
       Liver SOFA
       ------------------------------------------------------------- */
    CASE
        WHEN x.bilirubin_max_0_24h IS NULL
            THEN NULL
        WHEN x.bilirubin_max_0_24h >= 12
            THEN 4
        WHEN x.bilirubin_max_0_24h >= 6
            THEN 3
        WHEN x.bilirubin_max_0_24h >= 2
            THEN 2
        WHEN x.bilirubin_max_0_24h >= 1.2
            THEN 1
        ELSE 0
    END::integer AS liver_sofa_24h_raw,

    /* -------------------------------------------------------------
       Cardiovascular SOFA

       If epi/norepi is active but its rate is unresolved, any such drug
       still implies at least SOFA 3.

       If dopamine is active but its rate is unresolved, it implies at least
       SOFA 2; SOFA 3/4 requires a normalized dose.
       ------------------------------------------------------------- */
    CASE
        WHEN COALESCE(x.dopamine_max_mcg_kg_min_0_24h, 0) > 15
          OR COALESCE(x.epinephrine_max_mcg_kg_min_0_24h, 0) > 0.1
          OR COALESCE(x.norepinephrine_max_mcg_kg_min_0_24h, 0) > 0.1
            THEN 4

        WHEN COALESCE(x.dopamine_max_mcg_kg_min_0_24h, 0) > 5
          OR x.epinephrine_any_0_24h = 1
          OR x.norepinephrine_any_0_24h = 1
            THEN 3

        WHEN x.dopamine_any_0_24h = 1
          OR x.dobutamine_any_0_24h = 1
            THEN 2

        WHEN x.map_min_0_24h < 70
            THEN 1

        WHEN x.map_min_0_24h IS NULL
         AND x.dopamine_any_0_24h = 0
         AND x.dobutamine_any_0_24h = 0
         AND x.epinephrine_any_0_24h = 0
         AND x.norepinephrine_any_0_24h = 0
            THEN NULL

        ELSE 0
    END::integer AS cardiovascular_sofa_24h_raw,

    /* -------------------------------------------------------------
       CNS SOFA
       ------------------------------------------------------------- */
    CASE
        WHEN x.apache_gcs IS NULL
            THEN NULL
        WHEN x.apache_gcs < 6
            THEN 4
        WHEN x.apache_gcs BETWEEN 6 AND 9
            THEN 3
        WHEN x.apache_gcs BETWEEN 10 AND 12
            THEN 2
        WHEN x.apache_gcs BETWEEN 13 AND 14
            THEN 1
        ELSE 0
    END::integer AS cns_sofa_24h_raw

FROM tmp_eicu_stage3b_components AS x;

CREATE UNIQUE INDEX tmp_eicu_stage3b_scores_stay_idx
    ON tmp_eicu_stage3b_scores (patientunitstayid);

ANALYZE tmp_eicu_stage3b_scores;


/* =========================================================================
   15. Final nonrenal SOFA and QC flags
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3b_final
ON COMMIT PRESERVE ROWS AS
SELECT
    s.*,

    /* MIMIC-compatible final component handling: missing -> 0 */
    COALESCE(s.respiratory_sofa_24h_raw, 0)::integer
        AS respiratory_sofa_24h,

    COALESCE(s.coagulation_sofa_24h_raw, 0)::integer
        AS coagulation_sofa_24h,

    COALESCE(s.liver_sofa_24h_raw, 0)::integer
        AS liver_sofa_24h,

    COALESCE(s.cardiovascular_sofa_24h_raw, 0)::integer
        AS cardiovascular_sofa_24h,

    COALESCE(s.cns_sofa_24h_raw, 0)::integer
        AS cns_sofa_24h,

    (
          COALESCE(s.respiratory_sofa_24h_raw, 0)
        + COALESCE(s.coagulation_sofa_24h_raw, 0)
        + COALESCE(s.liver_sofa_24h_raw, 0)
        + COALESCE(s.cardiovascular_sofa_24h_raw, 0)
        + COALESCE(s.cns_sofa_24h_raw, 0)
    )::integer AS standard_nonrenal_sofa_24h,

    /* Number of components with directly evaluable source data */
    (
          CASE WHEN s.respiratory_sofa_24h_raw IS NOT NULL THEN 1 ELSE 0 END
        + CASE WHEN s.coagulation_sofa_24h_raw IS NOT NULL THEN 1 ELSE 0 END
        + CASE WHEN s.liver_sofa_24h_raw IS NOT NULL THEN 1 ELSE 0 END
        + CASE WHEN s.cardiovascular_sofa_24h_raw IS NOT NULL THEN 1 ELSE 0 END
        + CASE WHEN s.cns_sofa_24h_raw IS NOT NULL THEN 1 ELSE 0 END
    )::integer AS nonrenal_sofa_component_available_n,

    CASE
        WHEN s.respiratory_sofa_24h_raw IS NOT NULL
         AND s.coagulation_sofa_24h_raw IS NOT NULL
         AND s.liver_sofa_24h_raw IS NOT NULL
         AND s.cardiovascular_sofa_24h_raw IS NOT NULL
         AND s.cns_sofa_24h_raw IS NOT NULL
            THEN 1
        ELSE 0
    END::integer AS nonrenal_sofa_all_components_available_flag,

    /* Component-specific missingness */
    CASE
        WHEN s.respiratory_sofa_24h_raw IS NULL THEN 1 ELSE 0
    END::integer AS respiratory_sofa_missing_flag,

    CASE
        WHEN s.coagulation_sofa_24h_raw IS NULL THEN 1 ELSE 0
    END::integer AS coagulation_sofa_missing_flag,

    CASE
        WHEN s.liver_sofa_24h_raw IS NULL THEN 1 ELSE 0
    END::integer AS liver_sofa_missing_flag,

    CASE
        WHEN s.cardiovascular_sofa_24h_raw IS NULL THEN 1 ELSE 0
    END::integer AS cardiovascular_sofa_missing_flag,

    CASE
        WHEN s.cns_sofa_24h_raw IS NULL THEN 1 ELSE 0
    END::integer AS cns_sofa_missing_flag,

    /*
    The primary numeric score remains available for every cohort member because
    missing raw components are assigned 0 as in the standard MIMIC SOFA score.
    */
    1::integer AS stage3b_primary_covariate_complete_flag,

    CASE
        WHEN s.sofa_vaso_rate_unresolved_active_n_0_24h > 0
            THEN 1
        ELSE 0
    END::integer AS sofa_vaso_rate_unresolved_flag

FROM tmp_eicu_stage3b_scores AS s;

CREATE UNIQUE INDEX tmp_eicu_stage3b_final_stay_idx
    ON tmp_eicu_stage3b_final (patientunitstayid);

ANALYZE tmp_eicu_stage3b_final;

-- 5. Independent arithmetic, component-availability, source and range QC.
CREATE TEMP TABLE tmp_eicu05_qc
ON COMMIT PRESERVE ROWS AS
WITH independent_qc AS (
  SELECT
    f.*,
    c.hospitaladmitoffset AS index_hospitaladmitoffset,
    c.hospitaldischargeoffset AS index_hospitaldischargeoffset,
    c.hospitaldischargestatus AS index_hospitaldischargestatus,
    c.unitdischargeoffset AS index_unitdischargeoffset,
    (
      COALESCE(f.respiratory_sofa_24h_raw,0)
      + COALESCE(f.coagulation_sofa_24h_raw,0)
      + COALESCE(f.liver_sofa_24h_raw,0)
      + COALESCE(f.cardiovascular_sofa_24h_raw,0)
      + COALESCE(f.cns_sofa_24h_raw,0)
    )::integer AS qc_nonrenal_sum5_from_raw_24h,
    (
      f.respiratory_sofa_24h
      + f.coagulation_sofa_24h
      + f.liver_sofa_24h
      + f.cardiovascular_sofa_24h
      + f.cns_sofa_24h
    )::integer AS qc_nonrenal_sum5_from_scored_24h,
    (
      (f.respiratory_sofa_24h_raw IS NOT NULL)::integer
      + (f.coagulation_sofa_24h_raw IS NOT NULL)::integer
      + (f.liver_sofa_24h_raw IS NOT NULL)::integer
      + (f.cardiovascular_sofa_24h_raw IS NOT NULL)::integer
      + (f.cns_sofa_24h_raw IS NOT NULL)::integer
    )::integer AS qc_independent_observed_component_n
  FROM tmp_eicu_stage3b_final AS f
  INNER JOIN tmp_eicu_stage3b_cohort AS c
    ON c.patientunitstayid = f.patientunitstayid
)
SELECT
  q.*,
  CASE WHEN q.standard_nonrenal_sofa_24h IS DISTINCT FROM
                 q.qc_nonrenal_sum5_from_raw_24h
    THEN 1 ELSE 0 END::integer AS qc_raw_sum5_mismatch_flag,
  CASE WHEN q.standard_nonrenal_sofa_24h IS DISTINCT FROM
                 q.qc_nonrenal_sum5_from_scored_24h
    THEN 1 ELSE 0 END::integer AS qc_scored_sum5_mismatch_flag,
  CASE WHEN q.respiratory_sofa_24h IS DISTINCT FROM
                 COALESCE(q.respiratory_sofa_24h_raw,0)
         OR q.coagulation_sofa_24h IS DISTINCT FROM
                 COALESCE(q.coagulation_sofa_24h_raw,0)
         OR q.liver_sofa_24h IS DISTINCT FROM
                 COALESCE(q.liver_sofa_24h_raw,0)
         OR q.cardiovascular_sofa_24h IS DISTINCT FROM
                 COALESCE(q.cardiovascular_sofa_24h_raw,0)
         OR q.cns_sofa_24h IS DISTINCT FROM
                 COALESCE(q.cns_sofa_24h_raw,0)
    THEN 1 ELSE 0 END::integer AS qc_zero_imputation_mismatch_flag,
  CASE WHEN q.nonrenal_sofa_component_available_n IS DISTINCT FROM
                 q.qc_independent_observed_component_n
         OR q.nonrenal_sofa_all_components_available_flag IS DISTINCT FROM
                 (q.qc_independent_observed_component_n=5)::integer
         OR q.respiratory_sofa_missing_flag IS DISTINCT FROM
                 (q.respiratory_sofa_24h_raw IS NULL)::integer
         OR q.coagulation_sofa_missing_flag IS DISTINCT FROM
                 (q.coagulation_sofa_24h_raw IS NULL)::integer
         OR q.liver_sofa_missing_flag IS DISTINCT FROM
                 (q.liver_sofa_24h_raw IS NULL)::integer
         OR q.cardiovascular_sofa_missing_flag IS DISTINCT FROM
                 (q.cardiovascular_sofa_24h_raw IS NULL)::integer
         OR q.cns_sofa_missing_flag IS DISTINCT FROM
                 (q.cns_sofa_24h_raw IS NULL)::integer
    THEN 1 ELSE 0 END::integer AS qc_availability_flags_mismatch_flag,
  CASE WHEN q.sofa_vaso_rate_unresolved_flag IS DISTINCT FROM
                 (q.sofa_vaso_rate_unresolved_active_n_0_24h > 0)::integer
    THEN 1 ELSE 0 END::integer AS qc_vaso_unresolved_flag_mismatch_flag,
  CASE WHEN q.standard_nonrenal_sofa_24h IS NULL
              OR q.standard_nonrenal_sofa_24h NOT BETWEEN 0 AND 20
              OR q.nonrenal_sofa_component_available_n NOT BETWEEN 0 AND 5
              OR EXISTS (
                   SELECT 1 FROM UNNEST(ARRAY[
                     q.respiratory_sofa_24h_raw,
                     q.coagulation_sofa_24h_raw,
                     q.liver_sofa_24h_raw,
                     q.cardiovascular_sofa_24h_raw,
                     q.cns_sofa_24h_raw,
                     q.respiratory_sofa_24h,
                     q.coagulation_sofa_24h,
                     q.liver_sofa_24h,
                     q.cardiovascular_sofa_24h,
                     q.cns_sofa_24h
                   ]::integer[]) AS v(score)
                   WHERE v.score NOT BETWEEN 0 AND 4
              )
    THEN 1 ELSE 0 END::integer AS qc_sofa_score_out_of_range_flag
FROM independent_qc AS q;

CREATE UNIQUE INDEX tmp_eicu05_qc_stay_idx ON tmp_eicu05_qc(patientunitstayid);
ANALYZE tmp_eicu05_qc;

/* =========================================================================
   16. Reviewer component-level QC summary (no reference fingerprints)
   ========================================================================= */
CREATE TEMP TABLE tmp_eicu05_summary
ON COMMIT PRESERVE ROWS AS
SELECT
    COUNT(*)::integer AS qc_cohort_n,
    COUNT(DISTINCT patientunitstayid)::integer AS qc_distinct_stays_n,
    SUM(qc_raw_sum5_mismatch_flag)::integer AS qc_raw_sum5_mismatch_n,
    SUM(qc_scored_sum5_mismatch_flag)::integer AS qc_scored_sum5_mismatch_n,
    SUM(qc_zero_imputation_mismatch_flag)::integer AS qc_zero_imputation_mismatch_n,
    SUM(qc_availability_flags_mismatch_flag)::integer AS qc_availability_flags_mismatch_n,
    SUM(qc_vaso_unresolved_flag_mismatch_flag)::integer AS qc_vaso_unresolved_flag_mismatch_n,
    SUM(qc_sofa_score_out_of_range_flag)::integer AS qc_sofa_score_out_of_range_n,
    SUM(respiratory_sofa_missing_flag)::integer AS qc_resp_raw_missing_n,
    SUM(coagulation_sofa_missing_flag)::integer AS qc_coag_raw_missing_n,
    SUM(liver_sofa_missing_flag)::integer AS qc_liver_raw_missing_n,
    SUM(cardiovascular_sofa_missing_flag)::integer AS qc_cardio_raw_missing_n,
    SUM(cns_sofa_missing_flag)::integer AS qc_cns_raw_missing_n,
    SUM(nonrenal_sofa_all_components_available_flag)::integer
        AS qc_five_raw_components_available_n,
    SUM(sofa_vaso_rate_unresolved_flag)::integer
        AS qc_vaso_unresolved_patients_n,
    SUM(apacheapsvar_available_flag)::integer AS qc_apache_available_n,
    SUM(stage3b_primary_covariate_complete_flag)::integer
        AS qc_primary_covariate_complete_flag_n
FROM tmp_eicu05_qc;

/* =========================================================================
   17. Execution QC: show discrepancies, never alter the cohort or scores
   ========================================================================= */
DO $$
DECLARE
    v_count integer;
    v_unique integer;
    v_qc_errors integer;
BEGIN
    SELECT
        qc_cohort_n,
        qc_distinct_stays_n,
        qc_raw_sum5_mismatch_n
            + qc_scored_sum5_mismatch_n
            + qc_zero_imputation_mismatch_n
            + qc_availability_flags_mismatch_n
            + qc_vaso_unresolved_flag_mismatch_n
            + qc_sofa_score_out_of_range_n
    INTO v_count, v_unique, v_qc_errors
    FROM tmp_eicu05_summary;

    IF v_count <> v_unique THEN
        RAISE EXCEPTION
            'eICU 05: % rows but % distinct patientunitstayid.',
            v_count, v_unique;
    END IF;

    IF v_count <> 8495 THEN
        RAISE WARNING
            'eICU 05: % landmark stays (manuscript reference: 8495).',
            v_count;
    END IF;

    IF v_qc_errors <> 0 THEN
        RAISE WARNING
            'eICU 05: % component/QC deviations; inspect output flags.',
            v_qc_errors;
    END IF;
END $$;

/* =========================================================================
   FINAL EXPORT: Stage 3B output + reviewer QC + QC summary.
   Exactly one row per index ICU stay; no additional exclusions applied.
   ========================================================================= */
SELECT q.*, s.*
FROM tmp_eicu05_qc AS q
CROSS JOIN tmp_eicu05_summary AS s
ORDER BY q.patient_key, q.patienthealthsystemstayid, q.patientunitstayid;
