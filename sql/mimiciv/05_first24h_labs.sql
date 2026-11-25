/*
MIMIC-IV v3.1 | PostgreSQL | Public reproducibility script
05_first24h_labs.sql

Purpose
-------
Extract the following descriptive first-24-hour laboratory variables from
exactly the same first-qualifying-ICU, 60-h-landmark COPD cohort used in the
original analysis:
  * First nonmissing hemoglobin value in ICU hours 0-24
  * First nonmissing blood urea nitrogen (BUN) value in ICU hours 0-24
  * Earliest complete arterial blood gas (pH and pCO2 from the same row)

Provenance
----------
The complete executable SQL query and its output column names below are
preserved from the investigators' archived original source:
MIMIC_first24h_Hb_BUN_ABG_extract_optimized_v3.sql
This public-named version only adds documentation; no study definition or
measurement selection rule was changed. ABG variables are retained although
the final main manuscript baseline table reports Hb and BUN.

Dependencies
------------
mimiciv_hosp.diagnoses_icd, mimiciv_hosp.admissions,
mimiciv_icu.icustays, mimiciv_derived.age,
mimiciv_derived.complete_blood_count, mimiciv_derived.chemistry,
mimiciv_derived.bg.
The mimiciv_derived concept tables must be installed independently.

Execution and output
--------------------
This is a standalone PostgreSQL SELECT; it re-creates the 60-h-landmark
cohort directly, with no fixed ID list and no dependency on an output table
from 01/02/03/04. Execute the entire script and merge with other MIMIC
extracts on stay_id. Missing measurements remain NULL. Sampling timestamps
are included for quality control. The original uses MATERIALIZED CTEs and
DISTINCT ON, which are PostgreSQL-specific.

Suggested local validation (NOT performed here)
------------------------------------------------
  1. Rows = distinct stay_id = 7,794 for the paper's data and filters.
  2. Compare stay_id membership with the eligible-at-60-h subset of 01 and
     all landmark-eligible rows from 02.
  3. Compare per-stay hemoglobin, BUN, arterial pH, PaCO2 and measurement
     timestamps with the saved original export.
  4. Where timestamps tie, review deterministic selection; the original
     uses charttime and specimen_id for Hb and BUN, and charttime for ABG.

Restricted-data reminder
------------------------
The source is credentialed MIMIC-IV data. Publicly share code only, not
patient/stay-level query results or identifier-bearing exports.

Validation status: static code check only; NOT executed on the MIMIC database.
*/

/*
MIMIC-IV v3.1
Optimized first-24-h laboratory / arterial blood gas extraction
PostgreSQL dialect

Purpose
-------
Using the SAME COPD cohort-selection and ICU +60-h landmark logic as the
provided extraction code, extract ONLY four descriptive first-24-h variables:

  1. Hemoglobin: first valid measurement during ICU hours 0-24
  2. BUN:        first valid measurement during ICU hours 0-24
  3. Arterial pH: earliest complete arterial blood gas during ICU hours 0-24
  4. Arterial PaCO2: from the SAME earliest arterial blood-gas specimen as pH

No SOFA variables are calculated or returned.
No AKI trajectory variables are calculated here; merge later by stay_id.

Performance changes versus v1
-----------------------------
1. DO NOT reconstruct all blood-gas specimens from mimiciv_hosp.labevents.
   Instead, use the pre-pivoted official concept table mimiciv_derived.bg,
   which already contains one row per internally grouped blood-gas specimen with specimen, pH, and pCO2.
   Note: the public mimiciv_derived.bg output does NOT expose specimen_id.

2. DO NOT run one LATERAL search per ICU stay.
   Instead, join each derived laboratory table to the small MATERIALIZED
   60-h landmark cohort once, restrict to ICU hours 0-24, and then use
   PostgreSQL DISTINCT ON (stay_id) to retain the earliest qualifying row.

Population
----------
  1. Broad COPD hospitalization
  2. Age >=18 years
  3. Exclude ESRD or chronic dialysis documented during the candidate
     index hospitalization
  4. Retain the first ICU stay meeting baseline eligibility criteria per patient
  5. AFTER stay selection, require survival and continued hospitalization
     beyond ICU +60 hours

Measurement definitions
-----------------------
Hemoglobin:
  - Source: mimiciv_derived.complete_blood_count
  - first non-null hemoglobin from ICU admission through ICU +24 h

BUN:
  - Source: mimiciv_derived.chemistry
  - first non-null BUN from ICU admission through ICU +24 h

Arterial pH + PaCO2:
  - Source: mimiciv_derived.bg
  - specimen must equal 'ART.'
  - pH and pCO2 must both be non-null on the SAME derived.bg row
  - choose the earliest qualifying arterial specimen from ICU admission
    through ICU +24 h

Important
---------
  - Expected output: one row per stay_id in the ICU +60-h landmark cohort.
  - Expected cohort size with the current MIMIC-IV definition: 7,794.
  - Missing measurements remain NULL; they are NOT imputed.
  - Measurement timestamps are retained for QC.

Primary merge key:
  stay_id

Cross-check keys:
  subject_id
  hadm_id
*/

WITH copd_hadm AS MATERIALIZED (
    SELECT DISTINCT
        d.hadm_id
    FROM mimiciv_hosp.diagnoses_icd AS d
    WHERE
        (
            d.icd_version = 9
            AND d.icd_code IN (
                '49120',
                '49121',
                '49122',
                '4918',
                '4919',
                '4928',
                '496'
            )
        )
        OR
        (
            d.icd_version = 10
            AND d.icd_code IN (
                'J430',
                'J431',
                'J432',
                'J438',
                'J439',
                'J440',
                'J441',
                'J448',
                'J449'
            )
        )
),

esrd_chronic_dialysis_hadm AS MATERIALIZED (
    SELECT DISTINCT
        d.hadm_id
    FROM mimiciv_hosp.diagnoses_icd AS d
    WHERE
        (
            d.icd_version = 9
            AND d.icd_code IN (
                '5856',
                'V4511'
            )
        )
        OR
        (
            d.icd_version = 10
            AND d.icd_code IN (
                'N186',
                'Z992'
            )
        )
),

eligible_icu_candidates AS (
    SELECT
        i.subject_id,
        i.hadm_id,
        i.stay_id,
        i.intime AS icu_intime,
        i.outtime AS icu_outtime,
        a.dischtime,
        a.deathtime,
        a.hospital_expire_flag,
        ag.age
    FROM mimiciv_icu.icustays AS i
    INNER JOIN copd_hadm AS c
        ON c.hadm_id = i.hadm_id
    INNER JOIN mimiciv_hosp.admissions AS a
        ON a.hadm_id = i.hadm_id
    INNER JOIN mimiciv_derived.age AS ag
        ON ag.hadm_id = i.hadm_id
    LEFT JOIN esrd_chronic_dialysis_hadm AS e
        ON e.hadm_id = i.hadm_id
    WHERE ag.age >= 18
      AND e.hadm_id IS NULL
),

qualifying_icu_ranked AS (
    SELECT
        e.*,
        ROW_NUMBER() OVER (
            PARTITION BY e.subject_id
            ORDER BY e.icu_intime, e.stay_id
        ) AS qualifying_icu_rn
    FROM eligible_icu_candidates AS e
),

first_qualifying_icu AS (
    SELECT
        q.subject_id,
        q.hadm_id,
        q.stay_id,
        q.icu_intime,
        q.icu_outtime,
        q.dischtime,
        q.deathtime,
        q.hospital_expire_flag
    FROM qualifying_icu_ranked AS q
    WHERE q.qualifying_icu_rn = 1
),

cohort_with_landmarks AS (
    SELECT
        f.*,
        f.icu_intime + INTERVAL '24 hour' AS landmark_24h_time,
        f.icu_intime + INTERVAL '60 hour' AS landmark_60h_time,
        CASE
            WHEN f.hospital_expire_flag = 1
                THEN COALESCE(f.deathtime, f.dischtime)
            ELSE f.dischtime
        END AS outcome_or_discharge_time
    FROM first_qualifying_icu AS f
),

/*
Small cohort first: this is the anchor for all subsequent laboratory joins.
The patient need not remain physically in the ICU at hour 60.
*/
lab_cohort AS MATERIALIZED (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,
        c.icu_intime,
        c.landmark_24h_time,
        c.landmark_60h_time
    FROM cohort_with_landmarks AS c
    WHERE c.outcome_or_discharge_time > c.landmark_60h_time
),

/* =========================================================================
   First valid hemoglobin during ICU hours 0-24.
   Set-based join: scan/join once, then keep earliest row per stay_id.
   ========================================================================= */
hemoglobin_first AS MATERIALIZED (
    SELECT DISTINCT ON (c.stay_id)
        c.stay_id,
        cbc.charttime AS hemoglobin_first_24h_time,
        cbc.hemoglobin AS hemoglobin_first_24h
    FROM lab_cohort AS c
    INNER JOIN mimiciv_derived.complete_blood_count AS cbc
        ON cbc.subject_id = c.subject_id
       AND cbc.hadm_id = c.hadm_id
       AND cbc.charttime >= c.icu_intime
       AND cbc.charttime <= c.landmark_24h_time
    WHERE cbc.hemoglobin IS NOT NULL
    ORDER BY
        c.stay_id,
        cbc.charttime,
        cbc.specimen_id
),

/* =========================================================================
   First valid BUN during ICU hours 0-24.
   ========================================================================= */
bun_first AS MATERIALIZED (
    SELECT DISTINCT ON (c.stay_id)
        c.stay_id,
        chem.charttime AS bun_first_24h_time,
        chem.bun AS bun_first_24h
    FROM lab_cohort AS c
    INNER JOIN mimiciv_derived.chemistry AS chem
        ON chem.subject_id = c.subject_id
       AND chem.hadm_id = c.hadm_id
       AND chem.charttime >= c.icu_intime
       AND chem.charttime <= c.landmark_24h_time
    WHERE chem.bun IS NOT NULL
    ORDER BY
        c.stay_id,
        chem.charttime,
        chem.specimen_id
),

/* =========================================================================
   Earliest COMPLETE arterial blood gas during ICU hours 0-24.

   mimiciv_derived.bg is internally constructed by grouping source LABEVENTS
   by specimen_id, but specimen_id is not exposed in the final derived table.
   Requiring pH and pCO2 on the same bg row still ensures both values originate
   from the same internally grouped blood-gas specimen.
   ========================================================================= */
arterial_bg_first AS MATERIALIZED (
    SELECT DISTINCT ON (c.stay_id)
        c.stay_id,
        bg.charttime AS arterial_abg_first_24h_time,
        bg.ph AS arterial_ph_first_24h,
        bg.pco2 AS arterial_pco2_first_24h
    FROM lab_cohort AS c
    INNER JOIN mimiciv_derived.bg AS bg
        ON bg.subject_id = c.subject_id
       AND bg.hadm_id = c.hadm_id
       AND bg.charttime >= c.icu_intime
       AND bg.charttime <= c.landmark_24h_time
    WHERE bg.specimen = 'ART.'
      AND bg.ph IS NOT NULL
      AND bg.pco2 IS NOT NULL
    ORDER BY
        c.stay_id,
        bg.charttime
),

final_dataset AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,
        c.icu_intime,
        c.landmark_24h_time,
        c.landmark_60h_time,
        1::integer AS eligible_at_60h_flag,

        hb.hemoglobin_first_24h,
        hb.hemoglobin_first_24h_time,

        bun.bun_first_24h,
        bun.bun_first_24h_time,

        abg.arterial_ph_first_24h,
        abg.arterial_pco2_first_24h,
        abg.arterial_abg_first_24h_time,

        (hb.hemoglobin_first_24h IS NOT NULL)::integer
            AS hemoglobin_available_0_24h,

        (bun.bun_first_24h IS NOT NULL)::integer
            AS bun_available_0_24h,

        (
            abg.arterial_ph_first_24h IS NOT NULL
            AND abg.arterial_pco2_first_24h IS NOT NULL
        )::integer AS paired_arterial_abg_available_0_24h

    FROM lab_cohort AS c
    LEFT JOIN hemoglobin_first AS hb
        ON hb.stay_id = c.stay_id
    LEFT JOIN bun_first AS bun
        ON bun.stay_id = c.stay_id
    LEFT JOIN arterial_bg_first AS abg
        ON abg.stay_id = c.stay_id
)

SELECT *
FROM final_dataset
ORDER BY subject_id, stay_id;


/*
===============================================================================
OPTIONAL QC AFTER SAVING THE RESULT AS A TABLE
===============================================================================
Replace your_saved_first24h_lab_table with your actual saved table name.

SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT stay_id) AS n_distinct_stays,

    SUM(hemoglobin_available_0_24h) AS n_hb_available,
    ROUND(
        100.0 * SUM(hemoglobin_available_0_24h) / NULLIF(COUNT(*), 0),
        1
    ) AS pct_hb_available,

    SUM(bun_available_0_24h) AS n_bun_available,
    ROUND(
        100.0 * SUM(bun_available_0_24h) / NULLIF(COUNT(*), 0),
        1
    ) AS pct_bun_available,

    SUM(paired_arterial_abg_available_0_24h) AS n_paired_abg_available,
    ROUND(
        100.0 * SUM(paired_arterial_abg_available_0_24h)
            / NULLIF(COUNT(*), 0),
        1
    ) AS pct_paired_abg_available

FROM your_saved_first24h_lab_table;

Expected cohort checks:
  n_rows = 7,794
  n_distinct_stays = 7,794

After merging by stay_id with the final primary AKI-trajectory cohort, expected
primary-analysis sample size = 7,627.

===============================================================================
OPTIONAL PERFORMANCE DIAGNOSTIC
===============================================================================
If this optimized query is still unexpectedly slow, run EXPLAIN (ANALYZE,
BUFFERS) on the SELECT and inspect whether the derived tables are being scanned
once or repeatedly. Do NOT routinely run EXPLAIN ANALYZE together with a large
CREATE TABLE statement because it executes the query.
*/
