/*
MIMIC-IV v3.1 | PostgreSQL | Public reproducibility script
06_imv_vasopressor.sql

PURPOSE
-------
Extract two binary variables for the same first-qualifying-ICU-stay,
ICU +60-hour landmark cohort as the original MIMIC-IV analysis:
  1. imv_any_0_24h         -- any invasive mechanical ventilation overlapping
                             ICU hours 0-24;
  2. vasopressor_any_0_24h -- any qualifying positive-rate vasopressor infusion
                             overlapping ICU hours 0-24.

USE IN THE STUDY
----------------
These variables are descriptive baseline characteristics and covariates for
an additional-adjustment SENSITIVITY analysis. They were not part of the
original primary adjustment model. The target cohort is 7,794 landmark-
eligible stays, BEFORE excluding cases with indeterminate AKI trajectory.

PROVENANCE AND COMPUTATION
--------------------------
The entire executable SQL below is preserved verbatim from the archived
investigator source:
    MIMIC_revised_IMV_vasopressor_0_24h.sql
Only this documentation header and the public-facing filename were added.
No organ-support definition, time-window boundary, cohort inclusion rule,
output variable name, or NULL handling was changed.

IMV requires mimiciv_derived.ventilation.ventilation_status = 'InvasiveVent'.
Vasopressor requires a positive recorded rate for dopamine, epinephrine,
norepinephrine, phenylephrine or vasopressin in the
mimiciv_derived.vasoactive_agent table. Dobutamine and milrinone alone are
not qualifying. Missing qualifying intervals are coded 0, as originally.
The source's interval-overlap predicates are retained exactly, including
starttime <= ICU +24 h and NULL endtime handling.

DATABASE DEPENDENCIES
---------------------
mimiciv_hosp.diagnoses_icd
mimiciv_hosp.admissions
mimiciv_icu.icustays
mimiciv_derived.age
mimiciv_derived.ventilation
mimiciv_derived.vasoactive_agent

The mimiciv_derived concept tables must be installed in the target
PostgreSQL environment. This SQL has not been tested against a live
MIMIC-IV instance in this build environment.

EXECUTION / DATA INTERFACE
--------------------------
This is a self-contained PostgreSQL SELECT. It re-creates the eligible
landmark cohort and DOES NOT require materialized results from
01_cohort_landmark.sql or 02_aki_trajectory.sql, or any literal stay ID list.
Execute the complete script; save its result privately and merge with the
other extraction outputs on stay_id (cross-check subject_id and hadm_id).
Do not distribute identifier-bearing extracts or restricted data.

SUGGESTED LOCAL VALIDATION (NOT YET PERFORMED)
---------------------------------------------
  - 7,794 output rows, with 7,794 distinct stay_id values.
  - Identical stay_id set to the eligible-at-60-h subset of SQL 01.
  - Identical imv_any_0_24h and vasopressor_any_0_24h to the archived
    investigator export for every stay_id, including NULL/0 patterns.
  - After merging with the 7,627-person primary analysis cohort,
    check the descriptive Table 1 counts (IMV 2,452; vasopressor 2,165).
    These are counts among the FINAL 7,627 persons, not necessarily the
    totals of this 7,794-row SQL output.

VALIDATION STATUS
-----------------
Verbatim source-body comparison and basic static checks completed;
no database execution or clinical-results equivalence test performed here.
*/

/*
MIMIC-IV v3.1
Organ-support covariates: invasive mechanical ventilation and vasopressor
exposure during ICU hours 0-24
PostgreSQL dialect

Purpose
-------
Generate two prespecified early organ-support variables for later sensitivity
adjustment:

  - imv_any_0_24h
  - vasopressor_any_0_24h

These variables are NOT part of the primary adjustment model. They are retained
for an additional-adjustment sensitivity analysis.

Population
----------
  1. Broad COPD hospitalization
  2. Age >=18 years
  3. Exclude ESRD or chronic dialysis documented during the candidate
     index hospitalization
  4. Retain the first ICU stay meeting baseline eligibility criteria per patient
  5. AFTER stay selection, require survival and continued hospitalization
     at ICU +60 hours

Exposure definitions
--------------------
IMV:
  Any interval from mimiciv_derived.ventilation overlapping ICU hours 0-24
  with ventilation_status = 'InvasiveVent'.

  The following are NOT classified as IMV:
    - NonInvasiveVent
    - HFNC
    - SupplementalOxygen
    - None
    - other noninvasive/support categories

Vasopressor:
  Any interval from mimiciv_derived.vasoactive_agent overlapping ICU hours 0-24
  with a positive recorded rate for at least one of:
    - dopamine
    - epinephrine
    - norepinephrine
    - phenylephrine
    - vasopressin

  Dobutamine and milrinone are intentionally NOT counted as vasopressors.

Important
---------
  - Exposure is defined by interval overlap with ICU hours 0-24.
  - A NULL endtime is treated as continuing beyond the 24-h window.
  - Absence of a qualifying interval is coded as 0.
  - This query does not contain AKI trajectory variables; merge later by stay_id.
  - Expected output is one row per stay_id in the ICU +60-h landmark cohort.

Expected cohort size with the current MIMIC-IV definition:
  7,794

Primary merge key:
  stay_id

Cross-check keys:
  subject_id
  hadm_id
*/


WITH copd_hadm AS (
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

esrd_chronic_dialysis_hadm AS (
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

/*
Apply age and index-hospitalization ESRD/chronic-dialysis exclusions before
ranking ICU stays. This preserves the same first-qualifying-stay logic used in
Stages 1-3.
*/
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
            ORDER BY
                e.icu_intime,
                e.stay_id
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

        f.icu_intime
            + INTERVAL '24 hour'
            AS landmark_24h_time,

        f.icu_intime
            + INTERVAL '60 hour'
            AS landmark_60h_time,

        CASE
            WHEN f.hospital_expire_flag = 1
                THEN COALESCE(
                    f.deathtime,
                    f.dischtime
                )
            ELSE f.dischtime
        END AS outcome_or_discharge_time

    FROM first_qualifying_icu AS f
),

/*
The selected patient must still be alive and hospitalized beyond ICU +60 h.
The patient does not need to remain physically in the ICU at hour 60.
*/
organ_support_cohort AS MATERIALIZED (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,

        c.icu_intime,
        c.landmark_24h_time,
        c.landmark_60h_time

    FROM cohort_with_landmarks AS c

    WHERE c.outcome_or_discharge_time
              > c.landmark_60h_time
),

/* =========================================================================
   Any invasive mechanical ventilation during ICU hours 0-24
   ========================================================================= */
imv_features AS (
    SELECT
        c.stay_id,
        1::integer AS imv_any_0_24h

    FROM organ_support_cohort AS c

    INNER JOIN mimiciv_derived.ventilation AS v
        ON v.stay_id = c.stay_id
       AND v.ventilation_status = 'InvasiveVent'

       /* Interval overlaps [ICU admission, ICU +24 h] */
       AND v.starttime <= c.landmark_24h_time
       AND COALESCE(
               v.endtime,
               c.landmark_24h_time + INTERVAL '1 second'
           ) > c.icu_intime

    GROUP BY c.stay_id
),

/* =========================================================================
   Any qualifying vasopressor during ICU hours 0-24
   ========================================================================= */
vasopressor_features AS (
    SELECT
        c.stay_id,
        1::integer AS vasopressor_any_0_24h

    FROM organ_support_cohort AS c

    INNER JOIN mimiciv_derived.vasoactive_agent AS va
        ON va.stay_id = c.stay_id

       /* Interval overlaps [ICU admission, ICU +24 h] */
       AND va.starttime <= c.landmark_24h_time
       AND COALESCE(
               va.endtime,
               c.landmark_24h_time + INTERVAL '1 second'
           ) > c.icu_intime

    WHERE
           COALESCE(va.dopamine, 0) > 0
        OR COALESCE(va.epinephrine, 0) > 0
        OR COALESCE(va.norepinephrine, 0) > 0
        OR COALESCE(va.phenylephrine, 0) > 0
        OR COALESCE(va.vasopressin, 0) > 0

    GROUP BY c.stay_id
),

final_dataset AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,

        c.icu_intime,
        c.landmark_24h_time,
        c.landmark_60h_time,

        1 AS eligible_at_60h_flag,

        COALESCE(
            imv.imv_any_0_24h,
            0
        )::integer AS imv_any_0_24h,

        COALESCE(
            va.vasopressor_any_0_24h,
            0
        )::integer AS vasopressor_any_0_24h

    FROM organ_support_cohort AS c

    LEFT JOIN imv_features AS imv
        ON imv.stay_id = c.stay_id

    LEFT JOIN vasopressor_features AS va
        ON va.stay_id = c.stay_id
)

SELECT *
FROM final_dataset

ORDER BY
    subject_id,
    stay_id;


/*
Optional post-run checks after saving the output as a table:

SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT stay_id) AS n_distinct_stays,

    SUM(imv_any_0_24h) AS n_imv_0_24h,
    ROUND(
        100.0 * SUM(imv_any_0_24h) / NULLIF(COUNT(*), 0),
        1
    ) AS pct_imv_0_24h,

    SUM(vasopressor_any_0_24h) AS n_vasopressor_0_24h,
    ROUND(
        100.0 * SUM(vasopressor_any_0_24h) / NULLIF(COUNT(*), 0),
        1
    ) AS pct_vasopressor_0_24h

FROM your_saved_organ_support_table;

Expected:
  n_rows = n_distinct_stays = 7,794
*/
