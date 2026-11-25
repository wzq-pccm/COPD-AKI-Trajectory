/*
COPD and Early AKI Trajectory in Critically Ill Patients
MIMIC-IV v3.1 | PostgreSQL | Public reproducibility code

07_serial_creatinine.sql
-------------------------
Purpose
  Extract the complete measured serum creatinine event history used in
  the revision sensitivity analyses for each ICU stay in the +60-hour
  landmark population (before AKI-trajectory classifiability exclusion).

Provenance
  Adapted from the investigators' original restricted execution SQL:
  01_MIMIC_sCr_Long_ID_Optimized_7794.sql.
  The 21-column event-level SELECT, ordering, positive-sCr condition,
  50912 itemid, time boundaries, and event flags are unchanged.
  The literal stay_id anchor and its private cohort-signature check have
  been replaced by the original Stage-2 landmark cohort CTEs, copied
  from 02_aki_trajectory.sql.

Input relations
  mimiciv_hosp.diagnoses_icd
  mimiciv_hosp.admissions
  mimiciv_hosp.labevents
  mimiciv_icu.icustays
  mimiciv_derived.age  (official derived concept; must be installed)

Population / method
  - Broad COPD ICD-9/ICD-10 hospital diagnosis; age >=18; no recorded
    ESRD or chronic dialysis; first eligible ICU stay per patient,
    selected BEFORE applying the +60-hour landmark requirement.
  - Require outcome/discharge time strictly AFTER ICU +60 hours.
  - Expected landmark population in the manuscript: 7,794 unique stays.
    The 7,627-stay primary analysis population is NOT the input here.
  - Serum creatinine: labevents.itemid=50912; charttime non-null,
    valuenum non-null and strictly positive.
  - Only the index hospitalization (matching subject_id and hadm_id).
  - Extract from LEAST(GREATEST(hospital admission, ICU intake -7 days),
    ICU intake) through ICU intake +60 hours, inclusively. The LEAST
    condition also retains early ICU events if hospital admittime is
    later than ICU intime in the source data.
  - The pre-ICU reference-window flag is restricted to
    GREATEST(hospital admittime, ICU intime -7 days) <= charttime
    < ICU intime, preserving the fixed-reference algorithm.
  - Retain cohort stays with no qualifying sCr using LEFT JOIN,
    yielding one row with has_scr_event=0 and null event columns.
  - Raw serial events, not a redefined AKI exposure. Strict-KDIGO
    temporal pairing, monitoring restrictions and threshold tests are
    performed downstream in the R sensitivity-analysis scripts.

Run instructions
  - Run the entire multi-statement script within ONE PostgreSQL session.
  - The final SELECT is the only tabular output. Export it to a local
    restricted-access file for analysis. Never publish patient-level output.
  - Required temp-table privileges. This script is standalone: it does
    NOT depend on 01_cohort_landmark.sql having saved a physical table.
  - It is expected to produce 21 columns and 7,794 distinct stay_id values,
    with multiple sCr-event rows per stay where applicable.
  - The cohort-size QA below issues a NOTICE rather than silently imposing
    any fixed list of IDs; a difference must be investigated before use.

Validation status
  Source-level/static checks completed; not executed against an authorized
  MIMIC-IV v3.1 database in this environment. Verify stay_id equivalence
  and event-level output against the restricted original before publication.
*/

-- 0. Check required source relations without returning patient-level rows.
DO $$
DECLARE rel text;
BEGIN
    FOREACH rel IN ARRAY ARRAY[
        'mimiciv_hosp.diagnoses_icd',
        'mimiciv_hosp.admissions',
        'mimiciv_hosp.labevents',
        'mimiciv_icu.icustays',
        'mimiciv_derived.age'
    ] LOOP
        IF to_regclass(rel) IS NULL THEN
            RAISE EXCEPTION 'MIMIC 07: missing required relation %', rel;
        END IF;
    END LOOP;
END $$;

DROP TABLE IF EXISTS pg_temp.tmp_mimic07_cohort;

-- 1. Regenerate the exact first-qualifying +60h landmark cohort defined
--    in 02_aki_trajectory.sql; do not impose sCr classifiability here.
CREATE TEMP TABLE tmp_mimic07_cohort
ON COMMIT PRESERVE ROWS AS
WITH copd_hadm AS (
    SELECT DISTINCT
        d.hadm_id
    FROM mimiciv_hosp.diagnoses_icd AS d
    WHERE
        (
            d.icd_version = 9
            AND d.icd_code IN (
                '49120', '49121', '49122', '4918',
                '4919', '4928', '496'
            )
        )
        OR
        (
            d.icd_version = 10
            AND d.icd_code IN (
                'J430', 'J431', 'J432', 'J438', 'J439',
                'J440', 'J441', 'J448', 'J449'
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
            AND d.icd_code IN ('5856', 'V4511')
        )
        OR
        (
            d.icd_version = 10
            AND d.icd_code IN ('N186', 'Z992')
        )
),

eligible_icu_candidates AS (
    SELECT
        i.subject_id,
        i.hadm_id,
        i.stay_id,
        i.intime AS icu_intime,
        i.outtime AS icu_outtime,
        a.admittime,
        a.dischtime,
        a.deathtime,
        a.hospital_expire_flag,
        ag.age
    FROM mimiciv_icu.icustays AS i
    INNER JOIN copd_hadm AS c
        ON i.hadm_id = c.hadm_id
    INNER JOIN mimiciv_hosp.admissions AS a
        ON i.hadm_id = a.hadm_id
    INNER JOIN mimiciv_derived.age AS ag
        ON i.hadm_id = ag.hadm_id
    LEFT JOIN esrd_chronic_dialysis_hadm AS e
        ON i.hadm_id = e.hadm_id
    WHERE ag.age >= 18
      AND e.hadm_id IS NULL
),

qualifying_icu_ranked AS (
    SELECT
        c.*,
        ROW_NUMBER() OVER (
            PARTITION BY c.subject_id
            ORDER BY c.icu_intime, c.stay_id
        ) AS qualifying_icu_rn
    FROM eligible_icu_candidates AS c
),

cohort_base AS (
    SELECT
        q.subject_id,
        q.hadm_id,
        q.stay_id,
        q.icu_intime,
        q.icu_outtime,
        q.admittime,
        q.dischtime,
        q.deathtime,
        q.hospital_expire_flag,

        q.icu_intime + INTERVAL '36 hour'
            AS recovery_window_36_60_start,
        q.icu_intime + INTERVAL '42 hour'
            AS recovery_window_42_54_start,
        q.icu_intime + INTERVAL '48 hour'
            AS target_48h_time,
        q.icu_intime + INTERVAL '54 hour'
            AS recovery_window_42_54_end,
        q.icu_intime + INTERVAL '60 hour'
            AS landmark_60h_time,

        GREATEST(
            q.admittime,
            q.icu_intime - INTERVAL '7 day'
        ) AS scr_lookback_start,

        CASE
            WHEN q.hospital_expire_flag = 1
                THEN COALESCE(q.deathtime, q.dischtime)
            ELSE q.dischtime
        END AS outcome_or_discharge_time

    FROM qualifying_icu_ranked AS q
    WHERE q.qualifying_icu_rn = 1
),

cohort_landmark AS (
    SELECT
        c.*,

        CASE
            WHEN c.outcome_or_discharge_time > c.landmark_60h_time
                THEN 1 ELSE 0
        END AS eligible_at_60h_flag,

        CASE
            WHEN c.hospital_expire_flag = 1
             AND c.outcome_or_discharge_time > c.landmark_60h_time
                THEN 1 ELSE 0
        END AS hospital_death_after_60h,

        CASE
            WHEN c.outcome_or_discharge_time > c.landmark_60h_time
                THEN EXTRACT(
                    EPOCH FROM (
                        c.outcome_or_discharge_time
                        - c.landmark_60h_time
                    )
                ) / 86400.0
            ELSE NULL
        END AS followup_days_after_60h

    FROM cohort_base AS c
),

stage2_cohort AS (
    SELECT *
    FROM cohort_landmark
    WHERE eligible_at_60h_flag = 1
)

SELECT
    c.subject_id,
    c.hadm_id,
    c.stay_id,
    c.icu_intime,
    c.admittime,
    c.dischtime,
    c.deathtime,
    c.hospital_expire_flag,
    c.landmark_60h_time,
    c.scr_lookback_start,
    c.outcome_or_discharge_time
FROM stage2_cohort AS c;

CREATE UNIQUE INDEX tmp_mimic07_cohort_stay_idx
    ON tmp_mimic07_cohort (stay_id);
CREATE INDEX tmp_mimic07_cohort_hadm_idx
    ON tmp_mimic07_cohort (subject_id, hadm_id);
ANALYZE tmp_mimic07_cohort;

-- 2. Check cohort integrity before scanning the laboratory event table.
DO $$
DECLARE
    n_cohort integer;
    n_invalid_landmark integer;
BEGIN
    SELECT COUNT(*)::integer INTO n_cohort
    FROM tmp_mimic07_cohort;

    IF n_cohort <> 7794 THEN
        RAISE NOTICE
            'MIMIC 07: landmark N=%; expected N=7794 on the study version. Investigate before using output.',
            n_cohort;
    END IF;

    SELECT COUNT(*)::integer INTO n_invalid_landmark
    FROM tmp_mimic07_cohort
    WHERE icu_intime IS NULL
       OR admittime IS NULL
       OR outcome_or_discharge_time IS NULL
       OR outcome_or_discharge_time <= landmark_60h_time;

    IF n_invalid_landmark > 0 THEN
        RAISE EXCEPTION
            'MIMIC 07: % cohort stays fail the +60h landmark timing check.',
            n_invalid_landmark;
    END IF;
END $$;

-- 3. Original 21-column long-format sCr event output (unchanged except
--    that the cohort relation is the generated temp table, not ID anchor).
--    Keep ALL event filters in the ON clause so empty stays are retained.
SELECT
    c.subject_id,
    c.hadm_id,
    c.stay_id,
    c.icu_intime,
    c.admittime AS hospital_admittime,
    c.dischtime AS hospital_dischtime,
    c.deathtime AS recorded_hospital_deathtime,
    c.hospital_expire_flag,
    c.outcome_or_discharge_time,
    c.landmark_60h_time,
    c.scr_lookback_start,
    le.labevent_id,
    le.charttime AS scr_charttime,
    le.valuenum::numeric AS scr_numeric_as_extracted,
    le.valueuom::text AS scr_unit_raw,
    CASE WHEN le.labevent_id IS NULL THEN 0 ELSE 1 END AS has_scr_event,
    CASE WHEN le.charttime IS NOT NULL
        THEN EXTRACT(EPOCH FROM (le.charttime - c.icu_intime)) / 3600.0
    END AS scr_hours_from_icu,
    CASE WHEN le.charttime >= c.scr_lookback_start
               AND le.charttime < c.icu_intime THEN 1 ELSE 0
    END AS valid_pre_icu_reference_window,
    CASE WHEN le.charttime >= c.icu_intime
               AND le.charttime <= c.icu_intime + INTERVAL '24 hour' THEN 1 ELSE 0
    END AS in_icu_0_24h,
    CASE WHEN le.charttime >= c.icu_intime
               AND le.charttime <= c.icu_intime + INTERVAL '48 hour' THEN 1 ELSE 0
    END AS in_icu_0_48h,
    CASE WHEN le.charttime >= c.icu_intime + INTERVAL '36 hour'
               AND le.charttime <= c.landmark_60h_time THEN 1 ELSE 0
    END AS in_36_60h_assessment_window
FROM tmp_mimic07_cohort AS c
LEFT JOIN mimiciv_hosp.labevents AS le
    ON le.subject_id = c.subject_id
   AND le.hadm_id = c.hadm_id
   AND le.itemid = 50912
   AND le.charttime >= LEAST(c.scr_lookback_start, c.icu_intime)
   AND le.charttime <= c.landmark_60h_time
   AND le.valuenum IS NOT NULL
   AND le.valuenum > 0
ORDER BY
    c.stay_id,
    le.charttime NULLS LAST,
    le.labevent_id;
