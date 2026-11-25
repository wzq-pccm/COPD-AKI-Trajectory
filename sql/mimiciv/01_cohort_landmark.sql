/*
MIMIC-IV v3.1
Stage 1 (revised): broad COPD cohort, 60-h landmark eligibility,
timing anchors for AKI trajectory assessment, and outcomes
PostgreSQL dialect

Study design implemented in this query:
  1. Identify hospitalizations with a broad COPD diagnosis.
  2. Restrict ICU stays to adults aged >=18 years.
  3. Exclude hospitalizations with documented ESRD or chronic dialysis.
  4. Among these eligible ICU stays, retain the first qualifying ICU stay
     per patient.
  5. Define timing anchors required by the revised AKI trajectory design:
       - main renal-status assessment window: ICU hours 36-60
       - target assessment time: ICU hour 48
       - narrow sensitivity window: ICU hours 42-54
       - outcome landmark: ICU hour 60
       - reference-sCr pre-ICU lookback start:
         max(hospital admission, ICU admission - 7 days)
  6. After selecting the first qualifying ICU stay, determine eligibility
     at ICU +60 hours. The patient may have left the ICU before 60 hours,
     but must still be alive and hospitalized at 60 hours.
  7. Define subsequent in-hospital death after the 60-hour landmark and
     post-landmark follow-up time.

Important:
  - This Stage 1 query does NOT define reference sCr, early AKI, or renal
    recovery status. Those will be linked later by stay_id.
  - The 42-54-h window is a sensitivity-analysis trajectory window only;
    it is NOT a separate outcome landmark.
  - The primary outcome landmark remains ICU +60 hours.
  - The 36-60-h trajectory window ends exactly at the 60-h landmark,
    ensuring exposure ascertainment precedes post-landmark outcome follow-up.

Expected output:
  One row per selected first qualifying stay_id BEFORE applying the final
  60-hour eligibility restriction, with eligibility/exclusion flags retained.
  This is useful for Figure 1 cohort-flow accounting and later stay_id linkage.
*/


WITH copd_hadm AS (
    SELECT
        d.hadm_id,

        MAX(
            CASE
                WHEN d.seq_num = 1 THEN 1
                ELSE 0
            END
        ) AS broad_copd_principal_flag,

        COUNT(*) AS matched_copd_diagnosis_n,

        STRING_AGG(
            DISTINCT CONCAT(
                d.icd_version::text,
                ':',
                d.icd_code
            ),
            ',' ORDER BY CONCAT(
                d.icd_version::text,
                ':',
                d.icd_code
            )
        ) AS matched_copd_codes

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

    GROUP BY d.hadm_id
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
Apply adult and non-ESRD criteria before ranking ICU stays.
This ensures ROW_NUMBER() selects the patient's first ICU stay that
actually meets the baseline eligibility criteria.

Do NOT apply the 60-hour landmark criterion before ranking.
Otherwise an earlier qualifying stay with early death/discharge could be
skipped and a later stay from the same patient could be incorrectly selected.
*/
eligible_icu_candidates AS (
    SELECT
        i.subject_id,
        i.hadm_id,
        i.stay_id,

        i.intime AS icu_intime,
        i.outtime AS icu_outtime,
        i.first_careunit,
        i.last_careunit,

        a.admittime,
        a.dischtime,
        a.deathtime,
        a.hospital_expire_flag,

        a.admission_type,
        a.admission_location,
        a.discharge_location,
        a.insurance,
        a.language,
        a.marital_status,
        a.race,

        ag.age,
        p.gender,

        c.broad_copd_principal_flag,
        c.matched_copd_diagnosis_n,
        c.matched_copd_codes,

        0 AS esrd_chronic_dialysis_flag

    FROM mimiciv_icu.icustays AS i

    INNER JOIN copd_hadm AS c
        ON i.hadm_id = c.hadm_id

    INNER JOIN mimiciv_hosp.admissions AS a
        ON i.hadm_id = a.hadm_id

    INNER JOIN mimiciv_hosp.patients AS p
        ON i.subject_id = p.subject_id

    INNER JOIN mimiciv_derived.age AS ag
        ON i.hadm_id = ag.hadm_id

    LEFT JOIN esrd_chronic_dialysis_hadm AS e
        ON i.hadm_id = e.hadm_id

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
        q.*

    FROM qualifying_icu_ranked AS q

    WHERE q.qualifying_icu_rn = 1
),

timing_and_outcome AS (
    SELECT
        f.*,

        /* Main trajectory window: ICU hours 36-60 */
        f.icu_intime
            + INTERVAL '36 hour'
            AS recovery_window_36_60_start,

        f.icu_intime
            + INTERVAL '60 hour'
            AS recovery_window_36_60_end,

        /* Target renal-status assessment time */
        f.icu_intime
            + INTERVAL '48 hour'
            AS target_48h_time,

        /* Narrow sensitivity trajectory window: ICU hours 42-54 */
        f.icu_intime
            + INTERVAL '42 hour'
            AS recovery_window_42_54_start,

        f.icu_intime
            + INTERVAL '54 hour'
            AS recovery_window_42_54_end,

        /* Primary outcome landmark */
        f.icu_intime
            + INTERVAL '60 hour'
            AS landmark_60h_time,

        /*
        Pre-ICU measured reference-sCr search will later be restricted to:
          max(index hospital admission, ICU admission - 7 days)
          through strictly before ICU admission.
        */
        GREATEST(
            f.admittime,
            f.icu_intime - INTERVAL '7 day'
        ) AS scr_lookback_start,

        /*
        End of index hospitalization:
          death time when available for in-hospital deaths,
          otherwise discharge time.
        */
        CASE
            WHEN f.hospital_expire_flag = 1
                THEN COALESCE(
                    f.deathtime,
                    f.dischtime
                )
            ELSE f.dischtime
        END AS outcome_or_discharge_time

    FROM first_qualifying_icu AS f
)

SELECT
    t.subject_id,
    t.hadm_id,
    t.stay_id,

    t.icu_intime,
    t.icu_outtime,
    t.first_careunit,
    t.last_careunit,

    t.admittime,
    t.dischtime,
    t.deathtime,
    t.hospital_expire_flag,

    t.age,
    t.gender,
    t.race,

    t.admission_type,
    t.admission_location,
    t.discharge_location,
    t.insurance,
    t.language,
    t.marital_status,

    t.broad_copd_principal_flag,
    t.matched_copd_diagnosis_n,
    t.matched_copd_codes,
    t.esrd_chronic_dialysis_flag,

    /* Revised trajectory timing anchors */
    t.recovery_window_36_60_start,
    t.target_48h_time,
    t.recovery_window_36_60_end,

    t.recovery_window_42_54_start,
    t.recovery_window_42_54_end,

    /* Outcome landmark and reference-sCr lookback */
    t.landmark_60h_time,
    t.scr_lookback_start,
    t.outcome_or_discharge_time,

    /*
    Flow-diagram / landmark exclusion flags.
    The primary analysis requires outcome_or_discharge_time > landmark_60h_time.
    */
    CASE
        WHEN t.hospital_expire_flag = 1
         AND t.outcome_or_discharge_time <= t.landmark_60h_time
            THEN 1
        ELSE 0
    END AS death_by_60h_flag,

    CASE
        WHEN t.hospital_expire_flag = 0
         AND t.outcome_or_discharge_time <= t.landmark_60h_time
            THEN 1
        ELSE 0
    END AS alive_discharge_by_60h_flag,

    CASE
        WHEN t.outcome_or_discharge_time > t.landmark_60h_time
            THEN 1
        ELSE 0
    END AS eligible_at_60h_flag,

    CASE
        WHEN t.outcome_or_discharge_time IS NULL
            THEN 'missing_outcome_or_discharge_time'
        WHEN t.hospital_expire_flag = 1
         AND t.outcome_or_discharge_time <= t.landmark_60h_time
            THEN 'death_by_60h'
        WHEN t.hospital_expire_flag = 0
         AND t.outcome_or_discharge_time <= t.landmark_60h_time
            THEN 'alive_discharge_by_60h'
        WHEN t.outcome_or_discharge_time > t.landmark_60h_time
            THEN 'eligible_at_60h'
        ELSE 'other'
    END AS landmark_60h_status,

    /*
    Primary outcome:
      subsequent in-hospital death after the 60-hour landmark.
    This is meaningful for the 60-h eligible risk set.
    */
    CASE
        WHEN t.hospital_expire_flag = 1
         AND t.outcome_or_discharge_time > t.landmark_60h_time
            THEN 1
        ELSE 0
    END AS hospital_death_after_60h,

    CASE
        WHEN t.outcome_or_discharge_time > t.landmark_60h_time
            THEN EXTRACT(
                EPOCH FROM (
                    t.outcome_or_discharge_time
                    - t.landmark_60h_time
                )
            ) / 86400.0
        ELSE NULL
    END AS followup_days_after_60h

FROM timing_and_outcome AS t

ORDER BY
    t.subject_id,
    t.icu_intime,
    t.stay_id;
