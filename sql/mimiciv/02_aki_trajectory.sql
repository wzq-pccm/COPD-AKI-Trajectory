/*
COPD and Early AKI Trajectory in Critically Ill Patients
MIMIC-IV v3.1 | PostgreSQL | Public reproducibility code

02_aki_trajectory.sql
---------------------
Purpose
  Generate the fixed-reference, creatinine-defined early-AKI trajectory and
  post-60-hour-landmark outcome variables for the MIMIC-IV cohort.

Provenance
  Based on the investigators' finalized original source:
  MIMIC_stage2_revised_fixed_reference_AKI_trajectory_v2_fix.sql.
  The full executable query body and all output variable names are retained
  unchanged to avoid altering the verified analytical definition.

Required schemas / tables
  mimiciv_hosp.diagnoses_icd
  mimiciv_hosp.admissions
  mimiciv_hosp.labevents
  mimiciv_icu.icustays
  mimiciv_derived.age  (install the corresponding MIMIC derived concept)

Run instructions
  - Run on an authorized local PostgreSQL installation of MIMIC-IV v3.1.
  - This file is a STANDALONE SELECT query; it does not read a saved table
    from 01_cohort_landmark.sql. For portability, its CTEs independently
    reconstruct the same first-qualifying-ICU / 60-hour-landmark cohort.
  - Run the whole query, not individual CTE fragments.
  - Export the result as a local, restricted-access dataset for downstream
    analysis; do not commit patient-level output data to a public repository.

Definitions retained from the original study SQL
  * Eligible hospitalizations: broad ICD-9/10 COPD diagnoses; adult; no
    ESRD/chronic dialysis; first eligible ICU stay per patient, followed by
    survival and continued hospitalization at ICU hour 60.
  * Fixed reference sCr: most recent measured sCr within the preceding 7
    days of the index hospitalization before ICU entry; if unavailable,
    minimum measured ICU-hour-0-to-24 sCr (surrogate reference).
  * Early AKI: ICU hours 0-48, relative to this fixed reference; sCr increase
    >=0.3 mg/dL or >=1.5 x reference. Peak stage is also recorded.
  * Main assessment: measured sCr nearest hour 48 in hours 36-60, at or
    after first qualifying AKI sCr. Resolved only when BOTH cutoffs no
    longer apply; otherwise ongoing. Patients with no early AKI form group 0.
  * Narrow-window assessment: hours 42-54, retained as a sensitivity field.
  * Primary exposure groups: 0=no early AKI, 1=resolved, 2=ongoing.
  * Outcome: subsequent in-hospital death after ICU hour 60.

Methodological cautions
  - The main AKI definition applies creatinine MAGNITUDE criteria relative
    to a fixed reference; it does NOT require within-pair KDIGO time limits.
    The stricter temporal-pair sensitivity analysis is handled downstream.
  - No urine-output or RRT criterion is used for primary classification.
  - Reference sCr is not necessarily the patient's premorbid baseline.
  - The ICU-hour-0-to-24 surrogate may be measured after an earlier high sCr;
    this reproduces the original algorithm rather than imposing chronology.

Suggested checks against the revised manuscript (not executed here)
  - Landmark-eligible stays: 7,794
  - Classifiable primary cohort (primary_analysis_flag=1): 7,627
  - No early AKI / resolved / ongoing: 5,796 / 898 / 933
  - Unclassifiable at landmark: 167
  - Unique stay_id count should equal total row count.
  - To verify actual record identities, compare stay_id against the original
    Stage-2 export, not just the above aggregate counts.

NOTE: Static source audit only. This script has NOT been run in a live MIMIC-IV
      PostgreSQL database in this environment.
*/

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
),

/* =========================================================================
   Creatinine long table:
   same index hospitalization only; from pre-ICU lookback start through +60 h
   ========================================================================= */
creatinine_long AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,
        c.icu_intime,
        c.recovery_window_36_60_start,
        c.recovery_window_42_54_start,
        c.target_48h_time,
        c.recovery_window_42_54_end,
        c.landmark_60h_time,
        c.scr_lookback_start,
        le.labevent_id,
        le.charttime,
        le.valuenum::numeric AS scr

    FROM stage2_cohort AS c

    INNER JOIN mimiciv_hosp.labevents AS le
        ON le.subject_id = c.subject_id
       AND le.hadm_id = c.hadm_id
       /*
       Combined extraction must include ICU-period sCr even in the rare
       records where hospital admittime is later than ICU intime.
       The hospital-admission restriction applies only to PRE-ICU reference
       ascertainment, not to post-ICU measurements.
       */
       AND le.charttime >= LEAST(c.scr_lookback_start, c.icu_intime)
       AND le.charttime <= c.landmark_60h_time

    WHERE le.itemid = 50912
      AND le.charttime IS NOT NULL
      AND le.valuenum IS NOT NULL
      AND le.valuenum > 0
),

/* =========================================================================
   Fixed reference sCr components
   ========================================================================= */

/* Most recent measured pre-ICU sCr within the prespecified 7-day lookback */
pre_icu_reference_row AS (
    SELECT
        x.stay_id,
        x.scr AS pre_icu_7d_latest_scr,
        x.charttime AS pre_icu_7d_latest_time
    FROM (
        SELECT
            c.stay_id,
            c.scr,
            c.charttime,
            c.labevent_id,
            ROW_NUMBER() OVER (
                PARTITION BY c.stay_id
                ORDER BY c.charttime DESC, c.labevent_id DESC
            ) AS rn
        FROM creatinine_long AS c
        WHERE c.charttime >= c.scr_lookback_start
          AND c.charttime < c.icu_intime
    ) AS x
    WHERE x.rn = 1
),

pre_icu_reference_count AS (
    SELECT
        c.stay_id,
        COUNT(*) AS pre_icu_7d_scr_n
    FROM creatinine_long AS c
    WHERE c.charttime >= c.scr_lookback_start
      AND c.charttime < c.icu_intime
    GROUP BY c.stay_id
),

/* Minimum sCr during ICU hours 0-12; retained for diagnostics */
icu_0_12h_min_row AS (
    SELECT
        x.stay_id,
        x.scr AS icu_0_12h_min_scr,
        x.charttime AS icu_0_12h_min_scr_time
    FROM (
        SELECT
            c.stay_id,
            c.scr,
            c.charttime,
            c.labevent_id,
            ROW_NUMBER() OVER (
                PARTITION BY c.stay_id
                ORDER BY c.scr ASC, c.charttime ASC, c.labevent_id ASC
            ) AS rn
        FROM creatinine_long AS c
        WHERE c.charttime >= c.icu_intime
          AND c.charttime <= c.icu_intime + INTERVAL '12 hour'
    ) AS x
    WHERE x.rn = 1
),

/* Minimum sCr during ICU hours 0-24: primary surrogate reference fallback */
icu_0_24h_min_row AS (
    SELECT
        x.stay_id,
        x.scr AS icu_0_24h_min_scr,
        x.charttime AS icu_0_24h_min_scr_time
    FROM (
        SELECT
            c.stay_id,
            c.scr,
            c.charttime,
            c.labevent_id,
            ROW_NUMBER() OVER (
                PARTITION BY c.stay_id
                ORDER BY c.scr ASC, c.charttime ASC, c.labevent_id ASC
            ) AS rn
        FROM creatinine_long AS c
        WHERE c.charttime >= c.icu_intime
          AND c.charttime <= c.icu_intime + INTERVAL '24 hour'
    ) AS x
    WHERE x.rn = 1
),

/* First measured ICU sCr during hours 0-24; retained for diagnostic comparison */
first_icu_scr_row AS (
    SELECT
        x.stay_id,
        x.scr AS first_icu_scr_0_24h,
        x.charttime AS first_icu_scr_time_0_24h
    FROM (
        SELECT
            c.stay_id,
            c.scr,
            c.charttime,
            c.labevent_id,
            ROW_NUMBER() OVER (
                PARTITION BY c.stay_id
                ORDER BY c.charttime ASC, c.labevent_id ASC
            ) AS rn
        FROM creatinine_long AS c
        WHERE c.charttime >= c.icu_intime
          AND c.charttime <= c.icu_intime + INTERVAL '24 hour'
    ) AS x
    WHERE x.rn = 1
),

reference_window_counts AS (
    SELECT
        c.stay_id,

        COUNT(*) FILTER (
            WHERE c.charttime >= c.icu_intime
              AND c.charttime <= c.icu_intime + INTERVAL '12 hour'
        ) AS icu_0_12h_scr_n,

        COUNT(*) FILTER (
            WHERE c.charttime >= c.icu_intime
              AND c.charttime <= c.icu_intime + INTERVAL '24 hour'
        ) AS icu_0_24h_scr_n

    FROM creatinine_long AS c
    GROUP BY c.stay_id
),

fixed_reference AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,
        c.icu_intime,

        COALESCE(pc.pre_icu_7d_scr_n, 0) AS pre_icu_7d_scr_n,
        p.pre_icu_7d_latest_scr,
        p.pre_icu_7d_latest_time,

        COALESCE(rwc.icu_0_12h_scr_n, 0) AS icu_0_12h_scr_n,
        m12.icu_0_12h_min_scr,
        m12.icu_0_12h_min_scr_time,

        COALESCE(rwc.icu_0_24h_scr_n, 0) AS icu_0_24h_scr_n,
        m24.icu_0_24h_min_scr,
        m24.icu_0_24h_min_scr_time,

        fi.first_icu_scr_0_24h,
        fi.first_icu_scr_time_0_24h,

        CASE
            WHEN p.pre_icu_7d_latest_scr IS NOT NULL
                THEN p.pre_icu_7d_latest_scr
            WHEN m24.icu_0_24h_min_scr IS NOT NULL
                THEN m24.icu_0_24h_min_scr
            ELSE NULL
        END AS reference_scr,

        CASE
            WHEN p.pre_icu_7d_latest_scr IS NOT NULL
                THEN p.pre_icu_7d_latest_time
            WHEN m24.icu_0_24h_min_scr IS NOT NULL
                THEN m24.icu_0_24h_min_scr_time
            ELSE NULL
        END AS reference_scr_time,

        CASE
            WHEN p.pre_icu_7d_latest_scr IS NOT NULL
                THEN 'pre_icu_7d_latest'
            WHEN m24.icu_0_24h_min_scr IS NOT NULL
                THEN 'icu_0_24h_min'
            ELSE 'missing'
        END AS reference_scr_source

    FROM stage2_cohort AS c

    LEFT JOIN pre_icu_reference_row AS p
        ON p.stay_id = c.stay_id

    LEFT JOIN pre_icu_reference_count AS pc
        ON pc.stay_id = c.stay_id

    LEFT JOIN icu_0_12h_min_row AS m12
        ON m12.stay_id = c.stay_id

    LEFT JOIN icu_0_24h_min_row AS m24
        ON m24.stay_id = c.stay_id

    LEFT JOIN first_icu_scr_row AS fi
        ON fi.stay_id = c.stay_id

    LEFT JOIN reference_window_counts AS rwc
        ON rwc.stay_id = c.stay_id
),

/* =========================================================================
   Current ICU sCr records with fixed-reference KDIGO magnitude staging
   ========================================================================= */
current_scr_0_60h AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,
        c.icu_intime,
        c.target_48h_time,
        c.landmark_60h_time,
        c.labevent_id,
        c.charttime AS current_scr_time,
        c.scr AS current_scr,

        r.reference_scr,
        r.reference_scr_time,
        r.reference_scr_source,

        CASE
            WHEN r.reference_scr IS NOT NULL
                THEN 1
            ELSE 0
        END AS evaluable_comparison_flag,

        CASE
            WHEN r.reference_scr IS NULL
                THEN NULL

            WHEN
                c.scr >= 3.0 * r.reference_scr
                OR (
                    c.scr >= 4.0
                    AND c.scr >= r.reference_scr + 0.3
                )
                THEN 3

            WHEN c.scr >= 2.0 * r.reference_scr
                THEN 2

            WHEN
                c.scr >= 1.5 * r.reference_scr
                OR c.scr >= r.reference_scr + 0.3
                THEN 1

            ELSE 0
        END AS kdigo_scr_stage_fixed_ref,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND c.scr >= 1.5 * r.reference_scr
                THEN 1 ELSE 0
        END AS relative_criterion_flag,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND c.scr >= r.reference_scr + 0.3
                THEN 1 ELSE 0
        END AS absolute_criterion_flag

    FROM creatinine_long AS c

    INNER JOIN fixed_reference AS r
        ON r.stay_id = c.stay_id

    WHERE c.charttime >= c.icu_intime
      AND c.charttime <= c.landmark_60h_time
),

/* =========================================================================
   sCr availability by analysis window
   ========================================================================= */
scr_window_summary AS (
    SELECT
        c.stay_id,

        COUNT(*) FILTER (
            WHERE c.charttime >= c.icu_intime
              AND c.charttime <= c.target_48h_time
        ) AS scr_measurement_n_0_48h,

        COUNT(*) FILTER (
            WHERE c.charttime >= c.icu_intime + INTERVAL '42 hour'
              AND c.charttime <= c.icu_intime + INTERVAL '54 hour'
        ) AS scr_measurement_n_42_54h,

        COUNT(*) FILTER (
            WHERE c.charttime >= c.icu_intime + INTERVAL '36 hour'
              AND c.charttime <= c.landmark_60h_time
        ) AS scr_measurement_n_36_60h,

        COUNT(*) FILTER (
            WHERE c.charttime > c.target_48h_time
              AND c.charttime <= c.landmark_60h_time
        ) AS scr_measurement_n_48_60h

    FROM creatinine_long AS c
    GROUP BY c.stay_id
),

/* =========================================================================
   Early AKI: ICU hours 0-48 using the fixed reference sCr
   ========================================================================= */
early_aki_summary AS (
    SELECT
        k.stay_id,

        COUNT(*) AS scr_record_n_0_48h,

        COUNT(*) FILTER (
            WHERE k.evaluable_comparison_flag = 1
        ) AS evaluable_scr_n_0_48h,

        MAX(k.kdigo_scr_stage_fixed_ref) AS peak_aki_stage_0_48h,

        MAX(k.current_scr) AS observed_peak_scr_0_48h

    FROM current_scr_0_60h AS k

    WHERE k.current_scr_time >= k.icu_intime
      AND k.current_scr_time <= k.target_48h_time

    GROUP BY k.stay_id
),

early_aki_onset AS (
    SELECT
        x.stay_id,
        x.current_scr_time AS aki_onset_time,
        x.current_scr AS aki_onset_scr,
        x.kdigo_scr_stage_fixed_ref AS aki_stage_at_onset,
        x.relative_criterion_flag,
        x.absolute_criterion_flag

    FROM (
        SELECT
            k.*,

            ROW_NUMBER() OVER (
                PARTITION BY k.stay_id
                ORDER BY
                    k.current_scr_time,
                    k.kdigo_scr_stage_fixed_ref DESC,
                    k.current_scr DESC,
                    k.labevent_id
            ) AS rn

        FROM current_scr_0_60h AS k

        WHERE k.current_scr_time >= k.icu_intime
          AND k.current_scr_time <= k.target_48h_time
          AND k.kdigo_scr_stage_fixed_ref >= 1
    ) AS x

    WHERE x.rn = 1
),

early_peak_stage_record AS (
    SELECT
        x.stay_id,
        x.current_scr_time AS peak_aki_stage_time,
        x.current_scr AS peak_aki_stage_scr,
        x.kdigo_scr_stage_fixed_ref AS peak_aki_stage

    FROM (
        SELECT
            k.*,

            ROW_NUMBER() OVER (
                PARTITION BY k.stay_id
                ORDER BY
                    k.kdigo_scr_stage_fixed_ref DESC NULLS LAST,
                    k.current_scr DESC,
                    k.current_scr_time,
                    k.labevent_id
            ) AS rn

        FROM current_scr_0_60h AS k

        WHERE k.current_scr_time >= k.icu_intime
          AND k.current_scr_time <= k.target_48h_time
          AND k.kdigo_scr_stage_fixed_ref IS NOT NULL
    ) AS x

    WHERE x.rn = 1
),

/* =========================================================================
   Recovery assessment around ICU hour 48
   Must be at or after first early-AKI fulfillment.
   ========================================================================= */
renal_assessment_42_54h AS (
    SELECT
        x.stay_id,
        x.scr AS assessment_scr_42_54h,
        x.charttime AS assessment_scr_time_42_54h

    FROM (
        SELECT
            c.stay_id,
            c.scr,
            c.charttime,
            c.labevent_id,
            o.aki_onset_time,

            ROW_NUMBER() OVER (
                PARTITION BY c.stay_id
                ORDER BY
                    ABS(
                        EXTRACT(
                            EPOCH FROM (
                                c.charttime - c.target_48h_time
                            )
                        )
                    ),
                    c.charttime DESC,
                    c.labevent_id DESC
            ) AS rn

        FROM creatinine_long AS c

        INNER JOIN early_aki_onset AS o
            ON o.stay_id = c.stay_id

        WHERE c.charttime >= c.icu_intime + INTERVAL '42 hour'
          AND c.charttime <= c.icu_intime + INTERVAL '54 hour'
          AND c.charttime >= o.aki_onset_time
    ) AS x

    WHERE x.rn = 1
),

renal_assessment_36_60h AS (
    SELECT
        x.stay_id,
        x.scr AS assessment_scr_36_60h,
        x.charttime AS assessment_scr_time_36_60h

    FROM (
        SELECT
            c.stay_id,
            c.scr,
            c.charttime,
            c.labevent_id,
            o.aki_onset_time,

            ROW_NUMBER() OVER (
                PARTITION BY c.stay_id
                ORDER BY
                    ABS(
                        EXTRACT(
                            EPOCH FROM (
                                c.charttime - c.target_48h_time
                            )
                        )
                    ),
                    c.charttime DESC,
                    c.labevent_id DESC
            ) AS rn

        FROM creatinine_long AS c

        INNER JOIN early_aki_onset AS o
            ON o.stay_id = c.stay_id

        WHERE c.charttime >= c.icu_intime + INTERVAL '36 hour'
          AND c.charttime <= c.landmark_60h_time
          AND c.charttime >= o.aki_onset_time
    ) AS x

    WHERE x.rn = 1
),

/* =========================================================================
   Hours 48-60: retained for strict no-AKI sensitivity analysis
   ========================================================================= */
late_window_48_60h_summary AS (
    SELECT
        k.stay_id,

        COUNT(*) AS scr_record_n_48_60h,

        COUNT(*) FILTER (
            WHERE k.evaluable_comparison_flag = 1
        ) AS evaluable_scr_n_48_60h,

        MAX(k.kdigo_scr_stage_fixed_ref) FILTER (
            WHERE k.evaluable_comparison_flag = 1
        ) AS peak_aki_stage_48_60h

    FROM current_scr_0_60h AS k

    WHERE k.current_scr_time > k.target_48h_time
      AND k.current_scr_time <= k.landmark_60h_time

    GROUP BY k.stay_id
),

late_aki_onset_48_60h AS (
    SELECT
        x.stay_id,
        x.current_scr_time AS late_aki_onset_time,
        x.current_scr AS late_aki_onset_scr,
        x.kdigo_scr_stage_fixed_ref AS late_aki_stage_at_onset

    FROM (
        SELECT
            k.*,

            ROW_NUMBER() OVER (
                PARTITION BY k.stay_id
                ORDER BY
                    k.current_scr_time,
                    k.kdigo_scr_stage_fixed_ref DESC,
                    k.current_scr DESC,
                    k.labevent_id
            ) AS rn

        FROM current_scr_0_60h AS k

        WHERE k.current_scr_time > k.target_48h_time
          AND k.current_scr_time <= k.landmark_60h_time
          AND k.evaluable_comparison_flag = 1
          AND k.kdigo_scr_stage_fixed_ref >= 1
    ) AS x

    WHERE x.rn = 1
),

/* =========================================================================
   Merge trajectory features: one row per stay_id
   ========================================================================= */
trajectory_features AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,

        c.icu_intime,
        c.recovery_window_36_60_start,
        c.recovery_window_42_54_start,
        c.target_48h_time,
        c.recovery_window_42_54_end,
        c.landmark_60h_time,

        c.eligible_at_60h_flag,
        c.hospital_death_after_60h,
        c.outcome_or_discharge_time,
        c.followup_days_after_60h,

        /* Reference sCr details */
        r.pre_icu_7d_scr_n,
        r.pre_icu_7d_latest_scr,
        r.pre_icu_7d_latest_time,

        CASE
            WHEN r.pre_icu_7d_latest_time IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        c.icu_intime - r.pre_icu_7d_latest_time
                    )
                ) / 3600.0
            ELSE NULL
        END AS pre_icu_reference_hours_before_icu,

        r.icu_0_12h_scr_n,
        r.icu_0_12h_min_scr,
        r.icu_0_12h_min_scr_time,

        r.icu_0_24h_scr_n,
        r.icu_0_24h_min_scr,
        r.icu_0_24h_min_scr_time,

        r.first_icu_scr_0_24h,
        r.first_icu_scr_time_0_24h,

        r.reference_scr,
        r.reference_scr_time,
        r.reference_scr_source,

        CASE
            WHEN r.reference_scr_time IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        r.reference_scr_time - c.icu_intime
                    )
                ) / 3600.0
            ELSE NULL
        END AS reference_scr_hours_from_icu,

        CASE
            WHEN r.reference_scr_source = 'pre_icu_7d_latest'
                THEN 1 ELSE 0
        END AS pre_icu_reference_available_flag,

        CASE
            WHEN r.reference_scr_source = 'icu_0_24h_min'
                THEN 1 ELSE 0
        END AS surrogate_reference_0_24h_flag,

        CASE
            WHEN r.reference_scr IS NULL
                THEN 1 ELSE 0
        END AS reference_scr_missing_flag,

        /* sCr availability */
        COALESCE(sws.scr_measurement_n_0_48h, 0)
            AS scr_measurement_n_0_48h,
        COALESCE(sws.scr_measurement_n_42_54h, 0)
            AS scr_measurement_n_42_54h,
        COALESCE(sws.scr_measurement_n_36_60h, 0)
            AS scr_measurement_n_36_60h,
        COALESCE(sws.scr_measurement_n_48_60h, 0)
            AS scr_measurement_n_48_60h,

        /* Early AKI */
        COALESCE(es.scr_record_n_0_48h, 0)
            AS scr_record_n_0_48h,
        COALESCE(es.evaluable_scr_n_0_48h, 0)
            AS evaluable_scr_n_0_48h,
        es.peak_aki_stage_0_48h,
        es.observed_peak_scr_0_48h,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND es.observed_peak_scr_0_48h IS NOT NULL
                THEN es.observed_peak_scr_0_48h
                     / NULLIF(r.reference_scr, 0)
            ELSE NULL
        END AS peak_to_reference_ratio_0_48h,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND es.observed_peak_scr_0_48h IS NOT NULL
                THEN es.observed_peak_scr_0_48h - r.reference_scr
            ELSE NULL
        END AS peak_minus_reference_0_48h,

        o.aki_onset_time,

        CASE
            WHEN o.aki_onset_time IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        o.aki_onset_time - c.icu_intime
                    )
                ) / 3600.0
            ELSE NULL
        END AS aki_onset_hours_from_icu,

        o.aki_onset_scr,
        o.aki_stage_at_onset,
        o.relative_criterion_flag,
        o.absolute_criterion_flag,

        CASE
            WHEN o.aki_onset_time IS NOT NULL
             AND r.reference_scr_time IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        o.aki_onset_time - r.reference_scr_time
                    )
                ) / 3600.0
            ELSE NULL
        END AS reference_to_aki_onset_hours,

        CASE
            WHEN o.aki_onset_time IS NOT NULL
             AND r.reference_scr_time IS NOT NULL
             AND o.aki_onset_time < r.reference_scr_time
                THEN 1
            WHEN o.aki_onset_time IS NOT NULL
             AND r.reference_scr_time IS NOT NULL
                THEN 0
            ELSE NULL
        END AS aki_onset_before_reference_time_flag,

        /* Peak stage record */
        p.peak_aki_stage_time,
        p.peak_aki_stage_scr,
        p.peak_aki_stage,

        /* Narrow 42-54 h assessment */
        a42.assessment_scr_42_54h,
        a42.assessment_scr_time_42_54h,

        CASE
            WHEN a42.assessment_scr_time_42_54h IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        a42.assessment_scr_time_42_54h
                        - c.target_48h_time
                    )
                ) / 3600.0
            ELSE NULL
        END AS assessment_offset_from_48h_42_54h,

        CASE
            WHEN a42.assessment_scr_time_42_54h IS NOT NULL
                THEN ABS(
                    EXTRACT(
                        EPOCH FROM (
                            a42.assessment_scr_time_42_54h
                            - c.target_48h_time
                        )
                    ) / 3600.0
                )
            ELSE NULL
        END AS assessment_distance_from_48h_42_54h,

        CASE
            WHEN o.aki_onset_time IS NOT NULL
             AND a42.assessment_scr_time_42_54h IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        a42.assessment_scr_time_42_54h
                        - o.aki_onset_time
                    )
                ) / 3600.0
            ELSE NULL
        END AS onset_to_assessment_hours_42_54h,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND a42.assessment_scr_42_54h IS NOT NULL
                THEN a42.assessment_scr_42_54h
                     / NULLIF(r.reference_scr, 0)
            ELSE NULL
        END AS assessment_to_reference_ratio_42_54h,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND a42.assessment_scr_42_54h IS NOT NULL
                THEN a42.assessment_scr_42_54h - r.reference_scr
            ELSE NULL
        END AS assessment_minus_reference_42_54h,

        /* Main 36-60 h assessment */
        a36.assessment_scr_36_60h,
        a36.assessment_scr_time_36_60h,

        CASE
            WHEN a36.assessment_scr_time_36_60h IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        a36.assessment_scr_time_36_60h
                        - c.target_48h_time
                    )
                ) / 3600.0
            ELSE NULL
        END AS assessment_offset_from_48h_36_60h,

        CASE
            WHEN a36.assessment_scr_time_36_60h IS NOT NULL
                THEN ABS(
                    EXTRACT(
                        EPOCH FROM (
                            a36.assessment_scr_time_36_60h
                            - c.target_48h_time
                        )
                    ) / 3600.0
                )
            ELSE NULL
        END AS assessment_distance_from_48h_36_60h,

        CASE
            WHEN o.aki_onset_time IS NOT NULL
             AND a36.assessment_scr_time_36_60h IS NOT NULL
                THEN EXTRACT(
                    EPOCH FROM (
                        a36.assessment_scr_time_36_60h
                        - o.aki_onset_time
                    )
                ) / 3600.0
            ELSE NULL
        END AS onset_to_assessment_hours_36_60h,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND a36.assessment_scr_36_60h IS NOT NULL
                THEN a36.assessment_scr_36_60h
                     / NULLIF(r.reference_scr, 0)
            ELSE NULL
        END AS assessment_to_reference_ratio_36_60h,

        CASE
            WHEN r.reference_scr IS NOT NULL
             AND a36.assessment_scr_36_60h IS NOT NULL
                THEN a36.assessment_scr_36_60h - r.reference_scr
            ELSE NULL
        END AS assessment_minus_reference_36_60h,

        /* 48-60 h status */
        COALESCE(lws.scr_record_n_48_60h, 0)
            AS scr_record_n_48_60h,
        COALESCE(lws.evaluable_scr_n_48_60h, 0)
            AS evaluable_scr_n_48_60h,
        lws.peak_aki_stage_48_60h,
        late.late_aki_onset_time,
        late.late_aki_onset_scr,
        late.late_aki_stage_at_onset

    FROM stage2_cohort AS c

    LEFT JOIN fixed_reference AS r
        ON r.stay_id = c.stay_id

    LEFT JOIN scr_window_summary AS sws
        ON sws.stay_id = c.stay_id

    LEFT JOIN early_aki_summary AS es
        ON es.stay_id = c.stay_id

    LEFT JOIN early_aki_onset AS o
        ON o.stay_id = c.stay_id

    LEFT JOIN early_peak_stage_record AS p
        ON p.stay_id = c.stay_id

    LEFT JOIN renal_assessment_42_54h AS a42
        ON a42.stay_id = c.stay_id

    LEFT JOIN renal_assessment_36_60h AS a36
        ON a36.stay_id = c.stay_id

    LEFT JOIN late_window_48_60h_summary AS lws
        ON lws.stay_id = c.stay_id

    LEFT JOIN late_aki_onset_48_60h AS late
        ON late.stay_id = c.stay_id
),

/* =========================================================================
   Exposure classification
   ========================================================================= */
exposure_base AS (
    SELECT
        t.*,

        CASE
            WHEN t.reference_scr IS NULL
                THEN 'Unclassifiable: missing reference SCr'

            WHEN t.scr_measurement_n_0_48h = 0
                THEN 'Unclassifiable: no SCr during ICU 0-48h'

            WHEN t.peak_aki_stage_0_48h IS NULL
                THEN 'Unclassifiable: missing fixed-reference AKI stage'

            WHEN t.peak_aki_stage_0_48h = 0
                THEN 'No early AKI during ICU 0-48h'

            WHEN t.assessment_scr_36_60h IS NULL
                THEN 'Early AKI with missing 36-60h assessment SCr'

            WHEN t.assessment_scr_36_60h < t.reference_scr + 0.3
             AND t.assessment_scr_36_60h < 1.5 * t.reference_scr
                THEN 'Early AKI resolved near 48h'

            WHEN t.assessment_scr_36_60h >= t.reference_scr + 0.3
              OR t.assessment_scr_36_60h >= 1.5 * t.reference_scr
                THEN 'Early AKI ongoing near 48h'

            ELSE 'Unclassifiable: other'
        END AS primary_exposure_36_60h,

        CASE
            WHEN t.reference_scr IS NULL
                THEN 'Unclassifiable: missing reference SCr'

            WHEN t.scr_measurement_n_0_48h = 0
                THEN 'Unclassifiable: no SCr during ICU 0-48h'

            WHEN t.peak_aki_stage_0_48h IS NULL
                THEN 'Unclassifiable: missing fixed-reference AKI stage'

            WHEN t.peak_aki_stage_0_48h = 0
                THEN 'No early AKI during ICU 0-48h'

            WHEN t.assessment_scr_42_54h IS NULL
                THEN 'Early AKI with missing 42-54h assessment SCr'

            WHEN t.assessment_scr_42_54h < t.reference_scr + 0.3
             AND t.assessment_scr_42_54h < 1.5 * t.reference_scr
                THEN 'Early AKI resolved near 48h'

            WHEN t.assessment_scr_42_54h >= t.reference_scr + 0.3
              OR t.assessment_scr_42_54h >= 1.5 * t.reference_scr
                THEN 'Early AKI ongoing near 48h'

            ELSE 'Unclassifiable: other'
        END AS sensitivity_exposure_42_54h,

        CASE
            WHEN t.peak_aki_stage_0_48h = 0
             AND t.evaluable_scr_n_0_48h >= 1
             AND t.evaluable_scr_n_48_60h >= 1
             AND t.peak_aki_stage_48_60h = 0
                THEN 'No AKI detected through 60h'

            WHEN t.peak_aki_stage_0_48h = 0
             AND t.evaluable_scr_n_0_48h >= 1
             AND t.peak_aki_stage_48_60h >= 1
                THEN 'New AKI during hour 48-60'

            WHEN t.peak_aki_stage_0_48h = 0
             AND t.evaluable_scr_n_0_48h >= 1
             AND t.evaluable_scr_n_48_60h = 0
                THEN 'No early AKI; hour 48-60 status indeterminate'

            WHEN t.peak_aki_stage_0_48h >= 1
                THEN 'Not applicable: early AKI'

            ELSE 'Indeterminate: early AKI status unclassifiable'
        END AS late_48_60h_status

    FROM trajectory_features AS t
),

exposure_final AS (
    SELECT
        e.*,

        CASE
            WHEN e.primary_exposure_36_60h =
                 'No early AKI during ICU 0-48h'
                THEN 0
            WHEN e.primary_exposure_36_60h =
                 'Early AKI resolved near 48h'
                THEN 1
            WHEN e.primary_exposure_36_60h =
                 'Early AKI ongoing near 48h'
                THEN 2
            ELSE NULL
        END AS primary_exposure_code,

        CASE
            WHEN e.sensitivity_exposure_42_54h =
                 'No early AKI during ICU 0-48h'
                THEN 0
            WHEN e.sensitivity_exposure_42_54h =
                 'Early AKI resolved near 48h'
                THEN 1
            WHEN e.sensitivity_exposure_42_54h =
                 'Early AKI ongoing near 48h'
                THEN 2
            ELSE NULL
        END AS sensitivity_narrow_exposure_code,

        /*
        Pre-ICU measured reference sensitivity:
        same primary exposure definition, but only patients whose fixed
        reference is an actual measured pre-ICU sCr are eligible.
        */
        CASE
            WHEN e.reference_scr_source = 'pre_icu_7d_latest'
             AND e.primary_exposure_36_60h =
                 'No early AKI during ICU 0-48h'
                THEN 0
            WHEN e.reference_scr_source = 'pre_icu_7d_latest'
             AND e.primary_exposure_36_60h =
                 'Early AKI resolved near 48h'
                THEN 1
            WHEN e.reference_scr_source = 'pre_icu_7d_latest'
             AND e.primary_exposure_36_60h =
                 'Early AKI ongoing near 48h'
                THEN 2
            ELSE NULL
        END AS sensitivity_pre_icu_reference_exposure_code,

        /*
        Strict no-AKI sensitivity:
        no-AKI reference patients must have evaluable sCr after hour 48 and
        no fixed-reference AKI detected through hour 60.
        */
        CASE
            WHEN e.primary_exposure_36_60h =
                 'No early AKI during ICU 0-48h'
             AND e.late_48_60h_status =
                 'No AKI detected through 60h'
                THEN 0

            WHEN e.primary_exposure_36_60h =
                 'Early AKI resolved near 48h'
                THEN 1

            WHEN e.primary_exposure_36_60h =
                 'Early AKI ongoing near 48h'
                THEN 2

            ELSE NULL
        END AS sensitivity_strict_no_aki_exposure_code,

        /*
        >=12 h onset-to-assessment sensitivity:
        no-AKI patients remain included; early-AKI patients require >=12 h.
        */
        CASE
            WHEN e.primary_exposure_36_60h =
                 'No early AKI during ICU 0-48h'
                THEN 0

            WHEN e.primary_exposure_36_60h =
                 'Early AKI resolved near 48h'
             AND e.onset_to_assessment_hours_36_60h >= 12
                THEN 1

            WHEN e.primary_exposure_36_60h =
                 'Early AKI ongoing near 48h'
             AND e.onset_to_assessment_hours_36_60h >= 12
                THEN 2

            ELSE NULL
        END AS sensitivity_onset_ge12h_exposure_code,

        CASE
            WHEN e.peak_aki_stage_0_48h = 1
             AND e.primary_exposure_36_60h =
                 'Early AKI resolved near 48h'
                THEN 'Resolved, peak stage 1'

            WHEN e.peak_aki_stage_0_48h IN (2, 3)
             AND e.primary_exposure_36_60h =
                 'Early AKI resolved near 48h'
                THEN 'Resolved, peak stage 2-3'

            WHEN e.peak_aki_stage_0_48h = 1
             AND e.primary_exposure_36_60h =
                 'Early AKI ongoing near 48h'
                THEN 'Ongoing, peak stage 1'

            WHEN e.peak_aki_stage_0_48h IN (2, 3)
             AND e.primary_exposure_36_60h =
                 'Early AKI ongoing near 48h'
                THEN 'Ongoing, peak stage 2-3'

            ELSE NULL
        END AS peak_stage_recovery_group

    FROM exposure_base AS e
)

SELECT
    f.*,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.primary_exposure_code IN (0, 1, 2)
            THEN 1 ELSE 0
    END AS primary_analysis_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_narrow_exposure_code IN (0, 1, 2)
            THEN 1 ELSE 0
    END AS sensitivity_narrow_window_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_pre_icu_reference_exposure_code IN (0, 1, 2)
            THEN 1 ELSE 0
    END AS sensitivity_pre_icu_reference_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_onset_ge12h_exposure_code IN (0, 1, 2)
            THEN 1 ELSE 0
    END AS sensitivity_onset_ge12h_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_strict_no_aki_exposure_code IN (0, 1, 2)
            THEN 1 ELSE 0
    END AS sensitivity_strict_no_aki_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.primary_exposure_code IN (1, 2)
            THEN 1 ELSE 0
    END AS aki_only_analysis_flag,

    CASE
        WHEN f.primary_exposure_code = 0
            THEN 1 ELSE 0
    END AS no_early_aki_flag,

    CASE
        WHEN f.primary_exposure_code = 1
            THEN 1 ELSE 0
    END AS resolved_early_aki_flag,

    CASE
        WHEN f.primary_exposure_code = 2
            THEN 1 ELSE 0
    END AS ongoing_early_aki_flag,

    CASE
        WHEN f.late_48_60h_status = 'No AKI detected through 60h'
            THEN 1 ELSE 0
    END AS confirmed_no_aki_0_60h_flag,

    CASE
        WHEN f.late_48_60h_status = 'New AKI during hour 48-60'
            THEN 1 ELSE 0
    END AS new_aki_48_60h_flag,

    CASE
        WHEN f.late_48_60h_status =
             'No early AKI; hour 48-60 status indeterminate'
            THEN 1 ELSE 0
    END AS indeterminate_aki_48_60h_flag,

    CASE
        WHEN f.onset_to_assessment_hours_36_60h >= 12
            THEN 1
        WHEN f.aki_onset_time IS NOT NULL
         AND f.assessment_scr_time_36_60h IS NOT NULL
            THEN 0
        ELSE NULL
    END AS onset_to_assessment_ge12h_flag

FROM exposure_final AS f

ORDER BY
    f.subject_id,
    f.icu_intime,
    f.stay_id;
