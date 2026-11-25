/*
COPD and Early AKI Trajectory in Critically Ill Patients
MIMIC-IV v3.1 | PostgreSQL | Public reproducibility code

08_rrt_events.sql
-----------------
Purpose
  Recreate the +60-hour landmark cohort and extract the original RRT
  evidence/event long table used for the revision RRT-exclusion sensitivity
  analysis (Supplementary Table S9). It is an EVENT extraction, not an
  alternative definition of the primary AKI trajectory.

Provenance
  Based on the investigators' original restricted export:
  03_MIMIC_RRT_Long_Official_ID_Optimized_7794.sql.
  The source's downstream same-hospitalization ICU mapping, RRT evidence
  selection, event time filters, event flags, projection, and ordering are
  preserved exactly. Only the fixed stay_id list / MD5 signature check
  and original anchor-based cohort construction have been replaced.
  The eligible +60h cohort is generated with the same CTEs used in
  02_aki_trajectory.sql and 07_serial_creatinine.sql.

Required relations
  mimiciv_hosp.diagnoses_icd
  mimiciv_hosp.admissions
  mimiciv_icu.icustays
  mimiciv_icu.procedureevents
  mimiciv_derived.age
  mimiciv_derived.rrt   (official MIMIC derived treatment concept)

Event evidence and timing
  - Source 1: mimiciv_derived.rrt (charttime observations).
    dialysis_active=1 is active-treatment evidence; dialysis_present=1
    without active dialysis is NOT sufficient proof of treatment.
  - Source 2: mimiciv_icu.procedureevents (recorded procedure intervals).
    True treatment procedure IDs and catheter/filter-only IDs remain
    distinguishable. Treatment starttime is retained; record status is
    provided for downstream review.
  - Search EVERY ICU stay of each index hospital admission; retain the
    original index_stay_id and actual event_stay_id separately.
  - Extract events between hospital admission and discharge, retaining
    flags for time relative to index ICU 0-48h and 0-60h. Note that
    timestamp evidence does not guarantee treatment initiation time.
  - Source overlap is intentional. Do not count individual rows as
    separate RRT sessions, or classify treatment by dialysis_present alone.
  - Preserve one null-event row per patient without captured RRT evidence.

Run / output
  - Run the complete script in ONE PostgreSQL session. Its final SELECT
    is the only data result; export that table privately for downstream R.
  - No patient- or stay-ID list is embedded in this public SQL.
  - Expected distinct index_stay_id values: 7,794 (+60h landmark cohort).
    Total event rows will exceed or equal 7,794 and depend on records.
  - Neither code nor event flags independently determine whether recorded
    RRT is acute or chronic, nor guarantee ward dialysis ascertainment.
  - Patient-level exports must not be distributed in the public repository.

Validation
  Source-level/static audit only. This public variant has NOT been run
  against the study's authorized MIMIC-IV instance. Before release, compare
  index_stay_id sets AND all event-level fields with the frozen original
  export, not merely the cohort size.
*/

-- 0. Check official source relations before executing any data extraction.
DO $$
DECLARE rel text;
BEGIN
    FOREACH rel IN ARRAY ARRAY[
        'mimiciv_hosp.diagnoses_icd',
        'mimiciv_hosp.admissions',
        'mimiciv_icu.icustays',
        'mimiciv_icu.procedureevents',
        'mimiciv_derived.age',
        'mimiciv_derived.rrt'
    ] LOOP
        IF to_regclass(rel) IS NULL THEN
            RAISE EXCEPTION 'MIMIC 08: required relation not found: %', rel;
        END IF;
    END LOOP;
END $$;

-- Do not drop or overwrite the original investigators' private temp tables.
DROP TABLE IF EXISTS pg_temp.tmp_mimic08_events;
DROP TABLE IF EXISTS pg_temp.tmp_mimic08_related_icu;
DROP TABLE IF EXISTS pg_temp.tmp_mimic08_cohort;

-- 1. Rebuild the SAME first-qualifying-ICU / +60h landmark cohort used
--    in 02 and 07, BEFORE excluding records with unclassifiable AKI.
CREATE TEMP TABLE tmp_mimic08_cohort
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
    c.stay_id AS index_stay_id,
    c.icu_intime AS index_icu_intime,
    c.icu_outtime AS index_icu_outtime,
    c.admittime AS hospital_admittime,
    c.dischtime AS hospital_dischtime,
    c.deathtime AS hospital_deathtime,
    c.hospital_expire_flag,
    c.landmark_60h_time,
    c.outcome_or_discharge_time
FROM stage2_cohort AS c;

CREATE UNIQUE INDEX tmp_mimic08_cohort_stay_idx
    ON tmp_mimic08_cohort (index_stay_id);
CREATE INDEX tmp_mimic08_cohort_admission_idx
    ON tmp_mimic08_cohort (subject_id, hadm_id);
ANALYZE tmp_mimic08_cohort;

-- 2. Validate landmark timing and notify if the cohort size differs from
--    the manuscript on an alternate installation/version.
DO $$
DECLARE
    n_cohort integer;
    n_invalid_landmark integer;
BEGIN
    SELECT COUNT(*)::integer INTO n_cohort
    FROM tmp_mimic08_cohort;

    IF n_cohort <> 7794 THEN
        RAISE NOTICE
            'MIMIC 08: landmark N=% (manuscript N=7794). Check cohort SQL and database version before using results.',
            n_cohort;
    END IF;

    SELECT COUNT(*)::integer INTO n_invalid_landmark
    FROM tmp_mimic08_cohort
    WHERE index_icu_intime IS NULL
       OR hospital_admittime IS NULL
       OR hospital_dischtime IS NULL
       OR outcome_or_discharge_time IS NULL
       OR outcome_or_discharge_time <= landmark_60h_time;

    IF n_invalid_landmark > 0 THEN
        RAISE EXCEPTION
            'MIMIC 08: % cohort stays fail the index-hospitalization +60h timing QC.',
            n_invalid_landmark;
    END IF;
END $$;

-- 3. Preserve original source-specific RRT record extraction and
--    output structure; only referenced cohort/temp table names changed.
-- 4. Map ALL ICU stay IDs in the SAME hospitalization. This permits
--    capturing other-unit/re-admission ICU RRT within that admission,
--    while leaving the original index ICU stay unambiguous.
CREATE TEMP TABLE tmp_mimic08_related_icu AS
SELECT
  c.index_stay_id,
  i.stay_id AS event_stay_id,
  c.hadm_id,
  c.subject_id
FROM tmp_mimic08_cohort AS c
JOIN mimiciv_icu.icustays AS i
  ON i.hadm_id = c.hadm_id
 AND i.subject_id = c.subject_id;

CREATE UNIQUE INDEX ON tmp_mimic08_related_icu (event_stay_id);
CREATE INDEX ON tmp_mimic08_related_icu (index_stay_id);
ANALYZE tmp_mimic08_related_icu;

-- 5. Capture original official RRT observations and original procedure
--    starts/ends separately, without substituting charttime for starttime.
--    De-duplicate identical derived (stay, time, modality) rows, but do
--    NOT collapse different modalities or procedure orders.
CREATE TEMP TABLE tmp_mimic08_events AS
WITH official_rrt_deduplicated AS (
  SELECT
    rel.index_stay_id,
    r.stay_id AS event_stay_id,
    r.charttime AS event_time,
    r.dialysis_type::text AS modality_or_label,
    MAX(COALESCE(r.dialysis_present, 0))::integer AS dialysis_present,
    MAX(COALESCE(r.dialysis_active, 0))::integer AS dialysis_active
  FROM tmp_mimic08_related_icu AS rel
  JOIN tmp_mimic08_cohort AS c
    ON c.index_stay_id = rel.index_stay_id
  JOIN mimiciv_derived.rrt AS r
    ON r.stay_id = rel.event_stay_id
   AND r.charttime >= c.hospital_admittime
   AND r.charttime <= c.hospital_dischtime
  WHERE COALESCE(r.dialysis_present, 0) = 1
     OR COALESCE(r.dialysis_active, 0) = 1
  GROUP BY rel.index_stay_id, r.stay_id, r.charttime, r.dialysis_type
),
rrt_official AS (
  SELECT
    index_stay_id,
    event_stay_id,
    event_time,
    NULL::timestamp AS event_endtime,
    'mimiciv_derived.rrt'::text AS event_source,
    NULL::integer AS source_itemid,
    NULL::text AS source_event_id,
    modality_or_label,
    CASE WHEN dialysis_active = 1
         THEN 'derived_active_dialysis'
         ELSE 'derived_presence_only' END::text AS evidence_class,
    'charttime_observation_not_initiation'::text AS event_time_kind,
    dialysis_present AS official_dialysis_present,
    dialysis_active AS official_dialysis_active,
    NULL::text AS record_status,
    NULL::numeric AS recorded_value,
    NULL::text AS recorded_unit
  FROM official_rrt_deduplicated
),
rrt_procedures AS (
  SELECT
    rel.index_stay_id,
    p.stay_id AS event_stay_id,
    p.starttime AS event_time,
    p.endtime AS event_endtime,
    'mimiciv_icu.procedureevents'::text AS event_source,
    p.itemid::integer AS source_itemid,
    p.orderid::text AS source_event_id,
    CASE p.itemid
      WHEN 225441 THEN 'IHD'
      WHEN 225802 THEN 'CRRT'
      WHEN 225803 THEN 'CVVHD'
      WHEN 225805 THEN 'Peritoneal'
      WHEN 225809 THEN 'CVVHDF'
      WHEN 225955 THEN 'SCUF'
      WHEN 224270 THEN 'Dialysis catheter only'
      WHEN 225436 THEN 'CRRT filter change only'
    END::text AS modality_or_label,
    CASE WHEN p.itemid IN (224270,225436)
         THEN 'access_or_filter_only'
         ELSE 'direct_treatment_procedure' END::text AS evidence_class,
    'recorded_procedure_start'::text AS event_time_kind,
    NULL::integer AS official_dialysis_present,
    NULL::integer AS official_dialysis_active,
    p.statusdescription::text AS record_status,
    p.value::numeric AS recorded_value,
    p.valueuom::text AS recorded_unit
  FROM tmp_mimic08_related_icu AS rel
  JOIN tmp_mimic08_cohort AS c
    ON c.index_stay_id = rel.index_stay_id
  JOIN mimiciv_icu.procedureevents AS p
    ON p.stay_id = rel.event_stay_id
   AND p.starttime >= c.hospital_admittime
   AND p.starttime <= c.hospital_dischtime
  WHERE p.itemid IN (225441,225802,225803,225805,225809,225955,224270,225436)
    AND p.value IS NOT NULL
)
SELECT * FROM rrt_official
UNION ALL
SELECT * FROM rrt_procedures;

CREATE INDEX ON tmp_mimic08_events (index_stay_id, event_time);
ANALYZE tmp_mimic08_events;

-- 6. FINAL AND ONLY DATA RESULT SET. Preserve every indexed patient,
--    including one NULL-event row when no RRT evidence was found.
--    DO NOT simply sum rows across sources (overlap is expected).
SELECT
  c.subject_id,
  c.hadm_id,
  c.index_stay_id,
  c.index_icu_intime,
  c.index_icu_outtime,
  c.hospital_admittime,
  c.hospital_dischtime,
  c.landmark_60h_time,
  e.event_stay_id,
  e.event_time,
  e.event_endtime,
  CASE WHEN e.event_time IS NOT NULL
       THEN EXTRACT(EPOCH FROM (e.event_time - c.index_icu_intime)) / 3600.0
       ELSE NULL END AS hours_from_index_icu,
  CASE WHEN e.event_time IS NOT NULL AND e.event_time < c.index_icu_intime
       THEN 1 ELSE 0 END AS event_before_index_icu_flag,
  CASE WHEN e.event_time IS NOT NULL AND e.event_time >= c.index_icu_intime
              AND e.event_time <= c.index_icu_intime + INTERVAL '48 hour'
       THEN 1 ELSE 0 END AS event_in_icu_0_48h_flag,
  CASE WHEN e.event_time IS NOT NULL AND e.event_time >= c.index_icu_intime
              AND e.event_time <= c.landmark_60h_time
       THEN 1 ELSE 0 END AS event_in_icu_0_60h_flag,
  CASE WHEN e.event_time IS NOT NULL AND e.event_time <= c.landmark_60h_time
       THEN 1 ELSE 0 END AS event_at_or_before_landmark_flag,
  CASE WHEN e.event_time IS NOT NULL AND e.event_time > c.landmark_60h_time
       THEN 1 ELSE 0 END AS event_after_landmark_flag,
  CASE WHEN e.event_time IS NOT NULL AND e.event_stay_id = c.index_stay_id
       THEN 1 ELSE 0 END AS event_in_index_stay_flag,
  e.event_source,
  e.source_itemid,
  e.source_event_id,
  e.modality_or_label,
  e.evidence_class,
  e.event_time_kind,
  e.official_dialysis_present,
  e.official_dialysis_active,
  e.record_status,
  e.recorded_value,
  e.recorded_unit,
  CASE WHEN e.event_time IS NOT NULL AND e.event_endtime IS NOT NULL
              AND e.event_endtime < e.event_time
       THEN 1 ELSE 0 END AS invalid_procedure_interval_flag,
  CASE WHEN e.event_source = 'mimiciv_icu.procedureevents'
              AND e.evidence_class = 'direct_treatment_procedure'
       THEN 1 ELSE 0 END AS direct_treatment_procedure_flag,
  CASE WHEN e.event_source = 'mimiciv_derived.rrt'
              AND e.official_dialysis_active = 1
       THEN 1 ELSE 0 END AS derived_active_dialysis_flag,
  CASE WHEN e.event_time IS NULL THEN 0 ELSE 1 END AS has_rrt_evidence_record
FROM tmp_mimic08_cohort AS c
LEFT JOIN tmp_mimic08_events AS e
  ON e.index_stay_id = c.index_stay_id
ORDER BY
  c.index_stay_id,
  e.event_time NULLS LAST,
  e.event_source,
  e.source_itemid NULLS LAST,
  e.source_event_id NULLS LAST;
