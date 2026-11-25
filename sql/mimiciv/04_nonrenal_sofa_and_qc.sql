/*
MIMIC-IV v3.1 / PostgreSQL
Public reproducibility SQL 04: First-day nonrenal SOFA and component QC

Sources consolidated (calculation definitions retained):
  - MIMIC_stage3B_revised_nonrenal_SOFA.sql
  - 05_MIMIC_nonrenal_SOFA_Components_QC_ID_Optimized_7794(1).sql

PURPOSE
  1) Recreate the same broad-COPD, adult, non-ESRD, first qualifying ICU
     stay and +60-hour landmark cohort as the original Stage 3B.
  2) Produce the unchanged PRIMARY MODEL COVARIATE:
       standard_nonrenal_sofa_24h = first_day_sofa.sofa - first_day_sofa.renal
     If either source value is NULL, the model covariate remains NULL.
  3) Add independent QC of the five RAW nonrenal first-day SOFA
     components (respiratory, coagulation, liver, cardiovascular, CNS),
     including missingness, component-sum checks and range checks.

POPULATION / OUTPUT
  - Landmark cohort: expected 7,794 ICU stays for the paper's
    MIMIC-IV v3.1 data and study criteria; not filtered by AKI classifiability.
  - Output: one row per stay_id, with Stage 3B original variables first
    and the additional QC variables appended.
  - Primary merge key: stay_id; cross-check keys: subject_id, hadm_id.
  - No literal study stay IDs are embedded in this public SQL.
  - Execute the COMPLETE script in one PostgreSQL session/query tab.

IMPORTANT
  - Only mimiciv_derived.first_day_sofa is used; not the hourly
    mimiciv_derived.sofa concept.
  - Raw nonrenal SOFA components are not interchangeable with the
    primary model covariate. Only the QC sums use COALESCE(..., 0).
  - first_day_sofa is an official derived concept and may draw from
    the 6 hours before ICU admission, depending on its source logic.
  - This script does NOT apply AKI trajectory or outcome filters.
  - The final cohort is reconstructed directly, not via a fixed ID list.
  - Do not publish outputs containing patient or stay identifiers.

DEPENDENCIES
  mimiciv_hosp.diagnoses_icd, mimiciv_hosp.admissions,
  mimiciv_icu.icustays, mimiciv_derived.age,
  mimiciv_derived.first_day_sofa.

VALIDATION STATUS
  Static source audit completed. PostgreSQL execution and equality to
  prior exports must be checked in the authorized MIMIC environment.
*/

-- Avoid replacing any user-created permanent table with the same name.
DROP TABLE IF EXISTS pg_temp.tmp_mimic04_final;
DROP TABLE IF EXISTS pg_temp.tmp_mimic04_sofa_source;
DROP TABLE IF EXISTS pg_temp.tmp_mimic04_stage3b;

-- 1. Create the original Stage 3B cohort and primary covariate UNCHANGED.
--    The following CTEs and final_dataset are copied from the original SQL.
CREATE TEMP TABLE tmp_mimic04_stage3b
ON COMMIT PRESERVE ROWS AS

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
ranking ICU stays. The ICU +60-h criterion is intentionally applied only after
the first qualifying ICU stay has been selected.
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

cohort_with_landmark AS (
    SELECT
        f.*,

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

stage3b_cohort AS (
    SELECT
        c.*

    FROM cohort_with_landmark AS c

    WHERE c.outcome_or_discharge_time
              > c.landmark_60h_time
),

final_dataset AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,

        c.icu_intime,
        c.icu_outtime,
        c.landmark_60h_time,

        1 AS eligible_at_60h_flag,

        /* Source SOFA values retained for QC */
        fd.sofa
            AS total_sofa_24h,

        fd.renal
            AS renal_sofa_24h,

        /* Primary acute-severity covariate */
        fd.sofa - fd.renal
            AS standard_nonrenal_sofa_24h,

        /* Missingness indicators for later complete-case checks */
        CASE
            WHEN fd.sofa IS NULL THEN 1
            ELSE 0
        END AS total_sofa_24h_missing_flag,

        CASE
            WHEN fd.renal IS NULL THEN 1
            ELSE 0
        END AS renal_sofa_24h_missing_flag,

        CASE
            WHEN fd.sofa IS NULL
              OR fd.renal IS NULL
                THEN 1
            ELSE 0
        END AS standard_nonrenal_sofa_24h_missing_flag,

        CASE
            WHEN fd.sofa IS NOT NULL
             AND fd.renal IS NOT NULL
                THEN 1
            ELSE 0
        END AS stage3b_primary_covariate_complete_flag

    FROM stage3b_cohort AS c

    LEFT JOIN mimiciv_derived.first_day_sofa AS fd
        ON c.stay_id = fd.stay_id
)

SELECT *
FROM final_dataset;

CREATE UNIQUE INDEX tmp_mimic04_stage3b_stay_idx
  ON tmp_mimic04_stage3b (stay_id);
ANALYZE tmp_mimic04_stage3b;

-- 2. Extract the official first-day SOFA source for the reconstructed cohort.
--    This replaces the revision SQL's embedded fixed stay_id array.
--    Retaining source subject_id/hadm_id permits a mapping integrity check.
CREATE TEMP TABLE tmp_mimic04_sofa_source
ON COMMIT PRESERVE ROWS AS
SELECT
  fd.subject_id AS source_subject_id,
  fd.hadm_id AS source_hadm_id,
  fd.stay_id,
  fd.sofa,
  fd.respiration,
  fd.coagulation,
  fd.liver,
  fd.cardiovascular,
  fd.cns,
  fd.renal
FROM mimiciv_derived.first_day_sofa AS fd
JOIN tmp_mimic04_stage3b AS c
  ON fd.stay_id = c.stay_id;

-- Original reviewer-QC checks, independent of database-specific ID lists.
DO $$
DECLARE
  n_duplicates integer;
  n_mapping_errors integer;
  n_source integer;
BEGIN
  SELECT COUNT(*)::integer INTO n_duplicates
  FROM (
    SELECT stay_id FROM tmp_mimic04_sofa_source
    GROUP BY stay_id HAVING COUNT(*) > 1
  ) AS x;
  IF n_duplicates > 0 THEN
    RAISE EXCEPTION
      'MIMIC 04: first_day_sofa contains % duplicated landmark stay IDs.',
      n_duplicates;
  END IF;

  SELECT COUNT(*)::integer INTO n_mapping_errors
  FROM tmp_mimic04_sofa_source AS s
  JOIN tmp_mimic04_stage3b AS c ON c.stay_id = s.stay_id
  WHERE s.source_subject_id IS DISTINCT FROM c.subject_id
     OR s.source_hadm_id IS DISTINCT FROM c.hadm_id;
  IF n_mapping_errors > 0 THEN
    RAISE EXCEPTION
      'MIMIC 04: % first_day_sofa rows have inconsistent subject/hadm IDs.',
      n_mapping_errors;
  END IF;

  SELECT COUNT(*)::integer INTO n_source FROM tmp_mimic04_sofa_source;
  IF n_source = 0 THEN
    RAISE EXCEPTION
      'MIMIC 04: no first_day_sofa records were found for the landmark cohort.';
  END IF;
END $$;

CREATE UNIQUE INDEX tmp_mimic04_source_stay_idx
  ON tmp_mimic04_sofa_source (stay_id);
ANALYZE tmp_mimic04_sofa_source;

-- 3. Append component-level QC, preserving the original Stage 3B columns.
--    The reviewer's raw component definitions and QC formulas are retained.
CREATE TEMP TABLE tmp_mimic04_final
ON COMMIT PRESERVE ROWS AS
WITH source_values AS (
  SELECT
    c.*,
    CASE WHEN s.stay_id IS NOT NULL THEN 1 ELSE 0 END
      AS first_day_sofa_source_row_present_flag,
    s.respiration AS respiratory_sofa_24h_raw,
    s.coagulation AS coagulation_sofa_24h_raw,
    s.liver AS liver_sofa_24h_raw,
    s.cardiovascular AS cardiovascular_sofa_24h_raw,
    s.cns AS cns_sofa_24h_raw,
    -- Zero imputation ONLY for the independent QC sums.
    (COALESCE(s.respiration, 0)
     + COALESCE(s.coagulation, 0)
     + COALESCE(s.liver, 0)
     + COALESCE(s.cardiovascular, 0)
     + COALESCE(s.cns, 0)) AS nonrenal_sofa_sum5_24h,
    (COALESCE(s.respiration, 0)
     + COALESCE(s.coagulation, 0)
     + COALESCE(s.liver, 0)
     + COALESCE(s.cardiovascular, 0)
     + COALESCE(s.cns, 0)
     + COALESCE(s.renal, 0)) AS total_sofa_sum6_24h,
    (CASE WHEN s.respiration IS NOT NULL THEN 1 ELSE 0 END
     + CASE WHEN s.coagulation IS NOT NULL THEN 1 ELSE 0 END
     + CASE WHEN s.liver IS NOT NULL THEN 1 ELSE 0 END
     + CASE WHEN s.cardiovascular IS NOT NULL THEN 1 ELSE 0 END
     + CASE WHEN s.cns IS NOT NULL THEN 1 ELSE 0 END)
      AS nonrenal_sofa_component_available_n,
    CASE WHEN s.respiration IS NULL THEN 1 ELSE 0 END
      AS respiratory_sofa_missing_flag,
    CASE WHEN s.coagulation IS NULL THEN 1 ELSE 0 END
      AS coagulation_sofa_missing_flag,
    CASE WHEN s.liver IS NULL THEN 1 ELSE 0 END
      AS liver_sofa_missing_flag,
    CASE WHEN s.cardiovascular IS NULL THEN 1 ELSE 0 END
      AS cardiovascular_sofa_missing_flag,
    CASE WHEN s.cns IS NULL THEN 1 ELSE 0 END
      AS cns_sofa_missing_flag,
    CASE
      WHEN (s.sofa IS NOT NULL AND (s.sofa < 0 OR s.sofa > 24))
        OR (s.renal IS NOT NULL AND (s.renal < 0 OR s.renal > 4))
        OR (s.respiration IS NOT NULL AND (s.respiration < 0 OR s.respiration > 4))
        OR (s.coagulation IS NOT NULL AND (s.coagulation < 0 OR s.coagulation > 4))
        OR (s.liver IS NOT NULL AND (s.liver < 0 OR s.liver > 4))
        OR (s.cardiovascular IS NOT NULL AND (s.cardiovascular < 0 OR s.cardiovascular > 4))
        OR (s.cns IS NOT NULL AND (s.cns < 0 OR s.cns > 4))
      THEN 1 ELSE 0
    END AS qc_any_sofa_score_out_of_range_flag
  FROM tmp_mimic04_stage3b AS c
  LEFT JOIN tmp_mimic04_sofa_source AS s
    ON s.stay_id = c.stay_id
)
SELECT
  v.*,
  CASE WHEN v.nonrenal_sofa_component_available_n = 5 THEN 1 ELSE 0 END
    AS nonrenal_sofa_all_components_available_flag,
  v.nonrenal_sofa_sum5_24h - v.standard_nonrenal_sofa_24h
    AS nonrenal_sofa_sum5_minus_original_24h,
  v.total_sofa_sum6_24h - v.total_sofa_24h
    AS total_sofa_sum6_minus_official_24h,
  CASE
    WHEN v.standard_nonrenal_sofa_24h IS NULL THEN NULL
    WHEN v.nonrenal_sofa_sum5_24h = v.standard_nonrenal_sofa_24h THEN 1
    ELSE 0
  END AS qc_nonrenal_formula_match_flag,
  CASE
    WHEN v.total_sofa_24h IS NULL THEN NULL
    WHEN v.total_sofa_sum6_24h = v.total_sofa_24h THEN 1
    ELSE 0
  END AS qc_total_formula_match_flag
FROM source_values AS v;

CREATE UNIQUE INDEX tmp_mimic04_final_stay_idx
  ON tmp_mimic04_final (stay_id);
ANALYZE tmp_mimic04_final;

-- 4. Run-time notices: no fixed IDs or checksums are required for execution.
--    These checks do not silently remove any landmark stays or force scores.
DO $$
DECLARE
  n_rows integer;
  n_subjects integer;
  n_missing integer;
BEGIN
  SELECT COUNT(*)::integer, COUNT(DISTINCT subject_id)::integer,
         SUM(standard_nonrenal_sofa_24h_missing_flag)::integer
  INTO n_rows, n_subjects, n_missing
  FROM tmp_mimic04_final;

  IF n_rows <> n_subjects THEN
    RAISE EXCEPTION
      'MIMIC 04: % rows but % distinct subjects; check cohort selection.',
      n_rows, n_subjects;
  END IF;
  IF n_rows <> 7794 THEN
    RAISE WARNING
      'MIMIC 04: % landmark stays found (paper reference = 7794).', n_rows;
  END IF;
  IF n_missing <> 3 THEN
    RAISE NOTICE
      'MIMIC 04: % missing primary nonrenal SOFA (prior review extract = 3).',
      n_missing;
  END IF;
END $$;

-- 5. FINAL EXPORT: original Stage 3B columns + component QC + QC totals.
--    This is the only result-producing SELECT; run the full script first.
SELECT
  f.*,
  qc.qc_cohort_n,
  qc.qc_official_source_missing_n,
  qc.qc_original_nonrenal_missing_n,
  qc.qc_nonrenal_formula_mismatch_n,
  qc.qc_total_formula_mismatch_n,
  qc.qc_out_of_range_n,
  qc.qc_all_five_components_observed_n
FROM tmp_mimic04_final AS f
CROSS JOIN (
  SELECT
    COUNT(*)::integer AS qc_cohort_n,
    SUM(CASE WHEN first_day_sofa_source_row_present_flag = 0
             THEN 1 ELSE 0 END)::integer
      AS qc_official_source_missing_n,
    SUM(standard_nonrenal_sofa_24h_missing_flag)::integer
      AS qc_original_nonrenal_missing_n,
    SUM(CASE WHEN qc_nonrenal_formula_match_flag = 0
             THEN 1 ELSE 0 END)::integer
      AS qc_nonrenal_formula_mismatch_n,
    SUM(CASE WHEN qc_total_formula_match_flag = 0
             THEN 1 ELSE 0 END)::integer
      AS qc_total_formula_mismatch_n,
    SUM(qc_any_sofa_score_out_of_range_flag)::integer
      AS qc_out_of_range_n,
    SUM(nonrenal_sofa_all_components_available_flag)::integer
      AS qc_all_five_components_observed_n
  FROM tmp_mimic04_final
) AS qc
ORDER BY f.subject_id, f.icu_intime, f.stay_id;
