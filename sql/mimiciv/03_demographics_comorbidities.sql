/*
COPD and Early AKI Trajectory in Critically Ill Patients
MIMIC-IV v3.1 | PostgreSQL | Public reproducibility code

03_demographics_comorbidities.sql
---------------------------------
Purpose
  Generate patient-level demographics, chronic comorbidities, AECOPD phenotype,
  Charlson Comorbidity Index and admission descriptors for the MIMIC-IV
  60-hour landmark cohort. Merge the output with 02_aki_trajectory.sql by
  stay_id during downstream analysis.

Provenance
  Based on the investigators' finalized original SQL:
  MIMIC_stage3A_revised_demographics_comorbidities_AECOPD.sql.
  The original executable SQL query, including cohort-selection criteria,
  CTEs, output variable names and SELECT ordering, is preserved unchanged.
  Only this documentation header has been standardized.

Required schemas / tables
  mimiciv_hosp.diagnoses_icd
  mimiciv_hosp.admissions
  mimiciv_hosp.patients
  mimiciv_hosp.services
  mimiciv_icu.icustays
  mimiciv_derived.age
  mimiciv_derived.charlson

Run instructions
  - Run on an authorized MIMIC-IV v3.1 PostgreSQL installation with
    the specified derived-concept tables available.
  - This is a standalone SELECT query. It independently reconstructs the
    adult, non-ESRD, first-qualifying-ICU and 60-hour-landmark cohort;
    it does NOT consume a saved output from 01_cohort_landmark.sql or
    02_aki_trajectory.sql.
  - The query returns one row per 60-hour-landmark-eligible stay_id,
    expected to be 7,794 rows. The 7,627-patient primary analytic subset
    is defined later by the AKI-classifiability flag in Stage 2.
  - Export query results only to a secure, local analysis location. Do not
    publish row-level outputs or database record identifiers.

Definitions retained from source code
  - COPD: broad ICD-9/ICD-10 diagnoses in diagnoses_icd, including AECOPD.
  - AECOPD flags: ICD-9 49121/49122 or ICD-10 J440/J441, for any and
    principal diagnosis; additional code-specific descriptive flags.
  - Eligible stay: adult; no ESRD/chronic dialysis in the qualifying
    hospitalization; earliest qualifying ICU stay per patient; remains
    alive and hospitalized at ICU hour 60.
  - Model comorbidities: heart failure, diabetes, CKD, chronic liver
    disease and malignancy from mimiciv_derived.charlson. A missing
    Charlson record leaves the corresponding comorbidity flags NULL.
  - Charlson index, admission descriptors and surgical service indicators
    are retained for descriptive analyses; primary-model adjustment
    variables are identified in the original SQL comments below.

Suggested verification after execution (not executed here)
  - Row count = distinct stay_id count = 7,794.
  - Verify cohort stay_id membership against the 60-hour-eligible subset
    of 01_cohort_landmark.sql and 02_aki_trajectory.sql.
  - Compare all columns and per-stay values with the original Stage 3A
    export before using this public-named version for publication.
  - Inspect counts of AECOPD flags and comorbidity missingness.

NOTE: This file was source-reviewed only; it has NOT been executed against
      a live MIMIC-IV PostgreSQL database in this environment.
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

        MAX(
            CASE
                WHEN d.icd_version = 9
                 AND d.icd_code IN ('49121', '49122')
                    THEN 1
                WHEN d.icd_version = 10
                 AND d.icd_code IN ('J440', 'J441')
                    THEN 1
                ELSE 0
            END
        ) AS aecopd_any_flag,

        MAX(
            CASE
                WHEN d.seq_num = 1
                 AND (
                        (
                            d.icd_version = 9
                            AND d.icd_code IN ('49121', '49122')
                        )
                        OR
                        (
                            d.icd_version = 10
                            AND d.icd_code IN ('J440', 'J441')
                        )
                     )
                    THEN 1
                ELSE 0
            END
        ) AS aecopd_principal_flag,

        /*
        More specific AECOPD-related diagnostic indicators are retained
        for descriptive checks only.
        */
        MAX(
            CASE
                WHEN d.icd_version = 9
                 AND d.icd_code = '49121'
                    THEN 1
                WHEN d.icd_version = 10
                 AND d.icd_code = 'J441'
                    THEN 1
                ELSE 0
            END
        ) AS exacerbation_code_flag,

        MAX(
            CASE
                WHEN d.icd_version = 9
                 AND d.icd_code = '49122'
                    THEN 1
                WHEN d.icd_version = 10
                 AND d.icd_code = 'J440'
                    THEN 1
                ELSE 0
            END
        ) AS acute_infection_code_flag

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
Apply age and ESRD/chronic-dialysis exclusions before ranking ICU stays.
The ICU +60-hour landmark is deliberately applied only after selection of
the first qualifying ICU stay.
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
        p.gender AS sex_source,

        c.broad_copd_principal_flag,
        c.aecopd_any_flag,
        c.aecopd_principal_flag,
        c.exacerbation_code_flag,
        c.acute_infection_code_flag,

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

stage3a_cohort AS (
    SELECT
        c.*

    FROM cohort_with_landmark AS c

    WHERE c.outcome_or_discharge_time
              > c.landmark_60h_time
),

stage3a_features AS (
    SELECT
        c.subject_id,
        c.hadm_id,
        c.stay_id,

        c.icu_intime,
        c.icu_outtime,
        c.landmark_60h_time,

        1 AS eligible_at_60h_flag,

        /* =========================================================
           Demographics
           ========================================================= */
        c.age,

        c.sex_source,

        CASE
            WHEN c.sex_source = 'F' THEN 1
            WHEN c.sex_source = 'M' THEN 0
            ELSE NULL
        END AS female_flag,

        c.race,

        /* =========================================================
           Admission descriptors
           ========================================================= */
        c.admission_type,
        c.admission_location,
        c.discharge_location,

        c.first_careunit,
        c.last_careunit,

        c.insurance,
        c.language,
        c.marital_status,

        /* =========================================================
           COPD / AECOPD phenotype
           ========================================================= */
        c.broad_copd_principal_flag,
        c.aecopd_any_flag,
        c.aecopd_principal_flag,
        c.exacerbation_code_flag,
        c.acute_infection_code_flag,

        /* =========================================================
           Principal diagnosis / service descriptors
           ========================================================= */
        pd.principal_icd_version,
        pd.principal_icd_code,

        svc.first_hospital_service,
        svc.first_service_time,

        CASE
            WHEN svc.first_hospital_service IN (
                'CSURG',
                'NSURG',
                'ORTHO',
                'PSURG',
                'SURG',
                'TSURG',
                'VSURG',
                'TRAUM',
                'GU',
                'GYN',
                'ENT'
            )
                THEN 1
            ELSE 0
        END AS surgical_service_flag,

        CASE
            WHEN pd.principal_icd_version = 9
             AND LEFT(
                    pd.principal_icd_code,
                    3
                 ) BETWEEN '460' AND '519'
                THEN 'Respiratory'

            WHEN pd.principal_icd_version = 10
             AND LEFT(
                    pd.principal_icd_code,
                    1
                 ) = 'J'
                THEN 'Respiratory'

            WHEN pd.principal_icd_version = 9
             AND (
                    LEFT(
                        pd.principal_icd_code,
                        3
                    ) BETWEEN '001' AND '139'

                    OR pd.principal_icd_code IN (
                        '0380',
                        '0381',
                        '0382',
                        '0383',
                        '0384',
                        '03840',
                        '03841',
                        '03842',
                        '03843',
                        '03844',
                        '03849',
                        '0388',
                        '0389',
                        '78552',
                        '99591',
                        '99592'
                    )
                 )
                THEN 'Infection'

            WHEN pd.principal_icd_version = 10
             AND (
                    LEFT(
                        pd.principal_icd_code,
                        1
                    ) IN ('A', 'B')

                    OR LEFT(
                        pd.principal_icd_code,
                        3
                    ) = 'R65'
                 )
                THEN 'Infection'

            WHEN pd.principal_icd_version = 9
             AND LEFT(
                    pd.principal_icd_code,
                    3
                 ) BETWEEN '390' AND '459'
                THEN 'Cardiovascular'

            WHEN pd.principal_icd_version = 10
             AND LEFT(
                    pd.principal_icd_code,
                    1
                 ) = 'I'
                THEN 'Cardiovascular'

            WHEN svc.first_hospital_service IN (
                'CSURG',
                'NSURG',
                'ORTHO',
                'PSURG',
                'SURG',
                'TSURG',
                'VSURG',
                'TRAUM',
                'GU',
                'GYN',
                'ENT'
            )
                THEN 'Surgical/postoperative'

            ELSE 'Other'
        END AS admission_category,

        CASE
            WHEN UPPER(
                    COALESCE(
                        c.admission_type,
                        ''
                    )
                 ) LIKE '%ELECTIVE%'

              OR UPPER(
                    COALESCE(
                        c.admission_type,
                        ''
                    )
                 ) LIKE '%SURGICAL SAME DAY%'
                THEN 'Elective/planned'

            WHEN UPPER(
                    COALESCE(
                        c.admission_type,
                        ''
                    )
                 ) LIKE '%URGENT%'
                THEN 'Urgent'

            WHEN UPPER(
                    COALESCE(
                        c.admission_type,
                        ''
                    )
                 ) LIKE '%EMER%'

              OR UPPER(
                    COALESCE(
                        c.admission_type,
                        ''
                    )
                 ) LIKE '%OBSERVATION%'
                THEN 'Emergency/observation'

            ELSE 'Other'
        END AS admission_urgency_category,

        /* =========================================================
           Prespecified chronic comorbidities
           Primary model uses the five flags below.
           ========================================================= */
        CASE
            WHEN ch.hadm_id IS NULL
                THEN NULL
            ELSE COALESCE(
                    ch.congestive_heart_failure,
                    0
                 )
        END AS heart_failure_flag,

        CASE
            WHEN ch.hadm_id IS NULL
                THEN NULL
            ELSE GREATEST(
                    COALESCE(
                        ch.diabetes_without_cc,
                        0
                    ),
                    COALESCE(
                        ch.diabetes_with_cc,
                        0
                    )
                 )
        END AS diabetes_flag,

        CASE
            WHEN ch.hadm_id IS NULL
                THEN NULL
            ELSE COALESCE(
                    ch.renal_disease,
                    0
                 )
        END AS ckd_flag,

        CASE
            WHEN ch.hadm_id IS NULL
                THEN NULL
            ELSE GREATEST(
                    COALESCE(
                        ch.mild_liver_disease,
                        0
                    ),
                    COALESCE(
                        ch.severe_liver_disease,
                        0
                    )
                 )
        END AS chronic_liver_disease_flag,

        CASE
            WHEN ch.hadm_id IS NULL
                THEN NULL
            ELSE GREATEST(
                    COALESCE(
                        ch.malignant_cancer,
                        0
                    ),
                    COALESCE(
                        ch.metastatic_solid_tumor,
                        0
                    )
                 )
        END AS malignancy_flag,

        /* Retained for descriptive/supplementary use, not primary adjustment */
        ch.charlson_comorbidity_index,

        /* =========================================================
           Missingness checks
           ========================================================= */
        CASE
            WHEN ch.hadm_id IS NULL THEN 1
            ELSE 0
        END AS charlson_missing_flag,

        CASE
            WHEN pd.principal_icd_code IS NULL THEN 1
            ELSE 0
        END AS principal_diagnosis_missing_flag,

        CASE
            WHEN svc.first_hospital_service IS NULL THEN 1
            ELSE 0
        END AS first_service_missing_flag

    FROM stage3a_cohort AS c

    LEFT JOIN mimiciv_derived.charlson AS ch
        ON c.hadm_id = ch.hadm_id

    LEFT JOIN LATERAL (
        SELECT
            d.icd_version
                AS principal_icd_version,

            d.icd_code
                AS principal_icd_code

        FROM mimiciv_hosp.diagnoses_icd AS d

        WHERE d.hadm_id = c.hadm_id
          AND d.seq_num = 1

        ORDER BY
            d.icd_version,
            d.icd_code

        LIMIT 1
    ) AS pd
        ON TRUE

    LEFT JOIN LATERAL (
        SELECT
            s.curr_service
                AS first_hospital_service,

            s.transfertime
                AS first_service_time

        FROM mimiciv_hosp.services AS s

        WHERE s.hadm_id = c.hadm_id

        ORDER BY
            s.transfertime,
            s.curr_service

        LIMIT 1
    ) AS svc
        ON TRUE
),

final_dataset AS (
    SELECT
        f.*,

        CASE f.admission_category
            WHEN 'Respiratory' THEN 1
            WHEN 'Infection' THEN 2
            WHEN 'Cardiovascular' THEN 3
            WHEN 'Surgical/postoperative' THEN 4
            WHEN 'Other' THEN 5
            ELSE NULL
        END AS admission_category_code,

        CASE f.admission_urgency_category
            WHEN 'Emergency/observation' THEN 1
            WHEN 'Urgent' THEN 2
            WHEN 'Elective/planned' THEN 3
            WHEN 'Other' THEN 4
            ELSE NULL
        END AS admission_urgency_code,

        /*
        Completeness of covariates produced in THIS stage only.
        Nonrenal SOFA is added in Stage 3B and should be checked later.
        */
        CASE
            WHEN f.age IS NOT NULL
             AND f.female_flag IS NOT NULL
             AND f.ckd_flag IS NOT NULL
             AND f.heart_failure_flag IS NOT NULL
             AND f.diabetes_flag IS NOT NULL
             AND f.chronic_liver_disease_flag IS NOT NULL
             AND f.malignancy_flag IS NOT NULL
                THEN 1
            ELSE 0
        END AS stage3a_primary_covariates_complete_flag

    FROM stage3a_features AS f
)

SELECT *
FROM final_dataset

ORDER BY
    subject_id,
    icu_intime,
    stay_id;


/*
Optional post-run checks after saving the result as a table:

SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT stay_id) AS n_distinct_stays,
    SUM(stage3a_primary_covariates_complete_flag) AS n_complete_stage3a_covariates,
    SUM(aecopd_any_flag) AS n_aecopd_any,
    SUM(aecopd_principal_flag) AS n_aecopd_principal
FROM your_saved_stage3a_table;

Expected:
  n_rows = n_distinct_stays = 7,794
*/
