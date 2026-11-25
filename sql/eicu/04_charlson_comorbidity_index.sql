/*
eICU-CRD v2.0
04_charlson_comorbidity_index.sql
Stage 3C (revised): Charlson Comorbidity Index (CCI) for the ICU +60-hour
COPD cohort. PostgreSQL dialect (PostgreSQL 12+).

Purpose
-------
Apply the original eICU Charlson Comorbidity Index algorithm to the same
ICU +60-hour landmark population as SQL modules 01-03. The CCI is retained
for descriptive baseline reporting, not primary regression adjustment.

Public-release change
---------------------
The locally installed dictionary.d_icd_diagnosis join has been replaced by
the explicit diagnosis codes in Supplementary Table S10:
  COPD:   ICD-9-CM 491.20 or ICD-10-CM J44.9
  AECOPD: ICD-9-CM 491.21 or ICD-10-CM J44.1

Codes are matched in eicu_crd.diagnosis.icd9code (the field may contain
multiple comma-separated codes). The exact code-matching predicate is
copied from the previously checked no-dictionary Stage 3A module (03),
whose full exported output matched the original Stage 3A result.

This file runs independently: it rebuilds the eligible cohort and does not
require temporary tables from SQL modules 01-03. The legacy aliases
copd_800070_flag and aecopd_800108_flag are retained for downstream
compatibility, although no dictionary or codeid field is used.

Cohort definition (unchanged)
-----------------------------
  1. Identify COPD/AECOPD at the hospitalization level.
  2. Adults aged >=18 years.
  3. Exclude ESRD/chronic dialysis in the index hospitalization.
  4. Select the first ICU unit stay within each qualifying hospitalization.
  5. Select the first qualifying ICU hospitalization for each patient.
  6. Only after stay selection, require the patient to be alive and
     hospitalized beyond ICU admission +60 h.

CCI algorithm (unchanged)
-------------------------
  - Use structured pasthistorypath from the selected INDEX ICU unit only.
  - Set chronic pulmonary disease to 1 for all COPD cohort members.
  - Severe liver disease supersedes mild liver disease.
  - Metastatic malignancy supersedes nonmetastatic malignancy.
  - Undocumented non-COPD conditions contribute zero points.
  - Keep past-history source-availability and component QC flags.

Output variables
----------------
  - cci_disease_only: Charlson disease weights without age points.
  - age_adjusted_cci: disease weights plus Charlson age points
    (<50: 0; 50-59: 1; 60-69: 2; 70-79: 3; >=80: 4).
  - charlson_comorbidity_index: alias for age_adjusted_cci, retained
    for the original analysis data interface.

All calculations following the opening COPD cohort CTE are unchanged from
the supplied original Stage 3C file, including every CCI condition path,
score and output field. This eICU CCI algorithm is distinct from the
five harmonized primary-model comorbidity covariates from SQL module 03.

Execution and verification
--------------------------
  - Required schemas: eicu_crd.patient, eicu_crd.diagnosis,
    eicu_crd.pasthistory. No local dictionary or fixed stay-ID list.
  - Run this file as one SELECT statement in PostgreSQL 12+.
  - Expected output: 8,495 rows and 8,495 unique patientunitstayid;
    the source algorithm sets CCI chronic pulmonary disease component
    to 1 for all included patients.
  - Compare old versus new exports by patientunitstayid and compare the
    Charlson variables and each component/QC field, not only counts.
  - The original and public versions have not been executed against the
    user's eICU database in this packaging environment.
*/


WITH copd_hospitalizations AS MATERIALIZED (
    /* Table S10 COPD/AECOPD criteria, using the eICU-CRD diagnosis table.
       This is the same predicate as public SQL module 03. */
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
        p.patienthealthsystemstayid
),

/* =========================================================================
   ESRD / chronic-dialysis exclusion at hospitalization level
   ========================================================================= */
esrd_hospitalizations AS MATERIALIZED (
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
               LIKE '%dialysis dependence%'
),

/* =========================================================================
   Adult, non-ESRD COPD candidate ICU unit stays
   ========================================================================= */
candidate_units AS MATERIALIZED (
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

        p.age AS age_raw,
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
        p.unitdischargeoffset,

        c.broad_copd_flag,
        c.copd_800070_flag,
        c.aecopd_800108_flag,
        c.aecopd_flag,

        0::integer AS esrd_chronic_dialysis_flag

    FROM patient_age_parsed AS p

    INNER JOIN copd_hospitalizations AS c
        ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

    LEFT JOIN esrd_hospitalizations AS e
        ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

    WHERE p.age_numeric BETWEEN 18 AND 120
      AND e.patienthealthsystemstayid IS NULL
),

/* =========================================================================
   First ICU unit stay within each qualifying hospital admission
   ========================================================================= */
first_unit_per_hospitalization AS MATERIALIZED (
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

        FROM candidate_units AS c
    ) AS x

    WHERE x.hospital_first_unit_rn = 1
),

/* =========================================================================
   First recorded qualifying ICU hospitalization per patient

   Preserve the same deterministic eICU ordering rule used in Stages 1-3B:
     hospitaldischargeyear -> patienthealthsystemstayid -> patientunitstayid

   The +60-hour landmark is deliberately NOT applied before this ranking.
   ========================================================================= */
first_qualifying_stay AS MATERIALIZED (
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

        FROM first_unit_per_hospitalization AS h
    ) AS x

    WHERE x.qualifying_icu_rn = 1
),

/* =========================================================================
   ICU +60-hour landmark cohort and conventional CCI age points
   ========================================================================= */
eligible_60h AS MATERIALIZED (
    SELECT
        f.*,

        CASE
            WHEN f.age < 50 THEN 0
            WHEN f.age BETWEEN 50 AND 59 THEN 1
            WHEN f.age BETWEEN 60 AND 69 THEN 2
            WHEN f.age BETWEEN 70 AND 79 THEN 3
            ELSE 4
        END::integer AS age_score,

        60::integer AS landmark_hour,
        3600::integer AS landmark_offset_min,
        1::integer AS eligible_at_60h_flag

    FROM first_qualifying_stay AS f

    WHERE f.hospitaldischargeoffset IS NOT NULL
      AND f.hospitaldischargeoffset > 3600
      AND NULLIF(
            TRIM(
                COALESCE(
                    f.hospitaldischargestatus,
                    ''
                )
            ),
            ''
          ) IS NOT NULL
),

/* =========================================================================
   Validated CCI source:
   structured past-history paths from the selected INDEX ICU unit only.

   Do not broaden this to all unit stays in the hospitalization here:
   preserving the validated CCI algorithm is preferable for Stage 3C.
   ========================================================================= */
past_history_records AS MATERIALIZED (
    SELECT
        e.patientunitstayid,
        ph.pasthistorypath

    FROM eligible_60h AS e

    INNER JOIN eicu_crd.pasthistory AS ph
        ON ph.patientunitstayid = e.patientunitstayid

    WHERE ph.pasthistorypath LIKE
          'notes/Progress Notes/Past History/Organ Systems/%'
),

past_history_qc AS (
    SELECT
        patientunitstayid,
        COUNT(*)::integer AS past_history_record_count

    FROM past_history_records

    GROUP BY
        patientunitstayid
),

/* =========================================================================
   Charlson condition flags from validated past-history paths
   ========================================================================= */
charlson_flags_from_history AS (
    SELECT
        patientunitstayid,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Cardiovascular (R)/Myocardial Infarction/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS mi,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Cardiovascular (R)/Congestive Heart Failure/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS chf,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Cardiovascular (R)/Peripheral Vascular Disease/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS pvd,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Neurologic/Strokes/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS cerebrovascular,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Neurologic/Dementia/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS dementia,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Pulmonary/COPD/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS chronic_pulm_history,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Rheumatic/Rheumatoid Arthritis/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS rheum,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Peptic Ulcer Disease/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS pud,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Liver Disease/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS mild_liver,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Endocrine (R)/%Diabetes/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS diabetes,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Neurologic/Paralysis/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS paralysis,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Renal  (R)/Renal Failure/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS renal,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Hematology/Oncology (R)/Cancer/%'
                 AND pasthistorypath NOT LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Hematology/Oncology (R)/Cancer/Metastases/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS malignancy,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Gastrointestinal (R)/Cirrhosis/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS severe_liver,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Hematology/Oncology (R)/Cancer/Metastases/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS metastatic,

        MAX(
            CASE
                WHEN pasthistorypath LIKE
                     'notes/Progress Notes/Past History/Organ Systems/Infectious Disease (R)/HIV (only)/%'
                    THEN 1
                ELSE 0
            END
        )::integer AS aids

    FROM past_history_records

    GROUP BY
        patientunitstayid
),

/* =========================================================================
   Complete Charlson condition flags.

   Chronic pulmonary disease is deliberately fixed to 1 because every patient
   entered the cohort through a COPD diagnosis. chronic_pulm_history_qc records
   whether COPD was ALSO explicitly documented in the structured past history.
   ========================================================================= */
charlson_flags AS (
    SELECT
        e.patientunitstayid,

        COALESCE(h.mi, 0)::integer AS mi,
        COALESCE(h.chf, 0)::integer AS chf,
        COALESCE(h.pvd, 0)::integer AS pvd,
        COALESCE(h.cerebrovascular, 0)::integer AS cerebrovascular,
        COALESCE(h.dementia, 0)::integer AS dementia,

        1::integer AS chronic_pulm,

        COALESCE(
            h.chronic_pulm_history,
            0
        )::integer AS chronic_pulm_history_qc,

        COALESCE(h.rheum, 0)::integer AS rheum,
        COALESCE(h.pud, 0)::integer AS pud,
        COALESCE(h.mild_liver, 0)::integer AS mild_liver,
        COALESCE(h.diabetes, 0)::integer AS diabetes,
        COALESCE(h.paralysis, 0)::integer AS paralysis,
        COALESCE(h.renal, 0)::integer AS renal,
        COALESCE(h.malignancy, 0)::integer AS malignancy,
        COALESCE(h.severe_liver, 0)::integer AS severe_liver,
        COALESCE(h.metastatic, 0)::integer AS metastatic,
        COALESCE(h.aids, 0)::integer AS aids

    FROM eligible_60h AS e

    LEFT JOIN charlson_flags_from_history AS h
        ON h.patientunitstayid = e.patientunitstayid
),

/* =========================================================================
   Disease-weighted Charlson score

   Supersession rules:
     severe liver disease -> do not additionally count mild liver disease
     metastatic cancer    -> do not additionally count non-metastatic malignancy
   ========================================================================= */
cci_scores AS (
    SELECT
        patientunitstayid,

        (
              mi
            + chf
            + pvd
            + cerebrovascular
            + dementia
            + chronic_pulm
            + rheum
            + pud
            + CASE
                  WHEN severe_liver = 1 THEN 0
                  ELSE mild_liver
              END
            + diabetes
            + 2 * paralysis
            + 2 * renal
            + CASE
                  WHEN metastatic = 1 THEN 0
                  ELSE 2 * malignancy
              END
            + 3 * severe_liver
            + 6 * metastatic
            + 6 * aids
        )::integer AS cci_disease_only,

        mi,
        chf,
        pvd,
        cerebrovascular,
        dementia,
        chronic_pulm,
        chronic_pulm_history_qc,
        rheum,
        pud,
        mild_liver,
        severe_liver,
        diabetes,
        paralysis,
        renal,
        malignancy,
        metastatic,
        aids

    FROM charlson_flags
),

/* =========================================================================
   Final Stage 3C dataset
   ========================================================================= */
final_dataset AS (
    SELECT
        e.patient_key,
        e.uniquepid,
        e.patienthealthsystemstayid,
        e.patientunitstayid,

        e.hospitalid,
        e.wardid,

        e.landmark_hour,
        e.landmark_offset_min,
        e.eligible_at_60h_flag,

        COUNT(*) OVER ()::integer AS cohort_n_qc,

        e.age_raw,
        e.age,
        e.age_score,

        e.gender AS sex_source,
        e.ethnicity,

        e.unitvisitnumber,
        e.hospitaldischargeyear,
        e.hospitaladmitoffset,

        ROUND(
            e.hospitaladmitoffset / 60.0::numeric,
            2
        ) AS hours_hospital_admission_to_icu,

        e.broad_copd_flag,
        e.copd_800070_flag,
        e.aecopd_800108_flag,
        e.aecopd_flag,
        e.esrd_chronic_dialysis_flag,

        /* Main descriptive CCI variables */
        cs.cci_disease_only,

        (
            cs.cci_disease_only
            + e.age_score
        )::integer AS age_adjusted_cci,

        (
            cs.cci_disease_only
            + e.age_score
        )::integer AS charlson_comorbidity_index,

        CASE
            WHEN cs.cci_disease_only = 0 THEN 0
            WHEN cs.cci_disease_only BETWEEN 1 AND 2 THEN 1
            ELSE 2
        END::integer AS cci_disease_group,

        CASE
            WHEN (
                    cs.cci_disease_only
                    + e.age_score
                 ) = 0
                THEN 0

            WHEN (
                    cs.cci_disease_only
                    + e.age_score
                 ) BETWEEN 1 AND 2
                THEN 1

            ELSE 2
        END::integer AS age_adjusted_cci_group,

        /* Component flags retained for QC / audit */
        cs.mi,
        cs.chf,
        cs.pvd,
        cs.cerebrovascular,
        cs.dementia,
        cs.chronic_pulm,
        cs.rheum,
        cs.pud,
        cs.mild_liver,
        cs.severe_liver,
        cs.diabetes,
        cs.paralysis,
        cs.renal,
        cs.malignancy,
        cs.metastatic,
        cs.aids,

        /* Past-history source QC */
        CASE
            WHEN COALESCE(
                    phq.past_history_record_count,
                    0
                 ) > 0
                THEN 1
            ELSE 0
        END::integer AS has_past_history_record_qc,

        CASE
            WHEN COALESCE(
                    phq.past_history_record_count,
                    0
                 ) = 0
                THEN 1
            ELSE 0
        END::integer AS past_history_missing_qc_flag,

        COALESCE(
            phq.past_history_record_count,
            0
        )::integer AS past_history_record_count_qc,

        cs.chronic_pulm_history_qc,

        /*
        Should always be 1 in this COPD cohort. If not, there is a coding error.
        */
        CASE
            WHEN cs.chronic_pulm = 1 THEN 1
            ELSE 0
        END::integer AS cci_copd_component_fixed_qc_flag

    FROM eligible_60h AS e

    INNER JOIN cci_scores AS cs
        ON cs.patientunitstayid = e.patientunitstayid

    LEFT JOIN past_history_qc AS phq
        ON phq.patientunitstayid = e.patientunitstayid
)

SELECT *
FROM final_dataset

ORDER BY
    patient_key,
    patienthealthsystemstayid,
    patientunitstayid;


/* =========================================================================
   Recommended post-run QC

After saving the result, check:

SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT patientunitstayid) AS n_distinct_stays,

    MIN(cci_disease_only) AS min_cci_disease_only,
    MAX(cci_disease_only) AS max_cci_disease_only,

    MIN(charlson_comorbidity_index) AS min_charlson,
    MAX(charlson_comorbidity_index) AS max_charlson,

    SUM(has_past_history_record_qc) AS n_with_past_history,
    SUM(past_history_missing_qc_flag) AS n_without_past_history,

    SUM(chronic_pulm_history_qc) AS n_copd_documented_in_past_history,
    SUM(cci_copd_component_fixed_qc_flag) AS n_copd_component_fixed_correctly

FROM your_saved_eicu_stage3c_table;

Expected:
  n_rows = n_distinct_stays = 8,495
  min_cci_disease_only >= 1
  n_copd_component_fixed_correctly = 8,495


Suggested distribution check:

SELECT
    cci_disease_only,
    COUNT(*) AS n
FROM your_saved_eicu_stage3c_table
GROUP BY cci_disease_only
ORDER BY cci_disease_only;


Important interpretation:
  - A patient with no structured past-history record can still have
    cci_disease_only = 1 because chronic pulmonary disease is fixed to 1
    by cohort definition.
  - Therefore, CCI source missingness should NOT be interpreted as a
    missing numerical CCI value. Use the QC flag to describe source coverage.
   ========================================================================= */
