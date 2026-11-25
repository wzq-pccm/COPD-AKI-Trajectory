/*
eICU-CRD v2.0
03_demographics_comorbidities.sql
Stage 3A (revised): demographics, chronic comorbidities, COPD phenotype,
and admission descriptors
PostgreSQL dialect

Purpose
-------
Construct the SAME ICU +60-hour landmark cohort used in revised eICU
Stages 1-2 and extract the baseline covariates needed for the harmonized
MIMIC/eICU primary model.

Primary-model covariates produced in this stage:
  - age
  - female sex
  - chronic kidney disease
  - heart failure
  - diabetes mellitus
  - chronic liver disease
  - malignancy

Variables retained for descriptive / sensitivity analyses:
  - AECOPD flag
  - ethnicity
  - hospital / unit admission descriptors
  - APACHE admission diagnosis
  - hospital characteristics
  - elective-surgery indicator
  - source-availability / QC indicators for comorbidity ascertainment

Not produced here:
  - nonrenal SOFA (public module 05)
  - Charlson Comorbidity Index (public module 04)
  - invasive mechanical ventilation / vasopressor use (public module 07)
  - AKI trajectory (public module 02)

Population
----------
  1. Broad COPD hospitalization identified directly from eICU diagnosis codes:
       COPD: ICD-9-CM 491.20 or ICD-10-CM J44.9;
       acute COPD exacerbation: ICD-9-CM 491.21 or ICD-10-CM J44.1.
  2. Age >=18 years.
  3. Exclude ESRD / chronic dialysis at the index-hospitalization level.
  4. Select the first ICU unit stay within each qualifying hospitalization.
  5. Select the first recorded qualifying ICU hospitalization per patient.
  6. AFTER stay selection, require survival and continued hospitalization
     beyond ICU admission +60 hours.

Expected output
---------------
  - one row per patientunitstayid in the +60-hour landmark cohort
  - expected row count with the current cohort definition: 8,495
  - primary merge key: patientunitstayid
  - cross-check keys: uniquepid, patienthealthsystemstayid
  - expected AECOPD count in landmark cohort: 3,266
  - temporary result table: tmp_eicu_stage3a_final

Reproducibility notes
---------------------
  - Standalone PostgreSQL script; uses eicu_crd tables only.
  - No local dictionary schema, fixed stay-ID list, or external intermediate
    table is required.
  - Original output column names copd_800070_flag and aecopd_800108_flag
    are deliberately retained for compatibility with existing R imports.
  - Previously checked against the dictionary-based Stage 3A export:
    8,495 stays, 53 columns, and no differing cells after stay-ID alignment.
    That comparison verifies the cohort/output of this particular module;
    other eICU SQL modules require separate verification.

Comorbidity ascertainment
-------------------------
eICU does not provide the MIMIC-derived Charlson table. To preserve the
same CLINICAL covariates across databases, the five chronic comorbidities
are identified from eICU-specific sources:

  1. diagnosis:
       active problems from ALL ICU unit stays belonging to the same
       patienthealthsystemstayid;

  2. pasthistory:
       structured past medical history from ALL ICU unit stays belonging
       to the same patienthealthsystemstayid;

  3. apachepredvar, where a directly relevant chronic-health field exists:
       diabetes;
       cirrhosis / hepatic failure;
       metastatic cancer.

A positive signal from ANY applicable source sets the comorbidity flag to 1.
Otherwise the flag is 0. Source-availability variables are retained so that
unexpected interface sparsity can be checked after the query runs.

Important
---------
  - "gender" is the eICU source field. The manuscript should use "sex".
  - ESRD/chronic dialysis remains an exclusion, not a model covariate.
  - No renal laboratory value is used in this stage.
  - No comorbidity flag is derived from the revised AKI exposure.
*/


/* =========================================================================
   0. Clean up
   ========================================================================= */

DROP TABLE IF EXISTS tmp_eicu_stage3a_final;
DROP TABLE IF EXISTS tmp_eicu_stage3a_features;
DROP TABLE IF EXISTS tmp_eicu_stage3a_comorbidity;
DROP TABLE IF EXISTS tmp_eicu_stage3a_apache_hosp;
DROP TABLE IF EXISTS tmp_eicu_stage3a_pasthistory_hosp;
DROP TABLE IF EXISTS tmp_eicu_stage3a_diagnosis_hosp;
DROP TABLE IF EXISTS tmp_eicu_stage3a_cohort;
DROP TABLE IF EXISTS tmp_eicu_stage3a_first_qualifying;
DROP TABLE IF EXISTS tmp_eicu_stage3a_first_unit;
DROP TABLE IF EXISTS tmp_eicu_stage3a_candidates;
DROP TABLE IF EXISTS tmp_eicu_stage3a_esrd_hospitalizations;
DROP TABLE IF EXISTS tmp_eicu_stage3a_copd_hospitalizations;


/* =========================================================================
   1. Broad COPD hospitalizations and AECOPD indicator

   Public/reproducible implementation without the local dictionary schema.
   Codes correspond to Supplementary Table S10:
     COPD:   ICD-9-CM 491.20; ICD-10-CM J44.9
     AECOPD: ICD-9-CM 491.21; ICD-10-CM J44.1
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_copd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_stage3a_copd_hospitalizations_idx
    ON tmp_eicu_stage3a_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3a_copd_hospitalizations;


/* =========================================================================
   2. ESRD / chronic dialysis exclusion at hospitalization level
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_esrd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_stage3a_esrd_hospitalizations_idx
    ON tmp_eicu_stage3a_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3a_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD candidate ICU unit stays
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_candidates
ON COMMIT PRESERVE ROWS AS
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

    p.age_numeric AS age,
    p.gender,
    p.ethnicity,

    p.hospitalid,
    p.wardid,

    p.hospitaldischargeyear,
    p.hospitaladmittime24,
    p.hospitaladmitoffset,
    p.hospitaladmitsource,
    p.hospitaldischargetime24,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,
    p.hospitaldischargelocation,

    p.unitvisitnumber,
    p.unitstaytype,
    p.unittype,
    p.unitadmittime24,
    p.unitadmitsource,
    p.unitdischargetime24,
    p.unitdischargeoffset,
    p.unitdischargestatus,
    p.unitdischargelocation,

    p.apacheadmissiondx,

    c.broad_copd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,
    c.aecopd_flag,

    0::integer AS esrd_chronic_dialysis_flag

FROM patient_age_parsed AS p

INNER JOIN tmp_eicu_stage3a_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu_stage3a_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu_stage3a_candidates_stay_idx
    ON tmp_eicu_stage3a_candidates (patientunitstayid);

CREATE INDEX tmp_eicu_stage3a_candidates_hosp_idx
    ON tmp_eicu_stage3a_candidates (patienthealthsystemstayid);

CREATE INDEX tmp_eicu_stage3a_candidates_patient_idx
    ON tmp_eicu_stage3a_candidates (patient_key);

ANALYZE tmp_eicu_stage3a_candidates;


/* =========================================================================
   4. First ICU unit stay in each qualifying hospitalization
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_first_unit
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

    FROM tmp_eicu_stage3a_candidates AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage3a_first_unit_stay_idx
    ON tmp_eicu_stage3a_first_unit (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage3a_first_unit_hosp_idx
    ON tmp_eicu_stage3a_first_unit (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3a_first_unit;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_first_qualifying
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

    FROM tmp_eicu_stage3a_first_unit AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage3a_first_qualifying_stay_idx
    ON tmp_eicu_stage3a_first_qualifying (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage3a_first_qualifying_patient_idx
    ON tmp_eicu_stage3a_first_qualifying (patient_key);

ANALYZE tmp_eicu_stage3a_first_qualifying;


/* =========================================================================
   6. ICU +60-hour landmark cohort

   Hospital discharge, not ICU-unit discharge, determines eligibility.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_cohort
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

    FROM tmp_eicu_stage3a_first_qualifying AS f
)

SELECT
    s.*,
    3600::integer AS landmark_60h_offset

FROM status_normalized AS s

WHERE s.hospitaldischargeoffset IS NOT NULL
  AND s.hospitaldischargestatus_normalized <> ''
  AND s.hospitaldischargeoffset > 3600;

CREATE UNIQUE INDEX tmp_eicu_stage3a_cohort_stay_idx
    ON tmp_eicu_stage3a_cohort (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage3a_cohort_hosp_idx
    ON tmp_eicu_stage3a_cohort (patienthealthsystemstayid);

CREATE INDEX tmp_eicu_stage3a_cohort_hospital_idx
    ON tmp_eicu_stage3a_cohort (hospitalid);

ANALYZE tmp_eicu_stage3a_cohort;


/* =========================================================================
   7. Diagnosis-derived chronic comorbidity evidence

   All unit stays within the selected index hospitalization are searched.
   diagnosis in eICU is an active-problem list; both mapped ICD strings and
   diagnosisstring are used.

   NOTE:
   - CKD patterns intentionally require chronic renal/kidney disease or
     chronic-renal ICD families; acute renal failure alone is not CKD.
   - malignancy is intended to approximate the solid-tumor / metastatic
     malignancy construct used in the MIMIC model.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_diagnosis_hosp
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patienthealthsystemstayid,

    COUNT(d.diagnosisid)::integer
        AS diagnosis_record_n,

    MAX(
        CASE
            WHEN
                /* Charlson-like CHF ICD-9 families */
                COALESCE(d.icd9code, '') ~* E'(^|[^0-9A-Z])(398\\.?91|402\\.?(01|11|91)|404\\.?(01|03|11|13|91|93)|425\\.?[4-9]|428(\\.?[0-9]+)?)([^0-9A-Z]|$)'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%congestive heart failure%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%heart failure%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       ~ '(^|[^a-z])chf([^a-z]|$)'

                THEN 1
            ELSE 0
        END
    )::integer AS heart_failure_dx_flag,

    MAX(
        CASE
            WHEN
                COALESCE(d.icd9code, '') ~* E'(^|[^0-9A-Z])250(\\.?[0-9]{1,2})?([^0-9A-Z]|$)'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%diabetes mellitus%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%diabetic%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       ~ '(^|[^a-z])diabetes([^a-z]|$)'

                THEN 1
            ELSE 0
        END
    )::integer AS diabetes_dx_flag,

    MAX(
        CASE
            WHEN
                COALESCE(d.icd9code, '') ~* E'(^|[^0-9A-Z])(582(\\.?[0-9]+)?|583(\\.?[0-9]+)?|585\\.?[1-5]|586|588(\\.?[0-9]+)?|V42\\.?0)([^0-9A-Z]|$)'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%chronic kidney disease%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%chronic renal disease%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%chronic renal failure%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%chronic renal insufficiency%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       ~ '(^|[^a-z])ckd([^a-z]|$)'

                THEN 1
            ELSE 0
        END
    )::integer AS ckd_dx_flag,

    MAX(
        CASE
            WHEN
                COALESCE(d.icd9code, '') ~* E'(^|[^0-9A-Z])(456\\.?[0-2]|571(\\.?[0-9]+)?|572\\.?[2-8]|070\\.?(22|23|32|33|44|54)|V42\\.?7)([^0-9A-Z]|$)'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%cirrhos%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%chronic liver disease%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%hepatic failure%'

                OR LOWER(COALESCE(d.diagnosisstring, ''))
                       LIKE '%portal hypertension%'

                THEN 1
            ELSE 0
        END
    )::integer AS chronic_liver_disease_dx_flag,

    MAX(
        CASE
            WHEN
                /* Solid malignancy / metastatic disease; excludes ICD-9 173 */
                COALESCE(d.icd9code, '') ~* E'(^|[^0-9A-Z])(1(4[0-9]|5[0-9]|6[0-9]|7[0-2]|7[4-9]|8[0-9]|9[0-9])\\.?[0-9A-Z]*)([^0-9A-Z]|$)'

                OR (
                    (
                           LOWER(COALESCE(d.diagnosisstring, ''))
                               LIKE '%cancer%'

                        OR LOWER(COALESCE(d.diagnosisstring, ''))
                               LIKE '%malignan%'

                        OR LOWER(COALESCE(d.diagnosisstring, ''))
                               LIKE '%carcinoma%'

                        OR LOWER(COALESCE(d.diagnosisstring, ''))
                               LIKE '%solid tumor%'

                        OR LOWER(COALESCE(d.diagnosisstring, ''))
                               LIKE '%metastatic%'
                    )
                    AND LOWER(COALESCE(d.diagnosisstring, ''))
                            NOT LIKE '%leukemia%'
                    AND LOWER(COALESCE(d.diagnosisstring, ''))
                            NOT LIKE '%lymphoma%'
                )

                THEN 1
            ELSE 0
        END
    )::integer AS malignancy_dx_flag

FROM tmp_eicu_stage3a_cohort AS c

INNER JOIN eicu_crd.patient AS p_all
    ON p_all.patienthealthsystemstayid = c.patienthealthsystemstayid

LEFT JOIN eicu_crd.diagnosis AS d
    ON d.patientunitstayid = p_all.patientunitstayid

GROUP BY
    c.patienthealthsystemstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3a_diagnosis_hosp_idx
    ON tmp_eicu_stage3a_diagnosis_hosp (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3a_diagnosis_hosp;


/* =========================================================================
   8. Past-history-derived chronic comorbidity evidence

   pasthistory is explicitly intended to describe conditions present before
   the current ICU presentation. All text-bearing fields are searched.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_pasthistory_hosp
ON COMMIT PRESERVE ROWS AS
WITH ph_normalized AS (
    SELECT
        c.patienthealthsystemstayid,
        ph.pasthistoryid,

        LOWER(
            CONCAT_WS(
                ' | ',
                COALESCE(ph.pasthistorypath, ''),
                COALESCE(ph.pasthistoryvalue, ''),
                COALESCE(ph.pasthistoryvaluetext, '')
            )
        ) AS ph_text

    FROM tmp_eicu_stage3a_cohort AS c

    INNER JOIN eicu_crd.patient AS p_all
        ON p_all.patienthealthsystemstayid = c.patienthealthsystemstayid

    LEFT JOIN eicu_crd.pasthistory AS ph
        ON ph.patientunitstayid = p_all.patientunitstayid
)

SELECT
    patienthealthsystemstayid,

    COUNT(pasthistoryid)::integer
        AS pasthistory_record_n,

    MAX(
        CASE
            WHEN
                   ph_text LIKE '%congestive heart failure%'
                OR ph_text LIKE '%heart failure%'
                OR ph_text ~ '(^|[^a-z])chf([^a-z]|$)'
                THEN 1
            ELSE 0
        END
    )::integer AS heart_failure_ph_flag,

    MAX(
        CASE
            WHEN
                   ph_text LIKE '%diabetes%'
                OR ph_text LIKE '%diabetic%'
                THEN 1
            ELSE 0
        END
    )::integer AS diabetes_ph_flag,

    MAX(
        CASE
            WHEN
                   ph_text LIKE '%chronic kidney%'
                OR ph_text LIKE '%chronic renal%'
                OR ph_text LIKE '%renal insufficiency%'
                OR ph_text ~ '(^|[^a-z])ckd([^a-z]|$)'
                THEN 1
            ELSE 0
        END
    )::integer AS ckd_ph_flag,

    MAX(
        CASE
            WHEN
                   ph_text LIKE '%cirrhos%'
                OR ph_text LIKE '%chronic liver%'
                OR ph_text LIKE '%hepatic failure%'
                OR ph_text LIKE '%portal hypertension%'
                THEN 1
            ELSE 0
        END
    )::integer AS chronic_liver_disease_ph_flag,

    MAX(
        CASE
            WHEN
                (
                       ph_text LIKE '%cancer%'
                    OR ph_text LIKE '%malignan%'
                    OR ph_text LIKE '%carcinoma%'
                    OR ph_text LIKE '%solid tumor%'
                    OR ph_text LIKE '%metastatic%'
                )
                AND ph_text NOT LIKE '%leukemia%'
                AND ph_text NOT LIKE '%lymphoma%'
                THEN 1
            ELSE 0
        END
    )::integer AS malignancy_ph_flag

FROM ph_normalized

GROUP BY
    patienthealthsystemstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3a_pasthistory_hosp_idx
    ON tmp_eicu_stage3a_pasthistory_hosp (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3a_pasthistory_hosp;


/* =========================================================================
   9. APACHE chronic-health fields

   apachepredvar contains direct chronic-health indicators for diabetes,
   cirrhosis/hepatic failure, and metastatic cancer. Positive (=1) values
   are used as additional evidence. Values other than 1 do not create a
   positive flag.

   All unit stays within the selected hospitalization are searched.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_apache_hosp
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patienthealthsystemstayid,

    COUNT(apv.apachepredvarid)::integer
        AS apachepredvar_record_n,

    MAX(
        CASE
            WHEN apv.diabetes = 1 THEN 1
            ELSE 0
        END
    )::integer AS diabetes_apache_flag,

    MAX(
        CASE
            WHEN apv.cirrhosis = 1
              OR apv.hepaticfailure = 1
                THEN 1
            ELSE 0
        END
    )::integer AS chronic_liver_disease_apache_flag,

    MAX(
        CASE
            WHEN apv.metastaticcancer = 1
                THEN 1
            ELSE 0
        END
    )::integer AS malignancy_apache_flag,

    MAX(
        CASE
            WHEN apv.electivesurgery = 1
                THEN 1
            ELSE 0
        END
    )::integer AS elective_surgery_apache_flag

FROM tmp_eicu_stage3a_cohort AS c

INNER JOIN eicu_crd.patient AS p_all
    ON p_all.patienthealthsystemstayid = c.patienthealthsystemstayid

LEFT JOIN eicu_crd.apachepredvar AS apv
    ON apv.patientunitstayid = p_all.patientunitstayid

GROUP BY
    c.patienthealthsystemstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3a_apache_hosp_idx
    ON tmp_eicu_stage3a_apache_hosp (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3a_apache_hosp;


/* =========================================================================
   10. Harmonized chronic comorbidity flags

   Any positive evidence from an applicable source -> 1.
   Otherwise -> 0.

   Separate source-level flags are retained in the final output for QC.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_comorbidity
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patienthealthsystemstayid,

    COALESCE(dx.diagnosis_record_n, 0)
        AS diagnosis_record_n,

    COALESCE(ph.pasthistory_record_n, 0)
        AS pasthistory_record_n,

    COALESCE(ap.apachepredvar_record_n, 0)
        AS apachepredvar_record_n,

    CASE
        WHEN COALESCE(dx.diagnosis_record_n, 0) > 0 THEN 1
        ELSE 0
    END AS diagnosis_data_available_flag,

    CASE
        WHEN COALESCE(ph.pasthistory_record_n, 0) > 0 THEN 1
        ELSE 0
    END AS pasthistory_data_available_flag,

    CASE
        WHEN COALESCE(ap.apachepredvar_record_n, 0) > 0 THEN 1
        ELSE 0
    END AS apachepredvar_data_available_flag,

    /* Source-level flags */
    COALESCE(dx.heart_failure_dx_flag, 0)
        AS heart_failure_dx_flag,
    COALESCE(ph.heart_failure_ph_flag, 0)
        AS heart_failure_ph_flag,

    COALESCE(dx.diabetes_dx_flag, 0)
        AS diabetes_dx_flag,
    COALESCE(ph.diabetes_ph_flag, 0)
        AS diabetes_ph_flag,
    COALESCE(ap.diabetes_apache_flag, 0)
        AS diabetes_apache_flag,

    COALESCE(dx.ckd_dx_flag, 0)
        AS ckd_dx_flag,
    COALESCE(ph.ckd_ph_flag, 0)
        AS ckd_ph_flag,

    COALESCE(dx.chronic_liver_disease_dx_flag, 0)
        AS chronic_liver_disease_dx_flag,
    COALESCE(ph.chronic_liver_disease_ph_flag, 0)
        AS chronic_liver_disease_ph_flag,
    COALESCE(ap.chronic_liver_disease_apache_flag, 0)
        AS chronic_liver_disease_apache_flag,

    COALESCE(dx.malignancy_dx_flag, 0)
        AS malignancy_dx_flag,
    COALESCE(ph.malignancy_ph_flag, 0)
        AS malignancy_ph_flag,
    COALESCE(ap.malignancy_apache_flag, 0)
        AS malignancy_apache_flag,

    /* Final model flags */
    GREATEST(
        COALESCE(dx.heart_failure_dx_flag, 0),
        COALESCE(ph.heart_failure_ph_flag, 0)
    )::integer AS heart_failure_flag,

    GREATEST(
        COALESCE(dx.diabetes_dx_flag, 0),
        COALESCE(ph.diabetes_ph_flag, 0),
        COALESCE(ap.diabetes_apache_flag, 0)
    )::integer AS diabetes_flag,

    GREATEST(
        COALESCE(dx.ckd_dx_flag, 0),
        COALESCE(ph.ckd_ph_flag, 0)
    )::integer AS ckd_flag,

    GREATEST(
        COALESCE(dx.chronic_liver_disease_dx_flag, 0),
        COALESCE(ph.chronic_liver_disease_ph_flag, 0),
        COALESCE(ap.chronic_liver_disease_apache_flag, 0)
    )::integer AS chronic_liver_disease_flag,

    GREATEST(
        COALESCE(dx.malignancy_dx_flag, 0),
        COALESCE(ph.malignancy_ph_flag, 0),
        COALESCE(ap.malignancy_apache_flag, 0)
    )::integer AS malignancy_flag,

    COALESCE(ap.elective_surgery_apache_flag, 0)
        AS elective_surgery_apache_flag

FROM tmp_eicu_stage3a_cohort AS c

LEFT JOIN tmp_eicu_stage3a_diagnosis_hosp AS dx
    ON dx.patienthealthsystemstayid = c.patienthealthsystemstayid

LEFT JOIN tmp_eicu_stage3a_pasthistory_hosp AS ph
    ON ph.patienthealthsystemstayid = c.patienthealthsystemstayid

LEFT JOIN tmp_eicu_stage3a_apache_hosp AS ap
    ON ap.patienthealthsystemstayid = c.patienthealthsystemstayid;

CREATE UNIQUE INDEX tmp_eicu_stage3a_comorbidity_idx
    ON tmp_eicu_stage3a_comorbidity (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage3a_comorbidity;


/* =========================================================================
   11. Assemble Stage 3A features
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_features
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patient_key,
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.patientunitstayid,

    1::integer AS eligible_at_60h_flag,
    c.landmark_60h_offset,

    /* Demographics */
    c.age,

    c.gender
        AS sex_source,

    CASE
        WHEN LOWER(TRIM(COALESCE(c.gender, ''))) = 'female'
            THEN 1
        WHEN LOWER(TRIM(COALESCE(c.gender, ''))) = 'male'
            THEN 0
        ELSE NULL
    END AS female_flag,

    c.ethnicity,

    /* COPD phenotype */
    c.broad_copd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,
    c.aecopd_flag,

    /* Primary chronic comorbidities */
    cm.heart_failure_flag,
    cm.diabetes_flag,
    cm.ckd_flag,
    cm.chronic_liver_disease_flag,
    cm.malignancy_flag,

    /* Source-level comorbidity QC */
    cm.diagnosis_record_n,
    cm.pasthistory_record_n,
    cm.apachepredvar_record_n,

    cm.diagnosis_data_available_flag,
    cm.pasthistory_data_available_flag,
    cm.apachepredvar_data_available_flag,

    cm.heart_failure_dx_flag,
    cm.heart_failure_ph_flag,

    cm.diabetes_dx_flag,
    cm.diabetes_ph_flag,
    cm.diabetes_apache_flag,

    cm.ckd_dx_flag,
    cm.ckd_ph_flag,

    cm.chronic_liver_disease_dx_flag,
    cm.chronic_liver_disease_ph_flag,
    cm.chronic_liver_disease_apache_flag,

    cm.malignancy_dx_flag,
    cm.malignancy_ph_flag,
    cm.malignancy_apache_flag,

    /* Admission / hospital descriptors for descriptive use only */
    c.hospitalid,
    c.wardid,

    c.hospitaldischargeyear,

    c.hospitaladmitsource,
    c.unitadmitsource,
    c.unitstaytype,
    c.unittype,

    c.apacheadmissiondx,

    cm.elective_surgery_apache_flag,

    CASE
        WHEN cm.elective_surgery_apache_flag = 1
            THEN 1

        WHEN LOWER(COALESCE(c.unitadmitsource, '')) LIKE '%operating room%'
          OR LOWER(COALESCE(c.unitadmitsource, '')) LIKE '%recovery room%'
          OR LOWER(COALESCE(c.unitadmitsource, '')) LIKE '%pacu%'
            THEN 1

        ELSE 0
    END::integer AS surgical_admission_flag,

    h.numbedscategory
        AS hospital_bed_category,

    h.teachingstatus
        AS hospital_teaching_status,

    h.region
        AS hospital_region

FROM tmp_eicu_stage3a_cohort AS c

LEFT JOIN tmp_eicu_stage3a_comorbidity AS cm
    ON cm.patienthealthsystemstayid = c.patienthealthsystemstayid

LEFT JOIN eicu_crd.hospital AS h
    ON h.hospitalid = c.hospitalid;

CREATE UNIQUE INDEX tmp_eicu_stage3a_features_idx
    ON tmp_eicu_stage3a_features (patientunitstayid);

ANALYZE tmp_eicu_stage3a_features;


/* =========================================================================
   12. Final Stage 3A output and completeness flag
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage3a_final
ON COMMIT PRESERVE ROWS AS
SELECT
    f.*,

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
    END::integer AS stage3a_primary_covariates_complete_flag,

    CASE
        WHEN f.diagnosis_data_available_flag = 1
          OR f.pasthistory_data_available_flag = 1
          OR f.apachepredvar_data_available_flag = 1
            THEN 1
        ELSE 0
    END::integer AS any_comorbidity_source_available_flag

FROM tmp_eicu_stage3a_features AS f;

CREATE UNIQUE INDEX tmp_eicu_stage3a_final_stay_idx
    ON tmp_eicu_stage3a_final (patientunitstayid);

ANALYZE tmp_eicu_stage3a_final;


/* =========================================================================
   Final patient-level Stage 3A output
   ========================================================================= */

SELECT *
FROM tmp_eicu_stage3a_final

ORDER BY
    patient_key,
    hospitaldischargeyear,
    patienthealthsystemstayid,
    patientunitstayid;


/* =========================================================================
   Recommended post-run checks after saving the result as a table

SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT patientunitstayid) AS n_distinct_stays,

    SUM(stage3a_primary_covariates_complete_flag)
        AS n_complete_stage3a_covariates,

    SUM(aecopd_flag)
        AS n_aecopd,

    SUM(heart_failure_flag)
        AS n_heart_failure,

    SUM(diabetes_flag)
        AS n_diabetes,

    SUM(ckd_flag)
        AS n_ckd,

    SUM(chronic_liver_disease_flag)
        AS n_chronic_liver_disease,

    SUM(malignancy_flag)
        AS n_malignancy,

    SUM(diagnosis_data_available_flag)
        AS n_with_diagnosis_data,

    SUM(pasthistory_data_available_flag)
        AS n_with_pasthistory_data,

    SUM(apachepredvar_data_available_flag)
        AS n_with_apachepredvar_data,

    SUM(any_comorbidity_source_available_flag)
        AS n_with_any_comorbidity_source

FROM your_saved_eicu_stage3a_table;

Expected key cohort check:
  n_rows = n_distinct_stays = 8,495
   ========================================================================= */
