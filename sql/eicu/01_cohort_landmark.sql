/*
eICU-CRD v2.0
Stage 1 (revised): broad COPD cohort, 60-hour landmark flow,
timing anchors, and post-landmark in-hospital outcome
PostgreSQL dialect

Purpose
-------
Construct the cohort-flow foundation for the revised fixed-reference
creatinine-defined AKI trajectory study.

This stage DOES NOT extract creatinine and DOES NOT classify AKI.
It returns all first recorded qualifying ICU hospitalizations BEFORE the
final 60-hour landmark exclusion, so the output can be used for Figure 1
and cohort auditing.

Revised study definitions
-------------------------
1. Broad COPD hospitalization (Supplementary Table S10):
     COPD: ICD-9-CM 491.20 or ICD-10-CM J44.9;
     AECOPD: ICD-9-CM 491.21 or ICD-10-CM J44.1.
     Codes are matched in eicu_crd.diagnosis.icd9code; no local
     diagnosis dictionary is required.

2. AECOPD subgroup:
     aecopd_flag = 1 when 491.21 or J44.1 is identified.

3. Adults aged >=18 years.

4. Exclude ESRD or chronic dialysis identified during the index hospital
   admission at the patienthealthsystemstayid level.

5. Select the first ICU unit stay within each qualifying hospital admission.

6. Select the first recorded qualifying ICU hospitalization per patient.

7. Apply the 60-hour landmark ONLY AFTER the first qualifying stay has been
   selected. Eligibility requires the patient to be alive and still
   hospitalized beyond ICU admission +60 hours.
   Remaining physically in the ICU at hour 60 is NOT required.

8. Primary outcome:
     in-hospital death after the 60-hour landmark.

Timing anchors for the revised Stage 2
--------------------------------------
All eICU offsets are minutes relative to ICU/unit admission.

  ICU 0 h                         = 0
  end of surrogate-reference     = 1440 min (24 h)
  primary recovery start         = 2160 min (36 h)
  narrow recovery start          = 2520 min (42 h)
  target renal-status time       = 2880 min (48 h)
  narrow recovery end            = 3240 min (54 h)
  primary recovery end           = 3600 min (60 h)
  outcome landmark               = 3600 min (60 h)

Reference-sCr lookback anchor
-----------------------------
The revised Stage 2 will use:
  most recent measured pre-ICU sCr within 7 days during the same
  hospitalization -> otherwise ICU 0-24 h minimum sCr.

For the pre-ICU portion, the lower offset bound is:
  max(hospitaladmitoffset, -10080 min)

Important:
  - 54 h is ONLY the end of the narrow 42-54-h trajectory sensitivity window.
  - There is NO 54-h outcome landmark in the revised design.
  - Stage 2 should retain eligible_at_60h_flag = 1.

Primary merge key:
  patientunitstayid

Cross-check keys:
  uniquepid
  patienthealthsystemstayid

Public-release adaptation
-------------------------
The cohort diagnosis-code predicate is taken from the previously checked
no-dictionary eICU Stage 3A implementation. Only the COPD/AECOPD diagnosis
identification block and explanatory comments differ from the original
Stage 1 script; later cohort, landmark, and outcome logic are unchanged.
The legacy columns copd_800070_flag and aecopd_800108_flag are retained
solely for downstream compatibility; they are derived directly from the
Table S10 diagnosis codes and do not query a dictionary.

Execution/validation
--------------------
Run the entire file in one PostgreSQL session (temporary tables are used).
The final Stage 1 SELECT includes pre-landmark stays; filter
eligible_at_60h_flag = 1 for the expected 8,495 landmark stays.
No database execution was possible in the packaging environment; verify
count, unique patientunitstayid and patient-level equality locally.
*/


/* =========================================================================
   0. Clean up
   ========================================================================= */

DROP TABLE IF EXISTS tmp_eicu_stage1_final;
DROP TABLE IF EXISTS tmp_eicu_first_qualifying_stay;
DROP TABLE IF EXISTS tmp_eicu_first_unit_per_hospitalization;
DROP TABLE IF EXISTS tmp_eicu_candidate_units;
DROP TABLE IF EXISTS tmp_eicu_esrd_hospitalizations;
DROP TABLE IF EXISTS tmp_eicu_copd_hospitalizations;


/* =========================================================================
   1. Broad COPD hospitalizations and AECOPD indicator

   Public/reproducible implementation without the local dictionary schema.
   Codes correspond to Supplementary Table S10:
     COPD:   ICD-9-CM 491.20; ICD-10-CM J44.9
     AECOPD: ICD-9-CM 491.21; ICD-10-CM J44.1
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_copd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_copd_hospitalizations_idx
    ON tmp_eicu_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_copd_hospitalizations;


/* =========================================================================
   2. Exclude ESRD / chronic dialysis at the hospital-admission level

   Any qualifying diagnosis recorded in any ICU unit stay belonging to the
   same patienthealthsystemstayid excludes that hospital admission.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_esrd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_esrd_hospitalizations_idx
    ON tmp_eicu_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD candidate ICU unit stays

   Age:
     - numeric age is converted directly;
     - eICU '> 89' is represented as 90;
     - other nonnumeric / missing ages remain NULL and are excluded.

   patient_key:
     - use uniquepid when available;
     - otherwise use a stay-specific fallback so the record is not lost.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_candidate_units
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
    c.aecopd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,

    0::integer AS esrd_chronic_dialysis_flag

FROM patient_age_parsed AS p

INNER JOIN tmp_eicu_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu_candidate_units_stay_idx
    ON tmp_eicu_candidate_units (patientunitstayid);

CREATE INDEX tmp_eicu_candidate_units_hospitalization_idx
    ON tmp_eicu_candidate_units (patienthealthsystemstayid);

CREATE INDEX tmp_eicu_candidate_units_patient_idx
    ON tmp_eicu_candidate_units (patient_key);

ANALYZE tmp_eicu_candidate_units;


/* =========================================================================
   4. First ICU unit stay within each qualifying hospital admission

   This prevents ICU-unit transfers from being treated as separate index
   hospitalizations.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_first_unit_per_hospitalization
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

    FROM tmp_eicu_candidate_units AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_first_unit_per_hospitalization_stay_idx
    ON tmp_eicu_first_unit_per_hospitalization (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_first_unit_per_hospitalization_hadm_idx
    ON tmp_eicu_first_unit_per_hospitalization (patienthealthsystemstayid);

ANALYZE tmp_eicu_first_unit_per_hospitalization;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient

   eICU lacks a complete absolute hospital-admission date suitable for
   reliably ordering all repeated hospitalizations across years. Preserve
   the deterministic prior rule:
     hospitaldischargeyear -> patienthealthsystemstayid -> patientunitstayid

   IMPORTANT:
   The 60-h landmark is deliberately NOT applied before this ranking.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_first_qualifying_stay
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

    FROM tmp_eicu_first_unit_per_hospitalization AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_first_qualifying_stay_stay_idx
    ON tmp_eicu_first_qualifying_stay (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_first_qualifying_stay_patient_idx
    ON tmp_eicu_first_qualifying_stay (patient_key);

ANALYZE tmp_eicu_first_qualifying_stay;


/* =========================================================================
   6. Revised timing anchors, 60-h flow variables, and post-landmark outcome

   hospitaldischargeoffset is in minutes relative to ICU/unit admission.

   Eligibility at hour 60:
     - hospital discharge time/status must be available;
     - hospitaldischargeoffset > 3600 min.

   A patient may have left the ICU before 60 h and still remain eligible
   provided the patient remains hospitalized beyond hour 60.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage1_final
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
        ) AS hospitaldischargestatus_normalized,

        CASE
            WHEN
                   LOWER(TRIM(COALESCE(f.hospitaldischargestatus, '')))
                       LIKE '%expired%'

                OR LOWER(TRIM(COALESCE(f.hospitaldischargestatus, '')))
                       LIKE '%dead%'

                OR LOWER(TRIM(COALESCE(f.hospitaldischargestatus, '')))
                       LIKE '%deceased%'

                THEN 1

            WHEN NULLIF(
                    TRIM(COALESCE(f.hospitaldischargestatus, '')),
                    ''
                 ) IS NOT NULL
                THEN 0

            ELSE NULL
        END AS hospital_expire_flag

    FROM tmp_eicu_first_qualifying_stay AS f
)

SELECT
    s.patient_key,
    s.uniquepid,
    s.patienthealthsystemstayid,
    s.patientunitstayid,

    s.age,
    s.gender,
    s.ethnicity,

    s.hospitalid,
    s.wardid,

    s.hospitaldischargeyear,
    s.hospitaladmittime24,
    s.hospitaladmitoffset,
    s.hospitaladmitsource,
    s.hospitaldischargetime24,
    s.hospitaldischargeoffset,
    s.hospitaldischargestatus,
    s.hospitaldischargelocation,

    s.unitvisitnumber,
    s.unitstaytype,
    s.unittype,
    s.unitadmittime24,
    s.unitadmitsource,
    s.unitdischargetime24,
    s.unitdischargeoffset,
    s.unitdischargestatus,
    s.unitdischargelocation,

    s.apacheadmissiondx,

    s.broad_copd_flag,
    s.copd_800070_flag,
    s.aecopd_800108_flag,
    s.aecopd_flag,
    s.esrd_chronic_dialysis_flag,

    /* -------------------------------------------------------------
       Revised trajectory/reference timing anchors
       ------------------------------------------------------------- */

    0::integer
        AS icu_admission_offset,

    1440::integer
        AS reference_fallback_0_24h_end_offset,

    2160::integer
        AS recovery_window_36_60_start_offset,

    2520::integer
        AS recovery_window_42_54_start_offset,

    2880::integer
        AS target_48h_offset,

    2880::integer
        AS early_aki_0_48h_end_offset,

    3240::integer
        AS recovery_window_42_54_end_offset,

    3600::integer
        AS recovery_window_36_60_end_offset,

    3600::integer
        AS landmark_60h_offset,

    /*
    Pre-ICU fixed-reference lower bound:
      most recent measured sCr between this offset and <0.
    */
    GREATEST(
        COALESCE(
            s.hospitaladmitoffset,
            -10080
        ),
        -10080
    )::integer AS scr_lookback_start_offset,

    s.hospital_expire_flag,

    /* -------------------------------------------------------------
       60-hour landmark eligibility and flow
       ------------------------------------------------------------- */

    CASE
        WHEN s.hospitaldischargeoffset IS NOT NULL
         AND s.hospitaldischargestatus_normalized <> ''
         AND s.hospitaldischargeoffset > 3600
            THEN 1
        ELSE 0
    END AS eligible_at_60h_flag,

    CASE
        WHEN s.hospital_expire_flag = 1
         AND s.hospitaldischargeoffset IS NOT NULL
         AND s.hospitaldischargeoffset <= 3600
            THEN 1
        ELSE 0
    END AS death_by_60h_flag,

    CASE
        WHEN s.hospital_expire_flag = 0
         AND s.hospitaldischargeoffset IS NOT NULL
         AND s.hospitaldischargeoffset <= 3600
            THEN 1
        ELSE 0
    END AS alive_discharge_by_60h_flag,

    CASE
        WHEN s.hospitaldischargeoffset IS NULL
          OR s.hospitaldischargestatus_normalized = ''
            THEN 'missing_outcome_or_discharge_info'

        WHEN s.hospital_expire_flag = 1
         AND s.hospitaldischargeoffset <= 3600
            THEN 'death_by_60h'

        WHEN s.hospital_expire_flag = 0
         AND s.hospitaldischargeoffset <= 3600
            THEN 'alive_discharge_by_60h'

        WHEN s.hospitaldischargeoffset > 3600
            THEN 'eligible_at_60h'

        ELSE 'other'
    END AS landmark_60h_status,

    /* -------------------------------------------------------------
       Post-landmark in-hospital outcome
       ------------------------------------------------------------- */

    CASE
        WHEN s.hospital_expire_flag = 1
         AND s.hospitaldischargeoffset > 3600
            THEN 1

        WHEN s.hospital_expire_flag = 0
         AND s.hospitaldischargeoffset > 3600
            THEN 0

        ELSE NULL
    END AS hospital_death_after_60h,

    CASE
        WHEN s.hospitaldischargeoffset > 3600
            THEN (
                s.hospitaldischargeoffset - 3600
            ) / 1440.0

        ELSE NULL
    END AS followup_days_after_60h,

    /* -------------------------------------------------------------
       Useful cohort-audit variables
       ------------------------------------------------------------- */

    CASE
        WHEN s.unitdischargeoffset IS NOT NULL
         AND s.unitdischargeoffset <= 3600
         AND s.hospitaldischargeoffset > 3600
            THEN 1
        ELSE 0
    END AS left_icu_before_60h_but_still_hospitalized_flag,

    CASE
        WHEN s.hospitaldischargeoffset IS NULL
            THEN 1
        ELSE 0
    END AS hospitaldischargeoffset_missing_flag,

    CASE
        WHEN s.hospitaldischargestatus_normalized = ''
            THEN 1
        ELSE 0
    END AS hospitaldischargestatus_missing_flag

FROM status_normalized AS s;

CREATE UNIQUE INDEX tmp_eicu_stage1_final_stay_idx
    ON tmp_eicu_stage1_final (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage1_final_patient_idx
    ON tmp_eicu_stage1_final (patient_key);

ANALYZE tmp_eicu_stage1_final;


/* =========================================================================
   7. Final Stage 1 output

   IMPORTANT:
   Do NOT filter eligible_at_60h_flag here.
   Keeping the pre-landmark flow population allows Figure 1 counts.
   The revised Stage 2 should restrict to eligible_at_60h_flag = 1.
   ========================================================================= */

SELECT
    patient_key,
    uniquepid,
    patienthealthsystemstayid,
    patientunitstayid,

    age,
    gender,
    ethnicity,

    hospitalid,
    wardid,

    hospitaldischargeyear,
    hospitaladmittime24,
    hospitaladmitoffset,
    hospitaladmitsource,
    hospitaldischargetime24,
    hospitaldischargeoffset,
    hospitaldischargestatus,
    hospitaldischargelocation,

    unitvisitnumber,
    unitstaytype,
    unittype,
    unitadmittime24,
    unitadmitsource,
    unitdischargetime24,
    unitdischargeoffset,
    unitdischargestatus,
    unitdischargelocation,

    apacheadmissiondx,

    broad_copd_flag,
    copd_800070_flag,
    aecopd_800108_flag,
    aecopd_flag,
    esrd_chronic_dialysis_flag,

    icu_admission_offset,
    reference_fallback_0_24h_end_offset,
    recovery_window_36_60_start_offset,
    recovery_window_42_54_start_offset,
    target_48h_offset,
    early_aki_0_48h_end_offset,
    recovery_window_42_54_end_offset,
    recovery_window_36_60_end_offset,
    landmark_60h_offset,
    scr_lookback_start_offset,

    hospital_expire_flag,

    eligible_at_60h_flag,
    death_by_60h_flag,
    alive_discharge_by_60h_flag,
    landmark_60h_status,

    hospital_death_after_60h,
    followup_days_after_60h,

    left_icu_before_60h_but_still_hospitalized_flag,
    hospitaldischargeoffset_missing_flag,
    hospitaldischargestatus_missing_flag

FROM tmp_eicu_stage1_final

ORDER BY
    patient_key,
    hospitaldischargeyear,
    patienthealthsystemstayid,
    patientunitstayid;


/* =========================================================================
   Optional post-run checks

   After saving the Stage 1 output as a persistent/result table, verify:

   1) one row per selected patient/index hospitalization;
   2) eligible_at_60h_flag = 1 should reproduce the established
      eICU 60-h cohort (previous feasibility analysis: n = 8,495);
   3) the landmark status categories sum to the total Stage 1 population.

Example:

SELECT
    COUNT(*) AS n_stage1_total,
    COUNT(DISTINCT patientunitstayid) AS n_distinct_stays,
    SUM(eligible_at_60h_flag) AS n_eligible_at_60h,
    SUM(death_by_60h_flag) AS n_death_by_60h,
    SUM(alive_discharge_by_60h_flag) AS n_alive_discharge_by_60h,
    SUM(hospitaldischargeoffset_missing_flag) AS n_missing_discharge_offset,
    SUM(hospitaldischargestatus_missing_flag) AS n_missing_discharge_status
FROM tmp_eicu_stage1_final;

Expected key cross-check:
  n_eligible_at_60h = 8,495
*/
