/*
eICU-CRD v2.0
Stage 2 (revised): fixed-reference creatinine-defined early AKI trajectory
and post-landmark in-hospital outcome
PostgreSQL dialect

Purpose
-------
Implement the SAME fixed-reference AKI-trajectory framework used in the
revised MIMIC-IV analysis, adapted only to eICU's minute-based offsets.

Primary exposure definition
---------------------------
1. Reference sCr is ascertained hierarchically:
     a) most recent measured serum creatinine within 7 days before ICU
        admission and during the same index hospitalization;
     b) if unavailable, minimum measured serum creatinine during ICU hours
        0-24 as a surrogate reference.

2. Early creatinine-defined AKI is assessed during ICU hours 0-48 using the
   fixed reference sCr. A creatinine measurement meets the AKI threshold if:
     current sCr >= reference sCr + 0.3 mg/dL
       OR
     current sCr >= 1.5 x reference sCr.

3. Among patients with early AKI, identify the FIRST OBSERVED creatinine
   measurement meeting the AKI threshold during ICU hours 0-48.

4. Primary renal-status assessment:
     - window: ICU hours 36-60 (2160-3600 min)
     - choose the sCr closest to ICU hour 48 (2880 min)
     - assessment must occur at or after first AKI-threshold fulfillment
     - ties are resolved in favor of the later measurement.

5. Resolved early AKI:
     assessment sCr < reference sCr + 0.3 mg/dL
       AND
     assessment sCr < 1.5 x reference sCr.
   Otherwise the early AKI is classified as ongoing.

6. Primary exposure code:
     0 = no early AKI during ICU hours 0-48
     1 = resolved early AKI around ICU hour 48
     2 = ongoing early AKI around ICU hour 48

Landmark / outcome
------------------
- Only patients alive and still hospitalized beyond ICU +60 h enter Stage 2.
- Remaining physically in the ICU at hour 60 is NOT required.
- Outcome is in-hospital death after the 60-h landmark.

Preserved sensitivity / supplementary variables
------------------------------------------------
- Narrow recovery window: ICU hours 42-54.
- Measured pre-ICU reference sCr only.
- At least 12 h between first AKI-threshold fulfillment and assessment.
- Strict no-AKI reference group with no fixed-reference AKI detected through
  ICU hour 60.
- Peak creatinine-defined AKI stage among early-AKI patients.
- New fixed-reference AKI during ICU hours 48-60.
- ICU 0-12-h minimum sCr and first ICU sCr during 0-24 h for diagnostics.

Creatinine extraction
---------------------
- Source: eicu_crd.lab.
- Serum creatinine is identified using labname = 'creatinine' after lower-case
  trimming. This matches the reference-sCr feasibility analysis.
- Only positive numeric creatinine values are used.
- Urine-output criteria are not used.

Important harmonization detail
------------------------------
The pre-ICU reference lower bound is restricted to the index hospitalization:
  max(hospitaladmitoffset, -10080 min).

However, the COMBINED creatinine extraction must still include ICU-period
measurements even in rare records where hospitaladmitoffset is positive.
Therefore the extraction lower bound is:
  min(scr_lookback_start_offset, 0).
The hospital-admission lower bound is applied specifically when selecting the
PRE-ICU reference measurement.

Expected output
---------------
- One row per patientunitstayid in the ICU +60-h landmark cohort.
- Expected Stage 2 cohort size: 8,495.
- Previous feasibility cross-check for the revised reference rule:
    pre-ICU measured reference available: 3,508
    final reference available using 0-24-h fallback: 8,004
    reference missing: 491

Primary merge key:
  patientunitstayid

Cross-check keys:
  uniquepid
  patienthealthsystemstayid

Public-release adaptation
-------------------------
COPD/AECOPD cohort identification is based directly on Supplementary
Table S10 codes from eicu_crd.diagnosis.icd9code (this field may contain
comma-separated ICD-9-CM and ICD-10-CM diagnosis codes):
  COPD:   ICD-9-CM 491.20 or ICD-10-CM J44.9;
  AECOPD: ICD-9-CM 491.21 or ICD-10-CM J44.1.
This exact predicate is shared with the already cross-checked no-dictionary
Stage 3A implementation; only the opening diagnosis-identification block
has changed compared with the original Stage 2 analysis script.
Legacy variable names copd_800070_flag and aecopd_800108_flag are kept
for compatibility, but use the explicit Table S10 codes rather than
an auxiliary locally installed diagnosis dictionary.

Execution and verification
--------------------------
- PostgreSQL, eICU-CRD v2.0, with eicu_crd schema.
- Run the entire SQL script in one database session. It creates TEMP
  tables and rebuilds the 60-hour landmark cohort independently of 01.
- Expected: 8,495 unique landmark patientunitstayid in Stage 2 output.
  Among these, 7,738 have a classified primary exposure:
  no early AKI = 6,644; resolved = 465; ongoing = 629.
- The full output retains non-classifiable landmark stays. Select
  primary_analysis_flag = 1 to form the 7,738-patient analysis dataset.
- Confirm patient IDs, AKI groups, reference-sCr availability, and all
  other fields against your original locally executed Stage 2 result.
- This file has been statically checked, not executed against an eICU DB.
*/


/* =========================================================================
   0. Clean up
   ========================================================================= */

DROP TABLE IF EXISTS tmp_eicu_stage2_final;
DROP TABLE IF EXISTS tmp_eicu_stage2_exposure_final;
DROP TABLE IF EXISTS tmp_eicu_stage2_exposure_base;
DROP TABLE IF EXISTS tmp_eicu_stage2_trajectory_features;
DROP TABLE IF EXISTS tmp_eicu_stage2_late_aki_onset;
DROP TABLE IF EXISTS tmp_eicu_stage2_late_summary;
DROP TABLE IF EXISTS tmp_eicu_stage2_assessment_36_60h;
DROP TABLE IF EXISTS tmp_eicu_stage2_assessment_42_54h;
DROP TABLE IF EXISTS tmp_eicu_stage2_peak;
DROP TABLE IF EXISTS tmp_eicu_stage2_onset;
DROP TABLE IF EXISTS tmp_eicu_stage2_early_summary;
DROP TABLE IF EXISTS tmp_eicu_stage2_window_summary;
DROP TABLE IF EXISTS tmp_eicu_stage2_fixed_kdigo;
DROP TABLE IF EXISTS tmp_eicu_stage2_scr_current;
DROP TABLE IF EXISTS tmp_eicu_stage2_reference;
DROP TABLE IF EXISTS tmp_eicu_stage2_scr;
DROP TABLE IF EXISTS tmp_eicu_stage2_cohort;
DROP TABLE IF EXISTS tmp_eicu_stage2_first_qualifying;
DROP TABLE IF EXISTS tmp_eicu_stage2_first_unit;
DROP TABLE IF EXISTS tmp_eicu_stage2_candidates;
DROP TABLE IF EXISTS tmp_eicu_stage2_esrd_hospitalizations;
DROP TABLE IF EXISTS tmp_eicu_stage2_copd_hospitalizations;


/* =========================================================================
   1. Broad COPD hospital admissions and AECOPD subgroup flag

   Public/reproducible implementation without the local dictionary schema.
   Codes correspond to Supplementary Table S10:
     COPD:   ICD-9-CM 491.20; ICD-10-CM J44.9
     AECOPD: ICD-9-CM 491.21; ICD-10-CM J44.1
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_copd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_stage2_copd_hospitalizations_idx
    ON tmp_eicu_stage2_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage2_copd_hospitalizations;


/* =========================================================================
   2. ESRD / chronic dialysis at the hospital-admission level
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_esrd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_stage2_esrd_hospitalizations_idx
    ON tmp_eicu_stage2_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage2_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD candidate ICU unit stays
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_candidates
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

INNER JOIN tmp_eicu_stage2_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu_stage2_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu_stage2_candidates_stay_idx
    ON tmp_eicu_stage2_candidates (patientunitstayid);

CREATE INDEX tmp_eicu_stage2_candidates_hospitalization_idx
    ON tmp_eicu_stage2_candidates (patienthealthsystemstayid);

CREATE INDEX tmp_eicu_stage2_candidates_patient_idx
    ON tmp_eicu_stage2_candidates (patient_key);

ANALYZE tmp_eicu_stage2_candidates;


/* =========================================================================
   4. First ICU unit stay within each qualifying hospital admission
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_first_unit
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

    FROM tmp_eicu_stage2_candidates AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_first_unit_stay_idx
    ON tmp_eicu_stage2_first_unit (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage2_first_unit_hospitalization_idx
    ON tmp_eicu_stage2_first_unit (patienthealthsystemstayid);

ANALYZE tmp_eicu_stage2_first_unit;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient

   Preserve the deterministic eICU ordering rule used in Stage 1.
   The 60-h landmark is applied only AFTER this selection.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_first_qualifying
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

    FROM tmp_eicu_stage2_first_unit AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_first_qualifying_stay_idx
    ON tmp_eicu_stage2_first_qualifying (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage2_first_qualifying_patient_idx
    ON tmp_eicu_stage2_first_qualifying (patient_key);

ANALYZE tmp_eicu_stage2_first_qualifying;


/* =========================================================================
   6. ICU +60-h landmark cohort and post-landmark in-hospital outcome

   54 h is NOT an outcome landmark. It is only the end of the narrow
   recovery sensitivity window.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_cohort
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

    FROM tmp_eicu_stage2_first_qualifying AS f
),
landmark_variables AS (
    SELECT
        s.*,

        0::integer AS icu_admission_offset,
        1440::integer AS reference_fallback_0_24h_end_offset,
        2160::integer AS recovery_window_36_60_start_offset,
        2520::integer AS recovery_window_42_54_start_offset,
        2880::integer AS target_48h_offset,
        2880::integer AS early_aki_0_48h_end_offset,
        3240::integer AS recovery_window_42_54_end_offset,
        3600::integer AS recovery_window_36_60_end_offset,
        3600::integer AS landmark_60h_offset,

        GREATEST(
            COALESCE(
                s.hospitaladmitoffset,
                -10080
            ),
            -10080
        )::integer AS scr_lookback_start_offset,

        CASE
            WHEN s.hospitaldischargeoffset IS NOT NULL
             AND s.hospitaldischargestatus_normalized <> ''
             AND s.hospitaldischargeoffset > 3600
                THEN 1
            ELSE 0
        END AS eligible_at_60h_flag,

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

        CASE
            WHEN s.unitdischargeoffset IS NOT NULL
             AND s.unitdischargeoffset <= 3600
             AND s.hospitaldischargeoffset > 3600
                THEN 1
            ELSE 0
        END AS left_icu_before_60h_but_still_hospitalized_flag

    FROM status_normalized AS s
)

SELECT
    l.*

FROM landmark_variables AS l

WHERE l.eligible_at_60h_flag = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_cohort_stay_idx
    ON tmp_eicu_stage2_cohort (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage2_cohort_patient_idx
    ON tmp_eicu_stage2_cohort (patient_key);

ANALYZE tmp_eicu_stage2_cohort;


/* =========================================================================
   7. Extract serum creatinine once

   IMPORTANT:
   Use LEAST(scr_lookback_start_offset, 0) as the combined extraction lower
   bound so ICU-period creatinine is not lost if hospitaladmitoffset happens
   to be positive. The index-hospitalization lower bound is applied again,
   specifically, when the pre-ICU reference is selected.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_scr
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patient_key,
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.patientunitstayid,

    c.scr_lookback_start_offset,
    c.target_48h_offset,
    c.landmark_60h_offset,

    lab.labid,
    lab.labresultoffset::integer AS scr_offset,
    lab.labresult::numeric AS scr

FROM tmp_eicu_stage2_cohort AS c

INNER JOIN eicu_crd.lab AS lab
    ON lab.patientunitstayid = c.patientunitstayid
   AND lab.labresultoffset >= LEAST(c.scr_lookback_start_offset, 0)
   AND lab.labresultoffset <= c.landmark_60h_offset

WHERE lab.labresultoffset IS NOT NULL
  AND lab.labresult IS NOT NULL
  AND lab.labresult > 0
  AND LOWER(TRIM(COALESCE(lab.labname, ''))) = 'creatinine';

CREATE INDEX tmp_eicu_stage2_scr_stay_time_idx
    ON tmp_eicu_stage2_scr (patientunitstayid, scr_offset, labid);

CREATE INDEX tmp_eicu_stage2_scr_stay_value_idx
    ON tmp_eicu_stage2_scr (patientunitstayid, scr, scr_offset, labid);

ANALYZE tmp_eicu_stage2_scr;


/* =========================================================================
   8. Fixed reference sCr

   Hierarchy:
     1) most recent pre-ICU measured sCr within 7 d and index hospitalization;
     2) otherwise minimum ICU 0-24-h sCr.

   ICU 0-12-h minimum and first ICU 0-24-h sCr are retained only for
   diagnostics / possible reviewer-requested analyses.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_reference
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patientunitstayid,

    COALESCE(pre.pre_icu_7d_scr_n, 0)::integer
        AS pre_icu_7d_scr_n,
    pre.pre_icu_7d_latest_scr,
    pre.pre_icu_7d_latest_offset,

    COALESCE(post12.icu_0_12h_scr_n, 0)::integer
        AS icu_0_12h_scr_n,
    post12.icu_0_12h_min_scr,
    post12.icu_0_12h_min_scr_offset,

    COALESCE(post24.icu_0_24h_scr_n, 0)::integer
        AS icu_0_24h_scr_n,
    post24.icu_0_24h_min_scr,
    post24.icu_0_24h_min_scr_offset,

    firsticu.first_icu_scr_0_24h,
    firsticu.first_icu_scr_offset_0_24h,

    CASE
        WHEN pre.pre_icu_7d_latest_scr IS NOT NULL
            THEN pre.pre_icu_7d_latest_scr

        WHEN post24.icu_0_24h_min_scr IS NOT NULL
            THEN post24.icu_0_24h_min_scr

        ELSE NULL
    END AS reference_scr,

    CASE
        WHEN pre.pre_icu_7d_latest_scr IS NOT NULL
            THEN pre.pre_icu_7d_latest_offset

        WHEN post24.icu_0_24h_min_scr IS NOT NULL
            THEN post24.icu_0_24h_min_scr_offset

        ELSE NULL
    END AS reference_scr_offset,

    CASE
        WHEN pre.pre_icu_7d_latest_scr IS NOT NULL
            THEN 'pre_icu_7d_latest'

        WHEN post24.icu_0_24h_min_scr IS NOT NULL
            THEN 'icu_0_24h_min'

        ELSE 'missing'
    END AS reference_scr_source

FROM tmp_eicu_stage2_cohort AS c

LEFT JOIN LATERAL (
    SELECT
        COUNT(*)::integer AS pre_icu_7d_scr_n,

        (ARRAY_AGG(
            s.scr
            ORDER BY
                s.scr_offset DESC,
                s.labid DESC
        ))[1] AS pre_icu_7d_latest_scr,

        (ARRAY_AGG(
            s.scr_offset
            ORDER BY
                s.scr_offset DESC,
                s.labid DESC
        ))[1]::integer AS pre_icu_7d_latest_offset

    FROM tmp_eicu_stage2_scr AS s

    WHERE s.patientunitstayid = c.patientunitstayid
      AND s.scr_offset >= c.scr_lookback_start_offset
      AND s.scr_offset < 0
) AS pre
    ON TRUE

LEFT JOIN LATERAL (
    SELECT
        COUNT(*)::integer AS icu_0_12h_scr_n,

        (ARRAY_AGG(
            s.scr
            ORDER BY
                s.scr ASC,
                s.scr_offset ASC,
                s.labid ASC
        ))[1] AS icu_0_12h_min_scr,

        (ARRAY_AGG(
            s.scr_offset
            ORDER BY
                s.scr ASC,
                s.scr_offset ASC,
                s.labid ASC
        ))[1]::integer AS icu_0_12h_min_scr_offset

    FROM tmp_eicu_stage2_scr AS s

    WHERE s.patientunitstayid = c.patientunitstayid
      AND s.scr_offset BETWEEN 0 AND 720
) AS post12
    ON TRUE

LEFT JOIN LATERAL (
    SELECT
        COUNT(*)::integer AS icu_0_24h_scr_n,

        (ARRAY_AGG(
            s.scr
            ORDER BY
                s.scr ASC,
                s.scr_offset ASC,
                s.labid ASC
        ))[1] AS icu_0_24h_min_scr,

        (ARRAY_AGG(
            s.scr_offset
            ORDER BY
                s.scr ASC,
                s.scr_offset ASC,
                s.labid ASC
        ))[1]::integer AS icu_0_24h_min_scr_offset

    FROM tmp_eicu_stage2_scr AS s

    WHERE s.patientunitstayid = c.patientunitstayid
      AND s.scr_offset BETWEEN 0 AND 1440
) AS post24
    ON TRUE

LEFT JOIN LATERAL (
    SELECT
        s.scr AS first_icu_scr_0_24h,
        s.scr_offset::integer AS first_icu_scr_offset_0_24h

    FROM tmp_eicu_stage2_scr AS s

    WHERE s.patientunitstayid = c.patientunitstayid
      AND s.scr_offset BETWEEN 0 AND 1440

    ORDER BY
        s.scr_offset ASC,
        s.labid ASC

    LIMIT 1
) AS firsticu
    ON TRUE;

CREATE UNIQUE INDEX tmp_eicu_stage2_reference_idx
    ON tmp_eicu_stage2_reference (patientunitstayid);

ANALYZE tmp_eicu_stage2_reference;


/* =========================================================================
   9. ICU 0-60-h creatinine with fixed-reference KDIGO magnitude staging
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_scr_current
ON COMMIT PRESERVE ROWS AS
SELECT
    s.patient_key,
    s.uniquepid,
    s.patienthealthsystemstayid,
    s.patientunitstayid,

    s.target_48h_offset,
    s.landmark_60h_offset,

    s.labid,
    s.scr_offset AS current_scr_offset,
    s.scr AS current_scr,

    r.reference_scr,
    r.reference_scr_offset,
    r.reference_scr_source

FROM tmp_eicu_stage2_scr AS s

INNER JOIN tmp_eicu_stage2_reference AS r
    ON r.patientunitstayid = s.patientunitstayid

WHERE s.scr_offset BETWEEN 0 AND 3600;

CREATE INDEX tmp_eicu_stage2_scr_current_stay_time_idx
    ON tmp_eicu_stage2_scr_current (
        patientunitstayid,
        current_scr_offset,
        labid
    );

ANALYZE tmp_eicu_stage2_scr_current;


CREATE TEMP TABLE tmp_eicu_stage2_fixed_kdigo
ON COMMIT PRESERVE ROWS AS
SELECT
    s.*,

    CASE
        WHEN s.reference_scr IS NOT NULL
            THEN 1
        ELSE 0
    END AS evaluable_comparison_flag,

    CASE
        WHEN s.reference_scr IS NULL
            THEN NULL

        WHEN
            s.current_scr >= 3.0 * s.reference_scr
            OR (
                s.current_scr >= 4.0
                AND s.current_scr >= s.reference_scr + 0.3
            )
            THEN 3

        WHEN s.current_scr >= 2.0 * s.reference_scr
            THEN 2

        WHEN
            s.current_scr >= 1.5 * s.reference_scr
            OR s.current_scr >= s.reference_scr + 0.3
            THEN 1

        ELSE 0
    END AS kdigo_scr_stage_fixed_ref,

    CASE
        WHEN s.reference_scr IS NOT NULL
         AND s.current_scr >= 1.5 * s.reference_scr
            THEN 1
        ELSE 0
    END AS relative_criterion_flag,

    CASE
        WHEN s.reference_scr IS NOT NULL
         AND s.current_scr >= s.reference_scr + 0.3
            THEN 1
        ELSE 0
    END AS absolute_criterion_flag

FROM tmp_eicu_stage2_scr_current AS s;

CREATE INDEX tmp_eicu_stage2_fixed_kdigo_stay_time_idx
    ON tmp_eicu_stage2_fixed_kdigo (
        patientunitstayid,
        current_scr_offset,
        labid
    );

ANALYZE tmp_eicu_stage2_fixed_kdigo;


/* =========================================================================
   10. Creatinine availability in prespecified windows
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_window_summary
ON COMMIT PRESERVE ROWS AS
SELECT
    s.patientunitstayid,

    COUNT(*) FILTER (
        WHERE s.scr_offset BETWEEN 0 AND 2880
    ) AS scr_measurement_n_0_48h,

    COUNT(*) FILTER (
        WHERE s.scr_offset BETWEEN 2520 AND 3240
    ) AS scr_measurement_n_42_54h,

    COUNT(*) FILTER (
        WHERE s.scr_offset BETWEEN 2160 AND 3600
    ) AS scr_measurement_n_36_60h,

    COUNT(*) FILTER (
        WHERE s.scr_offset > 2880
          AND s.scr_offset <= 3600
    ) AS scr_measurement_n_48_60h

FROM tmp_eicu_stage2_scr AS s

GROUP BY
    s.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage2_window_summary_idx
    ON tmp_eicu_stage2_window_summary (patientunitstayid);

ANALYZE tmp_eicu_stage2_window_summary;


/* =========================================================================
   11. Early AKI during ICU hours 0-48 using the FIXED reference
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_early_summary
ON COMMIT PRESERVE ROWS AS
SELECT
    k.patientunitstayid,

    COUNT(*) AS scr_record_n_0_48h,

    COUNT(*) FILTER (
        WHERE k.evaluable_comparison_flag = 1
    ) AS evaluable_scr_n_0_48h,

    MAX(k.kdigo_scr_stage_fixed_ref)
        AS peak_aki_stage_0_48h,

    MAX(k.current_scr)
        AS observed_peak_scr_0_48h

FROM tmp_eicu_stage2_fixed_kdigo AS k

WHERE k.current_scr_offset BETWEEN 0 AND 2880

GROUP BY
    k.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage2_early_summary_idx
    ON tmp_eicu_stage2_early_summary (patientunitstayid);

ANALYZE tmp_eicu_stage2_early_summary;


/* =========================================================================
   12. First observed sCr meeting the early-AKI threshold during 0-48 h
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_onset
ON COMMIT PRESERVE ROWS AS
SELECT
    x.patientunitstayid,

    x.current_scr_offset AS aki_onset_offset,
    x.current_scr AS aki_onset_scr,
    x.kdigo_scr_stage_fixed_ref AS aki_stage_at_onset,
    x.relative_criterion_flag,
    x.absolute_criterion_flag

FROM (
    SELECT
        k.*,

        ROW_NUMBER() OVER (
            PARTITION BY k.patientunitstayid
            ORDER BY
                k.current_scr_offset,
                k.kdigo_scr_stage_fixed_ref DESC,
                k.current_scr DESC,
                k.labid
        ) AS rn

    FROM tmp_eicu_stage2_fixed_kdigo AS k

    WHERE k.current_scr_offset BETWEEN 0 AND 2880
      AND k.kdigo_scr_stage_fixed_ref >= 1
) AS x

WHERE x.rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_onset_idx
    ON tmp_eicu_stage2_onset (patientunitstayid);

ANALYZE tmp_eicu_stage2_onset;


/* =========================================================================
   13. Record corresponding to the highest early creatinine-defined AKI stage
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_peak
ON COMMIT PRESERVE ROWS AS
SELECT
    x.patientunitstayid,

    x.current_scr_offset AS peak_aki_stage_offset,
    x.current_scr AS peak_aki_stage_scr,
    x.kdigo_scr_stage_fixed_ref AS peak_aki_stage

FROM (
    SELECT
        k.*,

        ROW_NUMBER() OVER (
            PARTITION BY k.patientunitstayid
            ORDER BY
                k.kdigo_scr_stage_fixed_ref DESC NULLS LAST,
                k.current_scr DESC,
                k.current_scr_offset,
                k.labid
        ) AS rn

    FROM tmp_eicu_stage2_fixed_kdigo AS k

    WHERE k.current_scr_offset BETWEEN 0 AND 2880
      AND k.kdigo_scr_stage_fixed_ref IS NOT NULL
) AS x

WHERE x.rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_peak_idx
    ON tmp_eicu_stage2_peak (patientunitstayid);

ANALYZE tmp_eicu_stage2_peak;


/* =========================================================================
   14. Narrow 42-54-h renal-status assessment

   Only early-AKI patients are assessed. The selected measurement must be at
   or after first AKI-threshold fulfillment.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_assessment_42_54h
ON COMMIT PRESERVE ROWS AS
SELECT
    x.patientunitstayid,
    x.scr AS assessment_scr_42_54h,
    x.scr_offset AS assessment_scr_offset_42_54h

FROM (
    SELECT
        s.patientunitstayid,
        s.scr,
        s.scr_offset,
        s.labid,
        o.aki_onset_offset,

        ROW_NUMBER() OVER (
            PARTITION BY s.patientunitstayid
            ORDER BY
                ABS(s.scr_offset - 2880),
                s.scr_offset DESC,
                s.labid DESC
        ) AS rn

    FROM tmp_eicu_stage2_scr AS s

    INNER JOIN tmp_eicu_stage2_onset AS o
        ON o.patientunitstayid = s.patientunitstayid

    WHERE s.scr_offset BETWEEN 2520 AND 3240
      AND s.scr_offset >= o.aki_onset_offset
) AS x

WHERE x.rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_assessment_42_54h_idx
    ON tmp_eicu_stage2_assessment_42_54h (patientunitstayid);

ANALYZE tmp_eicu_stage2_assessment_42_54h;


/* =========================================================================
   15. Primary 36-60-h renal-status assessment

   Only early-AKI patients are assessed. The selected measurement must be at
   or after first AKI-threshold fulfillment.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_assessment_36_60h
ON COMMIT PRESERVE ROWS AS
SELECT
    x.patientunitstayid,
    x.scr AS assessment_scr_36_60h,
    x.scr_offset AS assessment_scr_offset_36_60h

FROM (
    SELECT
        s.patientunitstayid,
        s.scr,
        s.scr_offset,
        s.labid,
        o.aki_onset_offset,

        ROW_NUMBER() OVER (
            PARTITION BY s.patientunitstayid
            ORDER BY
                ABS(s.scr_offset - 2880),
                s.scr_offset DESC,
                s.labid DESC
        ) AS rn

    FROM tmp_eicu_stage2_scr AS s

    INNER JOIN tmp_eicu_stage2_onset AS o
        ON o.patientunitstayid = s.patientunitstayid

    WHERE s.scr_offset BETWEEN 2160 AND 3600
      AND s.scr_offset >= o.aki_onset_offset
) AS x

WHERE x.rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_assessment_36_60h_idx
    ON tmp_eicu_stage2_assessment_36_60h (patientunitstayid);

ANALYZE tmp_eicu_stage2_assessment_36_60h;


/* =========================================================================
   16. Fixed-reference AKI status during ICU hours 48-60
       Retained for strict no-AKI sensitivity analysis.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_late_summary
ON COMMIT PRESERVE ROWS AS
SELECT
    k.patientunitstayid,

    COUNT(*) AS scr_record_n_48_60h,

    COUNT(*) FILTER (
        WHERE k.evaluable_comparison_flag = 1
    ) AS evaluable_scr_n_48_60h,

    MAX(k.kdigo_scr_stage_fixed_ref) FILTER (
        WHERE k.evaluable_comparison_flag = 1
    ) AS peak_aki_stage_48_60h

FROM tmp_eicu_stage2_fixed_kdigo AS k

WHERE k.current_scr_offset > 2880
  AND k.current_scr_offset <= 3600

GROUP BY
    k.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage2_late_summary_idx
    ON tmp_eicu_stage2_late_summary (patientunitstayid);

ANALYZE tmp_eicu_stage2_late_summary;


CREATE TEMP TABLE tmp_eicu_stage2_late_aki_onset
ON COMMIT PRESERVE ROWS AS
SELECT
    x.patientunitstayid,
    x.current_scr_offset AS late_aki_onset_offset,
    x.current_scr AS late_aki_onset_scr,
    x.kdigo_scr_stage_fixed_ref AS late_aki_stage_at_onset

FROM (
    SELECT
        k.*,

        ROW_NUMBER() OVER (
            PARTITION BY k.patientunitstayid
            ORDER BY
                k.current_scr_offset,
                k.kdigo_scr_stage_fixed_ref DESC,
                k.current_scr DESC,
                k.labid
        ) AS rn

    FROM tmp_eicu_stage2_fixed_kdigo AS k

    WHERE k.current_scr_offset > 2880
      AND k.current_scr_offset <= 3600
      AND k.evaluable_comparison_flag = 1
      AND k.kdigo_scr_stage_fixed_ref >= 1
) AS x

WHERE x.rn = 1;

CREATE UNIQUE INDEX tmp_eicu_stage2_late_aki_onset_idx
    ON tmp_eicu_stage2_late_aki_onset (patientunitstayid);

ANALYZE tmp_eicu_stage2_late_aki_onset;


/* =========================================================================
   17. Assemble one-row-per-stay trajectory features
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_trajectory_features
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patient_key,
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.patientunitstayid,

    c.age,
    c.gender,
    c.ethnicity,

    c.hospitalid,
    c.wardid,

    c.hospitaldischargeyear,
    c.hospitaladmitoffset,
    c.hospitaldischargeoffset,
    c.hospitaldischargestatus,
    c.hospitaldischargelocation,

    c.unitvisitnumber,
    c.unitstaytype,
    c.unittype,
    c.unitadmitsource,
    c.unitdischargeoffset,
    c.unitdischargestatus,
    c.unitdischargelocation,

    c.apacheadmissiondx,

    c.broad_copd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,
    c.aecopd_flag,
    c.esrd_chronic_dialysis_flag,

    c.icu_admission_offset,
    c.reference_fallback_0_24h_end_offset,
    c.recovery_window_36_60_start_offset,
    c.recovery_window_42_54_start_offset,
    c.target_48h_offset,
    c.early_aki_0_48h_end_offset,
    c.recovery_window_42_54_end_offset,
    c.recovery_window_36_60_end_offset,
    c.landmark_60h_offset,
    c.scr_lookback_start_offset,

    c.hospital_expire_flag,
    c.eligible_at_60h_flag,
    c.hospital_death_after_60h,
    c.followup_days_after_60h,
    c.left_icu_before_60h_but_still_hospitalized_flag,

    /* Reference-sCr details */
    r.pre_icu_7d_scr_n,
    r.pre_icu_7d_latest_scr,
    r.pre_icu_7d_latest_offset,

    CASE
        WHEN r.pre_icu_7d_latest_offset IS NOT NULL
            THEN -r.pre_icu_7d_latest_offset / 60.0
        ELSE NULL
    END AS pre_icu_reference_hours_before_icu,

    r.icu_0_12h_scr_n,
    r.icu_0_12h_min_scr,
    r.icu_0_12h_min_scr_offset,

    r.icu_0_24h_scr_n,
    r.icu_0_24h_min_scr,
    r.icu_0_24h_min_scr_offset,

    r.first_icu_scr_0_24h,
    r.first_icu_scr_offset_0_24h,

    r.reference_scr,
    r.reference_scr_offset,
    r.reference_scr_source,

    CASE
        WHEN r.reference_scr_offset IS NOT NULL
            THEN r.reference_scr_offset / 60.0
        ELSE NULL
    END AS reference_scr_hours_from_icu,

    CASE
        WHEN r.reference_scr_source = 'pre_icu_7d_latest'
            THEN 1
        ELSE 0
    END AS pre_icu_reference_available_flag,

    CASE
        WHEN r.reference_scr_source = 'icu_0_24h_min'
            THEN 1
        ELSE 0
    END AS surrogate_reference_0_24h_flag,

    CASE
        WHEN r.reference_scr IS NULL
            THEN 1
        ELSE 0
    END AS reference_scr_missing_flag,

    /* Window availability */
    COALESCE(ws.scr_measurement_n_0_48h, 0)
        AS scr_measurement_n_0_48h,

    COALESCE(ws.scr_measurement_n_42_54h, 0)
        AS scr_measurement_n_42_54h,

    COALESCE(ws.scr_measurement_n_36_60h, 0)
        AS scr_measurement_n_36_60h,

    COALESCE(ws.scr_measurement_n_48_60h, 0)
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

    o.aki_onset_offset,

    CASE
        WHEN o.aki_onset_offset IS NOT NULL
            THEN o.aki_onset_offset / 60.0
        ELSE NULL
    END AS aki_onset_hours_from_icu,

    o.aki_onset_scr,
    o.aki_stage_at_onset,
    o.relative_criterion_flag,
    o.absolute_criterion_flag,

    CASE
        WHEN o.aki_onset_offset IS NOT NULL
         AND r.reference_scr_offset IS NOT NULL
            THEN (
                o.aki_onset_offset - r.reference_scr_offset
            ) / 60.0
        ELSE NULL
    END AS reference_to_aki_onset_hours,

    CASE
        WHEN o.aki_onset_offset IS NOT NULL
         AND r.reference_scr_offset IS NOT NULL
         AND o.aki_onset_offset < r.reference_scr_offset
            THEN 1

        WHEN o.aki_onset_offset IS NOT NULL
         AND r.reference_scr_offset IS NOT NULL
            THEN 0

        ELSE NULL
    END AS aki_onset_before_reference_time_flag,

    /* Peak stage record */
    p.peak_aki_stage_offset,
    p.peak_aki_stage_scr,
    p.peak_aki_stage,

    /* Narrow 42-54-h assessment */
    a42.assessment_scr_42_54h,
    a42.assessment_scr_offset_42_54h,

    CASE
        WHEN a42.assessment_scr_offset_42_54h IS NOT NULL
            THEN (
                a42.assessment_scr_offset_42_54h - 2880
            ) / 60.0
        ELSE NULL
    END AS assessment_offset_from_48h_42_54h,

    CASE
        WHEN a42.assessment_scr_offset_42_54h IS NOT NULL
            THEN ABS(
                a42.assessment_scr_offset_42_54h - 2880
            ) / 60.0
        ELSE NULL
    END AS assessment_distance_from_48h_42_54h,

    CASE
        WHEN o.aki_onset_offset IS NOT NULL
         AND a42.assessment_scr_offset_42_54h IS NOT NULL
            THEN (
                a42.assessment_scr_offset_42_54h
                - o.aki_onset_offset
            ) / 60.0
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

    /* Main 36-60-h assessment */
    a36.assessment_scr_36_60h,
    a36.assessment_scr_offset_36_60h,

    CASE
        WHEN a36.assessment_scr_offset_36_60h IS NOT NULL
            THEN (
                a36.assessment_scr_offset_36_60h - 2880
            ) / 60.0
        ELSE NULL
    END AS assessment_offset_from_48h_36_60h,

    CASE
        WHEN a36.assessment_scr_offset_36_60h IS NOT NULL
            THEN ABS(
                a36.assessment_scr_offset_36_60h - 2880
            ) / 60.0
        ELSE NULL
    END AS assessment_distance_from_48h_36_60h,

    CASE
        WHEN o.aki_onset_offset IS NOT NULL
         AND a36.assessment_scr_offset_36_60h IS NOT NULL
            THEN (
                a36.assessment_scr_offset_36_60h
                - o.aki_onset_offset
            ) / 60.0
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

    /* Hours 48-60 */
    COALESCE(ls.scr_record_n_48_60h, 0)
        AS scr_record_n_48_60h,

    COALESCE(ls.evaluable_scr_n_48_60h, 0)
        AS evaluable_scr_n_48_60h,

    ls.peak_aki_stage_48_60h,

    late.late_aki_onset_offset,
    late.late_aki_onset_scr,
    late.late_aki_stage_at_onset

FROM tmp_eicu_stage2_cohort AS c

LEFT JOIN tmp_eicu_stage2_reference AS r
    ON r.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_window_summary AS ws
    ON ws.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_early_summary AS es
    ON es.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_onset AS o
    ON o.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_peak AS p
    ON p.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_assessment_42_54h AS a42
    ON a42.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_assessment_36_60h AS a36
    ON a36.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_late_summary AS ls
    ON ls.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_stage2_late_aki_onset AS late
    ON late.patientunitstayid = c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_stage2_trajectory_features_idx
    ON tmp_eicu_stage2_trajectory_features (patientunitstayid);

ANALYZE tmp_eicu_stage2_trajectory_features;


/* =========================================================================
   18. Primary, narrow-window, and late-window exposure labels
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_exposure_base
ON COMMIT PRESERVE ROWS AS
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

FROM tmp_eicu_stage2_trajectory_features AS t;

CREATE UNIQUE INDEX tmp_eicu_stage2_exposure_base_idx
    ON tmp_eicu_stage2_exposure_base (patientunitstayid);

ANALYZE tmp_eicu_stage2_exposure_base;


/* =========================================================================
   19. Numeric exposure codes and sensitivity-analysis versions
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_exposure_final
ON COMMIT PRESERVE ROWS AS
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

    /* Measured pre-ICU reference only */
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

    /* Strict no-AKI through 60 h */
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

    /* Require >=12 h from first threshold fulfillment to main assessment */
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

FROM tmp_eicu_stage2_exposure_base AS e;

CREATE UNIQUE INDEX tmp_eicu_stage2_exposure_final_idx
    ON tmp_eicu_stage2_exposure_final (patientunitstayid);

ANALYZE tmp_eicu_stage2_exposure_final;


/* =========================================================================
   20. Final analysis / sensitivity flags
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_stage2_final
ON COMMIT PRESERVE ROWS AS
SELECT
    f.*,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.primary_exposure_code IN (0, 1, 2)
            THEN 1
        ELSE 0
    END AS primary_analysis_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_narrow_exposure_code IN (0, 1, 2)
            THEN 1
        ELSE 0
    END AS sensitivity_narrow_window_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_pre_icu_reference_exposure_code IN (0, 1, 2)
            THEN 1
        ELSE 0
    END AS sensitivity_pre_icu_reference_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_onset_ge12h_exposure_code IN (0, 1, 2)
            THEN 1
        ELSE 0
    END AS sensitivity_onset_ge12h_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.sensitivity_strict_no_aki_exposure_code IN (0, 1, 2)
            THEN 1
        ELSE 0
    END AS sensitivity_strict_no_aki_flag,

    CASE
        WHEN f.eligible_at_60h_flag = 1
         AND f.primary_exposure_code IN (1, 2)
            THEN 1
        ELSE 0
    END AS aki_only_analysis_flag,

    CASE
        WHEN f.primary_exposure_code = 0
            THEN 1
        ELSE 0
    END AS no_early_aki_flag,

    CASE
        WHEN f.primary_exposure_code = 1
            THEN 1
        ELSE 0
    END AS resolved_early_aki_flag,

    CASE
        WHEN f.primary_exposure_code = 2
            THEN 1
        ELSE 0
    END AS ongoing_early_aki_flag,

    CASE
        WHEN f.late_48_60h_status = 'No AKI detected through 60h'
            THEN 1
        ELSE 0
    END AS confirmed_no_aki_0_60h_flag,

    CASE
        WHEN f.late_48_60h_status = 'New AKI during hour 48-60'
            THEN 1
        ELSE 0
    END AS new_aki_48_60h_flag,

    CASE
        WHEN f.late_48_60h_status =
             'No early AKI; hour 48-60 status indeterminate'
            THEN 1
        ELSE 0
    END AS indeterminate_aki_48_60h_flag,

    CASE
        WHEN f.onset_to_assessment_hours_36_60h >= 12
            THEN 1

        WHEN f.aki_onset_offset IS NOT NULL
         AND f.assessment_scr_offset_36_60h IS NOT NULL
            THEN 0

        ELSE NULL
    END AS onset_to_assessment_ge12h_flag

FROM tmp_eicu_stage2_exposure_final AS f;

CREATE UNIQUE INDEX tmp_eicu_stage2_final_stay_idx
    ON tmp_eicu_stage2_final (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_stage2_final_patient_idx
    ON tmp_eicu_stage2_final (patient_key);

ANALYZE tmp_eicu_stage2_final;


/* =========================================================================
   21. Final Stage 2 output
   ========================================================================= */

SELECT *

FROM tmp_eicu_stage2_final

ORDER BY
    patient_key,
    hospitaldischargeyear,
    patienthealthsystemstayid,
    patientunitstayid;


/* =========================================================================
   Optional post-run checks

   Run these after saving the Stage 2 output as a result/persistent table.

   A. Cohort / reference cross-check

   SELECT
       COUNT(*) AS n_stage2_rows,
       COUNT(DISTINCT patientunitstayid) AS n_distinct_stays,
       COUNT(*) FILTER (
           WHERE reference_scr_source = 'pre_icu_7d_latest'
       ) AS n_pre_icu_reference,
       COUNT(*) FILTER (
           WHERE reference_scr_source = 'icu_0_24h_min'
       ) AS n_0_24h_fallback,
       COUNT(*) FILTER (
           WHERE reference_scr IS NOT NULL
       ) AS n_reference_available,
       COUNT(*) FILTER (
           WHERE reference_scr IS NULL
       ) AS n_reference_missing
   FROM your_saved_eicu_stage2_table;

   Expected cross-check from feasibility work:
       n_stage2_rows        = 8,495
       n_distinct_stays     = 8,495
       n_pre_icu_reference  = 3,508
       n_reference_available= 8,004
       n_reference_missing  = 491

   B. Primary trajectory counts / mortality

   SELECT
       primary_exposure_code,
       primary_exposure_36_60h,
       COUNT(*) AS n,
       SUM(hospital_death_after_60h) AS deaths,
       ROUND(
           100.0 * AVG(hospital_death_after_60h::numeric),
           1
       ) AS mortality_pct
   FROM your_saved_eicu_stage2_table
   WHERE primary_analysis_flag = 1
   GROUP BY
       primary_exposure_code,
       primary_exposure_36_60h
   ORDER BY primary_exposure_code;

   C. Why patients are not in the primary analysis

   SELECT
       primary_exposure_36_60h,
       COUNT(*) AS n
   FROM your_saved_eicu_stage2_table
   WHERE primary_analysis_flag = 0
   GROUP BY primary_exposure_36_60h
   ORDER BY n DESC;

   D. Key sensitivities

   SELECT
       SUM(primary_analysis_flag) AS n_primary,
       SUM(sensitivity_narrow_window_flag) AS n_narrow_42_54,
       SUM(sensitivity_pre_icu_reference_flag) AS n_pre_icu_only,
       SUM(sensitivity_onset_ge12h_flag) AS n_onset_ge12h,
       SUM(sensitivity_strict_no_aki_flag) AS n_strict_no_aki
   FROM your_saved_eicu_stage2_table;
   ========================================================================= */
