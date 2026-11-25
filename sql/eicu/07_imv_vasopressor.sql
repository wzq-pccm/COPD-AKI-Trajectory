/*
eICU-CRD v2.0
07_imv_vasopressor.sql
Invasive mechanical ventilation (IMV) and vasopressor use during ICU 0-24 h
PostgreSQL dialect

Original source
---------------
eICU_revised_IMV_vasopressor_0_24h.sql (author-supplied original).

Purpose
-------
Independently reconstruct the +60-hour landmark cohort and extract the two
0-24-h organ-support indicators used in the descriptive table and the
prespecified additional-adjustment sensitivity analysis:
  - imv_any_0_24h
  - vasopressor_any_0_24h
These two exposures are not primary-model adjustment covariates.

Public-release change
---------------------
Replace the locally installed dictionary.d_icd_diagnosis lookup with direct
ICD-9-CM/ICD-10-CM matching, as specified in Supplementary Table S10:
  - COPD:   491.20 or J44.9
  - AECOPD: 491.21 or J44.1
The COPD/AECOPD predicate is copied verbatim from the previously validated
public Stage 3A (03) query. That query's 8,495-stay, 53-column output was
compared cell-by-cell with the dictionary-based original with no differences.
Legacy field names copd_800070_flag / aecopd_800108_flag are retained only
for downstream compatibility; the local dictionary is no longer needed.

All executable SQL following the initial COPD hospitalization query is
preserved exactly from the author-supplied organ-support code. The script
creates its own +60-hour cohort and does not require tables from 01-06.

Cohort definition (unchanged)
----------------------------
  1. Identify COPD hospitalizations using the code pairs above.
  2. Adults aged >=18 years; exclude ESRD / chronic dialysis at the
     hospitalization level.
  3. Select first ICU unit stay per qualifying hospitalization, then first
     recorded qualifying ICU hospitalization per patient.
  4. Require recorded discharge status and hospitalization continuing beyond
     ICU admission +60 hours after the stay selection.

IMV definition (unchanged)
--------------------------
IMV = 1 if any of the following is recorded during ICU hours 0-24:
  A. respiratorycharting with invasive-ventilation evidence;
  B. respiratorycare current or prior ventilation interval overlapping 0-24 h;
  C. respiratorycare evidence of an invasive airway (endotracheal tube,
     cricothyrotomy or double-lumen tube) within that period.
Noninvasive-ventilation and CPAP-only charting are excluded. Tracheostomy
alone does not establish active IMV. Missing ventilator end offsets are
handled according to the original respiratorycare status rule.

Vasopressor definition (unchanged)
----------------------------------
Active positive-rate infusion during ICU 0-24 h of norepinephrine,
epinephrine, vasopressin, phenylephrine or dopamine. Orders are QC only;
dobutamine and milrinone are not independently counted as vasopressors.
The primary sensitivity variable is vasopressor_any_0_24h, NOT the broader
medication-order or documentation indicators.

Output and validation
---------------------
  - Temporary output: tmp_eicu_support_final.
  - Key: patientunitstayid. Expected 8,495 rows and unique stays.
  - Run in a PostgreSQL session; temp tables are session-scoped.
  - Compare the new and original exports by patientunitstayid across every
    resulting column, not just the cohort count.
  - Use the commented QC queries at the end by replacing
    your_saved_eicu_support_table with tmp_eicu_support_final (same session).
  - Tables required: eicu_crd.patient, diagnosis, respiratorycharting,
    respiratorycare, infusiondrug and medication.
  - No fixed patient IDs, local dictionary joins, or pre-existing cohorts.
  - Not executed against the restricted eICU database here.
*/


/* =========================================================================
   0. Clean up
   ========================================================================= */

DROP TABLE IF EXISTS tmp_eicu_support_final;
DROP TABLE IF EXISTS tmp_eicu_support_vaso;
DROP TABLE IF EXISTS tmp_eicu_support_imv;
DROP TABLE IF EXISTS tmp_eicu_support_cohort;
DROP TABLE IF EXISTS tmp_eicu_support_first_qualifying;
DROP TABLE IF EXISTS tmp_eicu_support_first_unit;
DROP TABLE IF EXISTS tmp_eicu_support_candidates;
DROP TABLE IF EXISTS tmp_eicu_support_esrd_hospitalizations;
DROP TABLE IF EXISTS tmp_eicu_support_copd_hospitalizations;


/* =========================================================================
   1. Broad COPD hospitalizations
   Direct matching to ICD-9-CM/ICD-10-CM codes in Supplementary
   Table S10, using the validated public Stage 3A predicate (03).
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_copd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_support_copd_hosp_idx
    ON tmp_eicu_support_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_support_copd_hospitalizations;


/* =========================================================================
   2. ESRD / chronic-dialysis exclusion at hospitalization level
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_esrd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_support_esrd_hosp_idx
    ON tmp_eicu_support_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_support_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD COPD candidate ICU unit stays
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_candidates
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

    p.hospitalid,

    p.hospitaldischargeyear,
    p.hospitaldischargeoffset,
    p.hospitaldischargestatus,

    p.unitvisitnumber,

    c.broad_copd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,
    c.aecopd_flag

FROM patient_age_parsed AS p

INNER JOIN tmp_eicu_support_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu_support_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu_support_candidates_stay_idx
    ON tmp_eicu_support_candidates (patientunitstayid);

CREATE INDEX tmp_eicu_support_candidates_hosp_idx
    ON tmp_eicu_support_candidates (
        patienthealthsystemstayid,
        unitvisitnumber,
        patientunitstayid
    );

CREATE INDEX tmp_eicu_support_candidates_patient_idx
    ON tmp_eicu_support_candidates (
        patient_key,
        hospitaldischargeyear,
        patienthealthsystemstayid,
        patientunitstayid
    );

ANALYZE tmp_eicu_support_candidates;


/* =========================================================================
   4. First ICU unit stay within each qualifying hospitalization
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_first_unit
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

    FROM tmp_eicu_support_candidates AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_support_first_unit_stay_idx
    ON tmp_eicu_support_first_unit (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_support_first_unit_hosp_idx
    ON tmp_eicu_support_first_unit (patienthealthsystemstayid);

ANALYZE tmp_eicu_support_first_unit;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient

   The +60-hour landmark is deliberately applied only AFTER this selection.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_first_qualifying
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

    FROM tmp_eicu_support_first_unit AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_support_first_qualifying_stay_idx
    ON tmp_eicu_support_first_qualifying (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_support_first_qualifying_patient_idx
    ON tmp_eicu_support_first_qualifying (patient_key);

ANALYZE tmp_eicu_support_first_qualifying;


/* =========================================================================
   6. ICU +60-hour landmark cohort
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_cohort
ON COMMIT PRESERVE ROWS AS
SELECT
    f.patient_key,
    f.uniquepid,
    f.patienthealthsystemstayid,
    f.patientunitstayid,

    f.hospitalid,

    f.broad_copd_flag,
    f.copd_800070_flag,
    f.aecopd_800108_flag,
    f.aecopd_flag,

    1::integer AS eligible_at_60h_flag

FROM tmp_eicu_support_first_qualifying AS f

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
      ) IS NOT NULL;

CREATE UNIQUE INDEX tmp_eicu_support_cohort_stay_idx
    ON tmp_eicu_support_cohort (patientunitstayid);

ANALYZE tmp_eicu_support_cohort;


/* =========================================================================
   7. Invasive mechanical ventilation source flags
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_imv
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patientunitstayid,

    /* ---------------------------------------------------------------------
       A. Explicit mechanical-ventilator / invasive-mode charting
       --------------------------------------------------------------------- */
    CASE
        WHEN EXISTS (
            SELECT 1

            FROM eicu_crd.respiratorycharting AS rc

            CROSS JOIN LATERAL (
                SELECT
                    LOWER(
                        TRIM(
                            COALESCE(
                                rc.respchartvaluelabel,
                                ''
                            )
                        )
                    ) AS label,

                    LOWER(
                        TRIM(
                            COALESCE(
                                rc.respchartvalue,
                                ''
                            )
                        )
                    ) AS value
            ) AS n

            WHERE rc.patientunitstayid = c.patientunitstayid

              /* Observation time, not entry time */
              AND rc.respchartoffset BETWEEN 0 AND 1440

              /* Exclude absent/off/noninvasive documentation */
              AND n.value NOT IN (
                    '',
                    'off',
                    'none',
                    'no',
                    'stopped',
                    'stop',
                    'discontinued',
                    'not in use'
                  )

              AND n.value NOT LIKE '%turned off%'
              AND n.value NOT LIKE '%not using%'
              AND n.value NOT LIKE '%discontinue%'
              AND n.value NOT LIKE '%extubat%'

              AND n.value NOT LIKE '%non-invasive%'
              AND n.value NOT LIKE '%non invasive%'
              AND n.value NOT LIKE '%noninvasive%'
              AND n.value NOT LIKE '%niv%'
              AND n.value NOT LIKE '%bipap%'
              AND n.value NOT LIKE '%bi-pap%'
              AND n.value NOT LIKE '%avaps%'
              AND n.value NOT LIKE '%high flow%'
              AND n.value NOT LIKE '%hfnc%'

              /* CPAP-only documentation is not sufficient for invasive MV */
              AND n.value NOT LIKE '%cpap%'

              AND (
                    (
                        n.label IN (
                            'o2 device',
                            'oxygen delivery method'
                        )
                        AND n.value IN (
                            'ventilator',
                            'mechanical ventilator'
                        )
                    )

                    OR (
                        n.label LIKE '%oxygen delivery%'
                        AND n.value LIKE '%mechanical ventilator%'
                    )

                    OR (
                        n.label LIKE '%o2 device%'
                        AND n.value = 'ventilator'
                    )

                    OR (
                        n.label LIKE '%mechanical ventilator mode%'
                        AND (
                               n.value LIKE '%assist control%'
                            OR n.value LIKE '%assist-control%'
                            OR n.value LIKE '%volume control%'
                            OR n.value LIKE '%pressure control%'
                            OR n.value LIKE '%controlled mechanical%'
                            OR n.value LIKE '%prvc%'
                            OR n.value LIKE '%simv%'
                            OR n.value LIKE '%cmv%'
                            OR n.value LIKE '%pressure support%'
                            OR n.value ~ '(^|[^a-z])(a/c|ac|vc|pc)([^a-z]|$)'
                        )
                    )

                    OR (
                        n.label LIKE '%vent on/off%'
                        AND n.value IN (
                            'start',
                            'started',
                            'continued',
                            'continue',
                            'on'
                        )
                    )

                    OR n.label LIKE '%adult con pt/vent%'

                    OR (
                        n.label LIKE '%ventilator support mode%'
                        AND (
                               n.value LIKE '%assist control%'
                            OR n.value LIKE '%assist-control%'
                            OR n.value LIKE '%volume control%'
                            OR n.value LIKE '%pressure control%'
                            OR n.value LIKE '%controlled mechanical%'
                            OR n.value LIKE '%prvc%'
                            OR n.value LIKE '%simv%'
                            OR n.value LIKE '%cmv%'
                            OR n.value LIKE '%pressure support%'
                            OR n.value ~ '(^|[^a-z])(a/c|ac|vc|pc)([^a-z]|$)'
                        )
                    )
                  )

            LIMIT 1
        )
            THEN 1
        ELSE 0
    END::integer AS imv_respchart_0_24h_flag,

    /* ---------------------------------------------------------------------
       B. Current/prior ventilator interval overlapping ICU 0-24 h

       A documented end offset of exactly 0 means the episode ended at ICU
       admission and therefore does NOT count as overlap after admission.

       If end offset is missing, require a respiratoryCare status record
       within ICU 0-24 h as evidence the episode is active during the window.
       --------------------------------------------------------------------- */
    CASE
        WHEN EXISTS (
            SELECT 1

            FROM eicu_crd.respiratorycare AS rc

            WHERE rc.patientunitstayid = c.patientunitstayid

              AND (
                    (
                        rc.ventstartoffset IS NOT NULL
                        AND rc.ventstartoffset <= 1440
                        AND (
                              (
                                  rc.ventendoffset IS NOT NULL
                                  AND rc.ventendoffset >= rc.ventstartoffset
                                  AND rc.ventendoffset > 0
                              )
                              OR
                              (
                                  rc.ventendoffset IS NULL
                                  AND rc.respcarestatusoffset BETWEEN 0 AND 1440
                              )
                            )
                    )

                    OR

                    (
                        rc.priorventstartoffset IS NOT NULL
                        AND rc.priorventstartoffset <= 1440
                        AND (
                              (
                                  rc.priorventendoffset IS NOT NULL
                                  AND rc.priorventendoffset >= rc.priorventstartoffset
                                  AND rc.priorventendoffset > 0
                              )
                              OR
                              (
                                  rc.priorventendoffset IS NULL
                                  AND rc.respcarestatusoffset BETWEEN 0 AND 1440
                              )
                            )
                    )
                  )

            LIMIT 1
        )
            THEN 1
        ELSE 0
    END::integer AS imv_vent_interval_0_24h_flag,

    /* ---------------------------------------------------------------------
       C. Invasive airway evidence during ICU 0-24 h
       Tracheostomy alone is intentionally excluded.
       --------------------------------------------------------------------- */
    CASE
        WHEN EXISTS (
            SELECT 1

            FROM eicu_crd.respiratorycare AS rc

            CROSS JOIN LATERAL (
                SELECT
                    LOWER(
                        TRIM(
                            COALESCE(
                                rc.airwaytype,
                                ''
                            )
                        )
                    ) AS airwaytype
            ) AS a

            WHERE rc.patientunitstayid = c.patientunitstayid
              AND rc.respcarestatusoffset BETWEEN 0 AND 1440

              AND (
                    a.airwaytype IN (
                        'oral ett',
                        'nasal ett',
                        'cricothyrotomy',
                        'double-lumen tube'
                    )

                    OR a.airwaytype LIKE '%endotracheal%'
                    OR a.airwaytype LIKE '%oral ett%'
                    OR a.airwaytype LIKE '%nasal ett%'
                  )

            LIMIT 1
        )
            THEN 1
        ELSE 0
    END::integer AS imv_invasive_airway_0_24h_flag

FROM tmp_eicu_support_cohort AS c;

CREATE UNIQUE INDEX tmp_eicu_support_imv_idx
    ON tmp_eicu_support_imv (patientunitstayid);

ANALYZE tmp_eicu_support_imv;


/* =========================================================================
   8. Vasopressor source flags

   Primary flag is active infusion with a positive numeric drugRate OR
   infusionRate. Medication-table orders are retained only as QC.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_vaso
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patientunitstayid,

    /* Any named vasopressor infusion record in ICU 0-24 h */
    CASE
        WHEN EXISTS (
            SELECT 1

            FROM eicu_crd.infusiondrug AS id

            CROSS JOIN LATERAL (
                SELECT
                    LOWER(
                        TRIM(
                            COALESCE(
                                id.drugname,
                                ''
                            )
                        )
                    ) AS drugname
            ) AS n

            WHERE id.patientunitstayid = c.patientunitstayid
              AND id.infusionoffset BETWEEN 0 AND 1440

              AND (
                       n.drugname LIKE '%norepinephrine%'
                    OR n.drugname LIKE '%noradrenalin%'
                    OR n.drugname LIKE '%levophed%'

                    OR n.drugname LIKE '%epinephrine%'
                    OR n.drugname LIKE '%adrenalin%'

                    OR n.drugname LIKE '%vasopressin%'

                    OR n.drugname LIKE '%phenylephrine%'
                    OR n.drugname LIKE '%neosynephrine%'
                    OR n.drugname LIKE '%neo-synephrine%'

                    OR n.drugname LIKE '%dopamine%'
                  )

              AND n.drugname NOT LIKE '%dobutamine%'
              AND n.drugname NOT LIKE '%milrinone%'

            LIMIT 1
        )
            THEN 1
        ELSE 0
    END::integer AS vasopressor_infusion_named_0_24h_flag,

    /* Primary administration evidence: positive recorded infusion rate */
    CASE
        WHEN EXISTS (
            SELECT 1

            FROM eicu_crd.infusiondrug AS id

            CROSS JOIN LATERAL (
                SELECT
                    LOWER(
                        TRIM(
                            COALESCE(
                                id.drugname,
                                ''
                            )
                        )
                    ) AS drugname,

                    CASE
                        WHEN TRIM(COALESCE(id.drugrate, ''))
                                 ~ '^[+]?[0-9]*\.?[0-9]+$'
                            THEN TRIM(id.drugrate)::numeric
                        ELSE NULL
                    END AS drugrate_numeric,

                    CASE
                        WHEN TRIM(COALESCE(id.infusionrate, ''))
                                 ~ '^[+]?[0-9]*\.?[0-9]+$'
                            THEN TRIM(id.infusionrate)::numeric
                        ELSE NULL
                    END AS infusionrate_numeric
            ) AS n

            WHERE id.patientunitstayid = c.patientunitstayid
              AND id.infusionoffset BETWEEN 0 AND 1440

              AND (
                       n.drugname LIKE '%norepinephrine%'
                    OR n.drugname LIKE '%noradrenalin%'
                    OR n.drugname LIKE '%levophed%'

                    OR n.drugname LIKE '%epinephrine%'
                    OR n.drugname LIKE '%adrenalin%'

                    OR n.drugname LIKE '%vasopressin%'

                    OR n.drugname LIKE '%phenylephrine%'
                    OR n.drugname LIKE '%neosynephrine%'
                    OR n.drugname LIKE '%neo-synephrine%'

                    OR n.drugname LIKE '%dopamine%'
                  )

              AND n.drugname NOT LIKE '%dobutamine%'
              AND n.drugname NOT LIKE '%milrinone%'

              AND (
                       n.drugrate_numeric > 0
                    OR n.infusionrate_numeric > 0
                  )

            LIMIT 1
        )
            THEN 1
        ELSE 0
    END::integer AS vasopressor_infusion_positive_rate_0_24h_flag,

    /*
    Unresolved active-rate QC:
    a named vasopressor record exists but neither rate can be parsed as numeric.
    This does not automatically count as administered vasopressor.
    */
    CASE
        WHEN EXISTS (
            SELECT 1

            FROM eicu_crd.infusiondrug AS id

            CROSS JOIN LATERAL (
                SELECT
                    LOWER(
                        TRIM(
                            COALESCE(
                                id.drugname,
                                ''
                            )
                        )
                    ) AS drugname,

                    TRIM(
                        COALESCE(
                            id.drugrate,
                            ''
                        )
                    ) AS drugrate_text,

                    TRIM(
                        COALESCE(
                            id.infusionrate,
                            ''
                        )
                    ) AS infusionrate_text
            ) AS n

            WHERE id.patientunitstayid = c.patientunitstayid
              AND id.infusionoffset BETWEEN 0 AND 1440

              AND (
                       n.drugname LIKE '%norepinephrine%'
                    OR n.drugname LIKE '%noradrenalin%'
                    OR n.drugname LIKE '%levophed%'

                    OR n.drugname LIKE '%epinephrine%'
                    OR n.drugname LIKE '%adrenalin%'

                    OR n.drugname LIKE '%vasopressin%'

                    OR n.drugname LIKE '%phenylephrine%'
                    OR n.drugname LIKE '%neosynephrine%'
                    OR n.drugname LIKE '%neo-synephrine%'

                    OR n.drugname LIKE '%dopamine%'
                  )

              AND n.drugname NOT LIKE '%dobutamine%'
              AND n.drugname NOT LIKE '%milrinone%'

              AND NOT (
                    n.drugrate_text
                        ~ '^[+]?[0-9]*\.?[0-9]+$'
                  )

              AND NOT (
                    n.infusionrate_text
                        ~ '^[+]?[0-9]*\.?[0-9]+$'
                  )

            LIMIT 1
        )
            THEN 1
        ELSE 0
    END::integer AS vasopressor_infusion_unresolved_rate_0_24h_flag,

    /* Medication ORDER only; not used as the primary administered-use flag */
    CASE
        WHEN EXISTS (
            SELECT 1

            FROM eicu_crd.medication AS m

            CROSS JOIN LATERAL (
                SELECT
                    LOWER(
                        TRIM(
                            COALESCE(
                                m.drugname,
                                ''
                            )
                        )
                    ) AS drugname
            ) AS n

            WHERE m.patientunitstayid = c.patientunitstayid
              AND m.drugstartoffset IS NOT NULL
              AND m.drugstartoffset <= 1440

              AND (
                    m.drugstopoffset IS NULL
                    OR (
                        m.drugstopoffset >= m.drugstartoffset
                        AND m.drugstopoffset > 0
                    )
                  )

              AND (
                    m.drugnameid IN (
                        300001, 300130, 300140, 300166, 300352, 300509,
                        300528, 300529, 300803, 300804, 300805, 300806,
                        300807, 300808, 300809, 300810, 300811, 300812,
                        300813, 301018, 301019, 301020, 301021, 301022,
                        301023, 301024, 301025, 301103, 301104, 301105,
                        301106, 301107, 301108, 301198, 301376
                    )

                    OR n.drugname LIKE '%norepinephrine%'
                    OR n.drugname LIKE '%noradrenalin%'
                    OR n.drugname LIKE '%levophed%'

                    OR n.drugname LIKE '%epinephrine%'
                    OR n.drugname LIKE '%adrenalin%'

                    OR n.drugname LIKE '%vasopressin%'

                    OR n.drugname LIKE '%phenylephrine%'
                    OR n.drugname LIKE '%neosynephrine%'
                    OR n.drugname LIKE '%neo-synephrine%'

                    OR n.drugname LIKE '%dopamine%'
                  )

              AND n.drugname NOT LIKE '%dobutamine%'
              AND n.drugname NOT LIKE '%milrinone%'

            LIMIT 1
        )
            THEN 1
        ELSE 0
    END::integer AS vasopressor_medication_order_0_24h_flag

FROM tmp_eicu_support_cohort AS c;

CREATE UNIQUE INDEX tmp_eicu_support_vaso_idx
    ON tmp_eicu_support_vaso (patientunitstayid);

ANALYZE tmp_eicu_support_vaso;


/* =========================================================================
   9. Final organ-support variables
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_support_final
ON COMMIT PRESERVE ROWS AS
SELECT
    c.patient_key,
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.patientunitstayid,

    c.hospitalid,

    c.broad_copd_flag,
    c.copd_800070_flag,
    c.aecopd_800108_flag,
    c.aecopd_flag,

    c.eligible_at_60h_flag,

    /* IMV source QC */
    COALESCE(
        i.imv_respchart_0_24h_flag,
        0
    )::integer AS imv_respchart_0_24h_flag,

    COALESCE(
        i.imv_vent_interval_0_24h_flag,
        0
    )::integer AS imv_vent_interval_0_24h_flag,

    COALESCE(
        i.imv_invasive_airway_0_24h_flag,
        0
    )::integer AS imv_invasive_airway_0_24h_flag,

    /* Primary IMV variable */
    CASE
        WHEN
               COALESCE(i.imv_respchart_0_24h_flag, 0) = 1
            OR COALESCE(i.imv_vent_interval_0_24h_flag, 0) = 1
            OR COALESCE(i.imv_invasive_airway_0_24h_flag, 0) = 1
            THEN 1
        ELSE 0
    END::integer AS imv_any_0_24h,

    /* Vasopressor source QC */
    COALESCE(
        v.vasopressor_infusion_named_0_24h_flag,
        0
    )::integer AS vasopressor_infusion_named_0_24h_flag,

    COALESCE(
        v.vasopressor_infusion_positive_rate_0_24h_flag,
        0
    )::integer AS vasopressor_infusion_positive_rate_0_24h_flag,

    COALESCE(
        v.vasopressor_infusion_unresolved_rate_0_24h_flag,
        0
    )::integer AS vasopressor_infusion_unresolved_rate_0_24h_flag,

    COALESCE(
        v.vasopressor_medication_order_0_24h_flag,
        0
    )::integer AS vasopressor_medication_order_0_24h_flag,

    CASE
        WHEN
               COALESCE(v.vasopressor_infusion_named_0_24h_flag, 0) = 1
            OR COALESCE(v.vasopressor_medication_order_0_24h_flag, 0) = 1
            THEN 1
        ELSE 0
    END::integer AS vasopressor_documented_broad_0_24h_flag,

    /* Primary vasopressor variable for regression sensitivity analysis */
    COALESCE(
        v.vasopressor_infusion_positive_rate_0_24h_flag,
        0
    )::integer AS vasopressor_any_0_24h

FROM tmp_eicu_support_cohort AS c

LEFT JOIN tmp_eicu_support_imv AS i
    ON i.patientunitstayid = c.patientunitstayid

LEFT JOIN tmp_eicu_support_vaso AS v
    ON v.patientunitstayid = c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_support_final_stay_idx
    ON tmp_eicu_support_final (patientunitstayid);

ANALYZE tmp_eicu_support_final;


/* =========================================================================
   10. Final output
   ========================================================================= */

SELECT *
FROM tmp_eicu_support_final

ORDER BY
    patient_key,
    patienthealthsystemstayid,
    patientunitstayid;


/* =========================================================================
   Recommended post-run checks after saving the result

SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT patientunitstayid) AS n_distinct_stays,

    SUM(imv_any_0_24h)
        AS n_imv_0_24h,

    ROUND(
        100.0 * SUM(imv_any_0_24h) / NULLIF(COUNT(*), 0),
        1
    ) AS pct_imv_0_24h,

    SUM(vasopressor_any_0_24h)
        AS n_vasopressor_0_24h,

    ROUND(
        100.0 * SUM(vasopressor_any_0_24h) / NULLIF(COUNT(*), 0),
        1
    ) AS pct_vasopressor_0_24h,

    SUM(imv_respchart_0_24h_flag)
        AS n_imv_respchart,

    SUM(imv_vent_interval_0_24h_flag)
        AS n_imv_vent_interval,

    SUM(imv_invasive_airway_0_24h_flag)
        AS n_imv_invasive_airway,

    SUM(vasopressor_infusion_named_0_24h_flag)
        AS n_vaso_named_infusion,

    SUM(vasopressor_infusion_positive_rate_0_24h_flag)
        AS n_vaso_positive_rate,

    SUM(vasopressor_infusion_unresolved_rate_0_24h_flag)
        AS n_vaso_unresolved_rate,

    SUM(vasopressor_medication_order_0_24h_flag)
        AS n_vaso_medication_order,

    SUM(vasopressor_documented_broad_0_24h_flag)
        AS n_vaso_documented_broad

FROM your_saved_eicu_support_table;

Expected key cohort check:
  n_rows = n_distinct_stays = 8,495


Additional vasopressor cross-tab:

SELECT
    vasopressor_infusion_positive_rate_0_24h_flag,
    vasopressor_medication_order_0_24h_flag,
    COUNT(*) AS n
FROM your_saved_eicu_support_table
GROUP BY
    vasopressor_infusion_positive_rate_0_24h_flag,
    vasopressor_medication_order_0_24h_flag
ORDER BY
    vasopressor_infusion_positive_rate_0_24h_flag,
    vasopressor_medication_order_0_24h_flag;


Interpretation:
  - Use imv_any_0_24h and vasopressor_any_0_24h in the additional-adjustment
    sensitivity model.
  - The other source flags are QC variables only.
   ========================================================================= */
