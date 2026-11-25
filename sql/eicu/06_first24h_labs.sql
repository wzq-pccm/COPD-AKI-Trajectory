/*
eICU-CRD v2.0
06_first24h_labs.sql
First-24-h hemoglobin (Hb) and blood urea nitrogen (BUN)
PostgreSQL dialect

Original source
---------------
eICU_first24h_Hb_BUN_optimized.sql (author-supplied original SQL).

Purpose
-------
Reconstruct the same ICU +60-hour landmark cohort as the other eICU modules,
then extract first measured Hb and BUN in ICU hours 0-24 for descriptive
Table 2. This is an independent, directly executable SQL script: it does not
require previously created temporary tables from modules 01-05.

Public-release change
---------------------
The local dictionary.d_icd_diagnosis lookup has been removed. The COPD and
AECOPD predicates are copied literally from the publicly prepared Stage 3A
SQL (module 03), which was previously compared to the dictionary-based
Stage 3A export (8,495 stays, 53 output fields, no differing cells).

  COPD:   ICD-9-CM 491.20 or ICD-10-CM J44.9
  AECOPD: ICD-9-CM 491.21 or ICD-10-CM J44.1

The eicu_crd.diagnosis.icd9code source field can contain multiple codes;
code-boundary regular expressions prevent matching a substring of a longer
code. Legacy copd_800070_flag / aecopd_800108_flag column aliases are
retained only for internal cohort-table compatibility; no codeid or external
dictionary is used. The full SQL after cohort construction, including all
laboratory calculations and output fields, follows the original source.

Population (unchanged)
----------------------
  1. Identify broad COPD hospitalizations using the codes above.
  2. Include adults aged >=18 y.
  3. Exclude ESRD/chronic dialysis at the index hospitalization level.
  4. Select the first ICU unit stay in each qualifying hospitalization.
  5. Select the first qualifying hospitalization per patient.
  6. After stay selection, require continued hospitalization and recorded
     discharge status beyond ICU admission +60 h.

Laboratory definitions (unchanged)
----------------------------------
  - Hemoglobin: earliest nonmissing eICU lab row with normalized labname
    of 'hgb' or 'hemoglobin' in labresultoffset 0-1440 minutes inclusive.
  - BUN: earliest nonmissing eICU lab row with normalized labname 'bun'
    in labresultoffset 0-1440 minutes inclusive.
  - At identical earliest offset, choose the smallest labid.
  - Missing laboratory observations remain NULL after left joining to the
    complete cohort; no imputation is performed here.

Output and local validation
---------------------------
  - Temporary output table: tmp_eicu_lab_final.
  - Key: patientunitstayid; expected 8,495 rows and unique stays.
  - Seven columns (3 identifiers, 2 results, 2 timing offsets).
  - Both nonmissing timing offsets must be within 0-1440 minutes.
  - Compare old/new exports by patientunitstayid and all seven columns.
  - To run the commented QC SELECT at the end, uncomment it and replace
    your_saved_eicu_hb_bun_table with tmp_eicu_lab_final; run it in
    the same PostgreSQL session because the table is temporary.
  - Required eICU tables: patient, diagnosis, lab (schema eicu_crd).
  - No fixed stay-ID list, local dictionary, or pre-existing cohorts.
  - Not executed against the restricted eICU database in this environment.
*/

/* =========================================================================
   0. Clean up
   ========================================================================= */

DROP TABLE IF EXISTS tmp_eicu_lab_final;
DROP TABLE IF EXISTS tmp_eicu_lab_first_values;
DROP TABLE IF EXISTS tmp_eicu_lab_ranked;
DROP TABLE IF EXISTS tmp_eicu_lab_candidates_0_24h;
DROP TABLE IF EXISTS tmp_eicu_lab_cohort;
DROP TABLE IF EXISTS tmp_eicu_lab_first_qualifying;
DROP TABLE IF EXISTS tmp_eicu_lab_first_unit;
DROP TABLE IF EXISTS tmp_eicu_lab_candidates;
DROP TABLE IF EXISTS tmp_eicu_lab_esrd_hospitalizations;
DROP TABLE IF EXISTS tmp_eicu_lab_copd_hospitalizations;

/* =========================================================================
   1. Broad COPD hospitalizations
   Direct ICD-9-CM/ICD-10-CM matching according to Supplementary
   Table S10; same predicate as the validated public Stage 3A SQL (03).
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_copd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_lab_copd_hosp_idx
    ON tmp_eicu_lab_copd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_lab_copd_hospitalizations;


/* =========================================================================
   2. ESRD / chronic-dialysis exclusion at hospitalization level
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_esrd_hospitalizations
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

CREATE UNIQUE INDEX tmp_eicu_lab_esrd_hosp_idx
    ON tmp_eicu_lab_esrd_hospitalizations (patienthealthsystemstayid);

ANALYZE tmp_eicu_lab_esrd_hospitalizations;


/* =========================================================================
   3. Adult, non-ESRD COPD candidate ICU unit stays
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_candidates
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

INNER JOIN tmp_eicu_lab_copd_hospitalizations AS c
    ON c.patienthealthsystemstayid = p.patienthealthsystemstayid

LEFT JOIN tmp_eicu_lab_esrd_hospitalizations AS e
    ON e.patienthealthsystemstayid = p.patienthealthsystemstayid

WHERE p.age_numeric BETWEEN 18 AND 120
  AND e.patienthealthsystemstayid IS NULL;

CREATE UNIQUE INDEX tmp_eicu_lab_candidates_stay_idx
    ON tmp_eicu_lab_candidates (patientunitstayid);

CREATE INDEX tmp_eicu_lab_candidates_hosp_idx
    ON tmp_eicu_lab_candidates (
        patienthealthsystemstayid,
        unitvisitnumber,
        patientunitstayid
    );

CREATE INDEX tmp_eicu_lab_candidates_patient_idx
    ON tmp_eicu_lab_candidates (
        patient_key,
        hospitaldischargeyear,
        patienthealthsystemstayid,
        patientunitstayid
    );

ANALYZE tmp_eicu_lab_candidates;


/* =========================================================================
   4. First ICU unit stay within each qualifying hospitalization
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_first_unit
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

    FROM tmp_eicu_lab_candidates AS c
) AS x

WHERE x.hospital_first_unit_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_lab_first_unit_stay_idx
    ON tmp_eicu_lab_first_unit (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_lab_first_unit_hosp_idx
    ON tmp_eicu_lab_first_unit (patienthealthsystemstayid);

ANALYZE tmp_eicu_lab_first_unit;


/* =========================================================================
   5. First recorded qualifying ICU hospitalization per patient

   The +60-hour landmark is deliberately applied only AFTER this selection.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_first_qualifying
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

    FROM tmp_eicu_lab_first_unit AS h
) AS x

WHERE x.qualifying_icu_rn = 1;

CREATE UNIQUE INDEX tmp_eicu_lab_first_qualifying_stay_idx
    ON tmp_eicu_lab_first_qualifying (patientunitstayid);

CREATE UNIQUE INDEX tmp_eicu_lab_first_qualifying_patient_idx
    ON tmp_eicu_lab_first_qualifying (patient_key);

ANALYZE tmp_eicu_lab_first_qualifying;


/* =========================================================================
   6. ICU +60-hour landmark cohort
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_cohort
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

FROM tmp_eicu_lab_first_qualifying AS f

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

CREATE UNIQUE INDEX tmp_eicu_lab_cohort_stay_idx
    ON tmp_eicu_lab_cohort (patientunitstayid);

ANALYZE tmp_eicu_lab_cohort;


/* =========================================================================
   7. First-24-h Hb/BUN candidate laboratory rows

   The eICU lab table stores one laboratory measurement per row. Restrict the
   join to the already materialized 8,495-stay cohort and ICU offsets 0-1440
   before any ranking. This avoids repeated per-patient subqueries and avoids
   processing unrelated laboratory records.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_candidates_0_24h
ON COMMIT PRESERVE ROWS AS
SELECT
    l.patientunitstayid,
    l.labid,
    l.labresultoffset,
    l.labname,
    l.labresult,
    l.labmeasurenamesystem,

    CASE
        WHEN LOWER(TRIM(l.labname)) IN ('hgb', 'hemoglobin')
            THEN 'hemoglobin'
        WHEN LOWER(TRIM(l.labname)) = 'bun'
            THEN 'bun'
        ELSE NULL
    END AS analyte

FROM tmp_eicu_lab_cohort AS c

INNER JOIN eicu_crd.lab AS l
    ON l.patientunitstayid = c.patientunitstayid
   AND l.labresultoffset BETWEEN 0 AND 1440
   AND l.labresult IS NOT NULL

WHERE
       LOWER(TRIM(l.labname)) IN ('hgb', 'hemoglobin')
    OR LOWER(TRIM(l.labname)) = 'bun';

CREATE INDEX tmp_eicu_lab_candidates_0_24h_idx
    ON tmp_eicu_lab_candidates_0_24h (
        patientunitstayid,
        analyte,
        labresultoffset,
        labid
    );

ANALYZE tmp_eicu_lab_candidates_0_24h;


/* =========================================================================
   8. Rank candidate rows and retain the earliest measurement per analyte
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_ranked
ON COMMIT PRESERVE ROWS AS
SELECT
    x.*

FROM (
    SELECT
        l.*,

        ROW_NUMBER() OVER (
            PARTITION BY
                l.patientunitstayid,
                l.analyte
            ORDER BY
                l.labresultoffset,
                l.labid
        ) AS rn

    FROM tmp_eicu_lab_candidates_0_24h AS l
) AS x

WHERE x.rn = 1;

CREATE UNIQUE INDEX tmp_eicu_lab_ranked_idx
    ON tmp_eicu_lab_ranked (
        patientunitstayid,
        analyte
    );

ANALYZE tmp_eicu_lab_ranked;


/* =========================================================================
   9. Pivot the two earliest values to one row per ICU stay
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_first_values
ON COMMIT PRESERVE ROWS AS
SELECT
    r.patientunitstayid,

    MAX(r.labresult) FILTER (
        WHERE r.analyte = 'hemoglobin'
    ) AS hemoglobin_first_24h,

    MAX(r.labresultoffset) FILTER (
        WHERE r.analyte = 'hemoglobin'
    ) AS hemoglobin_first_24h_offset,

    MAX(r.labresult) FILTER (
        WHERE r.analyte = 'bun'
    ) AS bun_first_24h,

    MAX(r.labresultoffset) FILTER (
        WHERE r.analyte = 'bun'
    ) AS bun_first_24h_offset

FROM tmp_eicu_lab_ranked AS r

GROUP BY r.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_lab_first_values_idx
    ON tmp_eicu_lab_first_values (patientunitstayid);

ANALYZE tmp_eicu_lab_first_values;


/* =========================================================================
   10. Final lean output

   Only merge identifiers plus the two requested Table-2 variables are kept.
   Offsets are retained as lightweight QC so the first-24-h timing can be
   verified; they do not need to appear in the manuscript table.
   ========================================================================= */

CREATE TEMP TABLE tmp_eicu_lab_final
ON COMMIT PRESERVE ROWS AS
SELECT
    c.uniquepid,
    c.patienthealthsystemstayid,
    c.patientunitstayid,

    f.hemoglobin_first_24h,
    f.hemoglobin_first_24h_offset,

    f.bun_first_24h,
    f.bun_first_24h_offset

FROM tmp_eicu_lab_cohort AS c

LEFT JOIN tmp_eicu_lab_first_values AS f
    ON f.patientunitstayid = c.patientunitstayid;

CREATE UNIQUE INDEX tmp_eicu_lab_final_stay_idx
    ON tmp_eicu_lab_final (patientunitstayid);

ANALYZE tmp_eicu_lab_final;


/* =========================================================================
   11. Final output
   ========================================================================= */

SELECT *
FROM tmp_eicu_lab_final
ORDER BY
    patienthealthsystemstayid,
    patientunitstayid;


/* =========================================================================
   Recommended post-run QC after saving the output as a table

   Replace your_saved_eicu_hb_bun_table with the saved result table name.

SELECT
    COUNT(*) AS n_rows,
    COUNT(DISTINCT patientunitstayid) AS n_distinct_stays,

    COUNT(hemoglobin_first_24h) AS n_hgb_available,
    COUNT(*) - COUNT(hemoglobin_first_24h) AS n_hgb_missing,
    ROUND(
        100.0 * (COUNT(*) - COUNT(hemoglobin_first_24h)) / NULLIF(COUNT(*), 0),
        2
    ) AS pct_hgb_missing,

    COUNT(bun_first_24h) AS n_bun_available,
    COUNT(*) - COUNT(bun_first_24h) AS n_bun_missing,
    ROUND(
        100.0 * (COUNT(*) - COUNT(bun_first_24h)) / NULLIF(COUNT(*), 0),
        2
    ) AS pct_bun_missing,

    MIN(hemoglobin_first_24h_offset) AS min_hgb_offset,
    MAX(hemoglobin_first_24h_offset) AS max_hgb_offset,
    MIN(bun_first_24h_offset) AS min_bun_offset,
    MAX(bun_first_24h_offset) AS max_bun_offset

FROM your_saved_eicu_hb_bun_table;

Expected key cohort check:
  n_rows = n_distinct_stays = 8,495
  all non-missing measurement offsets should be between 0 and 1440.

Optional source-name QC on the temporary candidate table:

SELECT
    analyte,
    labname,
    labmeasurenamesystem,
    COUNT(*) AS n
FROM tmp_eicu_lab_candidates_0_24h
GROUP BY
    analyte,
    labname,
    labmeasurenamesystem
ORDER BY
    analyte,
    n DESC;
   ========================================================================= */
