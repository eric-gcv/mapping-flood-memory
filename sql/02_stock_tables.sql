-- Builds voluntary_policies view and the stock_* tables used by the R pipeline.
-- Run after 01_create_tables.sql.


--##################################
--###  1. STOCK TABLE STRUCTURE ####
--##################################

-- Drop existing tables if they exist
DROP TABLE IF EXISTS stock_ins;
DROP TABLE IF EXISTS stock_outs;
DROP TABLE IF EXISTS stock_deltas_extended;
DROP TABLE IF EXISTS stock_day;
DROP TABLE IF EXISTS stock_sfha_day;

-- 1. Policy inflow table (new policies starting each day)
CREATE TABLE stock_ins (
--    policy_group VARCHAR(20),  -- 'SFHA', 'voluntary', 'mandatory', 'disasterRequired'
    SFHAIndicator BOOLEAN,
    countyCode VARCHAR(10),
    dateIn DATE,
    nRows INT,
    nPols INT,
    PRIMARY KEY(SFHAIndicator, countyCode, dateIn)
);

-- 2. Policy outflow table (policies ending each day)
CREATE TABLE stock_outs (
--    policy_group VARCHAR(20),
    SFHAIndicator BOOLEAN,
    countyCode VARCHAR(10),
    dateOut DATE,
    nRows INT,
    nPols INT,
    PRIMARY KEY(SFHAIndicator, countyCode, dateOut)
);

-- 3. Daily net changes table
CREATE TABLE stock_deltas_extended (
    countyCode VARCHAR(10),
    SFHAIndicator BOOLEAN,
 --   policy_group VARCHAR(20),
    date DATE,
    hasIn TINYINT,
    hasOut TINYINT,
    nIn INT,
    nOut INT,
    delta INT,
    PRIMARY KEY(countyCode, SFHAIndicator, date)
);

-- 4. Cumulative stock table
CREATE TABLE stock_day (
    countyCode VARCHAR(10),
    SFHAIndicator BOOLEAN,
--    policy_group VARCHAR(20),
    dateStock DATE,
    nIn INT,
    nOut INT,
    delta INT,
    stock INT,
    PRIMARY KEY(countyCode, dateStock, SFHAIndicator)
);

-- 5. Summary table for analysis
CREATE TABLE stock_sfha_day (
    countyCode VARCHAR(10),
    dateStock DATE,
--    policy_group VARCHAR(20),
    stockSFHA INT,
    stockReg INT,
    stockTot INT,
    pcntSFHA DECIMAL(5,4),
    PRIMARY KEY(countyCode, dateStock)
);

-- Create voluntary policies view for easier reference
CREATE OR REPLACE VIEW voluntary_policies AS
SELECT *
FROM fima_nfip_policies 
WHERE mandatoryPurchaseFlag = FALSE 
  AND COALESCE(disasterAssistanceCoverageRequiredCode, 0) = 0
  AND SFHAIndicator IS NOT NULL
  AND policyEffectiveDate BETWEEN '2009-01-01' AND '2022-01-01'
  AND countyCode IS NOT NULL AND countyCode != '';


--#################################
--###  2. POPULATE STOCK TABLES ###
--#################################

-- ## INs and OUTS ##

-- Insert data for all policy groups
--TRUNCATE TABLE stock_ins; 
INSERT INTO stock_ins
SELECT 
    SFHAIndicator,
    countyCode,
    policyEffectiveDate AS dateIn,
    COUNT(id) as nRows,
    SUM(policyCount) as nPols
FROM voluntary_policies
GROUP BY SFHAIndicator, countyCode, policyEffectiveDate
ORDER BY SFHAIndicator, countyCode, policyEffectiveDate;


-- Policy endings (using policyTerminationDate)
--TRUNCATE TABLE stock_outs; 
INSERT INTO stock_outs
SELECT 
    SFHAIndicator,
    countyCode, 
    policyTerminationDate AS dateOut,
    COUNT(id) as nRows,
    SUM(policyCount) as nPols
FROM voluntary_policies
WHERE policyTerminationDate IS NOT NULL
GROUP BY SFHAIndicator, countyCode, policyTerminationDate
ORDER BY SFHAIndicator, countyCode, policyTerminationDate;


-- ## EXTENDED DATE RANGE ##
-- to not have lagoons with no dates

-- Step 2: Create monthly stock_deltas_extended
TRUNCATE TABLE stock_deltas_extended;

INSERT INTO stock_deltas_extended
WITH date_range AS (
    SELECT 
        CAST('2009-01-01' AS DATE) as tbegin,
        CAST('2022-01-01' AS DATE) as tend
),
distinct_dates AS (
    SELECT DISTINCT policyEffectiveDate as date
    FROM voluntary_policies
    WHERE policyEffectiveDate BETWEEN (SELECT tbegin FROM date_range) AND (SELECT tend FROM date_range)
    ORDER BY policyEffectiveDate
)

SELECT 
    D.countyCode,
    D.SFHAIndicator,
    D.date,
    I.dateIn IS NOT NULL as hasIn,
    O.dateOut IS NOT NULL as hasOut,
    COALESCE(I.nPols, 0) AS nIn,
    COALESCE(O.nPols, 0) AS nOut,
    COALESCE(I.nPols, 0) - COALESCE(O.nPols, 0) AS delta
FROM (
    SELECT dates.date, counties.countyCode, sfha.SFHAIndicator
    FROM distinct_dates dates
    CROSS JOIN (SELECT DISTINCT countyCode FROM voluntary_policies WHERE countyCode != '0') counties
    CROSS JOIN (SELECT DISTINCT SFHAIndicator FROM voluntary_policies) sfha
) D
LEFT JOIN stock_ins I 
    ON D.countyCode = I.countyCode
    AND D.date = I.dateIn
    AND D.SFHAIndicator = I.SFHAIndicator
LEFT JOIN stock_outs O 
    ON D.countyCode = O.countyCode
    AND D.SFHAIndicator = O.SFHAIndicator
    AND D.date = O.dateOut
WHERE D.countyCode IS NOT NULL
ORDER BY D.countyCode, D.SFHAIndicator, D.date;



-- Check how many combinations we're dealing with
/*SELECT 
    COUNT(DISTINCT policyEffectiveDate) as distinct_dates,
    COUNT(DISTINCT countyCode) as distinct_counties
FROM fima_nfip_policies 
WHERE policyEffectiveDate BETWEEN '2009-01-01' AND '2022-01-01'
  AND countyCode IS NOT NULL AND countyCode != '';*/



-- ## CUMULATIVE STOCK ##
-- TRUNCATE TABLE stock_day;
INSERT INTO stock_day
SELECT
    countyCode,
    SFHAIndicator,
    date as dateStock,
    nIn,
    nOut,
    delta,
    SUM(delta) OVER (
        PARTITION BY countyCode, SFHAIndicator 
        ORDER BY countyCode, date, SFHAIndicator
    ) as stock
FROM stock_deltas_extended
ORDER BY countyCode, dateStock, SFHAIndicator;
