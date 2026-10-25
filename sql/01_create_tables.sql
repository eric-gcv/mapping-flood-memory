-- Run with the DuckDB CLI / R DBI against data/flood_data.db.
-- Requires the NFIP 'FIMA NFIP Redacted Policies v2' CSV (extracted 2025-10-18) at ~/fima_nfip_policies_v2.csv
-- https://www.fema.gov/openfema-data-page/fima-nfip-redacted-policies-v2

DROP TABLE IF EXISTS fima_nfip_policies;

-- Create main table with NEW columns
CREATE TABLE fima_nfip_policies AS 
SELECT 
    *,
    -- Keep SFHA for comparison, but prioritize the real mandatory flag
    (ratedFloodZone LIKE 'A%' OR ratedFloodZone LIKE 'V%') AS SFHAIndicator,
    -- CORRECTED: Disaster required flag (TRUE when coverage required > 0, FALSE otherwise including NULLs)
    COALESCE(disasterAssistanceCoverageRequiredCode > 0, FALSE) AS disasterRequiredFlag
FROM read_csv_auto('~/fima_nfip_policies_v2.csv');

-- Essential indexes for stock analysis
CREATE INDEX idx_county_date ON fima_nfip_policies (countyCode, policyEffectiveDate);
CREATE INDEX idx_mandatory_county ON fima_nfip_policies (mandatoryPurchaseFlag, countyCode);
CREATE INDEX idx_dates ON fima_nfip_policies (policyEffectiveDate, policyTerminationDate);
