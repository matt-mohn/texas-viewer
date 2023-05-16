![imagen](https://github.com/matt-mohn/texas-viewer/assets/89479699/3c0bf90c-9419-4c52-a2d7-63ead5be73e3)
Texas Election Viewer by @mattmxhn

## Data breakdown
Shapefiles are available in the `geography` folder. County, region, CD, SD, HD, and SBOE district shapefiles are available in addition to the master precinct shapefile. The master precinct shapefile is simplified for performance and may have errors when zoomed in extremely closely. All shapefiles are clipped to the Gulf of Mexico.

Results for each election cycle are available in the main repository in the format `XXXX_election_results.csv`. While they are derived from the Texas Legislative Council, they have been significantly modified. Third-party candidates are excluded, as are most races which went uncontested by either the TX Democratic or Republican parties. Each precinct tally is associated with a race ID which can be joined with lookup tables to retrieve information excluded from each line for performance.

The `demography` folder contains files that match each precinct by its `master_key` (essentially a 2021 CNTYVTD) to TXLC demographic data and to some interpolated ACS demographic data.

The `geography` folder also contains the lookup tables for matching precincts to districts, counties, and regions by `master_key`.

Lastly, the lookup table for candidate names by race ID is `names.csv`, and the lookup table for office/year by raceID is `election_to_id.csv`.

## Notes
Races before 2012 (the start of TXLC data specifically published by 2021 precinct lines) are interpolated. Races that use the 2000s-era shapefile were disaggregated into U.S. Census tracts and then reaggregated into the 2020s-era shapefile using population (first) and area (second) as a weight. A similar process was applied to ACS data on median income and education, which are provided using Census boundaries.

To reduce the amount of data loaded, only 2020 Census information (via the TXLC) and 2021 ACS data is used for plotting. This may not correspond well to elections in the early 2000s-- Texas' Asian population has doubled from 2000 to 2020, its Bachelor degree attainment rate has increased by 50%, and some areas, like Collin County, have seen their population (and density) more than double.

Third-party races are currently unavailable.

In some instances, "ghost districts" can appear when a cross-district measure (for example, 2022 Texas House for all districts) colors in a district that was uncontested. This is either because (a) a precinct was incorrectly assigned to a district - apologies - or (b) a precinct cast votes in more than one district, which can happen (especially in historical instances where two precincts in separate districts are merged together, carrying each's data forward). Nonetheless, this won't happen when mapping and plotting directly by precinct.
