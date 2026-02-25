# Data and code for:
**Herbarium specimens reveal long-term decline in pollination services since the 1970s**  

## 1) Repository structure and file relationships

### Core dataset (public)
- **Caladenia_pollination_data_public.xlsx**  
  Public dataset derived from herbarium specimen observations.  
  This is the **primary input** for all analyses.

### Analysis scripts (R)
Each `.R` file reads the public dataset and reproduces one component of the analysis (and/or a figure/table).  
Scripts are designed to be run independently, but most assume the same core dataset.

- **GAM(NestedRE(Sub+Spe)).R** → main temporal trend model (GAMM / mixed effects)
- **Piecewise Regression.R** → segmented regression / breakpoint detection
- **HumanIndex.R** → human footprint extraction + model (HFP)
- **Temperature.R** → temperature anomaly model (annual aggregation)
- **Rainfall.R** → rainfall anomaly model(s)
- **PollinatorGAMM.R** → GAMM stratified by pollinator group
- **FoodDeceptionGAMM.R / SexualDeceptionGAMM.R / SelfGAMM.R** → GAMM stratified by pollination strategy
- **Pair-wise Pollinator.R** → post-hoc / pairwise comparisons among pollinator groups
- **Boxplot-Pollination Syndrome.R** → descriptive plots by pollination syndrome
- **food Piecewise Regression.R / sexualPiecewise Regression.R / selfPiecewise Regression.R** → breakpoint analyses per strategy
- **PollinatorPiecewise Regression.R** → breakpoint analyses per pollinator group

> Note: Some scripts require external secondary datasets (e.g., BOM climate anomalies, Human Footprint raster). These are cited in the manuscript. If they cannot be redistributed here, users should obtain them from the original sources.

---

## 2) Public dataset: `Caladenia_pollination_data_public.xlsx`

### What each row represents
Each row corresponds to a **single herbarium specimen record** (or specimen-level observation) with associated floral observations and derived pollination service indicators.

### Column definitions (variable dictionary)

#### Specimen identifiers and metadata
- **ID**  
  Internal unique identifier used for data management (specimen-level).
- **Catalog Number**  
  Herbarium catalog number associated with the specimen record.
- **Unnamed: 2**  
  Legacy column retained from the original spreadsheet export. Not used in analyses.
- **Invidual**  
  Identifier for the individual plant/specimen entity when recorded (if applicable).
- **No.Flower**  
  Number of flowers observed/recorded for the specimen (numeric).
- **Genus**  
  Genus name (e.g., *Caladenia*).
- **Species**  
  Species epithet (string).
- **Subgenus**  
  Subgenus classification (categorical; used as random effect grouping in some models).

#### Floral condition / evidence of pollination services
- **Pollinia**  
  Pollinia status as observed under microscope or specimen inspection.  
  Coding follows the manuscript definitions (e.g., 0 indicates pollinia absent/incomplete; 1 indicates present/complete).
- **Fertilised**  
  Ovary status indicator: 1 = fertilised, 0 = not fertilised/unknown (see manuscript for criteria).
- **Stigma pollen**  
  Presence of pollen on the stigma surface (indicator of visitation and successful pollen transfer).
- **Insect parts**  
  Presence of insect body parts (e.g., legs/hair) on the specimen (indicator of insect visitation).
- **Accessible flowers**  
  Whether flower structures allowed reliable assessment (e.g., stigma surface visible / flower open).  
  Used to exclude records where assessment was not possible.
- **Flowers**  
  Total number of flowers recorded for the specimen (may overlap with No.Flower depending on original recording format).
- **Botanical region**  
  Botanical district/region label used for regional grouping and reporting.
- **Year**  
  Year of specimen collection (numeric).

#### Pollination ecology attributes (from literature classification)
- **Pollination Syndrome**  
  Pollination strategy category (e.g., self-pollinating, food deception, sexual deception).
- **Pollinator**  
  Primary pollinator group assignment (e.g., bees, wasps, flies) based on the manuscript’s classification and supporting literature.
- **Specialist OR Generalist**  
  Pollinator specialization category (categorical): specialist vs generalist.

#### Sensitive location fields
- **Latitude**, **Longitude**  
  **Removed from this public dataset** to protect threatened orchid species and sensitive populations.

**Location data statement (important):**  
Latitude and longitude information were removed from the public repository in accordance with conservation and ethical best practice. Releasing precise coordinates could increase collection pressure and threaten vulnerable orchid populations. Analyses requiring geospatial extraction (e.g., human footprint index) were conducted on the full dataset with coordinates under controlled access, and only derived results are reported in the manuscript.

---

## 3) How the main analyses were performed (high-level)

### A) Temporal trends in pollination services
- Model type: GAMM (mgcv) with nested random effects (Subgenus and Species nested within Subgenus, as described in the manuscript).
- Response: pollination services (specimen-level indicator or aggregated values depending on analysis)
- Purpose: quantify long-term temporal change and compare among strategies/pollinator groups.

### B) Climate analyses (temperature/rainfall anomalies)
- National annual anomalies: linked by **collection year** and analysed using annual mean pollination services (time-series aggregation).
- Seasonal rainfall zones (regional grouping): botanical districts were classified into summer- or winter-dominant rainfall zones; records were merged by year with the corresponding seasonal rainfall anomaly series; annual mean pollination services were computed within each zone; separate GLMs were fitted by zone.

### C) Human footprint analysis (spatial extraction)
- Human footprint index values were extracted from a published raster dataset using specimen coordinates (restricted-access dataset).
- Model type: binomial GLM with Year controlled (and optional interaction terms as described in the manuscript).
- Output: partial effect curve and binned observed proportions used in Figure(s).


