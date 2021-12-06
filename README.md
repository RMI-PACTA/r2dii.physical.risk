# r2dii.physical.risk
Functions, workflows and applications to estimate physical climate risk for physical assets and financial portfolios. 

## Installation
Clone this repo:
``` bash
git clone https://github.com/2DegreesInvesting/r2dii.physical.risk.git
```

All R package dependencies can easily be installed from the R console using:
``` r
# install.packages("devtools")
devtools::install_deps(".")
```

## Default Input Data Paths

## Running Analysis
Preparing input data can be time consuming. Already prepared data can be found on DropBox at: `PortCheck/00_Data/01_ProcessedData/08_RiskData/climate_data`. To learn more about optionally preparing or updating this data, see the collapsible section "Preparing/ Updating Input Data" below. 

Analysis can be run from the R console using: 
``` r
source("setup_project_specifications.R")
source("analysis.R")
```

<details>
  <summary>Optional: Preparing/ Updating Input Data</summary>
  
  ## Asset Resolution
  `prepare_AR_data.R`
  
  ## Climate Data Factory
  `prepare_CDF_data.R`
  
  ## Climate Analytics 
  `prepare_climate_analytics_data.R`
  
  ## Open Street Map 
  `prepare_OSM_data.R`

</details>
