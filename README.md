# r2dii.physical.risk
This repo contains functions, workflows and applications to estimate physical
climate risk for physical assets and financial portfolios.

## Installation
First, clone this repo:
``` bash
git clone https://github.com/2DegreesInvesting/r2dii.physical.risk.git
```

All R package dependencies can easily be installed from the R console using:
``` r
# install.packages("devtools")
devtools::install_deps(".")
```

### Setting up Project Specifications
Project specifications can be set in the file `setup_project_specifications.R`. 
Things such as paths to input and output data directories are set in this file. 
The physical risk module requires many different inputs and outputs. An example 
full directory structure might look something like: 
``` bash
1  Users                                                    
2   °--path_to                                              
3       °--Dropbox                                          
4           ¦--PortCheck_v2                                 
5           ¦   °--10_Projects                              
6           ¦       °--SomeProject                          
7           ¦           °--60_Physical_Risk                 
8           °--PortCheck                                    
9               °--00_Data                                  
10                  ¦--00_RawData                           
11                  ¦   °--15_Risk                          
12                  ¦       ¦--Climate Data Factory         
13                  ¦       ¦   °--TCFD_Climate_Data-GeoTiff
14                  ¦       ¦       °--GeoTIFF              
15                  ¦       ¦           ¦--Indices          
16                  ¦       ¦           °--Variables        
17                  ¦       °--ClimateAnalytics             
18                  ¦--01_ProcessedData                     
19                  ¦   °--08_RiskData                      
20                  ¦       ¦--asset_level_data             
21                  ¦       ¦   ¦--distinct_geo_data        
22                  ¦       ¦   °--prepared_ald             
23                  ¦       °--climate_data                 
24                  ¦           °--CDF                      
25                  °--06_DataStore                         
26                      °--DataStore_export_timestamp       
27                          °--quarter                      
```

It is very important that the output data folder locations are set properly in 
the `setup_project_specifications.R` file (otherwise you risk over-writing 
someone else's project). For example: 

``` r
# PACTA project path
pacta_project <- "Some_Project"
path_db_pacta_project <- fs::path(r2dii.utils::dbox_port2_10proj(), pacta_project)

# ===============
# set output paths
# ===============

# PACTA project output path
path_db_pacta_project_pr_output <- fs::path(path_db_pacta_project, "06_Physical_Risk")
```
When you are happy with the specifications set in `setup_project_specifications`, you can source it from the R console:
``` r
source("setup_project_specifications.R")
```

### Running Analysis
Analysis can be run from the R console using:
``` r
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
