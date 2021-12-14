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
  Users                                                                  
   °--Path                                                               
       ¦                                                                 
       °--Dropbox                                                        
          ¦--PortCheck_v2                                                
          ¦   °--10_Projects                                             
          ¦       °--Some_Project                                        
          ¦           °--60_Physical_Risk   #path to final output        
          °--PortCheck                                                   
              °--00_Data                                                 
                  ¦--00_RawData                                          
                  ¦   °--15_Risk                                         
                  ¦       ¦--Climate Data Factory #raw CDF data          
                  ¦       ¦   °--TCFD_Climate_Data-GeoTiff               
                  ¦       ¦       °--GeoTIFF                             
                  ¦       ¦           ¦--Indices                         
                  ¦       ¦           °--Variables                       
                  ¦       °--ClimateAnalytics #raw climate analytics data
                  ¦--01_ProcessedData                                    
                  ¦   °--08_RiskData                                     
                  ¦       ¦--asset_level_data #processed asset-level data
                  ¦       ¦   ¦--distinct_geo_data                       
                  ¦       ¦   °--prepared_ald                            
                  ¦       °--climate_data #processed climate risk data   
                  ¦           °--CDF                                     
                  ¦--06_DataStore #datastore export                      
                  ¦   °--DataStore_export_timestamp                       
                          °--quarter                                      
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
