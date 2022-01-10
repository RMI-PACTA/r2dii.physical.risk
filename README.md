
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.physical.risk <a href='https://github.com/2DegreesInvesting/r2dii.physical.risk'><img src='https://imgur.com/A5ASZPE.png' align='right' height='43' /></a>

<!-- badges: start -->
<!-- badges: end -->

This repo contains functions, workflows and applications to estimate
physical climate risk for physical assets and financial portfolios.

## Installation

You can install the development version of r2dii.physical.risk from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.physical.risk", build_vignettes = T)
```

To access the workflows, you must also clone the repo itself:

``` bash
git clone https://github.com/2DegreesInvesting/r2dii.physical.risk.git
```

## Methodology

The physical risk methodology implemented here is too detailed to be
included concisely in this README. Once you have installed the R
package, you can access the methodology from the R console by calling:

``` r
vignette('physical-risk-methodology')
```

## Usage

### Setting up Project Specifications

Project specifications mustn be first set in the file
`setup_project_specifications.R`. Things such as paths to input and
output data directories are set in this file. The physical risk module
requires many different inputs and outputs. An example full directory
structure might look something like:

``` bash
  Users                                                                  
   °--Path                                                               
       °--Dropbox                                                        
          ¦--PortCheck_v2                                                
          ¦   °--10_Projects                                             
          ¦       °--Some_Project                                        
          ¦           °--06_Physical_Risk   #path to final output        
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
                  ¦   °--DataStore_export_05172021                       
                  ¦       °--2020Q4                                      
```

Unfortunately, at this point, most of these datasets are proprietary,
and 2DII internal.

It is very important that the output data folder locations are set
properly in the `setup_project_specifications.R` file (otherwise you
risk over-writing someone else’s project). For example:

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

When you are happy with the specifications set in
`setup_project_specifications`, you can source it from the R console:

``` r
source("setup_project_specifications.R")
```

### Running Analysis

Analysis can be run from the R console using:

``` r
source("analysis.R")
```

<details>
<summary>
Optional: Preparing/ Updating Input Data
</summary>

## Asset Resolution

`prepare_AR_data.R`

## Climate Data Factory

`prepare_CDF_data.R`

## Climate Analytics

`prepare_climate_analytics_data.R`

## Open Street Map

`prepare_OSM_data.R`

</details>
