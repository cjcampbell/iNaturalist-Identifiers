# iNaturalist-Identifiers
Analyses associated with 'Identifying the Identifiers: How iNaturalist facilitates collaborative, research-relevant data generation and why it matters for biodiversity science'

Raw data associated with this project will be repositoried on OSF.io: https://osf.io/j752n/.
## Contents

This repository contains:
### /R/
Scripts used to read, tidy, and standardize data and run analyses.
| Script                                 | Description |
| -----------                            | ----------- |
| 00_setup.R                             | Setup workspace, define universal objects |
| 01_downloadData.R                      | Download data used in study <br>ote that this script sources Google Drive, while the raw data are repositoried in OSF.io at https://osf.io/j752n/</i>. |
| 02_createDb.R                          | Assemble SQL database |                  
|03_fillInTaxonomy.R                     | Complete taxonomy table using targetted queries to iNaturalist.org |   
|04_checksAndQAQC.R                      | Generate files for external QA/QC checks. |                        
|05_countCumulativeEventsByDate.R        | Count events (observations, identifications, etc.) by date. |                                        
|06_userObsAndIDActivity.R               | Determine activity levels for all users. |                                                
|07_userGeographicInfo.R                 | Estimate user location (centroid of observations) and extent and distance of identifications. |            
|08_obsGeographicInfo.R                  | Determine geographic info (country, biome, etc.) of observations. | 
|09_summarizeTaxonomy.R                  | Find number of observations and identifications per taxa at varying taxonomic rank levels. |  
|10_countTables.R                        | Summarize user activity patterns across various axes |                                               
|11_obsQualityGrade.R                    | Estimate quality grade and time to quality grade changes for observations. |                              
|12_modelFitting.R                       | Fit models tracking changes from upload through quality grade status changes. |  
|13_plotPatternsOfIDerActivity.R         | Plot patterns of identifier activity, e.g., observation v. identification counts |             
|14_plotTaxPatterns.R                    | Plot patterns of identifier activity across taxon rank levels. |   
|15_plotUserSpecializationAndMaps.R      | Plot user identification patterns. |              
|16_plotGrowth.R                         | Plot growth of iNaturalist activity over time. |             
|17_plotModelResults.R                   | Plot sanke plot results of quality grade changes, based on model outputs. |


## Session info
Analyses were conducted with the following configuration:
```
R version 4.2.2 Patched (2022-11-10 r83330)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.5 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8       
 [4] LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
 [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C              
[10] LC_TELEPHONE=C             LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] webshot_0.5.4       scales_1.2.1        patchwork_1.1.2     gridExtra_2.3      
 [5] pals_1.7            geosphere_1.5-10    cowplot_1.1.1       ggridges_0.5.4     
 [9] ggpubr_0.5.0        geodata_0.5-3       terra_1.7-3         glmmLasso_1.6.2    
[13] sjPlot_2.8.11       optimx_2022-4.30    lme4_1.1-27.1       Matrix_1.5-1       
[17] lubridate_1.7.10    bettermc_1.1.2      maptools_1.1-1      rnaturalearth_0.1.0
[21] adehabitatHR_0.4.19 adehabitatLT_0.3.25 CircStats_0.2-6     boot_1.3-28        
[25] MASS_7.3-58         adehabitatMA_0.3.14 ade4_1.7-16         deldir_0.2-10      
[29] sp_1.4-5            sf_1.0-9            curl_4.3.1          httr_1.4.2         
[33] jsonlite_1.7.2      googledrive_1.0.1   networkD3_0.4       DBI_1.1.1          
[37] data.table_1.14.0   forcats_0.5.1       stringr_1.4.0       dplyr_1.0.5        
[41] purrr_0.3.4         readr_1.4.0         tidyr_1.1.3         tibble_3.1.1       
[45] ggplot2_3.4.0       tidyverse_1.3.1    

loaded via a namespace (and not attached):
 [1] readxl_1.3.1        backports_1.2.1     plyr_1.8.6          igraph_1.2.11      
 [5] splines_4.2.2       TH.data_1.0-10      digest_0.6.27       htmltools_0.5.1.1  
 [9] fansi_0.4.2         magrittr_2.0.1      memoise_2.0.0       modelr_0.1.8       
[13] sandwich_3.0-1      prettyunits_1.1.1   colorspace_2.0-0    blob_1.2.1         
[17] rvest_1.0.0         haven_2.4.1         xfun_0.22           crayon_1.4.1       
[21] survival_3.4-0      zoo_1.8-9           glue_1.4.2          gtable_0.3.0       
[25] emmeans_1.8.0       sjstats_0.18.1      sjmisc_2.8.9        car_3.0-12         
[29] maps_3.3.0          abind_1.4-5         mvtnorm_1.1-2       rstatix_0.7.1      
[33] ggeffects_1.1.3     Rcpp_1.0.6          viridisLite_0.4.0   xtable_1.8-4       
[37] progress_1.2.2      performance_0.9.2   units_0.7-1         foreign_0.8-82     
[41] bit_4.0.4           mapproj_1.2.7       proxy_0.4-25        datawizard_0.5.1   
[45] htmlwidgets_1.5.3   ellipsis_0.3.2      pkgconfig_2.0.3     dbplyr_2.1.1       
[49] utf8_1.2.1          tidyselect_1.1.1    rlang_1.0.6         effectsize_0.7.0.5 
[53] munsell_0.5.0       cellranger_1.1.0    tools_4.2.2         cachem_1.0.4       
[57] cli_3.5.0           unixtools_0.1-1     generics_0.1.0      RSQLite_2.2.7      
[61] sjlabelled_1.2.0    broom_0.7.6         fastmap_1.1.0       knitr_1.33         
[65] bit64_4.0.5         fs_1.5.0            nlme_3.1-160        xml2_1.3.2         
[69] compiler_4.2.2      rstudioapi_0.13     e1071_1.7-6         ggsignif_0.6.3     
[73] reprex_2.0.0        stringi_1.5.3       parameters_0.18.2   lattice_0.20-45    
[77] classInt_0.4-3      nloptr_1.2.2.3      vctrs_0.5.1         pillar_1.6.0       
[81] lifecycle_1.0.3     estimability_1.4.1  insight_0.18.2      R6_2.5.0           
[85] KernSmooth_2.23-20  codetools_0.2-18    dichromat_2.0-0.1   assertthat_0.2.1   
[89] withr_2.4.2         multcomp_1.4-17     bayestestR_0.12.1   hms_1.0.0          
[93] grid_4.2.2          class_7.3-20        minqa_1.2.4         carData_3.0-4      
[97] numDeriv_2016.8-1.1
```
