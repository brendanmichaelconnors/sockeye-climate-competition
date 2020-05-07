[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3813107.svg)](https://doi.org/10.5281/zenodo.3813107)

Code to reproduce analyses in: 
>Connors, B., Malick M., Ruggerone R., Adkison M., Irvine J., Campbell R., and K. Gorman. In press. Climate and competition influence sockeye salmon population dynamics across the Northeast Pacific Ocean. Canadian Journal of Fisheries and Aquatic Sciences.

Project made possible in part by grant from the [National Center for Ecological Analysis and Synthesis](https://www.nceas.ucsb.edu/) and the [State of Alaskan Salmon and People](https://alaskasalmonandpeople.org/) project.

## Files
- `make.R`: source this file to reproduce the project.

- `load.R`: load packages and scripts necessary for analysis. 

- `functions.R`: all functions written for the analysis are placed in this file.
  
- `data_download.R`: download data needed for project and write to CSV.

- `sock_data_clean.R`: clean/process the raw downloaded sockeye data.
  
- `sst_import_process.R`: read in SST data, generate anomalies and calculate average SST over a specified period and region.

- `sst_explore.R`: explore the SST anomalies. 

- `detrend_covars.R`: de-trend covariates.

- `sock_covariates.R`:  create stock specific covariates used in analysis.

- `sock_explore.R`:  exploratory graphics and summaries of sockeye and covariate data.

- `single_stock.R`:  exploratory single-stock generalized Ricker models.

- `hbm_fit.R`:  fit hierarchical bayesian models.

- `pub.R`:  generate all tables and figures.

- `hatch_effects.R`:  calculate estimated effects of hatchery pink production.



