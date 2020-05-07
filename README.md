Code to reproduce analyses in: 
>Connors, B., Malick M., Ruggerone R., Adkison M., Irvine J., Campbell R., and K. Gorman. In press. Climate and competition influence sockeye salmon population dynamics across the Northeast Pacific Ocean. Canadian Journal of Fisheries and Aquatic Sciences.

Project made possible in part by grant from the [National Center for Ecological Analysis and Synthesis](https://www.nceas.ucsb.edu/) and the [State of Alaskan Salmon and People project](https://alaskasalmonandpeople.org/)

## Files
- `make.R`: All code required to reproduce current set of simulations and figures.

- `load.R`: Loads packages and scripts necessary for analysis. This file should be sourced prior to running other scripts.

- `functions.R`: All functions written for the analysis should be placed in this file.
  
- `close_loop_sims.R`: Run closed loop forwad simulations.

- `figures.R`: Generate figures not associated with simulations (e.g., equilibrium trade-offs, populaiton diversity; in `figures` folder)
  
- `simulation_summary.Rmd`: R Markdown doc that summarizes simulations, and generates figures for manuscript (in `images` folder)

- `appendix_A.R`: R Markdown doc that details the stationary Ricker to time-varying Beverton-Holt formulation we used, and simulations to justify its parameterization in our closed-loop simulations. 
