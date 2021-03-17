# Estimating effect of mindful sessions on blood pressure using MIPACT data

## Scripts

`libraries.R` are the R packages used throughout the projects across the scripts. `functions.R` are useful functions I created to aid in data manipulation, plot theming, etc. The numbered scripts are to be run in numeric order.
* `01_load.R` loads the raw MIPACT data files and performs data manipulation, variable creation, and cleaning. It stores these files in my personal folder in the `umms-HDS629` folder on `turbo`.
* `02_explore.R` loads the saved files from `01_load.R` and performs some basic data exploration functions and create some plots, which are saved in `umms-HDS629/mmsalva/fig`. It also does some additional data manipulation and variable creation based on exploration performed throughout the script. Importantly, it merges across outcome, exposure, and covariate files and creates and saves a singular analytic dataset (`a_dat`).
* `03_analysis.R` performs the model analysis
