# Supplementary materials for the SL screening paper

This repository contains code to reproduce the analyses in ["Practical considerations for variable screening in the super learner"](https://arxiv.org/abs/2311.03313) by Williamson, King, and Huang, to be published in the _New England Journal of Statistics in Data Science_. All analyses were implemented in the freely available `R` programming language (version 4.0.2). 

This README file provides an overview of the code available in the repository.

## The `sim_code` directory

The `sim_code` directory contains all of the files necessary to reproduce the simulation analyses in the paper.

Prior to running any code, you should ensure that the following R packages are installed: `SuperLearner`, `tidyverse`, and `vimp`; all are available on CRAN.

The code is designed to run on a high-performance computing cluster using the Slurm job scheduling system (you will have to modify the shell scripts to use your username, etc.). If you are not running on this type of cluster, you will have to modify `run_screening_sim.R` to ensure that you get results for each combination of parameters.

## The `code` directory

The `code` directory contains all of the files necessary to reproduce the data analysis. In addition to the necessary packages for the simulations, you will need to have packages `rprojroot`, `data.table`, `glmnet`, `xgboost`, `ranger`, `earth`, `nloptr`, `quadprog`, `optparse`, `methods`, `cvAUC`, `kableExtra`.

## Reproducing the simulation analyses

To run all analyses on a cluster (using SLURM), run the following code:
```
chmod u+x *.sh
./submit_all_screening_estimators.sh
```

This, in turn, calls `submit_all_screening_sims.sh` (submit all simulations for a given estimation procedure) and `submit_screening_sim.sh` (submit all simulations for a given data-generating mechanism and estimator).

The main R code for the simulation is in `run_screening_sim.R` (runs the simulation for a specified set of parameters a set number of times, saves results) and `do_one.R` (run the simulation a single time). The data-generating mechanisms are specified in `gen_data.R`. Generally-useful functions are defined in `utils.R`. Screening algorithms are defined in `sl_screens.R`.

Once all simulations have finished, run `build_csv.R` (which calls `read_data.R`) to compile all output; then run `plot_performance_metrics.R` to create plots with results.

The files with suffix `_fixedV` can be used to reproduce supplemental results with a fixed number of cross-validation folds.

# Issues

If you encounter any bugs or have specific questions about the analysis, please [file an issue](https://github.com/bdwilliamson/sl_screening_supplementary/issues).