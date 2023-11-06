#!/bin/bash
# run all simulations
ml fhR/4.0.2-foss-2019b

outcome_types=("continuous" "binomial-probit")
feature_strengths=("strong" "weak")
outcome_regressions=("linear" "nonlinear")
corrs=("uncorrelated" "correlated")
# Args:
# 01: estimator (e.g., "lasso", "SL", "SL_screen")
# 02: whether or not to use restart queue
# 03: an array of jobs to resubmit (if necessary)
est=${1}
# estimators=("lasso" "SL" "SL_screen")
io_prefix="<modify this line>"
# for est in "${estimators[@]}"; do
for outcome_type in "${outcome_types[@]}"; do
  for feature_strength in "${feature_strengths[@]}"; do
    for outcome_regression in "${outcome_regressions[@]}"; do
      for corr in "${corrs[@]}"; do
        sim_name="${outcome_type}-${feature_strength}-${outcome_regression}-${corr}"
        this_io_prefix="${io_prefix}/output_${sim_name}/$est"
        mkdir -p $this_io_prefix
        ./submit_screening_sim.sh $sim_name 1000 5 $est 20 $this_io_prefix $2 $3
      done
    done
  done
done
