#!/bin/bash
# running the simulation on the cluster

# Takes in 4 command-line arguments
# 1: simulation name (e.g., "continuous-strong-linear")
# 2: number of total replicates (e.g., 1000)
# 3: number of replicates per job (e.g., 5)
# 4: estimator type (e.g., "lasso")
# 5: number of unique family/p/n combinations (20)
# 6: prefix for i/o files
# 7: whether or not to use restart queue
# 8: an array of jobs to re-run (if empty, runs all jobs)
# 9: the number to multiply the seed by

if [ "${8}" == "" ]; then
  njobs=`expr ${5} \* ${2} / ${3}`
  array="1-$njobs"
else
  array=${8}
fi
io_file="${6}/slurm-%A_%a.out"

echo -e \
'#!/bin/bash\n Rscript run_screening_sim.R --sim-name ${1} --nreps-total ${2}' \
     ' --nreps-per-job ${3} --est-type ${4} --seed-mult ${5}' > call_screening_sim.sh
chmod u+x call_screening_sim.sh

# modify the following lines to use your correct partition, etc.
if [ ${7} -eq 0 ]; then
  sbatch --partition=campus-new --array=$array -e $io_file -o $io_file \
      ./call_screening_sim.sh ${1} ${2} ${3} ${4} ${9}
else
  sbatch --qos=restart-new --partition=restart-new --array=$array -e $io_file -o $io_file \
      ./call_screening_sim.sh ${1} ${2} ${3} ${4} ${9}
fi
rm call_screening_sim.sh
