#!/bin/bash

# lasso only
./submit_all_screening_sims.sh "lasso" 1 "" 

# SL with no screens
./submit_all_screening_sims.sh "SL" 1 "" 
# SL with no screens, no lasso
./submit_all_screening_sims.sh "SL_no_lasso" 1 "" 

# SL with only lasso screen
./submit_all_screening_sims.sh "SL_screen_lasso_only" 1 "" 

./submit_all_screening_sims.sh "SL_no_lasso_screen_lasso_only" 1 "" 

# SL with screens (no lasso screen)
./submit_all_screening_sims.sh "SL_screen" 1 "" 

./submit_all_screening_sims.sh "SL_no_lasso_screen" 1 "" 

# SL with screens (including lasso screen)
./submit_all_screening_sims.sh "SL_screen_lasso" 0 "" 

./submit_all_screening_sims.sh "SL_no_lasso_screen_lasso" 0 "" 
