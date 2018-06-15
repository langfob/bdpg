#!/bin/bash

#                               build_all_csvs.sh

#   ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/build_all_csvs.sh

#  Cloned from ~/.../testgit.sh.  Not sure where that file came from...

#  Don't forget to make executable.
#  chmod u+x build_all_csvs.sh

#-------------------------------------------------------------------------------

#  Best to run this script from the top directory of a set of runsets,
#  which usually contains just one entry, i.e., a directory called
#  default_runset (or whatever name you use for your runset).

#  For example, my current test area on the nectar VM is:
#      ~/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_05_err_amt_100_reps_for_1st_large_multi_exp_gurobi_nectar_test
#  and it only contains the directory called default_runset.

#  I cd to that "~/tzar/..._gurobi_nectar_test" directory and run this
#  script it produces several intermediate files that lead up to one ".csv"
#  file for each reserve selector, e.g., "Gurobi.csv".

#-------------------------------------------------------------------------------

#  On nectar:
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    Gurobi         Gurobi         rsrun_results.csv    .
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    Marxan_SA      Marxan_SA      rsrun_results.csv    .
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    ZL_Backward    ZL_Backward    rsrun_results.csv    .
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    ZL_Forward     ZL_Forward     rsrun_results.csv    .
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    SR_Forward     SR_Forward     rsrun_results.csv    .
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    SR_Backward    SR_Backward    rsrun_results.csv    .
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    UR_Backward    UR_Backward    rsrun_results.csv    .
/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    UR_Forward     UR_Forward     rsrun_results.csv    .

/usr/local/lib/R/site-library/bdpg/ShellScripts/buildcsv.sh    Marxan_SA_SS    Marxan_SA    rsrun_results_summed_solution.csv    .

#-------------------------------------------------------------------------------

#  On the mac:
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    Gurobi         Gurobi         rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    Marxan_SA      Marxan_SA      rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    ZL_Backward    ZL_Backward    rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    ZL_Forward     ZL_Forward     rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    SR_Forward     SR_Forward     rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    SR_Backward    SR_Backward    rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    UR_Backward    UR_Backward    rsrun_results.csv    .
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    UR_Forward     UR_Forward     rsrun_results.csv    .
#
# ~/D/Projects/ProblemDifficulty/pkgs/bdpg/inst/ShellScripts/buildcsv.sh    Marxan_SA_SS    Marxan_SA    rsrun_results_summed_solution.csv    .

#-------------------------------------------------------------------------------
