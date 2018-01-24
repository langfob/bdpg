#===============================================================================

                            #  test_z_funcs.R

#===============================================================================

options(warn=2)
library (bdpg)

source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/test_greedy_reserve_selection.R")  #  for gen_dummy_bpm()
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/do_zonation_analysis_and_output.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/test_z.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/greedy_reserve_selection.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/z_using_funcs.R")

verbose = FALSE

zmeths = "funcs"

#zu =test_z (zmeths, seed = 456, num_spp = 4, num_PUs = 3)
#zu =test_z (zmeths, seed = 456, num_spp = 20, num_PUs = 15)
#zu =test_z (zmeths, seed = 456, num_spp = 40, num_PUs = 30)
zu =test_z (zmeths, seed = 456, num_spp = 400, num_PUs = 300)

#zu =test_z (zmeths, seed = 1223, num_spp = 6, num_PUs = 5)

cat ("\n")
print (zu)

#   user  system elapsed
#  4.225                  first test with hard-coded func names
#  4.109                  second test of same
#  4.394   0.127   4.533
#  4.498   0.145   4.654
#  4.377   0.056   4.434

#  4.135   0.053   4.189  using function names as variables
#  4.350   0.119   4.478
#  4.415   0.051   4.469
#  4.188   0.049   4.238

#===============================================================================

