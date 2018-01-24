#===============================================================================

                    #  test_unprotected_richness.R

#===============================================================================

options(warn=2)
library (bdpg)

source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/test_greedy_reserve_selection.R")  #  for gen_dummy_bpm()

source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/do_unprotected_richness_analysis_and_output.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/greedy_reserve_selection.R")

verbose = FALSE

#uruf =test_unprotected_richness (seed = 456, num_spp = 4, num_PUs = 3)
#uruf =test_unprotected_richness (seed = 456, num_spp = 20, num_PUs = 15)
#uruf =test_unprotected_richness (seed = 456, num_spp = 40, num_PUs = 30)
#uruf =test_unprotected_richness (seed = 456, num_spp = 400, num_PUs = 300)

#uruf =test_unprotected_richness (seed = 1223, num_spp = 6, num_PUs = 5)
uruf =test_unprotected_richness (seed = 1223, num_spp = 400, num_PUs = 300)

cat ("\n")
print (uruf)

#===============================================================================


