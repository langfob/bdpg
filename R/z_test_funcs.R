#===============================================================================

                            #  z_test_funcs.R

#===============================================================================

options(warn=2)
library (bdpg)

source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/do_zonation_analysis_and_output.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/z_test.R")
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

#===============================================================================

