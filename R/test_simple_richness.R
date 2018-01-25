#===============================================================================

                    #  test_simple_richness.R

#===============================================================================

test_simple_richness <- function (seed = 456, num_spp = 4, num_PUs = 3)
    {
                                                                                if (verbose) {
                                                                                cat ("\n  num_spp = ", num_spp)
                                                                                cat ("\n  num_PUs = ", num_PUs)
                                                                                }
    bpm = gen_dummy_bpm (num_spp, num_PUs, seed)
                                                                                if (verbose) {
                                                                                cat ("\n\n  bpm = \n")
                                                                                print (bpm)
                                                                                }

    forward = TRUE
    set.seed (seed + 12345)
    timings_using_funcs_forward = system.time ({
        sruf_FORWARD = simple_richness (num_spp, num_PUs, bpm, forward)
    })

    cat ("\n\ntimings_using_funcs FORWARD = \n")
    print (timings_using_funcs_forward)

    full_sruf_FORWARD = sruf_FORWARD$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_sruf_FORWARD) = ", length (full_sruf_FORWARD))
    print (full_sruf_FORWARD)

    short_sruf_FORWARD = sruf_FORWARD$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_sruf_FORWARD) = ", length (short_sruf_FORWARD))
    cat ("\nshort_sruf_FORWARD = \n")
    print (short_sruf_FORWARD)

    cat ("\n------------------------------------------------------------------------")

    forward = FALSE
    set.seed (seed + 12345)
    timings_using_funcs_BACKWARD = system.time ({
        sruf_BACKWARD = simple_richness (num_spp, num_PUs, bpm, forward)
    })

    cat ("\n\ntimings_using_funcs_BACKWARD = \n")
    print (timings_using_funcs_BACKWARD)

    full_sruf_BACKWARD = sruf_BACKWARD$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_sruf_BACKWARD) = ", length (full_sruf_BACKWARD))
    print (full_sruf_BACKWARD)

    short_sruf_BACKWARD = sruf_BACKWARD$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_sruf_BACKWARD) = ", length (short_sruf_BACKWARD))
    cat ("\nshort_sruf_BACKWARD = \n")
    print (short_sruf_BACKWARD)

    cat ("\n------------------------------------------------------------------------")

cat ("\n")
print (bpm)
cat ("\n")
print (rowSums (bpm [,short_sruf_BACKWARD]))

    return (list (sruf_FORWARD_long  = sruf_FORWARD$full_ranked_solution_PU_IDs_vec,
                  sruf_FORWARD_short = sruf_FORWARD$short_ranked_solution_PU_IDs_vec,
                  sruf_BACKWARD_long  = sruf_BACKWARD$full_ranked_solution_PU_IDs_vec,
                  sruf_BACKWARD_short = sruf_BACKWARD$short_ranked_solution_PU_IDs_vec))
    }

#===============================================================================

options(warn=2)
library (bdpg)

source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/test_greedy_reserve_selection.R")  #  for gen_dummy_bpm()

source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/do_simple_richness_analysis_and_output.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/greedy_reserve_selection.R")

verbose = FALSE
forward = FALSE

#sruf =test_simple_richness (seed = 456, num_spp = 4, num_PUs = 3)
#sruf =test_simple_richness (seed = 456, num_spp = 20, num_PUs = 15)
#sruf =test_simple_richness (seed = 456, num_spp = 40, num_PUs = 30)
#sruf =test_simple_richness (seed = 456, num_spp = 400, num_PUs = 300)

#sruf =test_simple_richness (seed = 1223, num_spp = 6, num_PUs = 5)
sruf =test_simple_richness (seed = 1223, num_spp = 400, num_PUs = 300)

cat ("\n")
print (sruf)

#===============================================================================


