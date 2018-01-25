#===============================================================================

                    #  test_unprotected_richness.R

#===============================================================================

test_unprotected_richness <- function (seed = 456, num_spp = 4, num_PUs = 3)
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
        uruf_FORWARD = unprotected_richness (num_spp, num_PUs, bpm, forward)
    })

    cat ("\n\ntimings_using_funcs FORWARD = \n")
    print (timings_using_funcs_forward)

    full_uruf_FORWARD = uruf_FORWARD$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_uruf_FORWARD) = ", length (full_uruf_FORWARD))
    print (full_uruf_FORWARD)

    short_uruf_FORWARD = uruf_FORWARD$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_uruf_FORWARD) = ", length (short_uruf_FORWARD))
    cat ("\nshort_uruf_FORWARD = \n")
    print (short_uruf_FORWARD)

    cat ("\n------------------------------------------------------------------------")

    forward = FALSE
    set.seed (seed + 12345)
    timings_using_funcs_BACKWARD = system.time ({
        uruf_BACKWARD = unprotected_richness (num_spp, num_PUs, bpm, forward)
    })

    cat ("\n\ntimings_using_funcs_BACKWARD = \n")
    print (timings_using_funcs_BACKWARD)

    full_uruf_BACKWARD = uruf_BACKWARD$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_uruf_BACKWARD) = ", length (full_uruf_BACKWARD))
    print (full_uruf_BACKWARD)

    short_uruf_BACKWARD = uruf_BACKWARD$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_uruf_BACKWARD) = ", length (short_uruf_BACKWARD))
    cat ("\nshort_uruf_BACKWARD = \n")
    print (short_uruf_BACKWARD)

    cat ("\n------------------------------------------------------------------------")

cat ("\n")
print (bpm)
cat ("\n")
print (rowSums (bpm [,short_uruf_BACKWARD]))

    return (list (uruf_FORWARD_long  = uruf_FORWARD$full_ranked_solution_PU_IDs_vec,
                  uruf_FORWARD_short = uruf_FORWARD$short_ranked_solution_PU_IDs_vec,
                  uruf_BACKWARD_long  = uruf_BACKWARD$full_ranked_solution_PU_IDs_vec,
                  uruf_BACKWARD_short = uruf_BACKWARD$short_ranked_solution_PU_IDs_vec))
    }

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


