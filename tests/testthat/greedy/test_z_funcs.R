#===============================================================================

                            #  test_z_funcs.R

#===============================================================================

test_z_using_funcs <- function (seed = 456, num_spp = 4, num_PUs = 3)
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

    wt_spp_vec = rep (1, num_spp)    #c(2,3,4,5)    #rep (2, num_spp)    #  weight of species j
                                                                                if (verbose) {
                                                                                cat ("\n  wt_spp_vec = ", wt_spp_vec)
                                                                                }

    c_PU_vec = rep (1, num_PUs)      #c(10,20,30)    #rep (1, num_PUs)  #  cost of PU i
                                                                                if (verbose) {
                                                                                cat ("\n  c_PU_vec = ", c_PU_vec)
                                                                                }

    forward = TRUE
    set.seed (seed + 12345)
    timings_using_funcs_forward = system.time ({
        zuf_FORWARD = z (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm, forward, z_meth = "funcs")
    })

    cat ("\n\ntimings_using_funcs FORWARD = \n")
    print (timings_using_funcs_forward)

    full_zuf_FORWARD = zuf_FORWARD$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_zuf_FORWARD) = ", length (full_zuf_FORWARD))
    print (full_zuf_FORWARD)

    short_zuf_FORWARD = zuf_FORWARD$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_zuf_FORWARD) = ", length (short_zuf_FORWARD))
    cat ("\nshort_zuf_FORWARD = \n")
    print (short_zuf_FORWARD)

    cat ("\n------------------------------------------------------------------------")

    forward = FALSE
    set.seed (seed + 12345)
    timings_using_funcs_BACKWARD = system.time ({
        zuf_BACKWARD = z (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm, forward, z_meth = "funcs")
    })

    cat ("\n\ntimings_using_funcs_BACKWARD = \n")
    print (timings_using_funcs_BACKWARD)

    full_zuf_BACKWARD = zuf_BACKWARD$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_zuf_BACKWARD) = ", length (full_zuf_BACKWARD))
    print (full_zuf_BACKWARD)

    short_zuf_BACKWARD = zuf_BACKWARD$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_zuf_BACKWARD) = ", length (short_zuf_BACKWARD))
    cat ("\nshort_zuf_BACKWARD = \n")
    print (short_zuf_BACKWARD)

    cat ("\n------------------------------------------------------------------------")

cat ("\n")
print (rowSums (bpm [,short_zuf_FORWARD, drop=FALSE]))
cat ("\n")
print (rowSums (bpm [,short_zuf_BACKWARD, drop=FALSE]))

    return (list (zuf_FORWARD_long  = zuf_FORWARD$full_ranked_solution_PU_IDs_vec,
                  zuf_FORWARD_short = zuf_FORWARD$short_ranked_solution_PU_IDs_vec,
                  zuf_BACKWARD_long  = zuf_BACKWARD$full_ranked_solution_PU_IDs_vec,
                  zuf_BACKWARD_short = zuf_BACKWARD$short_ranked_solution_PU_IDs_vec))
    }

#===============================================================================

options(warn=2)
library (bdpg)

source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/test_greedy_reserve_selection.R")  #  for gen_dummy_bpm()
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/do_zonation_analysis_and_output.R")
#source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/test_z.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/greedy_reserve_selection.R")
source ("/Users/bill/D/Projects/ProblemDifficulty/pkgs/bdpg/R/z_using_funcs.R")

verbose = FALSE

zmeths = "funcs"

forward = FALSE

#zu =test_z_using_funcs (seed = 456, num_spp = 4, num_PUs = 3)
#zu =test_z_using_funcs (seed = 456, num_spp = 20, num_PUs = 15)
#zu =test_z_using_funcs (seed = 456, num_spp = 40, num_PUs = 30)
#zu =test_z_using_funcs (seed = 456, num_spp = 400, num_PUs = 300)

#zu =test_z_using_funcs (seed = 1223, num_spp = 6, num_PUs = 5)
#zu =test_z_using_funcs (seed = 1223, num_spp = 400, num_PUs = 300)

zu =test_z_using_funcs (seed = 17, num_spp = 200, num_PUs = 600)

cat ("\n")
print (zu)

cat ("\nlength(short_zuf_FORWARD) = ", length (zu$zuf_FORWARD_short))
cat ("\nlength(short_zuf_BACKWARD) = ", length (zu$zuf_BACKWARD_short))

#===============================================================================

