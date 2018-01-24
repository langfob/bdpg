#===============================================================================

                #  do_simple_richness_analysis_and_output.R

#===============================================================================

init_for_choosing_PUs_simple_richness <- function (input_vars_list)
    {
    return (list (bpm = input_vars_list$bpm))
    }

#===============================================================================

choose_next_PU_simple_richness <- function (S_remaining_PUs_vec, vars_list)
    {
    richness_vec_PU = colSums (vars_list$bpm)

        #  This is a 1 element vector unless some eligible PUs have
        #  the same max loss.
    chosen_PUs_vec =
        which (richness_vec_PU == max (richness_vec_PU[S_remaining_PUs_vec]))

        #  Now we know what are ALL of the PUs in the whole system that
        #  match the min in S, but some of those can be ones that we've
        #  already added to the solution set earlier and this can lead
        #  to the same PU being added to the solution more than once.
        #  So, now we need to intersect the chosen_PUs_vec with S because
        #  ONLY PUs in S are allowed to be selected in this round.

    chosen_PUs_vec = chosen_PUs_vec [chosen_PUs_vec %in% S_remaining_PUs_vec]
    if (length (chosen_PUs_vec) < 1)
        stop_bdpg ("chosen_PUs_vec is empty in choose_next_PU_simple_richness")

        #  When there is a tie in min of max loss,
        #  break the tie randomly.
    if (length (chosen_PUs_vec) > 1)
        {
        chosen_PU = break_tie_randomly (chosen_PUs_vec)
        }
    else  chosen_PU = chosen_PUs_vec[1]

    vars_list$chosen_PU = chosen_PU

    return (vars_list)
    }

#===============================================================================

simple_richness_using_funcs <- function (num_spp, num_PUs, bpm)
    {
    input_vars_list = list (bpm = bpm)

    ranked_solution_PU_IDs_vec =
        greedy_using_funcs (num_spp, num_PUs, input_vars_list,
                            init_for_choosing_PUs_simple_richness,
                            choose_next_PU_simple_richness,
                            reverse_solution_order = FALSE)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

simple_richness <- function (num_spp, num_PUs, bpm,
                             spp_rep_targets = rep (1, num_spp))
    {
    ranked_solution_PU_IDs_vec =
        simple_richness_using_funcs (num_spp, num_PUs, bpm)

    short_ranked_solution_PU_IDs_vec =
        find_first_solution_with_all_rep_tgts_met (bpm,
                                                   ranked_solution_PU_IDs_vec,
                                                   spp_rep_targets)

    return (list (short_ranked_solution_PU_IDs_vec =
                      short_ranked_solution_PU_IDs_vec,
                  full_ranked_solution_PU_IDs_vec = ranked_solution_PU_IDs_vec))
    }

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

    set.seed (seed + 12345)
    timings_using_funcs = system.time ({
        sruf = simple_richness (num_spp, num_PUs, bpm)
    })

    cat ("\n\ntimings_using_funcs = \n")
    print (timings_using_funcs)

    full_sruf = sruf$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_sruf) = ", length (full_sruf))
    print (full_sruf)

    short_sruf = sruf$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_sruf) = ", length (short_sruf))
    cat ("\nshort_sruf = \n")
    print (short_sruf)

    cat ("\n------------------------------------------------------------------------")

    return (list (sruf_long  = sruf$full_ranked_solution_PU_IDs_vec,
                  sruf_short = sruf$short_ranked_solution_PU_IDs_vec))
    }

#===============================================================================

