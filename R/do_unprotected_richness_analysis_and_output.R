#===============================================================================

                #  do_unprotected_richnesss_analysis_and_output.R

#===============================================================================

init_for_choosing_PUs_unprotected_richnesss <- function (input_vars_list)
    {
    return (list (bpm = input_vars_list$bpm,
                  spp_rep_targets = input_vars_list$spp_rep_targets))
    }

#===============================================================================

choose_next_PU_unprotected_richnesss <- function (S_remaining_PUs_vec, vars_list)
    {
    spp_rep_targets = vars_list$spp_rep_targets

    bpm             = vars_list$bpm
    num_spp         = dim(bpm)[1]
    num_PUs         = dim(bpm)[2]

        #---------------------------------------------------------------------
        #  Find which species have already met or exceeded their target.
        #  On the first pass, nothing has been reserved, so use all of bpm
        #  in that case, i.e., when S is the whole PU set.
        #  If you don't test for that case, S_remaining_PUs_vec will be the
        #  entire set of PUs and when you remove that set from itself,
        #  cur_solution_PUs will be NULL.
        #---------------------------------------------------------------------

    if (length (S_remaining_PUs_vec) < num_PUs)
        {
        all_PUs                          = 1:num_PUs
        cur_solution_PUs                 = all_PUs [-S_remaining_PUs_vec]

            #---------------------------------------------------------------
            #  If there is only one column left, then R will complain that
            #  it's no longer a matrix when it tries to do rowSums.
            #  In that case, the rowSums for that single column are just
            #  the values in the column since they're not added to
            #  anything else.
            #  If you don't trap for this case, the error you get is:
            #      Error in rowSums(bpm[, cur_solution_PUs]) :
            #        'x' must be an array of at least two dimensions
            #---------------------------------------------------------------

        if (length (cur_solution_PUs) == 1)
            {
            cur_spp_reps_in_solution = bpm [, cur_solution_PUs]
            }
        else
            {
            cur_spp_reps_in_solution = rowSums (bpm [, cur_solution_PUs])
            }

            #  Remove species already meeting their targets.
        cur_spp_meeting_or_exceeding_tgt =
                which (cur_spp_reps_in_solution >= spp_rep_targets)

        num_spp_meeting_or_exceeding_tgt = length (cur_spp_meeting_or_exceeding_tgt)
        if (num_spp_meeting_or_exceeding_tgt < num_spp)
            unprotected_spp_bpm = bpm [-cur_spp_meeting_or_exceeding_tgt,]

        } else
        {
        num_spp_meeting_or_exceeding_tgt = 0
        unprotected_spp_bpm              = bpm
        }

    if (num_spp_meeting_or_exceeding_tgt < num_spp)
        {
        if (num_spp_meeting_or_exceeding_tgt == (num_spp - 1))
            {
            unprotected_richness_vec_PU = unprotected_spp_bpm
            } else
            {
            unprotected_richness_vec_PU = colSums (unprotected_spp_bpm)
            }

            #  This is a 1 element vector unless some eligible PUs have
            #  the same max loss.
        chosen_PUs_vec =
            which (unprotected_richness_vec_PU == max (unprotected_richness_vec_PU[S_remaining_PUs_vec]))

            #  Now we know what are ALL of the PUs in the whole system that
            #  match the min in S, but some of those can be ones that we've
            #  already added to the solution set earlier and this can lead
            #  to the same PU being added to the solution more than once.
            #  So, now we need to intersect the chosen_PUs_vec with S because
            #  ONLY PUs in S are allowed to be selected in this round.

        chosen_PUs_vec = chosen_PUs_vec [chosen_PUs_vec %in% S_remaining_PUs_vec]
        if (length (chosen_PUs_vec) < 1)
            stop_bdpg ("chosen_PUs_vec is empty in choose_next_PU_unprotected_richnesss")

        } else
        {
        chosen_PUs_vec = S_remaining_PUs_vec
        }

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

unprotected_richnesss_using_funcs <- function (num_spp, num_PUs, bpm,
                                               spp_rep_targets)
    {
    input_vars_list = list (bpm = bpm,
                            spp_rep_targets = spp_rep_targets)

    ranked_solution_PU_IDs_vec =
        greedy_using_funcs (num_spp, num_PUs, input_vars_list,
                            init_for_choosing_PUs_unprotected_richnesss,
                            choose_next_PU_unprotected_richnesss,
                            reverse_solution_order = FALSE)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

unprotected_richnesss <- function (num_spp, num_PUs, bpm,
                                   spp_rep_targets = rep (1, num_spp))
    {
    ranked_solution_PU_IDs_vec =
        unprotected_richnesss_using_funcs (num_spp, num_PUs, bpm,
                                           spp_rep_targets)

    short_ranked_solution_PU_IDs_vec =
        find_first_solution_with_all_rep_tgts_met (bpm,
                                                   ranked_solution_PU_IDs_vec,
                                                   spp_rep_targets)

    return (list (short_ranked_solution_PU_IDs_vec =
                      short_ranked_solution_PU_IDs_vec,
                  full_ranked_solution_PU_IDs_vec = ranked_solution_PU_IDs_vec))
    }

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

    set.seed (seed + 12345)
    timings_using_funcs = system.time ({
        uruf = unprotected_richnesss (num_spp, num_PUs, bpm)
    })

    cat ("\n\ntimings_using_funcs = \n")
    print (timings_using_funcs)

    full_uruf = uruf$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_uruf) = ", length (full_uruf))
    print (full_uruf)

    short_uruf = uruf$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_uruf) = ", length (short_uruf))
    cat ("\nshort_uruf = \n")
    print (short_uruf)

    cat ("\n------------------------------------------------------------------------")

    return (list (uruf_long  = uruf$full_ranked_solution_PU_IDs_vec,
                  uruf_short = uruf$short_ranked_solution_PU_IDs_vec))
    }

#===============================================================================

