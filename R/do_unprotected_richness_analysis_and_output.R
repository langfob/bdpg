#===============================================================================

                #  do_unprotected_richness_analysis_and_output.R

#===============================================================================

init_for_choosing_PUs_unprotected_richness <- function (input_vars_list)
    {
    return (list (bpm = input_vars_list$bpm,
                  spp_rep_targets = input_vars_list$spp_rep_targets))
    }

#===============================================================================

choose_next_PU_unprotected_richness <- function (S_remaining_PUs_vec, vars_list,
                                                  forward)
    {
    spp_rep_targets = vars_list$spp_rep_targets
    bpm             = vars_list$bpm
    num_spp         = dim(bpm)[1]
    num_PUs         = dim(bpm)[2]

        #-----------------------------------------------------------------
        #  Find which species have already met or exceeded their target.
        #-----------------------------------------------------------------

    all_PUs          = 1:num_PUs
    cur_solution_PUs = all_PUs [-S_remaining_PUs_vec]

        #-----------------------------------------------------------------
        #  Compute the total number of occurrences of all species in
        #  the current solution.
        #  If there is only one PU column in the current solution,
        #  then R will complain that it's not a matrix when it tries
        #  to do row sum for each species, so you need to use drop=FALSE
        #  to be sure it's still viewed as a matrix rather than a vector.
        #  If you don't have drop=FALSE, the error you get is:
        #      Error in rowSums(bpm[, cur_solution_PUs]) :
        #        'x' must be an array of at least two dimensions
        #-----------------------------------------------------------------

    cur_spp_reps_in_solution = rowSums (bpm [, cur_solution_PUs, drop=FALSE])

        #------------------------------------------------------------------
        #  Determine which species already meet their targets so that you
        #  can remove them from the matrix calculations.
        #------------------------------------------------------------------

    cur_spp_meeting_or_exceeding_tgt =
            which (cur_spp_reps_in_solution >= spp_rep_targets)
    num_spp_meeting_or_exceeding_tgt = length (cur_spp_meeting_or_exceeding_tgt)

    if (num_spp_meeting_or_exceeding_tgt == num_spp)
        {
            #------------------------------------------------------------------
            #  All species already meet their targets, so the remaining PUs
            #  are all superflous and it doesn't matter which one you choose.
            #------------------------------------------------------------------

        chosen_PUs_vec = S_remaining_PUs_vec

        } else
        {
            #----------------------------------------------------------------
            #  At least one species does not meet its target.
            #  Strip the already protected species out of the matrix and
            #  compute the richness of just the unprotected species on each
            #  PU.
            #  Again, we have to use drop=FALSE to keep R from converting
            #  the bpm matrix to a vector when it gets down to 1 row.
            #----------------------------------------------------------------

        unprotected_spp_bpm = bpm [-cur_spp_meeting_or_exceeding_tgt,, drop=FALSE]
        unprotected_richness_vec_PU = colSums (unprotected_spp_bpm)
if (forward)
{
            chosen_PUs_vec =
                which (unprotected_richness_vec_PU ==
                       max (unprotected_richness_vec_PU[S_remaining_PUs_vec]))
} else
{
            chosen_PUs_vec =
                which (unprotected_richness_vec_PU ==
                       min (unprotected_richness_vec_PU[S_remaining_PUs_vec]))
}


            #----------------------------------------------------------------
            #  Now we know what are ALL of the PUs in the whole system that
            #  match the min in S, but some of those can be ones that we've
            #  already added to the solution set earlier.
            #  To avoid adding the same PU to the solution more than once,
            #  we need to intersect the chosen_PUs_vec with S because
            #  ONLY PUs in S are allowed to be selected in this round.
            #----------------------------------------------------------------

        chosen_PUs_vec = chosen_PUs_vec [chosen_PUs_vec %in% S_remaining_PUs_vec]
        if (length (chosen_PUs_vec) < 1)
            stop_bdpg ("chosen_PUs_vec is empty in choose_next_PU_unprotected_richness")
        }

        #-------------------------------------------------------------------
        #  When there is a tie in min of max loss, break the tie randomly.
        #-------------------------------------------------------------------

    if (length (chosen_PUs_vec) > 1)
        {
        chosen_PU = break_tie_randomly (chosen_PUs_vec)
        }
    else  chosen_PU = chosen_PUs_vec[1]

    vars_list$chosen_PU = chosen_PU

    return (vars_list)
    }

#===============================================================================

unprotected_richness_using_funcs <- function (num_spp, num_PUs, bpm,
                                               forward,
                                               spp_rep_targets)
    {
    input_vars_list = list (bpm = bpm,
                            spp_rep_targets = spp_rep_targets)

    ranked_solution_PU_IDs_vec =
        greedy_using_funcs (num_spp, num_PUs, input_vars_list,
                            init_for_choosing_PUs_unprotected_richness,
                            choose_next_PU_unprotected_richness,
                            forward)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

unprotected_richness <- function (num_spp, num_PUs, bpm,
                                  forward = TRUE,
                                  spp_rep_targets = rep (1, num_spp))
    {
    ranked_solution_PU_IDs_vec =
        unprotected_richness_using_funcs (num_spp, num_PUs, bpm,
                                           forward,
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

