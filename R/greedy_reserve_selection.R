#===============================================================================

                    #  greedy_reserve_selection.R

#===============================================================================

break_tie_randomly <- function (chosen_PUs_vec)
    {
    chosen_PU = sample (chosen_PUs_vec, 1)

    return (chosen_PU)
    }

#===============================================================================

break_tie_using_min_summed_loss <- function (chosen_PUs_vec,
                                             S_remaining_PUs_vec,
                                             d_mat)
    {
    PU_summed_loss_vec = colSums (d_mat)

    chosen_PUs_vec =
        which (PU_summed_loss_vec == min (PU_summed_loss_vec [S_remaining_PUs_vec]))
                                                                                if (verbose) {
                                                                                cat ("\n\n  initial chosen_PU = ", chosen_PUs_vec, "\n")
                                                                                }

        #  Now we know what are ALL of the PUs in the whole system that
        #  match the min in S, but some of those can be ones that we've
        #  already added to the solution set earlier and this can lead
        #  to the same PU being added to the solution more than once.
        #  So, now we need to intersect the chosen_PUs_vec with S because
        #  ONLY PUs in S are allowed to be selected in this round.

    chosen_PUs_vec = chosen_PUs_vec [chosen_PUs_vec %in% S_remaining_PUs_vec]
    if (length (chosen_PUs_vec) < 1) browser()

    if (length (chosen_PUs_vec) > 1)
        chosen_PU = break_tie_randomly (chosen_PUs_vec)    else
        chosen_PU = chosen_PUs_vec [1]
                                                                                if (verbose) {
                                                                                cat ("\n\n  A possibly sampled chosen_PU = ", chosen_PU, "\n")
                                                                                }
    return (chosen_PU)
    }

#===============================================================================

solution_within_budget <- function (budget, cost, ranked_solution_PUs)
    {
    cum_costs = cumsum (cost [ranked_solution_PUs])
    highest_rank_under_budget = max (which (cum_costs <= budget))

    return (ranked_solution_PUs [1:highest_rank_under_budget])
    }

#===============================================================================

find_first_solution_with_all_rep_tgts_met <- function (bpm,
                                                       ranked_solution_PUs,
                                                       spp_rep_targets)
    {
    num_spp = dim(bpm)[1]
    num_PUs = dim(bpm)[2]

    for (cur_idx in 1:num_PUs)
        {
        cur_solution_PUs = ranked_solution_PUs [1:cur_idx]
        cur_spp_reps_in_solution = rowSums (bpm [, cur_solution_PUs, drop=FALSE])
        cur_spp_meeting_or_exceeding_tgt =
            which (cur_spp_reps_in_solution > spp_rep_targets)
        if (length (cur_spp_meeting_or_exceeding_tgt) == num_spp)  break
        }

    return (ranked_solution_PUs [1:cur_idx])
    }

#===============================================================================

greedy_using_funcs <- function (num_spp,
                                num_PUs,
                                input_vars_list,
                                init_for_choosing_PUs,
                                choose_next_PU,
                                reverse_solution_order)
    {
    vars_list = init_for_choosing_PUs (input_vars_list)       #  <<<<<----------

    ranked_solution_PU_IDs_vec    = rep (0, num_PUs)
    S_remaining_PUs_vec           = 1:num_PUs
    vars_list$S_remaining_PUs_vec = S_remaining_PUs_vec

    for (cur_rank in 1:num_PUs)
        {
        if (cur_rank == num_PUs)    #  Last PU can just be copied into solution.
            {
            chosen_PU = S_remaining_PUs_vec [1]
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

            } else    #  Not the last PU, so need to do some computation
            {
            vars_list =
                choose_next_PU (S_remaining_PUs_vec, vars_list)  #  <<<<<-------

            chosen_PU = vars_list$chosen_PU

                #  Add current PU to ranked solution vector and
                #  remove it from the set of candidates for next
                #  round.
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

            idx_of_chosen_PU_in_S = which (S_remaining_PUs_vec == chosen_PU)
            S_remaining_PUs_vec = S_remaining_PUs_vec [-idx_of_chosen_PU_in_S]
            }
        }

    if (reverse_solution_order)
        ranked_solution_PU_IDs_vec = rev (ranked_solution_PU_IDs_vec)

    if (length (ranked_solution_PU_IDs_vec) !=
        length (unique (ranked_solution_PU_IDs_vec)))
        stop_bdpg ("ranked_solution_PU_IDs_vec contains duplicate entries")

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

