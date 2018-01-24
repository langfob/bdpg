#===============================================================================

                    #  do_zonation_analysis_and_output.R

#===============================================================================

#-------------------------------------------------------------------------------
#  NOTE:  You could swap the 2 criteria here and choose first on min(sum(loss))
#         and break ties on min(max(loss)) instead of zonation's normal EF
#         which is the other way around.
#-------------------------------------------------------------------------------
#  This also makes me wonder if there is a theoretically reasoned way to choose
#  a criteria that is most robust to errors in the input data.
#  I'm thinking about this in relation to the A-B/A+B stuff, where some kinds of
#  arithmetic expressions are more stable than others.
#  What might that look like here?
#  Summing seems more stable than max-ing.
#  min ((I * Sum(x_i + err(x_i))) == I*Sum(x_i) + I*(err(x_i))
#      vs.
#  min (Max (x_i + err(x_i))) ?
#
#  Also, does it matter whether you're going forward or backwards in the search?
#  That would be quite easy to phrase in a test, though it means reversing
#  max() and min() calls inside the code in several places.
#  Could those by localized to some very small bit of code that is all you need
#  to swap out?
#  For that matter, you could just replace max and min with inner and outer
#  and in one case, set inner=max, outer=min and in the other, vice versa
#  and then be able to use exactly the same base code?
#-------------------------------------------------------------------------------

#===============================================================================

gen_dummy_bpm <- function (num_spp=4, num_PUs=3, seed = 456)
    {
    set.seed (seed)

    bpm = matrix (0, nrow=num_spp, ncol=num_PUs)
                                                                                if (verbose) {
                                                                                cat ("\ndim(bpm) = ", dim(bpm))
                                                                                }
    for (cur_spp in 1:num_spp)
        {
        occ_PUs_for_this_spp = sample (1:num_PUs, 2, replace=FALSE)

        bpm [cur_spp, occ_PUs_for_this_spp] = 1
        }

    return (bpm)
    }

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




