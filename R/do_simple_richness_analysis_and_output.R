#===============================================================================

                #  do_simple_richness_analysis_and_output.R

#===============================================================================

init_for_choosing_PUs_simple_richness <- function (input_vars_list)
    {
    return (list (bpm = input_vars_list$bpm))
    }

#===============================================================================

choose_next_PU_simple_richness <- function (S_remaining_PUs_vec, vars_list,
                                            forward)
    {
    richness_vec_PU = colSums (vars_list$bpm)

        #  This is a 1 element vector unless some eligible PUs have
        #  the same max loss.
if (forward)
{
    chosen_PUs_vec =
        which (richness_vec_PU == max (richness_vec_PU[S_remaining_PUs_vec]))
} else
{
    chosen_PUs_vec =
        which (richness_vec_PU == min (richness_vec_PU[S_remaining_PUs_vec]))
}

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

simple_richness_using_funcs <- function (num_spp, num_PUs, bpm, forward)
    {
    input_vars_list = list (bpm = bpm)

    ranked_solution_PU_IDs_vec =
        greedy_using_funcs (num_spp, num_PUs, input_vars_list,
                            init_for_choosing_PUs_simple_richness,
                            choose_next_PU_simple_richness,
                            forward)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

simple_richness <- function (num_spp, num_PUs, bpm,
                             forward = TRUE,
                             spp_rep_targets = rep (1, num_spp))
    {
    ranked_solution_PU_IDs_vec =
        simple_richness_using_funcs (num_spp, num_PUs, bpm, forward)

    short_ranked_solution_PU_IDs_vec =
        find_first_solution_with_all_rep_tgts_met (bpm,
                                                   ranked_solution_PU_IDs_vec,
                                                   spp_rep_targets)

    return (list (short_ranked_solution_PU_IDs_vec =
                      short_ranked_solution_PU_IDs_vec,
                  full_ranked_solution_PU_IDs_vec = ranked_solution_PU_IDs_vec))
    }

#===============================================================================

