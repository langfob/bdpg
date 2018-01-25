#===============================================================================

                            #  z_using_funcs.R

#===============================================================================

init_for_choosing_PUs_z <- function (input_vars_list)
    {
        #  Extract input variables for this function
    bpm = input_vars_list$bpm
    c_PU_vec = input_vars_list$c_PU_vec
    wt_spp_vec = input_vars_list$wt_spp_vec

        #  Compute original_frac_abund_spp_j_on_PU_i, i.e.,
        #  Normalize each species's abundance on each PU by total abundance
        #  for that species, i.e., compute rel_spp_abundance
    q_mat = sweep (bpm, MARGIN=1, FUN="/",STATS=rowSums (bpm))

    qw_spp_weighted_q_mat =
        sweep (q_mat, MARGIN=1, FUN="*",STATS=wt_spp_vec)

    d_fixed_part_mat =
        sweep (qw_spp_weighted_q_mat, MARGIN=2, FUN="/",STATS=c_PU_vec)

    return (list (q_mat         = q_mat,
                  d_fixed_part_mat = d_fixed_part_mat))
    }

#===============================================================================

choose_next_PU_z <- function (S_remaining_PUs_vec, vars_list, forward)
    {
        #---------------------------------------------
        #  Extract input variables for this function
        #---------------------------------------------

    q_mat = vars_list$q_mat
    d_fixed_part_mat = vars_list$d_fixed_part_mat

        #---------------------------------------------
        #  Extract input variables for this function
        #---------------------------------------------

    Q_vec_spp = rowSums (q_mat [,S_remaining_PUs_vec, drop=FALSE])
    d_mat = sweep (d_fixed_part_mat, MARGIN=1, FUN="/",STATS=Q_vec_spp)

                #--------------------------------------------------------------
                #  If Q is 0, then it will cause a divide by zero error
                #  in computing d.
                #  Having a Q value of 0 means that the species no longer
                #  has any occurrences left in the eligible PUs, so
                #  the species has no relevance to the decision about
                #  which PU to throw out next.
                #  In the next step, we will compute the maximum value of
                #  d across all species, so if we give d a value of -Inf,
                #  it will guarantee that it's not selected as the max unless
                #  there are no species left on any of the patches in the
                #  eligible PU set S.  In that case, it wouldn't make any
                #  difference which PU you end up picking since they're all
                #  useless, so it doesn't matter if -Inf is picked as the
                #  max since all species will have a d of -Inf at that point.
                #--------------------------------------------------------------

    indices_of_spp_that_are_0 = which (Q_vec_spp == 0)
    d_mat [indices_of_spp_that_are_0, ] = -Inf

if (forward)  #  not the normal Zonation order
{
    PU_max_loss_vec = apply (d_mat, 2, min)

        #  This is a 1 element vector unless some eligible PUs have
        #  the same max loss.
    chosen_PUs_vec =
        which (PU_max_loss_vec == max (PU_max_loss_vec[S_remaining_PUs_vec]))

} else  # normal Zonation order
{
    PU_max_loss_vec = apply (d_mat, 2, max)

        #  This is a 1 element vector unless some eligible PUs have
        #  the same max loss.
    chosen_PUs_vec =
        which (PU_max_loss_vec == min (PU_max_loss_vec[S_remaining_PUs_vec]))
}

        #  Now we know what are ALL of the PUs in the whole system that
        #  match the min in S, but some of those can be ones that we've
        #  already added to the solution set earlier and this can lead
        #  to the same PU being added to the solution more than once.
        #  So, now we need to intersect the chosen_PUs_vec with S because
        #  ONLY PUs in S are allowed to be selected in this round.

    chosen_PUs_vec = chosen_PUs_vec [chosen_PUs_vec %in% S_remaining_PUs_vec]
    if (length (chosen_PUs_vec) < 1)
        stop_bdpg ("chosen_PUs_vec is empty in choose_next_PU_z")

        #  When there is a tie in min of max loss,
        #  break the tie based on whichever tied PU has the
        #  min of summed loss.
    if (length (chosen_PUs_vec) > 1)
        {
        chosen_PU = break_tie_using_min_summed_loss (chosen_PUs_vec,
                                                     S_remaining_PUs_vec,
                                                     d_mat,
                                                     forward)
        }
    else  chosen_PU = chosen_PUs_vec[1]

    vars_list$chosen_PU = chosen_PU

    return (vars_list)
    }

#===============================================================================

z_using_funcs <- function (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm,
                           forward = FALSE
                           )
    {
    input_vars_list = list (wt_spp_vec = wt_spp_vec,
                            c_PU_vec   = c_PU_vec,
                            bpm        = bpm)

    ranked_solution_PU_IDs_vec =
        greedy_using_funcs (num_spp, num_PUs, input_vars_list,
                            init_for_choosing_PUs_z,
                            choose_next_PU_z,
                            forward)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================


