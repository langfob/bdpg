#===============================================================================

                            #  z_using_inline.R

#===============================================================================

z_using_inline <- function (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm,
                            forward = FALSE  #  Normally true for zonation.
                           )
    {
#--------------------------------------------
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#  This section could be a function, but it might incur a lot of
#  overhead copying large matrices on every iteration.

        #  Compute original_frac_abund_spp_j_on_PU_i, i.e.,
        #  Normalize each species's abundance on each PU by total abundance
        #  for that species, i.e., compute rel_spp_abundance
    q_mat = sweep (bpm, MARGIN=1, FUN="/",STATS=rowSums (bpm))

    qw_spp_weighted_q_mat =
        sweep (q_mat, MARGIN=1, FUN="*",STATS=wt_spp_vec)

    d_fixed_part_mat = sweep (qw_spp_weighted_q_mat,
                              MARGIN=2,
                              FUN="/",STATS=c_PU_vec)
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#--------------------------------------------

    ranked_solution_PU_IDs_vec = rep (0, num_PUs)
    S_remaining_PUs_vec = 1:num_PUs

    for (cur_rank in 1:num_PUs)
        {
        if (cur_rank == num_PUs)    #  Last PU can just be copied into solution.
            {
            chosen_PU = S_remaining_PUs_vec [1]
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

            } else    #  Not the last PU, so need to do some computation
            {
#--------------------------------------------
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

#  This section could be a function, but it might incur a lot of
#  overhead returning and copying large matrices on every iteration.
            Q_vec_spp = rowSums (q_mat [,S_remaining_PUs_vec, drop=FALSE])

            d_mat = sweep (d_fixed_part_mat, MARGIN=1, FUN="/",STATS=Q_vec_spp)

                #---------------------------------------------------------------
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
                #---------------------------------------------------------------

            indices_of_spp_that_are_0 = which (Q_vec_spp == 0)
            d_mat [indices_of_spp_that_are_0, ] = -Inf

if (forward)
{
            PU_max_loss_vec = apply (d_mat, 2, min)

                #  This is a 1 element vector unless some eligible PUs have
                #  the same max loss.
            chosen_PUs_vec =
                which (PU_max_loss_vec ==
                       max (PU_max_loss_vec[S_remaining_PUs_vec]))
} else
{
            PU_max_loss_vec = apply (d_mat, 2, max)

                #  This is a 1 element vector unless some eligible PUs have
                #  the same max loss.
            chosen_PUs_vec =
                which (PU_max_loss_vec ==
                       min (PU_max_loss_vec[S_remaining_PUs_vec]))
}


                #  Now we know what are ALL of the PUs in the whole system that
                #  match the min in S, but some of those can be ones that we've
                #  already added to the solution set earlier and this can lead
                #  to the same PU being added to the solution more than once.
                #  So, now we need to intersect the chosen_PUs_vec with S because
                #  ONLY PUs in S are allowed to be selected in this round.

            chosen_PUs_vec =
                chosen_PUs_vec [chosen_PUs_vec %in% S_remaining_PUs_vec]
            if (length (chosen_PUs_vec) < 1) browser()

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

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#--------------------------------------------

                #  Add current PU to ranked solution vector and
                #  remove it from the set of candidates for next
                #  round.
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

            idx_of_chosen_PU_in_S = which (S_remaining_PUs_vec == chosen_PU)
            S_remaining_PUs_vec = S_remaining_PUs_vec [-idx_of_chosen_PU_in_S]
            }
        }

    if (!forward)    #  Normally true for zonation
        ranked_solution_PU_IDs_vec = rev (ranked_solution_PU_IDs_vec)

    if (length (ranked_solution_PU_IDs_vec) !=
        length (unique (ranked_solution_PU_IDs_vec)))
        stop_bdpg ("ranked_solution_PU_IDs_vec contains duplicate entries")

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

