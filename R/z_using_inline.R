#===============================================================================

                            #  z_using_inline.R

#===============================================================================

z_using_inline <- function (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm,
                           reverse_solution_order = TRUE  #  Always true for zonation.
                           )
    {
#--------------------------------------------
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
#  This section could be a function, but it would incur a lot of
#  overhead copying large matrices on every iteration.

        #  Compute original_frac_abund_spp_j_on_PU_i, i.e.,
        #  Normalize each species's abundance on each PU by total abundance
        #  for that species, i.e., compute rel_spp_abundance
    q_mat = sweep (bpm, MARGIN=1, FUN="/",STATS=rowSums (bpm))
                                                                                if (verbose) {
                                                                                cat ("\n\n  q_mat = ", q_mat, "\n")
                                                                                print (q_mat)
                                                                                }
    qw_spp_weighted_q_mat =
        sweep (q_mat, MARGIN=1, FUN="*",STATS=wt_spp_vec)
                                                                                if (verbose) {
                                                                                cat ("\n\n  qw_spp_weighted_q_mat = ", qw_spp_weighted_q_mat, "\n")
                                                                                print (qw_spp_weighted_q_mat)
                                                                                }
    d_fixed_part_mat = sweep (qw_spp_weighted_q_mat, MARGIN=2, FUN="/",STATS=c_PU_vec)
                                                                                if (verbose) {
                                                                                cat ("\n\n  d_fixed_part_mat = \n")
                                                                                print (d_fixed_part_mat)
                                                                                }
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
#--------------------------------------------

    ranked_solution_PU_IDs_vec = rep (0, num_PUs)
                                                                                if (verbose) {
                                                                                cat ("\n  ranked_solution_PU_IDs_vec = ", ranked_solution_PU_IDs_vec)
                                                                                }
    S_remaining_PUs_vec = 1:num_PUs
                                                                                if (verbose) {
                                                                                cat ("\n  S_remaining_PUs_vec = ", S_remaining_PUs_vec)
                                                                                }
    for (cur_rank in 1:num_PUs)
        {
                                                                                if (verbose) {
                                                                                cat ("\n\n========================")
                                                                                cat ("\n  cur_rank = ", cur_rank)
                                                                                }
        if (cur_rank == num_PUs)    #  Last PU can just be copied into solution.
            {
            chosen_PU = S_remaining_PUs_vec [1]
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

            } else    #  Not the last PU, so need to do some computation
            {
#--------------------------------------------
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

#  This section could be a function, but it would incur a lot of
#  overhead returning and copying large matrices on every iteration.
                                                                                if (verbose) {
                                                                                cat ("\n\n  bpm = \n")
                                                                                print (bpm)
                                                                                }
            Q_vec_spp = rowSums (q_mat [,S_remaining_PUs_vec, drop=FALSE])
                                                                                if (verbose) {
                                                                                cat ("\n\n  Q_vec_spp = ", Q_vec_spp, "\n")
                                                                                }
            d_mat = sweep (d_fixed_part_mat, MARGIN=1, FUN="/",STATS=Q_vec_spp)
d_mat [is.infinite (d_mat)] = 0  #  trying to fix NA problem, but this didn't work either...
                                                                                if (verbose) {
                                                                                cat ("\n\n  d_mat = \n")
                                                                                print (d_mat)
                                                                                }
            PU_max_loss_vec = apply (d_mat, 2, max)
                                                                                if (verbose) {
                                                                                cat ("\n\n  PU_max_loss_vec = ", PU_max_loss_vec, "\n")
                                                                                }
                #  This is a 1 element vector unless some eligible PUs have
                #  the same max loss.
            chosen_PUs_vec =
                which (PU_max_loss_vec == min (PU_max_loss_vec[S_remaining_PUs_vec]))
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

                #  When there is a tie in min of max loss,
                #  break the tie based on whichever tied PU has the
                #  min of summed loss.
            if (length (chosen_PUs_vec) > 1)
                {
                chosen_PU = break_tie_using_min_summed_loss (chosen_PUs_vec,
                                                             S_remaining_PUs_vec,
                                                             d_mat)
                }
            else  chosen_PU = chosen_PUs_vec[1]
                                                                                if (verbose) {
                                                                                cat ("\n\n  possibly sampled chosen_PU = ", chosen_PU, "\n")
                                                                                }
 # print(chosen_PU)
 # if (cur_rank >= 4) browser()
             bpm [,chosen_PU] = 0
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                                                                                print (d_fixed_part_mat)
#--------------------------------------------

                #  Add current PU to ranked solution vector and
                #  remove it from the set of candidates for next
                #  round.
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

#            S_remaining_PUs_vec = S_remaining_PUs_vec [-chosen_PU]
idx_of_chosen_PU_in_S = which (S_remaining_PUs_vec == chosen_PU)
S_remaining_PUs_vec = S_remaining_PUs_vec [-idx_of_chosen_PU_in_S]
            }
                                                                                if (verbose) {
                                                                                cat ("\nAt cur_rank = ", cur_rank, "\nranked_solution_PU_IDs_vec = \n")
                                                                                print (ranked_solution_PU_IDs_vec)
                                                                                }
        }

    if (reverse_solution_order)    #  Always true for zonation
        ranked_solution_PU_IDs_vec = rev (ranked_solution_PU_IDs_vec)
                                                                                if (verbose) {
                                                                                cat ("\nFinal ranked_solution_PU_IDs_vec = \n")
                                                                                print (ranked_solution_PU_IDs_vec)
                                                                                }

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

