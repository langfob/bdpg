#===============================================================================

                            #  z_using_funcs.R

#===============================================================================

init_for_choosing_PUs <- function (input_vars_list)
    {
        #  Extract input variables for this function
    bpm = input_vars_list$bpm
    c_PU_vec = input_vars_list$c_PU_vec
    wt_spp_vec = input_vars_list$wt_spp_vec

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
    d_fixed_part_mat =
        sweep (qw_spp_weighted_q_mat, MARGIN=2, FUN="/",STATS=c_PU_vec)
                                                                                if (verbose) {
                                                                                cat ("\n\n  d_fixed_part_mat = \n")
                                                                                print (d_fixed_part_mat)
                                                                                }
    return (list (q_mat         = q_mat,
                  d_fixed_part_mat = d_fixed_part_mat))
    }

#===============================================================================

choose_next_PU <- function (S_remaining_PUs_vec, vars_list)
    {
        #  Extract input variables for this function
    q_mat = vars_list$q_mat
    d_fixed_part_mat = vars_list$d_fixed_part_mat

    Q_vec = rowSums (q_mat [,S_remaining_PUs_vec, drop=FALSE])
                                                                                if (verbose) {
                                                                                cat ("\n\n  Q_vec = ", Q_vec, "\n")
                                                                                }
    d_mat = sweep (d_fixed_part_mat, MARGIN=1, FUN="/",STATS=Q_vec)
                                                                                if (verbose) {
                                                                                cat ("\n\n  d_mat = \n")
                                                                                print (d_mat)
                                                                                }
    PU_max_loss_vec = apply (d_mat, 2, max)
                                                                                if (verbose) {
                                                                                cat ("\n\n  PU_max_loss_vec = ", PU_max_loss_vec, "\n")
                                                                                }
browser()
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

    # bpm [,chosen_PU] = 0
    #                                                                             cat ("\n\n  bpm = \n")
    #                                                                             print (bpm)
    vars_list$chosen_PU = chosen_PU
#    vars_list$bpm    = bpm

    return (vars_list)
    }

#===============================================================================

greedy_using_funcs <- function (num_spp,
                                num_PUs,
                                input_vars_list,
                                reverse_solution_order)
    {
    vars_list = init_for_choosing_PUs (input_vars_list)       #  <<<<<----------

    ranked_solution_PU_IDs_vec    = rep (0, num_PUs)
    S_remaining_PUs_vec           = 1:num_PUs
    vars_list$S_remaining_PUs_vec = S_remaining_PUs_vec
    bpm = input_vars_list$bpm

    for (cur_rank in 1:num_PUs)
        {
        if (cur_rank == num_PUs)    #  Last PU can just be copied into solution.
            {
            chosen_PU = S_remaining_PUs_vec [1]
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

            } else    #  Not the last PU, so need to do some computation
            {
            vars_list = choose_next_PU (S_remaining_PUs_vec, vars_list)            #  <<<<<----------
            chosen_PU = vars_list$chosen_PU

                #  Add current PU to ranked solution vector and
                #  remove it from the set of candidates for next
                #  round.
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU
            bpm [,chosen_PU] = 0

#            S_remaining_PUs_vec = S_remaining_PUs_vec [-chosen_PU]
idx_of_chosen_PU_in_S = which (S_remaining_PUs_vec == chosen_PU)
S_remaining_PUs_vec = S_remaining_PUs_vec [-idx_of_chosen_PU_in_S]
            }
        }

    if (reverse_solution_order)
        ranked_solution_PU_IDs_vec = rev (ranked_solution_PU_IDs_vec)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

z_using_funcs <- function (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
    {
    input_vars_list = list (wt_spp_vec = wt_spp_vec,
                            c_PU_vec   = c_PU_vec,
                            bpm        = bpm)

    ranked_solution_PU_IDs_vec =
        greedy_using_funcs (num_spp, num_PUs, input_vars_list,
                            reverse_solution_order = TRUE)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

