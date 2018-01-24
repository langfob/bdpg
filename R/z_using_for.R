#===============================================================================

                            #  z_using_for.R

#===============================================================================

choose_next_PU_using_for <- function (num_spp,
                                      num_PUs,
                                      S_remaining_PUs_vec,
                                      q_mat,
                                      #c_vec_PU,
                                      #w_vec_spp,
                                      d_fixed_part_mat,


cur_rank




                                      )
    {

cat ("\nIn choose...(): cur_rank = ", cur_rank)

    #initial_value = NA
    initial_value = -1000

    delta_vec = rep (initial_value, num_PUs)
    Q_vec_spp = rep (initial_value, num_spp)
    d_mat     = matrix (initial_value, nrow = num_spp, ncol = num_PUs)

        #  Compute delta
    for (i_PU in 1:num_PUs)
        {
        for (j_spp in 1:num_spp)
            {
            Q_vec_spp [j_spp] = sum (q_mat [j_spp, S_remaining_PUs_vec])

                #---------------------------------------------------------------
                #  If Q is 0, then it will cause a divide by zero error
                #  in computing d.
                #  Having a Q value of 0 means that the species no longer
                #  has any occurrences left in the eligible PUs, so
                #  the species has no relevance to the decision about
                #  which PU to throw out next.
                #  In the next step, we will compute the maximum value of
                #  d across all species, giving d a value of -Inf will
                #  guarantee that it's not selected as the max unless
                #  there are no species left on any of the patches in the
                #  eligible PU set S.  In that case, it wouldn't make any
                #  difference which PU you end up picking since they're all
                #  useless, so it doesn't matter if -Inf is picked as the
                #  max since all species will have a d of -Inf at that point.
                #---------------------------------------------------------------

            if (Q_vec_spp [j_spp] == 0)
                {
                d_mat [j_spp, i_PU] = -Inf

                } else
                {
                d_mat [j_spp, i_PU] =
                            d_fixed_part_mat [j_spp, i_PU] / Q_vec_spp [j_spp]

                            # (q_mat [j_spp, i_PU] * w_vec_spp [j_spp]) /
                            # (Q_vec_spp [j_spp] * c_vec_PU [i_PU])
                }
            }  #  end for - j_spp

        delta_vec [i_PU] = max (d_mat [, i_PU])  #  largest loss VALUE across all spp on this PU

        }  #  end for - i_PU

    PU_max_loss_vec = delta_vec
#if (cur_rank > 1) browser()
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
                                                     d_mat
                                                     )
        }
    else  chosen_PU = chosen_PUs_vec[1]
                                                                                if (verbose) {
                                                                                cat ("\n\n  possibly sampled chosen_PU = ", chosen_PU, "\n")
                                                                                }

    return (chosen_PU)
    }

#===============================================================================

z_using_for <- function (num_spp,
                         num_PUs,
                         w_vec_spp,
                         c_vec_PU,
                         bpm,
                         reverse_solution_order = TRUE  #  Always true for zonation.
                         )
    {
    num_spp = vn (num_spp, range_lo = 1)
    num_PUs = vn (num_PUs, range_lo = 1)

    if (length (which (w_vec_spp == 0)) > 0)
        stop ("Weight vector has at least one 0 value.")
    if (length (which (c_vec_PU == 0)) > 0)
        stop ("Cost vector has at least one 0 value.")

    reverse_solution_order = vb (reverse_solution_order)

        #--------------------------------------------------------------------
        #  Compute original_frac_abund_spp_j_on_PU_i, i.e.,
        #  Normalize each species's abundance on each PU by total abundance
        #  for that species, i.e., compute rel_spp_abundance
        #--------------------------------------------------------------------

    tot_abund_vec_spp = rowSums (bpm)
    q_mat = matrix (0, nrow = num_spp, ncol = num_PUs)
    for (j_spp in 1:num_spp)
        {
        for (i_PU in 1:num_PUs)
            {
            q_mat [j_spp, i_PU] = bpm [j_spp, i_PU] / tot_abund_vec_spp [j_spp]
            }
        }

        #--------------------------------------------------------
        #  Compute the part of the d matrix that never changes,
        #  i.e., everything but the Q value.
        #--------------------------------------------------------

    d_fixed_part_mat = matrix (NA, nrow = num_spp, ncol = num_PUs)
    for (i_PU in 1:num_PUs)
        {
        for (j_spp in 1:num_spp)
            {
            d_fixed_part_mat [j_spp, i_PU] =
                    (q_mat [j_spp, i_PU] * w_vec_spp [j_spp]) / c_vec_PU [i_PU]

            }  #  end for - j_spp
        }  #  end for - i_PU


        #-----------------------------------------------
        #  Choose PUs in reverse order of their value.
        #-----------------------------------------------

    S_remaining_PUs_vec = 1:num_PUs
    ranked_solution_PU_IDs_vec = rep (0, num_PUs)

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
            chosen_PU = choose_next_PU_using_for (num_spp,
                                                  num_PUs,
                                                  S_remaining_PUs_vec,
                                                  q_mat,
                                                  #c_vec_PU,
                                                  #w_vec_spp,
                                                  d_fixed_part_mat,

cur_rank
                                                  )

###            bpm [,chosen_PU] = 0

                #  Add current PU to ranked solution vector and
                #  remove it from the set of candidates for next
                #  round.
            ranked_solution_PU_IDs_vec [cur_rank] = chosen_PU

    #            S_remaining_PUs_vec = S_remaining_PUs_vec [-chosen_PU]
            idx_of_chosen_PU_in_S = which (S_remaining_PUs_vec == chosen_PU)
            S_remaining_PUs_vec = S_remaining_PUs_vec [-idx_of_chosen_PU_in_S]


if (verbose)
{
cat ("\n\nAfter chosen assignment at rank ", cur_rank)
if (cur_rank > 1)
    {
    cat ("\nspp reps in cur solution:\n")
    cur_spp_reps = rowSums (bpm [, ranked_solution_PU_IDs_vec > 0])
    print (cur_spp_reps)
    }
cat ("\n\ncurrent ranked_solution_PU_IDs_vec = \n")
print (ranked_solution_PU_IDs_vec)
#browser()
}


            }  #  end else - not working on last PU
        }  #  end for - cur_rank

    if (reverse_solution_order)
        ranked_solution_PU_IDs_vec = rev (ranked_solution_PU_IDs_vec)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

