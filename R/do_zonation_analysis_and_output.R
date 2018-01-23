#===============================================================================

                    #  do_zonation_analysis_and_output.R

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
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
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
                                                     ##delta_mat
                                                     #delta_vec
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
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
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
    qw_over_c_mat = sweep (qw_spp_weighted_q_mat, MARGIN=2, FUN="/",STATS=c_PU_vec)
                                                                                if (verbose) {
                                                                                cat ("\n\n  qw_over_c_mat = \n")
                                                                                print (qw_over_c_mat)
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
            Q_vec = rowSums (q_mat [,S_remaining_PUs_vec, drop=FALSE])
                                                                                if (verbose) {
                                                                                cat ("\n\n  Q_vec = ", Q_vec, "\n")
                                                                                }
            delta_mat = sweep (qw_over_c_mat, MARGIN=1, FUN="/",STATS=Q_vec)
delta_mat [is.infinite (delta_mat)] = 0  #  trying to fix NA problem, but this didn't work either...
                                                                                if (verbose) {
                                                                                cat ("\n\n  delta_mat = \n")
                                                                                print (delta_mat)
                                                                                }
            PU_max_loss_vec = apply (delta_mat, 2, max)
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
                                                             delta_mat)
                }
            else  chosen_PU = chosen_PUs_vec[1]
                                                                                if (verbose) {
                                                                                cat ("\n\n  possibly sampled chosen_PU = ", chosen_PU, "\n")
                                                                                }
 # print(chosen_PU)
 # if (cur_rank >= 4) browser()
             bpm [,chosen_PU] = 0
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                                                                                print (qw_over_c_mat)
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
#===============================================================================
#===============================================================================
#===============================================================================
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
    qw_over_c_mat =
        sweep (qw_spp_weighted_q_mat, MARGIN=2, FUN="/",STATS=c_PU_vec)
                                                                                if (verbose) {
                                                                                cat ("\n\n  qw_over_c_mat = \n")
                                                                                print (qw_over_c_mat)
                                                                                }
    return (list (q_mat         = q_mat,
                  qw_over_c_mat = qw_over_c_mat))
    }

#===============================================================================

choose_next_PU <- function (S_remaining_PUs_vec, vars_list)
    {
        #  Extract input variables for this function
    q_mat = vars_list$q_mat
    qw_over_c_mat = vars_list$qw_over_c_mat

    Q_vec = rowSums (q_mat [,S_remaining_PUs_vec, drop=FALSE])
                                                                                if (verbose) {
                                                                                cat ("\n\n  Q_vec = ", Q_vec, "\n")
                                                                                }
    delta_mat = sweep (qw_over_c_mat, MARGIN=1, FUN="/",STATS=Q_vec)
                                                                                if (verbose) {
                                                                                cat ("\n\n  delta_mat = \n")
                                                                                print (delta_mat)
                                                                                }
    PU_max_loss_vec = apply (delta_mat, 2, max)
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
                                                     delta_mat)
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

test_z <- function (zmeths, seed = 456, num_spp = 4, num_PUs = 3)
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

    wt_spp_vec = rep (1, num_spp)    #c(2,3,4,5)    #rep (2, num_spp)    #  weight of species j
                                                                                if (verbose) {
                                                                                cat ("\n  wt_spp_vec = ", wt_spp_vec)
                                                                                }

    c_PU_vec = rep (1, num_PUs)      #c(10,20,30)    #rep (1, num_PUs)  #  cost of PU i
                                                                                if (verbose) {
                                                                                cat ("\n  c_PU_vec = ", c_PU_vec)
                                                                                }

zui = 1
if ("inline" %in% zmeths)
{
set.seed (seed + 12345)
cat ("\n------------------------------------------------------------------------\n")
                                            timings_using_inline = system.time ({
    zui = z_using_inline (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_inline = \n")
    print (timings_using_inline)
    print (zui)
}

zuf = 2
if ("funcs" %in% zmeths)
{
set.seed (seed + 12345)
cat ("\n------------------------------------------------------------------------\n")
                                            timings_using_funcs = system.time ({
    zuf = z_using_funcs (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_funcs = \n")
    print (timings_using_funcs)
    print (zuf)
}
browser()
zufor = 3
if ("for" %in% zmeths)
{
set.seed (seed + 12345)
cat ("\n------------------------------------------------------------------------")
                                            timings_using_for = system.time ({
    zufor = z_using_for (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_for = \n")
    print (timings_using_for)
    print (zufor)
}

cat ("\n------------------------------------------------------------------------")
    cat ("\nzui == zuf: ", all.equal (zui, zuf))
    cat ("\nzui == zufor: ", all.equal (zui, zufor))
    cat ("\nzuf == zufor: ", all.equal (zuf, zufor), "\n")
cat ("\n------------------------------------------------------------------------")

return (list (zui=zui, zuf=zuf, zufor=zufor))
    }

#===============================================================================

options(warn=2)
library (bdpg)

verbose = FALSE

zmeths = "funcs"

zmeths = "for"

#zu =test_z (zmeths, seed = 456, num_spp = 4, num_PUs = 3)
zu =test_z (zmeths, seed = 456, num_spp = 20, num_PUs = 15)
#zu =test_z (zmeths, seed = 456, num_spp = 40, num_PUs = 30)
#zu =test_z (zmeths, seed = 456, num_spp = 400, num_PUs = 300)

#zu =test_z (zmeths, seed = 1223, num_spp = 6, num_PUs = 5)

cat ("\n")
print (zu)




