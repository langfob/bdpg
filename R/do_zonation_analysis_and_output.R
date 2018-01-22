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
                                             delta_mat)
    {
    PU_summed_loss_vec = colSums (delta_mat)

    chosen_PUs_vec =
        which (PU_summed_loss_vec == min (PU_summed_loss_vec [S_remaining_PUs_vec]))
                                                                                if (verbose) {
                                                                                cat ("\n\n  initial chosen_PU = ", chosen_PUs_vec, "\n")
                                                                                }
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

z_using_inline <- function (num_PUs, wt_spp_vec, c_PU_vec, bpm,
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
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                                                                                print (qw_over_c_mat)
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
                                                                                cat ("\n\n===========================================================")
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
# if (cur_rank >= 8) browser()
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

zOld_using_inline <- function (num_PUs, wt_spp_vec, c_PU_vec, bpm,
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
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^                                                                                print (qw_over_c_mat)
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
                                                                                cat ("\n\n===========================================================")
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
# if (cur_rank >= 8) browser()
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

z_using_funcs <- function (num_PUs, wt_spp_vec, c_PU_vec, bpm)
    {
    input_vars_list = list (wt_spp_vec = wt_spp_vec,
                            c_PU_vec   = c_PU_vec,
                            bpm        = bpm)

    ranked_solution_PU_IDs_vec =
        greedy_using_funcs (num_PUs, input_vars_list,
                            reverse_solution_order = TRUE)

    return (ranked_solution_PU_IDs_vec)
    }

#===============================================================================

greedy_using_funcs <- function (num_PUs,
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

test_z <- function (seed = 456, num_spp = 4, num_PUs = 3)
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

cat ("\n------------------------------------------------------------------------")
                                            timings_using_inline = system.time ({
    zui = z_using_inline (num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_inline = \n")
    print (timings_using_inline)
    print (zui)

cat ("\n------------------------------------------------------------------------")
                                            timings_using_funcs = system.time ({
    zuf = z_using_funcs (num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_funcs = \n")
    print (timings_using_funcs)
    print (zuf)
cat ("\n------------------------------------------------------------------------")

    cat ("\nzui == zuf: ", all.equal (zui, zuf), "\n")
    }

#===============================================================================

verbose = FALSE
# test_z (seed = 456, num_spp = 4, num_PUs = 3)

test_z (seed = 456, num_spp = 400, num_PUs = 300)
#test_z (seed = 1223, num_spp = 6, num_PUs = 5)



