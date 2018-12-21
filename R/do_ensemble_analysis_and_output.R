#===============================================================================
#
#                       do_ensemble_analysis_and_output.R
#
#  Run code to run ensemble and dump results to file.
#
#===============================================================================
#===============================================================================
#===============================================================================

collect_all_ens_cand_sols <- function (ens_RSrun_dirs,
                                       ens_prob_dirs,
                                       num_PUs
                                       #,
                                       #COR_bd_prob    #  unnecessary?
                                       )
    {
    num_ensemble_elements      = length (ens_prob_dirs)
    all_ens_marxan_BEST_sols   = vector (mode = "list", length = num_ensemble_elements)
    all_ens_marxan_SUMMED_sols = vector (mode = "list", length = num_ensemble_elements)

    for (cur_idx in 1:num_ensemble_elements)
        {
        cur_RSrun_dir = ens_RSrun_dirs [cur_idx]
#         fileName = paste0 ("saved.", basename (cur_RSrun_dir), ".rds")
#         APP_marxan_run = readRDS (file.path (cur_RSrun_dir, fileName))

#         cur_prob_dir = ens_prob_dirs [cur_idx]
#         fileName = paste0 ("saved.", basename (cur_prob_dir), ".rds")
#         APP_bd_prob = readRDS (file.path (cur_prob_dir, fileName))
# browser()
#         rs_best_and_summed_solution_PU_IDs =
#                 get_marxan_best_and_summed_solution_PU_IDs (APP_marxan_run,
#                                                             ens_starting_dir,
#
#                                                             #COR_bd_prob,    #  unnecessary?  just reuse APP_bd_prob?
#                                                             APP_bd_prob,
#                                                             APP_bd_prob)
# browser()

                #-------------------------
                #  Best OVERALL solution
                #-------------------------

        Marxan_SA_best_solution_file_name =
                "Marxan_SA_best_solution_PU_IDs.csv"
        Marxan_SA_best_solution_file_path =
                file.path (cur_RSrun_dir, Marxan_SA_best_solution_file_name)
        rs_best_solution_PU_IDs =
                scan (Marxan_SA_best_solution_file_path, sep=",")

        all_ens_marxan_BEST_sols [[cur_idx]] = rs_best_solution_PU_IDs
            # rs_best_and_summed_solution_PU_IDs$rs_best_solution_PU_IDs
            #        rs_best_and_summed_solution_PU_IDs$marxan_best_solution_PU_IDs

                #-------------------
                #  SUMMED solution
                #-------------------

        Marxan_SA_SS_summed_solution_file_name =
                "Marxan_SA_SS_summed_solution_PU_IDs.csv"
        Marxan_SA_SS_summed_solution_file_path =
                file.path (cur_RSrun_dir, Marxan_SA_SS_summed_solution_file_name)
        marxan_summed_solution_PU_IDs =
                scan (Marxan_SA_SS_summed_solution_file_path, sep=",")

        all_ens_marxan_SUMMED_sols [[cur_idx]] = marxan_summed_solution_PU_IDs
            # rs_best_and_summed_solution_PU_IDs$marxan_best_summed_solution_PU_IDs


cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
cat ("At bottom of loop COLLECTING CANDIDATE SOLUTIONS.")
cat ("cur_idx = ", cur_idx, "\n")
cat ("\n\nall_ens_marxan_BEST_sols = \n")
print (all_ens_marxan_BEST_sols)
cat ("\n\nall_ens_marxan_SUMMED_sols = \n")
print (all_ens_marxan_SUMMED_sols)
cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
#browser()
        }

cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
cat ("About to return from COLLECTING CANDIDATE SOLUTIONS.")
cat ("cur_idx = ", cur_idx, "\n")
cat ("\n\nall_ens_marxan_BEST_sols = \n")
print (all_ens_marxan_BEST_sols)
cat ("\n\nall_ens_marxan_SUMMED_sols = \n")
print (all_ens_marxan_SUMMED_sols)
cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
#browser()

    return (list (best_sols   = all_ens_marxan_BEST_sols,
                  summed_sols = all_ens_marxan_SUMMED_sols))

    }  #  end function - collect_all_ens_cand_sols()

#===============================================================================

get_pseudo_opt_costs_for_all_ens_probs <- function (all_ens_best_cand_sols, #PSEUDO-OPT
                                                    num_probs_in_ensemble,
                                                    PU_costs_vec)
    {
    pseudo_opt_costs = vector (mode="numeric", length=num_probs_in_ensemble)

    for (cur_idx in 1:num_probs_in_ensemble)
        {
        pseudo_opt_costs [cur_idx] =
            compute_solution_cost (all_ens_best_cand_sols [cur_idx],
                                   PU_costs_vec)
        }

    return (pseudo_opt_costs)
    }

#===============================================================================

score_ONE_cand_sol_against_ONE_prob <- function (cur_bpm,
                                                 cur_cand_sol,
                                                 spp_rep_targets,
                                                 num_spp,
                                                 pseudo_opt_cost,
                                                 PU_costs_vec)
    {
        #-------------------------
        #  Compute spp rep error
        #-------------------------

    spp_rep_results_list = compute_and_verify_rep_scores_wrt (cur_bpm,
                                                              cur_cand_sol,
                                                              spp_rep_targets,
                                                              num_spp)
        #----------------------
        #  Compute cost error
        #----------------------

    cost_results_list =
        compute_RS_solution_cost_scores_wrt_COR_costs_vec (cur_cand_sol,
                                                           pseudo_opt_cost,
                                                           PU_costs_vec)

        #--------------------------------------------------------
        #  Compute euclidean combination of rep and cost errors
        #--------------------------------------------------------

    euc_out_err_frac =
        compute_euc_out_err_frac ("COR",  #  Doesn't matter COR or APP here (or anywhere, really) ???
                                  cost_results_list$rs_solution_cost_err_frac,
                                  spp_rep_results_list$frac_spp_covered)

        #---------------------

    return (list (spp_rep_shortfall      = spp_rep_results_list$spp_rep_shortfall,
                  solution_cost_err_frac = cost_results_list$rs_solution_cost_err_frac,
                  euc_out_err_frac       = euc_out_err_frac$rsr_COR_euc_out_err_frac))
    }

#===============================================================================

score_ALL_cand_sols_against_ONE_prob <- function (all_cand_sols,         #  list of vecs of PU IDs
                                                  pseudo_opt_cost,       #  marxan/gurobi computed cost on apparent problem
                                                  num_probs_in_ensemble,
                                                  num_spp,
                                                  cur_bpm,
                                                  PU_costs_vec,
                                                  spp_rep_targets)
    {
    # all_cand_spp_rep_err_scores = all_cand_scores_list$all_cand_spp_rep_err_scores
    # all_cand_cost_err_scores    = all_cand_scores_list$all_cand_cost_err_scores
    # all_cand_euc_err_scores     = all_cand_scores_list$all_cand_euc_err_scores

    all_cand_spp_rep_err_scores = vector (mode="numeric", length=num_probs_in_ensemble)
    all_cand_cost_err_scores    = vector (mode="numeric", length=num_probs_in_ensemble)
    all_cand_euc_err_scores     = vector (mode="numeric", length=num_probs_in_ensemble)

    for (cur_idx in 1:num_probs_in_ensemble)
        {
        cur_cand_sol = all_cand_sols [[cur_idx]]

        scores_list = score_ONE_cand_sol_against_ONE_prob (cur_bpm,
                                                           cur_cand_sol,
                                                           spp_rep_targets,
                                                           num_spp,
                                                           pseudo_opt_cost,
                                                           PU_costs_vec)

        all_cand_spp_rep_err_scores [cur_idx] = scores_list$spp_rep_shortfall
        all_cand_cost_err_scores [cur_idx]    = scores_list$solution_cost_err_frac
        all_cand_euc_err_scores [cur_idx]     = scores_list$euc_out_err_frac
        }

      return (list (all_cand_spp_rep_err_scores = all_cand_spp_rep_err_scores,
                    all_cand_cost_err_scores    = all_cand_spp_rep_err_scores,
                    all_cand_euc_err_scores     = all_cand_euc_err_scores))
    }

#===============================================================================

score_ALL_cand_sols_against_ALL_probs <- function (ens_prob_dirs,
                                                   all_ens_cand_sols,
                                                   pseudo_opt_costs,
                                                   num_probs_in_ensemble,
                                                   num_spp,
                                                   PU_costs_vec,
                                                   spp_rep_targets
                                                   )
    {
    all_cand_spp_rep_err_scores =
        matrix (NA, nrow=num_probs_in_ensemble, ncol=num_probs_in_ensemble)
    all_cand_cost_err_scores =
        matrix (NA, nrow=num_probs_in_ensemble, ncol=num_probs_in_ensemble)
    all_cand_euc_err_scores =
        matrix (NA, nrow=num_probs_in_ensemble, ncol=num_probs_in_ensemble)

    for (cur_prob_idx in 1:num_probs_in_ensemble)
        {
        cur_prob_dir = ens_prob_dirs [cur_prob_idx]
        filename = paste0 ("saved.",
                           basename (cur_prob_dir),
                           ".rds")
        cur_prob = readRDS (file.path (cur_prob_dir, filename))
        cur_bpm = cur_prob@bpm

        results_list = score_ALL_cand_sols_against_ONE_prob (
                                                        all_ens_cand_sols,
                                                        pseudo_opt_costs [cur_prob_idx],       #  marxan/gurobi computed cost on apparent problem
                                                        num_spp,
                                                        cur_bpm,
                                                        PU_costs_vec,
                                                        spp_rep_targets)

        all_cand_spp_rep_err_scores [, cur_prob_idx] =
                results_list$all_cand_spp_rep_err_scores    #  where this is a vector of length N
        all_cand_cost_err_scores [, cur_prob_idx]    =
                results_list$all_cand_cost_err_scores       #  where this is a vector of length N
        all_cand_euc_err_scores [, cur_prob_idx]     =
                results_list$all_cand_euc_err_scores        #  where this is a vector of length N
        }

    return (list (all_cand_spp_rep_err_scores = all_cand_spp_rep_err_scores,
                  all_cand_cost_err_scores    = all_cand_spp_rep_err_scores,
                  all_cand_euc_err_scores     = all_cand_euc_err_scores))
    }

#===============================================================================

compute_stats_for_all_sols_across_all_ens_probs <-
                                         function (all_ens_cand_sol_scores,
                                                   num_probs_in_ensemble)
    {
    all_cand_score_stats =
        matrix (NA, nrow=num_probs_in_ensemble, ncol=6)

    for (cur_cand_idx in 1:length (num_probs_in_ensemble))
        {
            #  Get vector of 1 candidate solution's scores on all ens probs.
        cur_cand_scores = all_ens_cand_sol_scores [cur_cand_idx]

        cur_cand_fivenum_stats = fivenum (cur_cand_scores)
        cur_cand_mean          = mean (cur_cand_scores)

        all_cand_score_stats [cur_cand_idx, 1:5] = cur_cand_fivenum_stats
        all_cand_score_stats [cur_cand_idx, 6]   = cur_cand_mean
        }

    all_cand_score_stats = as_tibble (all_cand_score_stats)
    names (all_cand_score_stats) = c ("min", "Q1", "median", "Q3", "max", "mean")

    return (all_cand_score_stats)
    }

#===============================================================================

ensemble <- function (APP_bd_prob,
                      parameters,
                      starting_dir,
                      ens_elems_starting_dir,
                      RS_specific_params)
    {
    cat ("\nInside ensemble function.\n")

        #  Modify parameters to block graph calculations and to set
        #  set reserve selectors to block infinite recursion (i.e, don't
        #  call ensemble again) and to only call reserve selectors that
        #  are meant to be used to build the ensemble.
        #
        #  NOTE:  2018 12 16 - BTL
        #         THERE'S A LIKELY FUTURE PROBLEM HERE BECAUSE THIS LIST
        #         OF RESERVE SELECTORS TO BE TURNED OFF REQUIRES YOU TO KNOW
        #         AND KEEP CURRENT, ALL POSSIBLE RESERVE SELECTORS, WHICH
        #         MEANS THAT ADDING A NEW ONE ELSEWHERE WOULD MEAN YOU'D HAVE
        #         TO BE SURE TO UPDATE THIS LIST.  NOT A GOOD IDEA.
        #         WILL LEAVE IT FOR THE MOMENT WHILE I GET THIS WORKING, BUT
        #         DEFINITELY NEED TO FIGURE OUT A MORE ROBUST STRATEGY, EVEN
        #         IF IT'S JUST TO PASS THE LIST OF RESERVE SELECTORS TO TURN
        #         OFF IN TO THIS ROUTINE.

    parameters$compute_network_metrics          = FALSE

            #  Need to make sure that marxan (or in the future, whatever RS is
            #  used by the ensemble) is turned on.
            #  If it's not turned on, then even though it will be run on all
            #  the ensemble subproblems, it won't be run on the APP problem
            #  they'are all derived from.  That would be an issue because
            #  that problem is also included in the ensemble and therefore,
            #  needs to have the same reserve selector run on it that is run
            #  on all the other ensemble subproblems.

    run_marxan = vb (parameters$run_marxan, def_on_empty=TRUE)
    if (! run_marxan)
        stop_bdpg ("run_marxan must be TRUE when running ensemble.")

    parameters$do_ensemble                      = FALSE  #  To avoid infinite recursion

    parameters$do_gurobi                        = FALSE
    parameters$do_simple_richness_forward       = FALSE
    parameters$do_simple_richness_backward      = FALSE
    parameters$do_unprotected_richness_forward  = FALSE
    parameters$do_unprotected_richness_backward = FALSE
    parameters$do_zonation_like_forward         = FALSE
    parameters$do_zonation_like_backward        = FALSE

#  2018 12 16 - BTL - For quick testing at the moment...
FP_err_amt = 0.001
FN_err_amt = 0.01

#  2018 12 16 - BTL - No cost errors for the moment...
gen_combined_cost_and_FP_FN_errors = FALSE
cost_err_amt = 0



    num_PUs = APP_bd_prob@num_PUs
    PU_costs_vec = APP_bd_prob@PU_costs_vec
    num_spp = APP_bd_prob@num_spp
    spp_rep_targets = APP_bd_prob@spp_rep_targets




    num_probs_in_ensemble = RS_specific_params$num_probs_in_ensemble
    if (num_probs_in_ensemble < 2)
        stop_bdpg (paste0 ("num_probs_in_ensemble = '", num_probs_in_ensemble,
                           "' must be >= 2."))

        #----------------------------------------------------------------------
        #  Make the original APP problem that the ensemble is trying to solve
        #  be the first element of the ensemble.
        #----------------------------------------------------------------------

    prob_dirs = vector (mode="character", length=num_probs_in_ensemble)
    prob_dirs [1] = get_RSprob_path_topdir (APP_bd_prob, starting_dir)

    marxan_rsrun_dirs = vector (mode="character", length=num_probs_in_ensemble)
    marxan_rsrun_dirs [1] = RS_specific_params$marxan_run_dir

    for (cur_prob_idx in 2:num_probs_in_ensemble)
        {
                #-------------------------------------------
                #  Variant 3:  FP & FN, counts NOT matched
                #-------------------------------------------

                        # (list (bd_prob_topdir = APP_bd_prob_topdir,
                        #       RS_topDirs_list = RS_topDirs_list))
        prob_and_rsruns_topdirs_list =
            gen_1_app_variant (APP_bd_prob,    #base_bd_prob,
                               parameters,
                               ens_elems_starting_dir,    #starting_dir,

                               gen_cost_errors = gen_combined_cost_and_FP_FN_errors,
                               cost_error_frac_bound = cost_err_amt,    #err_amt,

                               gen_FP_FN_errors = TRUE,

                               spp_occ_FP_const_rate = FP_err_amt,    #err_amt,
                               spp_occ_FN_const_rate = FN_err_amt,    #err_amt,

                               match_error_counts = FALSE)

        prob_dirs [cur_prob_idx] = prob_and_rsruns_topdirs_list$bd_prob_topdir
        marxan_rsrun_dirs [cur_prob_idx] = prob_and_rsruns_topdirs_list$RS_topDirs_list$marxan

        cat ("\n=============================================\n")
        cat ("After gen_1_app_variant() for cur_prob_idx = ",
             cur_prob_idx, ", prob_and_rsruns_topdirs_list = \n")
        print (prob_and_rsruns_topdirs_list)
        cat ("=============================================\n")

        }  #  end for - all problems in ensemble

        #--------------------------------------------
        #  Echo ensemble problem dirs and run dirs.
        #--------------------------------------------

    cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
    cat ("Finished generating ensemble.")
    cat ("prob_dirs = \n")
    print (prob_dirs)
    cat ("\n\nmarxan_rsrun_dirs = \n")
    print (marxan_rsrun_dirs)
    cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")

        #---------------------------------------------
        #  Collect all ensemble candidate solutions.
        #---------------------------------------------

all_ens_cand_sols = collect_all_ens_cand_sols (marxan_rsrun_dirs,
                                               prob_dirs,
                                               num_PUs)
    all_ens_marxan_BEST_sols   = all_ens_cand_sols$best_sols
    all_ens_marxan_SUMMED_sols = all_ens_cand_sols$summed_sols

        #-----------------------------------------------------------------
        #  Echo ensemble candidate solutions for marxan best and summed.
        #-----------------------------------------------------------------

    cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
    cat ("FINISHED COLLECTING CANDIDATE SOLUTIONS.")
    cat ("all_ens_marxan_BEST_sols = \n")
    print (all_ens_marxan_BEST_sols)
    cat ("\n\nall_ens_marxan_SUMMED_sols = \n")
    print (all_ens_marxan_SUMMED_sols)
    cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")

        #--------------------------------------------------------------
        #  Compute the pseudo-optimum cost for each ensemble problem.
        #  This is the cost of the optimal solution for each apparent
        #  problem in the ensemble as if it was the correct problem.
        #  Since we don't know that optimal value, we'll use our best
        #  available guess at the optimum solution, which is either
        #  marxan's best solution or gurobi's.  Gurobi's is correct
        #  but may not be available due to run-time constraints or
        #  if it's just not being run, as is the case right now where
        #  we're just running marxan.
        #--------------------------------------------------------------

    pseudo_opt_costs =
        get_pseudo_opt_costs_for_all_ens_probs (all_ens_marxan_BEST_sols, #PSEUDO-OPT SOLUTION
                                                num_probs_in_ensemble,
                                                PU_costs_vec)

        #-----------------------------------------------------------------
        #  Compute the 3 main scores for each candidate solution applied
        #  to each ensemble problem and compared to the pseudo-optimum
        #  solution cost.
        #  Do this separately for the marxan best solutions and for the
        #  marxan summed solutions.
        #-----------------------------------------------------------------

    all_ens_marxan_BEST_sol_scores =
        score_ALL_cand_sols_against_ALL_probs (ens_prob_dirs,

                                    all_ens_marxan_BEST_sols,
                                               pseudo_opt_costs,
                                               num_probs_in_ensemble,
                                               num_spp,
                                               PU_costs_vec,
                                               spp_rep_targets
                                               )

    #----------

    all_ens_marxan_BEST_spp_rep_err_scores =
            all_ens_marxan_BEST_sol_scores$all_cand_spp_rep_err_scores
    all_ens_marxan_BEST_spp_rep_err_score_stats =
            compute_stats_for_all_sols_across_all_ens_probs (
                    all_ens_marxan_BEST_spp_rep_err_scores,
                    num_probs_in_ensemble)

    all_ens_marxan_BEST_cost_err_scores =
            all_ens_marxan_BEST_sol_scores$all_cand_cost_err_scores
    all_ens_marxan_BEST_cost_err_score_stats =
            compute_stats_for_all_sols_across_all_ens_probs (
                    all_ens_marxan_BEST_cost_err_scores,
                    num_probs_in_ensemble)

    all_ens_marxan_BEST_euc_err_scores =
            all_ens_marxan_BEST_sol_scores$all_cand_euc_err_scores
    all_ens_marxan_BEST_euc_err_score_stats =
            compute_stats_for_all_sols_across_all_ens_probs (
                    all_ens_marxan_BEST_euc_err_scores,
                    num_probs_in_ensemble)

    #----------

    all_ens_marxan_SUMMED_spp_rep_err_scores =
            all_ens_marxan_SUMMED_sol_scores$all_cand_spp_rep_err_scores
    all_ens_marxan_SUMMED_spp_rep_err_score_stats =
            compute_stats_for_all_sols_across_all_ens_probs (
                    all_ens_marxan_SUMMED_spp_rep_err_scores,
                    num_probs_in_ensemble)

    all_ens_marxan_SUMMED_cost_err_scores =
            all_ens_marxan_SUMMED_sol_scores$all_cand_cost_err_scores
    all_ens_marxan_SUMMED_cost_err_score_stats =
            compute_stats_for_all_sols_across_all_ens_probs (
                    all_ens_marxan_SUMMED_cost_err_scores,
                    num_probs_in_ensemble)

    all_ens_marxan_SUMMED_euc_err_scores =
            all_ens_marxan_SUMMED_sol_scores$all_cand_euc_err_scores
    all_ens_marxan_SUMMED_euc_err_score_stats =
            compute_stats_for_all_sols_across_all_ens_probs (
                    all_ens_marxan_SUMMED_euc_err_scores,
                    num_probs_in_ensemble)

        #-----------------------------------------------------------------
        #  Now that you have the scores for each candidate solution on
        #  each ensemble subproblem, you can apply each of the different
        #  types of aggregation to choose an overall winning solution,
        #  e.g.,
        #      - best median score across all problems
        #      - best worst score across all problems
        #      - best quantile score across all problems, e.g., the
        #        25% point might be a good compromise between best median
        #        and best worst so that it's like a maximin but not quite
        #        so influenced by the most extreme values.
        #        (Can use fivenum() to get values like these easily.)
        #      - best mean score across all problems
        #      - sum votes for all PUs across all problems and choose
        #        cutoff like in the greedy reserve selectors
        #
        #  (Remember the fivenum() function since it may be usefull here.)
        #-----------------------------------------------------------------


choose_ensemble_winning_solution (method="median")
#  ...
choose_ensemble_winning_solution (method="summed_votes")

    }  #  end function - ensemble()

#===============================================================================

do_ensemble <- function (APP_bd_prob,    #  <<<<<-----------------
                         COR_bd_prob,    #  <<<<<-----------------
                         parameters,
                         starting_dir,

                         rs_method_name,
                         resSel_func,

                         RS_specific_params,

                         src_rds_file_dir,
                         spp_rep_targets)
    {
    cat ("\nInside do_ensemble():", sep='')
    cat ("\n        starting_dir = '", starting_dir, "'", sep='')
    cat ("\n        rs_method_name = '", rs_method_name, "'", sep='')
    cat ("\n        RS_specific_params = \n", sep='')
    print (RS_specific_params)
    cat ("\n        src_rds_file_dir = '", src_rds_file_dir, "'", sep='')
    cat ("\n        spp_rep_targets = \n", sep='')
    print (spp_rep_targets)
    cat ("\n")

    ResSel_run <- create_RSrun (APP_bd_prob@UUID,
                                spp_rep_targets,
                                parameters,
                                starting_dir,
                                APP_bd_prob@cor_or_app_str,
                                APP_bd_prob@basic_or_wrapped_or_comb_str,
                                rs_method_name)

    base_outdir = get_RSrun_path_topdir (ResSel_run, starting_dir)

    ens_elems_starting_dir = file.path (base_outdir, "Ens_elems")
    dir.create (ens_elems_starting_dir)
    cat ("\n\nens_elems_starting_dir = '", ens_elems_starting_dir, "'\n")

    ensemble (APP_bd_prob,
              parameters,
              starting_dir,
              ens_elems_starting_dir,
              RS_specific_params)

    RSrun_topdir = get_RSrun_path_topdir (ResSel_run, starting_dir)

    return (RSrun_topdir)

    }  #  end function - do_ensemble()

#===============================================================================

