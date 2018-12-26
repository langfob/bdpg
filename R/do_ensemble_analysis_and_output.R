#===============================================================================
#
#                       do_ensemble_analysis_and_output.R
#
#  Run code to run ensemble and dump results to file.
#
#===============================================================================
#===============================================================================
#===============================================================================

#  General function to save the solution vector and scoring restults, etc
#  for any reserve selector.
#
#  This function is not particular to ensembles and should probably be
#  moved to a more generic source file.

#-------------------------------------------------------------------------------

save_rs_sol_vec_and_results <- function (solution_file_name,

#2018 12 23#                                    rsrun_dir,

                                         solution_PU_IDs,
                                         run_id,
                                    starting_dir,
                                         APP_run,
                                         COR_bd_prob,
                                         APP_bd_prob,
                                         rs_method_name,
                                         csv_outfile_name,
                                         rs_control_values,
                                         src_rds_file_dir)
    {
              #---------------------------------------------------------
              #  Save the solution vector to disk in its own file
              #  to make it easier to find and retrieve.
              #---------------------------------------------------------

rsrun_dir = starting_dir    #  rsrun_dir was an arg, but I think it was the same as starting_dir
                            #  Adding this for now to preserve what it was used for until I know
                            #  whether it can be replaced by starting_dir
                            #  2018 12 23 - BTL
                            #  Will flag all related references for this with: #2018 12 23#
    solution_file_path = file.path (rsrun_dir, solution_file_name)
    write (solution_PU_IDs, solution_file_path, sep=",")

              #---------------------------------------------------------
              #  Save all of the input variables and output scores etc
              #  to disk as one big one line table with headers.
              #---------------------------------------------------------

    save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs (
                                            solution_PU_IDs,
                                            tzar_run_ID = run_id,
                                            exp_root_dir = starting_dir,
                                            APP_run,
                                            COR_bd_prob,
                                            APP_bd_prob,
                                            rs_method_name,
                                            csv_outfile_name,
                                            rs_control_values,
                                            src_rds_file_dir)
    }

#===============================================================================

#  Save 1 ensemble solution vector for a given error measure
#  and its results to disk.

#-------------------------------------------------------------------------------

save_ens_sol_vec_and_results <- function (sol_vec,
                                          ens_type_name,
                                          score_name_str,
                                          rs_name_str,
#2018 12 23#                                          ens_run_dir,
                                          run_id,
                                          starting_dir,
                                          APP_ens_run,
                                          COR_bd_prob,
                                          APP_bd_prob,
                                          rs_control_values,
                                          src_rds_file_dir)
    {
    rs_method_name     = paste0 (ens_type_name, "_", rs_name_str, "_", score_name_str)
    csv_outfile_name   = paste0 ("rsrun_results_", rs_method_name, "_", "_solution.csv")
    solution_file_name = paste0 ("ENS_", rs_method_name, "_solution_PU_IDs.csv")

    save_rs_sol_vec_and_results (solution_file_name,
#2018 12 23#                                 rsrun_dir          = ens_run_dir,
                                 solution_PU_IDs    = sol_vec,
                                 run_id,
                                 starting_dir,
                                 APP_run            = APP_ens_run,
                                 COR_bd_prob,
                                 APP_bd_prob,
                                 rs_method_name,
                                 csv_outfile_name,
                                 rs_control_values,
                                 src_rds_file_dir)
    }

#===============================================================================

#  Save all 5 ensemble solution vectors for a given error measure
#  and their results to disk.
#
#  Currently, the 5 variants are min, Q1 (first 25 percentile), median, mean,
#  and summed votes for each PU ID.

#-------------------------------------------------------------------------------

save_ens_sol_vectors_and_results <-
                function (solution_vectors,
                          score_name_str, rs_name_str,
#2018 12 23#                          ens_run_dir,
                          run_id,
                          starting_dir, APP_ens_run,
                          COR_bd_prob, APP_bd_prob,
                          rs_control_values, src_rds_file_dir)
    {

    save_ens_sol_vec_and_results (
        sol_vec = solution_vectors$sol_vec_by_min,
        ens_type_name = "min",
        score_name_str, rs_name_str,
#2018 12 23#        ens_run_dir,
            run_id,
            starting_dir, APP_ens_run,
            COR_bd_prob, APP_bd_prob,
            rs_control_values, src_rds_file_dir)

    save_ens_sol_vec_and_results (
        sol_vec = solution_vectors$sol_vec_by_Q1,
        ens_type_name = "Q1",
        score_name_str, rs_name_str,
#2018 12 23#            ens_run_dir,
            run_id,
            starting_dir, APP_ens_run,
            COR_bd_prob, APP_bd_prob,
            rs_control_values, src_rds_file_dir)

    save_ens_sol_vec_and_results (
        sol_vec = solution_vectors$sol_vec_by_median,
        ens_type_name = "median",
        score_name_str, rs_name_str,
#2018 12 23#            ens_run_dir,
            run_id,
            starting_dir, APP_ens_run,
            COR_bd_prob, APP_bd_prob,
            rs_control_values, src_rds_file_dir)

    save_ens_sol_vec_and_results (
        sol_vec = solution_vectors$sol_vec_by_mean,
        ens_type_name = "mean",
        score_name_str, rs_name_str,
#2018 12 23#            ens_run_dir,
            run_id,
            starting_dir, APP_ens_run,
            COR_bd_prob, APP_bd_prob,
            rs_control_values, src_rds_file_dir)

    save_ens_sol_vec_and_results (
        sol_vec = solution_vectors$sol_vec_by_summed_votes,
        ens_type_name = "summed_votes",
        score_name_str, rs_name_str,
#2018 12 23#            ens_run_dir,
            run_id,
            starting_dir, APP_ens_run,
            COR_bd_prob, APP_bd_prob,
            rs_control_values, src_rds_file_dir)
    }

#===============================================================================
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

        cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
        cat ("At bottom of loop COLLECTING CANDIDATE SOLUTIONS.")
        cat ("cur_idx = ", cur_idx, "\n")
        cat ("\n\nall_ens_marxan_BEST_sols = \n")
        print (all_ens_marxan_BEST_sols)
        cat ("\n\nall_ens_marxan_SUMMED_sols = \n")
        print (all_ens_marxan_SUMMED_sols)
        cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
        }

    cat ("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n")
    cat ("About to return from COLLECTING CANDIDATE SOLUTIONS.")
    cat ("cur_idx = ", cur_idx, "\n")
    cat ("\n\nall_ens_marxan_BEST_sols = \n")
    print (all_ens_marxan_BEST_sols)
    cat ("\n\nall_ens_marxan_SUMMED_sols = \n")
    print (all_ens_marxan_SUMMED_sols)
    cat ("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")

    return (list (best_sols   = all_ens_marxan_BEST_sols,
                  summed_sols = all_ens_marxan_SUMMED_sols))

    }  #  end function - collect_all_ens_cand_sols()

#===============================================================================

get_pseudo_opt_costs_for_all_ens_probs <- function (all_ens_best_cand_sols, #PSEUDO-OPT
                                                    num_probs_in_ensemble,
                                                    PU_costs)
    {
    pseudo_opt_costs = vector (mode="numeric", length=num_probs_in_ensemble)

    for (cur_idx in 1:num_probs_in_ensemble)
        {
        pseudo_opt_costs [cur_idx] =
            compute_solution_cost (all_ens_best_cand_sols [[cur_idx]],
                                   PU_costs)
        }

    return (pseudo_opt_costs)
    }

#===============================================================================

score_ONE_cand_sol_against_ONE_prob <- function (cur_bpm,
                                                 cur_cand_sol,
                                                 spp_rep_targets,
                                                 num_spp,
                                                 pseudo_opt_cost,
                                                 PU_costs)
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
                                                           PU_costs)

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
                                                  PU_costs,
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
                                                           PU_costs)

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
                                                   PU_costs,
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

        results_list =
            score_ALL_cand_sols_against_ONE_prob (all_ens_cand_sols,
                                                  pseudo_opt_costs [cur_prob_idx],       #  marxan/gurobi computed cost on apparent problem
                                                  num_probs_in_ensemble,
                                                  num_spp,
                                                  cur_bpm,
                                                  PU_costs,
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

    all_cand_score_stats = tibble::as_tibble (all_cand_score_stats)
    names (all_cand_score_stats) = c ("min", "Q1", "median", "Q3", "max", "mean")

    return (all_cand_score_stats)
    }

#===============================================================================

convert_lists_of_PU_IDs_to_matrix_of_0_1 <- function (all_cand_sols,
                                                      num_PUs,
                                                      num_probs_in_ensemble)
    {
    all_cand_matrix_of_pres_abs = matrix (0, nrow=num_probs_in_ensemble,
                                             ncol=num_PUs)

    for (cur_cand_sol_idx in 1:num_probs_in_ensemble)
        {
        cur_cand_sol = all_cand_sols [[cur_cand_sol_idx]]
        all_cand_matrix_of_pres_abs [cur_cand_sol_idx, cur_cand_sol] = 1
        }

    return (all_cand_matrix_of_pres_abs)
    }

#===============================================================================

compute_stats_and_winners_for_given_score_type <-
                            function (all_cand_scores_across_all_ens_probs,
                                      all_cand_sols,
                                      bpm,
                                      num_probs_in_ensemble,
                                      rs_name_str,
                                      score_name_str)
    {

# plot each candidate's scores on all probs
# or all candidate scores for each problem

    all_cand_score_stats =
        compute_stats_for_all_sols_across_all_ens_probs (all_cand_scores_across_all_ens_probs,
                                                         num_probs_in_ensemble)

                    # If this extremum is unique (or empty), the results are the same as
                    # (but more efficient than) which(x == min(x, na.rm = TRUE)) or
                    # which(x == max(x, na.rm = TRUE)) respectively.

    winning_sol_vector_by_min    = which.max (all_cand_score_stats$min)
    winning_sol_vector_by_Q1     = which.max (all_cand_score_stats$Q1)
    winning_sol_vector_by_median = which.max (all_cand_score_stats$median)
    winning_sol_vector_by_mean   = which.max (all_cand_score_stats$mean)

        #----------

    all_cand_matrix_of_pres_abs =
        convert_lists_of_PU_IDs_to_matrix_of_0_1 (all_cand_sols, num_PUs,
                                                  num_probs_in_ensemble)

    votes_for_each_PU_ID =
        tibble::tibble (PU_ID=1:num_PUs,
                           votes=colSums (all_cand_matrix_of_pres_abs))

    PUs_sorted_by_votes = dplyr::arrange (votes_for_each_PU_ID, desc (votes))

    winning_sol_vector_by_summed_votes =
        find_first_solution_with_all_rep_tgts_met (bpm,
                                                   PUs_sorted_by_votes,
                                                   spp_rep_targets)

        #----------

    return (list (sol_vec_by_min          = winning_sol_vector_by_min,
                  sol_vec_by_Q1           = winning_sol_vector_by_Q1,
                  sol_vec_by_median       = winning_sol_vector_by_median,
                  sol_vec_by_mean         = winning_sol_vector_by_mean,
                  sol_vec_by_summed_votes = winning_sol_vector_by_summed_votes))
    }

#===============================================================================

ensemble <- function (APP_bd_prob,
                      parameters,
                      starting_dir,
                      ens_elems_starting_dir,
                      RS_specific_params,
                      COR_bd_prob,
                      APP_ens_run,
                      src_rds_file_dir=NULL)
    {
    cat ("\nInside ensemble function.\n")

        #---------------------------------------------------------------------
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
        #---------------------------------------------------------------------

    parameters$compute_network_metrics          = FALSE

            #-------------------------------------------------------------------
            #  Need to make sure that marxan (or in the future, whatever RS is
            #  used by the ensemble) is turned on.
            #  If it's not turned on, then even though it will be run on all
            #  the ensemble subproblems, it won't be run on the APP problem
            #  they'are all derived from.  That would be an issue because
            #  that problem is also included in the ensemble and therefore,
            #  needs to have the same reserve selector run on it that is run
            #  on all the other ensemble subproblems.
            #-------------------------------------------------------------------

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

        #--------------------------------------------------------------
        #  Make local copies of structures used from the APP_bd_prob.
        #--------------------------------------------------------------

    num_PUs         = APP_bd_prob@num_PUs
    num_spp         = APP_bd_prob@num_spp
    bpm             = APP_bd_prob@bpm
    spp_rep_targets = APP_bd_prob@spp_rep_targets
    PU_costs        = APP_bd_prob@PU_costs

        #-----------------------------------------------------------------------
        #  Get and validate input parameters specific to this reserve selector.
        #-----------------------------------------------------------------------

    run_id = parameters$run_id

    num_probs_in_ensemble = RS_specific_params$num_probs_in_ensemble
    if (num_probs_in_ensemble < 2)
        stop_bdpg (paste0 ("num_probs_in_ensemble = '", num_probs_in_ensemble,
                           "' must be >= 2."))

#  2018 12 16 - BTL - For quick testing at the moment...
FP_err_amt = 0.001
FN_err_amt = 0.01

#  2018 12 16 - BTL - No cost errors for the moment...
gen_combined_cost_and_FP_FN_errors = FALSE
cost_err_amt = 0



    #  Create ensemble control values list:
ens_control_values = list()
#  This is what marxan_control_values looks like when it's created in
#  gscp_13_write_marxan_control_file_and_run_marxan.R::set_marxan_controls_and_run_marxan():
    # retVal = list ()
    # retVal$marxan_PROP           = marxan_PROP
    # retVal$marxan_RANDSEED       = marxan_RANDSEED
    # retVal$marxan_NUMREPS        = marxan_NUMREPS
    # retVal$marxan_NUMITNS        = marxan_NUMITNS
    # retVal$marxan_STARTTEMP      = marxan_STARTTEMP
    # retVal$marxan_NUMTEMP        = marxan_NUMTEMP
    # retVal$marxan_COSTTHRESH     = marxan_COSTTHRESH
    # retVal$marxan_THRESHPEN1     = marxan_THRESHPEN1
    # retVal$marxan_THRESHPEN2     = marxan_THRESHPEN2
    # retVal$marxan_RUNMODE        = marxan_RUNMODE
    # retVal$marxan_MISSLEVEL      = marxan_MISSLEVEL
    # retVal$marxan_ITIMPTYPE      = marxan_ITIMPTYPE
    # retVal$marxan_HEURTYPE       = marxan_HEURTYPE
    # retVal$marxan_CLUMPTYPE      = marxan_CLUMPTYPE
    #
    # retVal$RS_user_time          = marxan_timings["user.self"]
    # retVal$RS_system_time        = marxan_timings["sys.self"]
    # retVal$RS_elapsed_time       = marxan_elapsed_time
    # retVal$RS_user_child_time    = marxan_timings["user.child"]
    # retVal$RS_sys_child_time     = marxan_timings["sys.child"]





    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

        #----------------------------------------------------------------------
        #  Generate the set of apparent problems that make up the ensemble.
        #
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

    #---------------------------------------------------------------------------

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

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

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

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

        #--------------------------------------------------------------
        #  Compute the pseudo-optimum cost for each ensemble problem.
        #
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
                                                PU_costs)

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

        #-----------------------------------------------------------------
        #  Compute the 3 main scores for each candidate solution applied
        #  to each ensemble problem and compared to the pseudo-optimum
        #  solution cost.
        #  Do this separately for the marxan best solutions and for the
        #  marxan summed solutions.
        #-----------------------------------------------------------------

    all_ens_marxan_BEST_sol_scores =
        score_ALL_cand_sols_against_ALL_probs (prob_dirs,

                                    all_ens_marxan_BEST_sols,
                                               pseudo_opt_costs,
                                               num_probs_in_ensemble,
                                               num_spp,
                                               PU_costs,
                                               spp_rep_targets
                                               )

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
        #-----------------------------------------------------------------

    winning_marxan_BEST_sols_by_spp_rep_err_score =
        compute_stats_and_winners_for_given_score_type (all_ens_marxan_BEST_sol_scores$all_cand_spp_rep_err_scores,

                                                        all_cand_sols = all_ens_marxan_BEST_sols,
                                                        bpm,
                                                        num_probs_in_ensemble,
                                                        rs_name_str = "marxan_BEST",
                                                        score_name_str = "spp_rep")

    save_ens_sol_vectors_and_results (winning_marxan_BEST_sols_by_spp_rep_err_score,
                                      score_name_str = "spp_rep",
                                      rs_name_str = "marxan_BEST",
#2018 12 23#                                      ens_run_dir,
                                      run_id,
                                      starting_dir, APP_ens_run,
                                      COR_bd_prob, APP_bd_prob,
                                      ens_control_values, src_rds_file_dir)

    #----------

    winning_marxan_BEST_sols_by_cost_err_score =
        compute_stats_and_winners_for_given_score_type (all_ens_marxan_BEST_sol_scores$all_cand_cost_err_scores,

                                                        all_cand_sols = all_ens_marxan_BEST_sols,
                                                        bpm,
                                                        num_probs_in_ensemble,
                                                        rs_name_str = "marxan_BEST",
                                                        score_name_str = "cost")

    save_ens_sol_vectors_and_results (winning_marxan_BEST_sols_by_cost_err_score,
                                      score_name_str = "cost",
                                      rs_name_str = "marxan_BEST",
#2018 12 23#                                      ens_run_dir,
                                      run_id,
                                      starting_dir, APP_ens_run,
                                      COR_bd_prob, APP_bd_prob,
                                      ens_control_values, src_rds_file_dir)

    #----------

    winning_marxan_BEST_sols_by_euc_err_score =
        compute_stats_and_winners_for_given_score_type (all_ens_marxan_BEST_sol_scores$all_ens_marxan_BEST_euc_err_scores,

                                                        all_cand_sols = all_ens_marxan_BEST_sols,
                                                        bpm,
                                                        num_probs_in_ensemble,
                                                        rs_name_str = "marxan_BEST",
                                                        score_name_str = "euc")

    save_ens_sol_vectors_and_results (winning_marxan_BEST_sols_by_euc_err_score,
                                      score_name_str = "euc",
                                      rs_name_str = "marxan_BEST",
#2018 12 23#                                      ens_run_dir,
                                      run_id,
                                      starting_dir, APP_ens_run,
                                      COR_bd_prob, APP_bd_prob,
                                      ens_control_values, src_rds_file_dir)

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

    #---------------------------------
    #  Now, marxan_SUMMED_sol_scores
    #---------------------------------

    all_ens_marxan_SUMMED_sol_scores =
        score_ALL_cand_sols_against_ALL_probs (prob_dirs,

                                    all_ens_marxan_SUMMED_sols,
                                               pseudo_opt_costs,
                                               num_probs_in_ensemble,
                                               num_spp,
                                               PU_costs,
                                               spp_rep_targets
                                               )

    #----------
    #----------

    winning_marxan_SUMMED_sols_by_spp_rep_err_score =
        compute_stats_and_winners_for_given_score_type (all_ens_marxan_SUMMED_sol_scores$all_cand_spp_rep_err_scores,

                                                        all_cand_sols = all_ens_marxan_SUMMED_sols,
                                                        bpm,
                                                        num_probs_in_ensemble,
                                                        rs_name_str = "marxan_SUMMED",
                                                        score_name_str = "spp_rep")

    save_ens_sol_vectors_and_results (winning_marxan_SUMMED_sols_by_spp_rep_err_score,
                                      score_name_str = "spp_rep",
                                      rs_name_str = "marxan_SUMMED",
#2018 12 23#                                      ens_run_dir,
                                      run_id,
                                      starting_dir, APP_ens_run,
                                      COR_bd_prob, APP_bd_prob,
                                      ens_control_values, src_rds_file_dir)

    #----------

    winning_marxan_SUMMED_sols_by_cost_err_score =
        compute_stats_and_winners_for_given_score_type (all_ens_marxan_SUMMED_sol_scores$all_cand_cost_err_scores,

                                                        all_cand_sols = all_ens_marxan_SUMMED_sols,
                                                        bpm,
                                                        num_probs_in_ensemble,
                                                        rs_name_str = "marxan_SUMMED",
                                                        score_name_str = "cost")

    save_ens_sol_vectors_and_results (winning_marxan_SUMMED_sols_by_cost_err_score,
                                      score_name_str = "cost",
                                      rs_name_str = "marxan_SUMMED",
#2018 12 23#                                      ens_run_dir,
                                      run_id,
                                      starting_dir, APP_ens_run,
                                      COR_bd_prob, APP_bd_prob,
                                      ens_control_values, src_rds_file_dir)

    #----------

    winning_marxan_SUMMED_sols_by_euc_err_score =
        compute_stats_and_winners_for_given_score_type (all_ens_marxan_SUMMED_sol_scores$all_ens_marxan_SUMMED_euc_err_scores,

                                                        all_cand_sols = all_ens_marxan_SUMMED_sols,
                                                        bpm,
                                                        num_probs_in_ensemble,
                                                        rs_name_str = "marxan_SUMMED",
                                                        score_name_str = "euc")

    save_ens_sol_vectors_and_results (winning_marxan_SUMMED_sols_by_euc_err_score,
                                      score_name_str = "euc",
                                      rs_name_str = "marxan_SUMMED",
#2018 12 23#                                      ens_run_dir,
                                      run_id,
                                      starting_dir, APP_ens_run,
                                      COR_bd_prob, APP_bd_prob,
                                      ens_control_values, src_rds_file_dir)

    #----------------------------------------------------------------------

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
              RS_specific_params,
              COR_bd_prob,
              ResSel_run,
              src_rds_file_dir)

    RSrun_topdir = get_RSrun_path_topdir (ResSel_run, starting_dir)  #  isn't this the same base_outdir above?

    return (RSrun_topdir)

    }  #  end function - do_ensemble()

#===============================================================================

