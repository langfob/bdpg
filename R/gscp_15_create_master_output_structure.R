#===============================================================================

                #  gscp_15_create_master_output_structure.R

#===============================================================================

compute_euc_out_err_frac <- function (cor_or_app_str,
                                        solution_cost_err_frac,
                                        frac_spp_covered)
    {
    euc_out_err_frac = sqrt ((solution_cost_err_frac ^ 2) + ((1 - frac_spp_covered) ^2))

    if (cor_or_app_str == "COR")
        {
        results_list = list (rsr_COR_euc_out_err_frac = euc_out_err_frac)

        } else if (cor_or_app_str == "APP")
        {
        results_list = list (rsr_APP_euc_out_err_frac = euc_out_err_frac)

        } else
        {
        stop_bdpg (paste0 ("cor_or_app_str = '", cor_or_app_str,
                           "'.  Must be 'COR' or 'APP'"))
        }

    return (results_list)
    }

#===============================================================================

compute_RS_solution_cost_scores_wrt_COR_costs_vec <-
                                            function (rs_solution_PU_IDs_vec,
                                                      cor_optimum_cost,
                                                      PU_costs_vec)
    {
    #---------------------------------------------------------------------------
    #         Compute error in cost of reserve selector's solution.
    #---------------------------------------------------------------------------

    rs_solution_cost = compute_solution_cost (rs_solution_PU_IDs_vec,
                                              PU_costs_vec)
    rs_solution_cost_err_frac = (rs_solution_cost - cor_optimum_cost) /
                                cor_optimum_cost
    abs_rs_solution_cost_err_frac = abs (rs_solution_cost_err_frac)

    #---------------------------------------------------------------------
    #  Giving errors as a fraction of the correct optimum cost
    #  may be misleading sometimes, e.g., when the correct optimum cost
    #  is nearly the cost of the full landscape.
    #  Even just guessing the cost of the whole landscape would not give
    #  much percentage error in that case.
    #  So, we'll also compute the error as a fraction of the maximum
    #  possible over-optimum or under-optimum error to see if that is
    #  more informative/predictable than guessing the usual percentage
    #  error.
    #---------------------------------------------------------------------

    rs_over_opt_cost_err_frac_of_possible_overcost = NA
    total_landscape_cost = sum (PU_costs_vec)
    rs_max_overcost = total_landscape_cost - cor_optimum_cost
    if (rs_solution_cost_err_frac > 0)
        rs_over_opt_cost_err_frac_of_possible_overcost =
            (rs_solution_cost - cor_optimum_cost) / rs_max_overcost

    rs_under_opt_cost_err_frac_of_possible_undercost = NA
    if (rs_solution_cost_err_frac < 0)
        rs_under_opt_cost_err_frac_of_possible_undercost = abs_rs_solution_cost_err_frac

    #---------------------------------------------------------------------

    return (list (cor_optimum_cost = cor_optimum_cost,
                  rs_solution_cost = rs_solution_cost,
                  rs_solution_cost_err_frac = rs_solution_cost_err_frac,
                  abs_rs_solution_cost_err_frac = abs_rs_solution_cost_err_frac,
                  rs_over_opt_cost_err_frac_of_possible_overcost = rs_over_opt_cost_err_frac_of_possible_overcost,
                  rs_under_opt_cost_err_frac_of_possible_undercost = rs_under_opt_cost_err_frac_of_possible_undercost
                 ))
    }

#===============================================================================

build_and_write_COR_and_APP_scores_lists <- function (rs_best_solution_PU_IDs,
                                                      COR_bd_prob,
                                                      APP_bd_prob,
                                                      rsrun)
    {
    rs_best_num_patches_in_solution = length (rs_best_solution_PU_IDs)
    cat ("\nrs_best_num_patches_in_solution =", rs_best_num_patches_in_solution)

        #------------------------------------------------------------
        #  Need to specially initialize the FP and FN rates because
        #  the call below that produces app_scores_list crashes on
        #  NULL APP_bd_prob@APP_prob_info, which is the value when
        #  the problem is a COR instead of an APP problem.
        #------------------------------------------------------------

    if (APP_bd_prob@cor_or_app_str == "APP")
        {
        FP_const_rate = APP_bd_prob@APP_prob_info@FP_const_rate
        FN_const_rate = APP_bd_prob@APP_prob_info@FN_const_rate

        } else
        {
        FP_const_rate = 0
        FN_const_rate = 0
        }

    num_patches_in_cor_solution = sum (COR_bd_prob@nodes$dependent_set_member)

    #----------

        #-------------------------------------------------------------
        #  Compute rep scores and scores based on confusion matrices
        #  with respect to the CORRECT problem.
        #-------------------------------------------------------------

    cor_scores_list =
        build_and_write_rep_and_cm_scores_list (APP_bd_prob@cor_or_app_str,    #rsrun,
                                                 COR_bd_prob@bpm,
                                                 rs_best_solution_PU_IDs,
                                                 rsrun@targets,
                                                 COR_bd_prob@num_spp,
                                                 rs_best_num_patches_in_solution,
                                                 COR_bd_prob@num_PUs,
                                                 num_patches_in_cor_solution,
                                                 FP_const_rate,
                                                 FN_const_rate)

        #------------------------------------------------------------
        #  Relabel the generically labelled rep score components to
        #  proper output names for CORRECT.
        #------------------------------------------------------------

    cor_scores_list$rsr_COR_spp_rep_shortfall = cor_scores_list$spp_rep_shortfall
    cor_scores_list$spp_rep_shortfall = NULL

    cor_scores_list$rsr_COR_solution_NUM_spp_covered = cor_scores_list$num_spp_covered
    cor_scores_list$num_spp_covered = NULL

    cor_scores_list$rsr_COR_solution_FRAC_spp_covered = cor_scores_list$frac_spp_covered
    cor_scores_list$frac_spp_covered = NULL

    #----------

        #-------------------------------------------------------------
        #  Compute rep scores and scores based on confusion matrices
        #  with respect to the APPARENT problem.
        #-------------------------------------------------------------

    app_scores_list =
        build_and_write_rep_and_cm_scores_list (APP_bd_prob@cor_or_app_str,    #rsrun,
                                                 APP_bd_prob@bpm,
                                                 rs_best_solution_PU_IDs,
                                                 rsrun@targets,
                                                 COR_bd_prob@num_spp,
                                                 rs_best_num_patches_in_solution,
                                                 COR_bd_prob@num_PUs,
                                                 num_patches_in_cor_solution,
                                                 FP_const_rate,
                                                 FN_const_rate)

        #------------------------------------------------------------
        #  Relabel the generically labelled rep score components to
        #  proper output names for APPARENT.
        #------------------------------------------------------------

    app_scores_list$rsr_APP_spp_rep_shortfall = app_scores_list$spp_rep_shortfall
    app_scores_list$spp_rep_shortfall = NULL

    app_scores_list$rsr_APP_solution_NUM_spp_covered = app_scores_list$num_spp_covered
    app_scores_list$num_spp_covered = NULL

    app_scores_list$rsr_APP_solution_FRAC_spp_covered = app_scores_list$frac_spp_covered
    app_scores_list$frac_spp_covered = NULL

    #----------

    return (list (cor_scores_list=cor_scores_list,
                  app_scores_list=app_scores_list))
    }

#===============================================================================

get_greedy_best_solution_PU_IDs <- function (rsrun, top_dir)
    {
    greedy_output_dir = get_RSrun_path_output (rsrun, top_dir)
    greedy_results = readRDS (file.path (greedy_output_dir, "results.rds"))

    greedy_solution_PU_IDs = greedy_results$short_ranked_solution_PU_IDs_vec

    return (greedy_solution_PU_IDs)
    }

#===============================================================================

get_gurobi_best_solution_PU_IDs <- function (rsrun, top_dir)
    {
    gurobi_output_dir = get_RSrun_path_output (rsrun, top_dir)
    gurobi_solution_PU_IDs =
        readRDS (file.path (gurobi_output_dir, "solution_PU_IDs.rds"))

    return (gurobi_solution_PU_IDs)
    }

#===============================================================================

# get_marxan_best_solution_PU_IDs <- function (rsrun,
#                                              exp_root_dir,
#                                              COR_bd_prob,
#                                              APP_bd_prob)
get_marxan_best_and_summed_solution_PU_IDs <- function (rsrun,
                                             exp_root_dir,
                                             COR_bd_prob,
                                             APP_bd_prob)
    {
        #-----------------------------------------------------------
        #  Read in the useful values from the marxan output.
        #-----------------------------------------------------------
        #  marxan_output_values is a list containing the following
        #  named elements:
        #    - marxan_best_df_sorted
        #    - marxan_ssoln_df_sorted_by_PU
        #    - marxan_mvbest_df_sorted_by_ConservationFeature
        #-----------------------------------------------------------

    marxan_output_values =
        read_marxan_output_files (get_RSrun_path_output (rsrun, exp_root_dir),
                                  COR_bd_prob@all_PU_IDs)

        #--------------------------------------------------------------------
        #  Find which PUs the reserve selector chose for its best solution.
        #--------------------------------------------------------------------

    rs_best_solution_PU_IDs =
        which (marxan_output_values$marxan_best_df_sorted$SOLUTION > 0)

        #----------------------------
        #  Get summed solution IDs.
        #----------------------------

    marxan_best_summed_solution_PU_IDs =
    find_best_marxan_solutions_and_plot_incremental_summed_solution_reps (
                                                        rsrun,
                                                        exp_root_dir,
                                                        COR_bd_prob,
                                                        APP_bd_prob,
                                                        marxan_output_values)


    return (list (rs_best_solution_PU_IDs = rs_best_solution_PU_IDs,
                  marxan_best_summed_solution_PU_IDs =
                      marxan_best_summed_solution_PU_IDs))
    }

#===============================================================================

get_rs_best_solution_PU_IDs <- function (rs_method_name,
                                         rsrun,
                                         exp_root_dir,
                                         COR_bd_prob,
                                         APP_bd_prob,
                                         rs_control_values)
    {
        #-------------------------------------
    if (rs_method_name == "Marxan_SA")
        {
        # rs_best_solution_PU_IDs =
        #                     get_marxan_best_solution_PU_IDs (rsrun,
        #                                                      exp_root_dir,
        #                                                      COR_bd_prob,
        #                                                      APP_bd_prob)
        rs_best_and_summed_solution_PU_IDs =
                get_marxan_best_and_summed_solution_PU_IDs (rsrun,
                                                            exp_root_dir,
                                                            COR_bd_prob,
                                                            APP_bd_prob)
        rs_best_solution_PU_IDs =
            rs_best_and_summed_solution_PU_IDs$rs_best_solution_PU_IDs

        #-------------------------------------
        } else if (rs_method_name == "Gurobi")
        {
        rs_best_solution_PU_IDs =
                            get_gurobi_best_solution_PU_IDs (rsrun,
                                                             exp_root_dir)
        #-------------------------------------
        } else if ((rs_method_name == "SR_Forward") ||
                   (rs_method_name == "SR_Backward"))
        {
        rs_best_solution_PU_IDs =
                    get_greedy_best_solution_PU_IDs (rsrun, exp_root_dir)
        #-------------------------------------
        } else if ((rs_method_name == "UR_Forward") ||
                   (rs_method_name == "UR_Backward"))
        {
        rs_best_solution_PU_IDs =
                    get_greedy_best_solution_PU_IDs (rsrun, exp_root_dir)
        #-------------------------------------
        } else if ((rs_method_name == "ZL_Forward") ||
                   (rs_method_name == "ZL_Backward"))
        {
        rs_best_solution_PU_IDs =
                    get_greedy_best_solution_PU_IDs (rsrun, exp_root_dir)
        #-------------------------------------
        } else
        {
        stop_bdpg (paste0 ("Unknown reserve selector name '",
                           rs_method_name, "'"))
        }
        #-------------------------------------

    return (rs_best_solution_PU_IDs)
    }

#===============================================================================

save_rsrun_results_data_for_one_rsrun <- function (tzar_run_ID,
                                                   exp_root_dir,
                                                   rsrun,
                                                   COR_bd_prob,
                                                   APP_bd_prob,
                                                   rs_method_name,
                                                   rs_control_values=NULL,
                                                   src_rds_file_dir=NULL
                                                   )
    {
    rs_best_solution_PU_IDs = get_rs_best_solution_PU_IDs (rs_method_name,
                                                           rsrun,
                                                           exp_root_dir,
                                                           COR_bd_prob,
                                                           APP_bd_prob,
                                                           rs_control_values)

    save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs (
                                                        rs_best_solution_PU_IDs,
                                                        tzar_run_ID,
                                                        exp_root_dir,
                                                        rsrun,
                                                        COR_bd_prob,
                                                        APP_bd_prob,
                                                        rs_method_name,
                                csv_outfile_name = "rsrun_results.csv",
                                                        rs_control_values,
                                                        src_rds_file_dir)
    }

#===============================================================================

save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs <-
    function (rs_best_solution_PU_IDs,
              tzar_run_ID,
              exp_root_dir,
              rsrun,
              COR_bd_prob,
              APP_bd_prob,
              rs_method_name,
        csv_outfile_name,
              rs_control_values=NULL,
              src_rds_file_dir=NULL
              )
    {
        #------------------------------------------------------------------
        #  Compute costs and cost error measures for the chosen solution.
        #------------------------------------------------------------------
        #  2018 02 04 - BTL
        #  Here, we are only computing the cost score against the correct
        #  costs.  We could also compute it against the apparent costs,
        #  but it would be hard to figure out what that meant and it
        #  wouldn't give very useful information.
        #  For example, the total landscape cost is used in computing some
        #  of the measures and using apparent costs that were grossly
        #  underestimating true costs, the total landscape cost could
        #  actually be less than the correct optimum cost, which would
        #  lead to some very strange numbers.
        #  So, I'm just going to leave this computing only correct costs.
        #------------------------------------------------------------------

    app_cost_scores_list_wrt_COR_costs_vec =
        compute_RS_solution_cost_scores_wrt_COR_costs_vec (rs_best_solution_PU_IDs,
                                                           COR_bd_prob@correct_solution_cost,
                                                           COR_bd_prob@PU_costs)

# app_cost_scores_list_wrt_COR_costs_vec:
# (list (           cor_optimum_cost = cor_optimum_cost,
#                   rs_solution_cost = rs_solution_cost,
#                   rs_solution_cost_err_frac = rs_solution_cost_err_frac,
#                   abs_rs_solution_cost_err_frac = abs_rs_solution_cost_err_frac,
#                   rs_over_opt_cost_err_frac_of_possible_overcost = rs_over_opt_cost_err_frac_of_possible_overcost,
#                   rs_under_opt_cost_err_frac_of_possible_undercost = rs_under_opt_cost_err_frac_of_possible_undercost
#                  ))

        #--------------------------------------------------------------
        #  app_rep_scores_list_according_to_RS is a list containing
        #  the following named elements:
        #    - rsr_app_spp_rep_shortfall__fromRS
        #    - rsr_app_solution_NUM_spp_covered__fromRS
        #    - rsr_app_solution_FRAC_spp_covered__fromRS
        #--------------------------------------------------------------

    # app_rep_scores_list_according_to_RS =
    #     compute_and_verify_APP_rep_scores_according_to_RS (
    #                                                     rs_best_solution_PU_IDs,
    #                                                     COR_bd_prob@num_spp,
    #                                                     APP_bd_prob@bpm,
    #                                                     rsrun@targets)

        #-----------------------------------------------------------------------

    cor_and_app_scores_lists =
        build_and_write_COR_and_APP_scores_lists (rs_best_solution_PU_IDs,
                                                  COR_bd_prob,
                                                  APP_bd_prob,
                                                  rsrun)

    cor_scores_list = cor_and_app_scores_lists$cor_scores_list
    app_scores_list = cor_and_app_scores_lists$app_scores_list

        #-----------------------------------------------------------------------

    euc_COR_scores_list = compute_euc_out_err_frac (
        "COR",
        app_cost_scores_list_wrt_COR_costs_vec$abs_rs_solution_cost_err_frac,
        # cor_scores_list$rep_scores_list$rsr_COR_solution_FRAC_spp_covered)
        cor_scores_list$rsr_COR_solution_FRAC_spp_covered)

        #-----------------------------------------------------------------------
        #  Build or read a list for each aspect of the run.
        #  Make a NULL list for any section that doesn't apply in this run,
        #  e.g., if a type of network metric was not computed for this problem.
        #-----------------------------------------------------------------------

cat ("\n@@@TRACKING rand_seed in save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs:: rsrun@rand_seed = ", rsrun@rand_seed, "\n")
    tzar_run_ID_list          = list (rsr_tzar_run_ID = tzar_run_ID,
                                      rsr_UUID = rsrun@UUID,
                                      rsr_checksum = rsrun@checksum,
                                      rsr_rand_seed = rsrun@rand_seed,
                                      rs_method_name = rs_method_name)

    use_src_rds_file_dir = ! (is.null (src_rds_file_dir))

    prob_characteristics_list = read_prob_characteristics_list (APP_bd_prob,
                                                                src_rds_file_dir,
                                                                exp_root_dir,
                                                                use_src_rds_file_dir)

        #-----------------------------------------------------------------------

    combined_err_label_list = list (rsp_combined_err_label = APP_bd_prob@combined_err_label)

#-----------------------------------------------------------------------
# 2018 01 30 - BTL
# THESE MIGHT NOT COME FROM DISK.
# THEY MIGHT BE STORED IN THE PROBLEM OBJECT ITSELF IF NETWORK COMPUTATIONS
# WERE DONE IN BATCH INSTEAD OF DONE AT THE TIME OF PROBLEM CREATION.
# MIGHT BE SAFER JUST TO ALWAYS TAKE THEM FROM THE OBJECT HERE.

    # bipartite_measures_list   = read_bipartite_measures_list (APP_bd_prob,
    #                                                           src_rds_file_dir,
    #                                                           exp_root_dir,
    #                                                           use_src_rds_file_dir)
    #
    # igraph_measures_list      = read_igraph_measures_list (APP_bd_prob,
    #                                                        src_rds_file_dir,
    #                                                        exp_root_dir,
    #                                                        use_src_rds_file_dir)

    bipartite_measures_list   = as.list (APP_bd_prob@bipartite_metrics_from_bipartite_package)
    igraph_measures_list      = as.list (APP_bd_prob@bipartite_metrics_from_igraph_package_df)
#-----------------------------------------------------------------------

       #-----------------------------------------------------------------------


        #----------------------------------------------------------------
        #  Concatenate all of the lists and write the full list to file
        #  as a data frame.
        #----------------------------------------------------------------

    results_list = c (
                      tzar_run_ID_list,
                      prob_characteristics_list,
                      igraph_measures_list,
                      bipartite_measures_list,

                      app_cost_scores_list_wrt_COR_costs_vec,
                      # app_rep_scores_list_according_to_RS,

                      euc_COR_scores_list,
                      combined_err_label_list,

                      cor_scores_list,
                      app_scores_list,

                      rs_control_values
                    )

    write_results_to_files (
#        csv_outfile_name = "rsrun_results.csv",
        csv_outfile_name,
        results_df       =
            list_as_data_frame_with_0_length_vals_replaced_by_NA (results_list),
        tzar_run_ID,
        out_dir                = get_RSrun_path_topdir (rsrun, exp_root_dir),
        tzar_run_id_field_name = "rsr_tzar_run_ID")
    }

#===============================================================================

