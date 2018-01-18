#===============================================================================

                #  gscp_15_create_master_output_structure.R

#===============================================================================

compute_RS_solution_cost_scores_wrt_COR_costs_vec <- function (rs_solution_PU_IDs_vec,
                                             cor_optimum_cost,    #  cor_solution_PU_IDs_vec,
                                             PU_costs_vec)
    {
    #---------------------------------------------------------------------------
    #         Compute error in cost of reserve selector's solution.
    #---------------------------------------------------------------------------

    # cor_optimum_cost = compute_solution_cost (cor_solution_PU_IDs_vec,
    #                                           PU_costs_vec)

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

save_rsrun_results_data_for_one_rsrun <- function (parameters,
                                                   rsrun,
                                                   COR_bd_prob,
                                                   APP_bd_prob,
                                                   rs_method_name,
                                                   marxan_control_values=NULL,
                                                   src_rds_file_dir=NULL
                                                   )
    {
    tzar_run_ID  = parameters$run_id
    exp_root_dir = parameters$fullOutputDir_NO_slash
    out_dir      = get_RSrun_path_topdir (rsrun, exp_root_dir)


        #-----------------------------------------------------------------------

    marxan_output_values =
        read_marxan_output_files (get_RSrun_path_output (rsrun, exp_root_dir),     #rs_output_dir_path,
                                  COR_bd_prob@all_PU_IDs  #all_correct_node_IDs
                                  )

        #-----------------------------------------------------------------------

    cor_PU_costs_vec = COR_bd_prob@PU_costs


        #-----------------------------------------------------------------------

        #-----------------------------------------------------------------------
        #  These calls used to be part of read_marxan_output_files(), but
        #  they didn't need to be in there since they return nothing and
        #  are only called for their verificationa and plotting side effects.
        #-----------------------------------------------------------------------


    find_best_marxan_solutions_and_plot_incremental_summed_solution_reps_for_COR_and_APP (
                          get_RSrun_path_output (rsrun, exp_root_dir),     #rs_output_dir_path,
                          COR_bd_prob@num_spp,
                          COR_bd_prob@PU_costs,    #cor_PU_costs,
                  COR_bd_prob@bpm,
              COR_bd_prob@bpm,
                    marxan_output_values$marxan_best_df_sorted,
                        get_RSrun_path_plots (rsrun, exp_root_dir),    #plot_output_dir,
                          COR_bd_prob@num_PUs,     #largest_PU_ID,
                          COR_bd_prob@num_spp,     #largest_spp_ID,
                          rsrun@targets,           #targets,
                          get_RSrun_path_output (rsrun, exp_root_dir),    #topdir),    #rs_top_dir,
                    marxan_output_values$marxan_ssoln_df,    #marxan_ssoln_df_sorted_by_PU,
                          COR_bd_prob@correct_solution_cost    #correct_solution_cost
                          )

    find_best_marxan_solutions_and_plot_incremental_summed_solution_reps_for_COR_and_APP (
                          get_RSrun_path_output (rsrun, exp_root_dir),     #rs_output_dir_path,
                          COR_bd_prob@num_spp,
                          COR_bd_prob@PU_costs,    #cor_PU_costs,
                  COR_bd_prob@bpm,
              APP_bd_prob@bpm,
                    marxan_output_values$marxan_best_df_sorted,
                        get_RSrun_path_plots (rsrun, exp_root_dir),    #plot_output_dir,
                          COR_bd_prob@num_PUs,     #largest_PU_ID,
                          COR_bd_prob@num_spp,     #largest_spp_ID,
                          rsrun@targets,           #targets,
                          get_RSrun_path_output (rsrun, exp_root_dir),    #topdir),    #rs_top_dir,
                    marxan_output_values$marxan_ssoln_df,    #marxan_ssoln_df_sorted_by_PU,
                          COR_bd_prob@correct_solution_cost    #correct_solution_cost
                          )

        #-----------------------------------------------------------------------

        #  Find which PUs the reserve selector (marxan only, for now)
        #  chose for its best solution.
        #  Then, compute costs and cost error measures for the chosen solution.

    rs_best_solution_PU_IDs = which (marxan_output_values$marxan_best_df_sorted$SOLUTION > 0)

#  2017 11 26 - BTL
#  Here, we are only computing the cost score against the correct
#  costs.  Need to add another call to compute against apparent
#  costs and add those results to the output.
#  However, that requires adding a new element for apparent costs
#  in the app_prob_info structure, so I'm going to postpone
#  doing that until I know the changes I've just made here in
#  computing the correct cost scores are working correctly.
#  Not sure though whether the apparent cost score should be computed
#  against the correct_solution_cost or the apparent cost of the correct
#  solution...
    app_cost_scores_list_wrt_COR_costs_vec =
        compute_RS_solution_cost_scores_wrt_COR_costs_vec (rs_best_solution_PU_IDs,
                                                           COR_bd_prob@correct_solution_cost,    #  cor_solution_vector,
                                                           cor_PU_costs_vec)

        #-----------------------------------------------------------------------

    rs_best_num_patches_in_solution = length (rs_best_solution_PU_IDs)
    cat ("\nrs_best_num_patches_in_solution =", rs_best_num_patches_in_solution)

    app_rep_scores_list_according_to_RS =
        compute_and_verify_APP_rep_scores_according_to_RS (marxan_output_values$marxan_mvbest_df,
                                                           COR_bd_prob@num_spp)

    if (APP_bd_prob@cor_or_app_str == "APP")
        {
        FP_const_rate = APP_bd_prob@APP_prob_info@FP_const_rate
        FN_const_rate = APP_bd_prob@APP_prob_info@FN_const_rate

        } else
        {
        FP_const_rate = 0
        FN_const_rate = 0
        }

    # nodes = COR_bd_prob@nodes
    # cor_solution_vector = nodes$dependent_set_member
    # num_patches_in_cor_solution = sum (cor_solution_vector)
    num_patches_in_cor_solution = sum (COR_bd_prob@nodes$dependent_set_member)

    cor_scores_list = build_and_write_scores_list (rsrun,
                COR_bd_prob@bpm,                         #cor_bpm,
                rs_best_solution_PU_IDs,
                rsrun@targets,                           #spp_rep_targets,
                COR_bd_prob@num_spp,
        rs_best_num_patches_in_solution,         #marxan_best_num_patches_in_solution,     #num_PUs_in_cand_solution,
                COR_bd_prob@num_PUs,
        num_patches_in_cor_solution,             #num_PUs_in_optimal_solution,
            FP_const_rate,
            FN_const_rate
                )

    app_scores_list = build_and_write_scores_list (rsrun,
                APP_bd_prob@bpm,                         #app_bpm,
                rs_best_solution_PU_IDs,
                rsrun@targets,                           #spp_rep_targets,
                COR_bd_prob@num_spp,
        rs_best_num_patches_in_solution,         #marxan_best_num_patches_in_solution,     #num_PUs_in_cand_solution,
                COR_bd_prob@num_PUs,
        num_patches_in_cor_solution,             #num_PUs_in_optimal_solution,
            FP_const_rate,
            FN_const_rate
                )

        #-----------------------------------------------------------------------
        #  Build or read a list for each aspect of the run.
        #  Make a NULL list for any section that doesn't apply in this run,
        #  e.g., if a type of network metric was not computed for this problem.
        #-----------------------------------------------------------------------

    tzar_run_ID_list          = list (rsr_tzar_run_ID = tzar_run_ID,
                                      rsr_UUID = rsrun@UUID,
                                      rsr_checksum = rsrun@checksum,
                                      rsr_rand_seed = rsrun@rand_seed)

    use_src_rds_file_dir = ! (is.null (src_rds_file_dir))

    prob_characteristics_list = read_prob_characteristics_list (APP_bd_prob,     #rsprob,
                                                                src_rds_file_dir,
                                                                exp_root_dir,
                                                                use_src_rds_file_dir,
                                                                parameters)

    bipartite_measures_list   = read_bipartite_measures_list (APP_bd_prob,    #rsprob,
                                                              src_rds_file_dir,
                                                              exp_root_dir,
                                                              use_src_rds_file_dir)

    igraph_measures_list      = read_igraph_measures_list (APP_bd_prob,    #rsprob,
                                                           src_rds_file_dir,
                                                           exp_root_dir,
                                                           use_src_rds_file_dir)

        #-----------------------------------------------------------------------


        #----------------------------------------------------------------
        #  Concatenate all of the lists and write the full list to file
        #  as a data frame.
        #----------------------------------------------------------------

    results_list = c (tzar_run_ID_list,
                      prob_characteristics_list,
                      igraph_measures_list,
                      bipartite_measures_list,

                      app_cost_scores_list_wrt_COR_costs_vec,
                      app_rep_scores_list_according_to_RS,

                      cor_scores_list,
                      app_scores_list,

                      marxan_control_values
                    )

    write_results_to_files ("rsrun_results.csv",

            #as.data.frame (results_list),
            list_as_data_frame_with_0_length_vals_replaced_by_NA (results_list),

                            parameters,
                            out_dir,
                            "rsr_tzar_run_ID")
    }

#===============================================================================

