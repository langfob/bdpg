#===============================================================================

                #  gscp_15_create_master_output_structure.R

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

    cor_scores_list =
        build_and_write_scores_list (rsrun,
                                     COR_bd_prob@bpm,
                                     rs_best_solution_PU_IDs,
                                     rsrun@targets,
                                     COR_bd_prob@num_spp,
                                     rs_best_num_patches_in_solution,
                                     COR_bd_prob@num_PUs,
                                     num_patches_in_cor_solution,
                                     FP_const_rate,
                                     FN_const_rate)

    app_scores_list =
        build_and_write_scores_list (rsrun,
                                     APP_bd_prob@bpm,
                                     rs_best_solution_PU_IDs,
                                     rsrun@targets,
                                     COR_bd_prob@num_spp,
                                     rs_best_num_patches_in_solution,
                                     COR_bd_prob@num_PUs,
                                     num_patches_in_cor_solution,
                                     FP_const_rate,
                                     FN_const_rate)

    return (list (cor_scores_list=cor_scores_list,
                  app_scores_list=app_scores_list))
    }

#===============================================================================

get_gurobi_best_solution_PU_IDs <- function (rs_control_values)
    {
    return (which (rs_control_values$gurobi_solution_vector > 0))
    }

#===============================================================================

get_marxan_best_solution_PU_IDs <- function (rsrun,
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

        #----------------------------------------------------------------
        #  This call used to be part of read_marxan_output_files(), but
        #  didn't need to be in there since it returns nothing and is
        #  only called for its verification and plotting side effects.
        #----------------------------------------------------------------
        #  May want to get rid of it in the end or maybe make it more
        #  generic and run against the return of any reserve selector.
        #----------------------------------------------------------------

    find_best_marxan_solutions_and_plot_incremental_summed_solution_reps (
                                                        rsrun,
                                                        exp_root_dir,
                                                        COR_bd_prob,
                                                        APP_bd_prob,
                                                        marxan_output_values)


    return (rs_best_solution_PU_IDs)
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
        rs_best_solution_PU_IDs =
                            get_marxan_best_solution_PU_IDs (rsrun,
                                                             exp_root_dir,
                                                             COR_bd_prob,
                                                             APP_bd_prob)
        #-------------------------------------
        } else if (rs_method_name == "Gurobi")
        {
        rs_best_solution_PU_IDs =
                            get_gurobi_best_solution_PU_IDs (rs_control_values)
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

        #--------------------------------------------------------------
        #  app_rep_scores_list_according_to_RS is a list containing
        #  the following named elements:
        #    - rsr_app_spp_rep_shortfall__fromRS
        #    - rsr_app_solution_NUM_spp_covered__fromRS
        #    - rsr_app_solution_FRAC_spp_covered__fromRS
        #--------------------------------------------------------------

    app_rep_scores_list_according_to_RS =
        compute_and_verify_APP_rep_scores_according_to_RS (
                                                        rs_best_solution_PU_IDs,
                                                        COR_bd_prob@num_spp,
                                                        APP_bd_prob@bpm,
                                                        rsrun@targets,
                                                        "Gurobi")

        #------------------------------------------------------------------
        #  Compute costs and cost error measures for the chosen solution.
        #------------------------------------------------------------------

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
                                                           COR_bd_prob@correct_solution_cost,
                                                           COR_bd_prob@PU_costs)

        #-----------------------------------------------------------------------


    cor_and_app_scores_lists =
        build_and_write_COR_and_APP_scores_lists (rs_best_solution_PU_IDs,
                                                  COR_bd_prob,
                                                  APP_bd_prob,
                                                  rsrun)

    cor_scores_list = cor_and_app_scores_lists$cor_scores_list
    app_scores_list = cor_and_app_scores_lists$app_scores_list


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

    prob_characteristics_list = read_prob_characteristics_list (APP_bd_prob,
                                                                src_rds_file_dir,
                                                                exp_root_dir,
                                                                use_src_rds_file_dir)

    bipartite_measures_list   = read_bipartite_measures_list (APP_bd_prob,
                                                              src_rds_file_dir,
                                                              exp_root_dir,
                                                              use_src_rds_file_dir)

    igraph_measures_list      = read_igraph_measures_list (APP_bd_prob,
                                                           src_rds_file_dir,
                                                           exp_root_dir,
                                                           use_src_rds_file_dir)

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
                      app_rep_scores_list_according_to_RS,

                      cor_scores_list,
                      app_scores_list,

                      rs_control_values
                    )

    write_results_to_files (
        csv_outfile_name = "rsrun_results.csv",
        results_df       =
            list_as_data_frame_with_0_length_vals_replaced_by_NA (results_list),
        tzar_run_ID,
        out_dir                = get_RSrun_path_topdir (rsrun, exp_root_dir),
        tzar_run_id_field_name = "rsr_tzar_run_ID")
    }

#===============================================================================

