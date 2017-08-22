#===============================================================================

                #  gscp_15_create_master_output_structure.R

#===============================================================================

#  Build a master table containing:
    #  planning unit IDs
            #  marxan_best_df_sorted$PUID
    #  correct (optimal) answer (as boolean flags on sorted planning units)
    #  best marxan guess
            #  marxan_best_df_sorted$SOLUTION
    #  marxan number of votes for each puid
            #  marxan_ssoln_df$number
    #  difference between correct and best (i.e., (cor - best), FP, FN, etc)
    #  absolute value of difference (to make counting them easier)
    #  number of species on each patch (i.e., simple richness)

#  Need to bind together:
#   problem setup
#       - planning unit IDs (goes with all of these, even if they're split
#         into separate tables)
#       - number of species (simple richness) on patch as counts
#   correct/optimal solution
#       - correct/optimal solution as 0/1
#   marxan solution(s)
#       - marxan best solution as 0/1
#       - marxan solution votes as counts
#   performance measure(s)
#       - difference between marxan best and optimal solution to represent
#         error direction (e.g., FP, FN, etc.)
#       - abs (difference) to represent error or no error

    #  *** Need to be sure that the puid column matches in the nodes data frame
    #  and the marxan data frames.  Otherwise, there could be a mismatch in
    #  the assignments for inclusion or exclusion of patches in the solutions.

    #  Create table holding all the information to compare solutions.

#===============================================================================

save_RS_options <- function ()
    {
    results_list = list()
      # data.frame (
      #                 #  Marxan options
      #             marxan_spf_const = rep (NA, num_runs),
      #             marxan_PROP = rep (NA, num_runs),
      #             marxan_RANDSEED = rep (NA, num_runs),
      #             marxan_NUMREPS = rep (NA, num_runs),
      #
      #                 #  Marxan Annealing Parameters
      #             marxan_NUMITNS = rep (NA, num_runs),
      #             marxan_STARTTEMP = rep (NA, num_runs),
      #             marxan_NUMTEMP = rep (NA, num_runs),
      #
      #                 #  Marxan Cost Threshold
      #             marxan_COSTTHRESH = rep (NA, num_runs),
      #             marxan_THRESHPEN1 = rep (NA, num_runs),
      #             marxan_THRESHPEN2 = rep (NA, num_runs),
      #
      #                 #  Marxan Program control
      #             marxan_RUNMODE = rep (NA, num_runs),
      #             marxan_MISSLEVEL = rep (NA, num_runs),
      #             marxan_ITIMPTYPE = rep (NA, num_runs),
      #             marxan_HEURTYPE = rep (NA, num_runs),
      #             marxan_CLUMPTYPE = rep (NA, num_runs)
      #             )

    #---------------------------------------------------------------------------

        #  Marxan options

    results_list$marxan_spf_const                                                  = spf_const
    results_list$marxan_PROP                                                       = marxan_PROP
    results_list$marxan_RANDSEED                                                   = marxan_RANDSEED
    results_list$marxan_NUMREPS                                                    = marxan_NUMREPS

        #  Marxan Annealing Parameters
    results_list$marxan_NUMITNS                                                    = marxan_NUMITNS
    results_list$marxan_STARTTEMP                                                  = marxan_STARTTEMP
    results_list$marxan_NUMTEMP                                                    = marxan_NUMTEMP

        #  Marxan Cost Threshold
    results_list$marxan_COSTTHRESH                                                 = marxan_COSTTHRESH
    results_list$marxan_THRESHPEN1                                                 = marxan_THRESHPEN1
    results_list$marxan_THRESHPEN2                                                 = marxan_THRESHPEN2

        #  Marxan Program control
    results_list$marxan_RUNMODE                                                    = marxan_RUNMODE
    results_list$marxan_MISSLEVEL                                                  = marxan_MISSLEVEL
    results_list$marxan_ITIMPTYPE                                                  = marxan_ITIMPTYPE
    results_list$marxan_HEURTYPE                                                   = marxan_HEURTYPE
    results_list$marxan_CLUMPTYPE                                                  = marxan_CLUMPTYPE

    #---------------------------------------------------------------------------

    write_results_to_files (results_list,
                            parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

#===============================================================================

initialize_results_list <- function ()
    {
    results_list =
      data.frame (runset_abbrev = rep (NA, num_runs),
                  run_ID = rep (NA, num_runs),

                  exceeded_thresh_for_num_spp = rep (NA, num_runs),

                  num_PUs = rep (NA, num_runs),
                  num_spp = rep (NA, num_runs),
                  num_spp_per_PU = rep (NA, num_runs),
                  seed = rep (NA, num_runs),

                      #  Xu options
                  n__num_groups = rep (NA, num_runs),
                  alpha__ = rep (NA, num_runs),
                  p__prop_of_links_between_groups = rep (NA, num_runs),
                  r__density = rep (NA, num_runs),

                  cor_spp_rep_shortfall = rep (NA, num_runs),
                  cor_NUM_spp_covered = rep (NA, num_runs),
                  cor_FRAC_spp_covered = rep (NA, num_runs),

                  cor_TP = rep (NA, num_runs),
                  cor_TN = rep (NA, num_runs),
                  cor_FP = rep (NA, num_runs),
                  cor_FN = rep (NA, num_runs),

                  cor_cSe = rep (NA, num_runs),
                  cor_cSp = rep (NA, num_runs),
                  cor_cPPV = rep (NA, num_runs),
                  cor_cNPV = rep (NA, num_runs),

                  cor_acc_frac = rep (NA, num_runs),
                  cor_acc_err_frac = rep (NA, num_runs),
                  cor_cost_savings = rep (NA, num_runs),

                  cor_opt_cost_savings = rep (NA, num_runs),

                  cor_TSS = rep (NA, num_runs),
                  cor_max_cSe_cSp = rep (NA, num_runs),
                  cor_min_cSe_cSp = rep (NA, num_runs),
                  cor_mean_cSe_cSp = rep (NA, num_runs),
                  cor_prod_cSe_cSp = rep (NA, num_runs),
                  cor_euc_cSe_cSp = rep (NA, num_runs),
                  cor_acc_err_mag = rep (NA, num_runs),

                      #  Apparent results as computed by bdpg
                  app_spp_rep_shortfall = rep (NA, num_runs),
                  app_solution_NUM_spp_covered = rep (NA, num_runs),
                  app_solution_FRAC_spp_covered = rep (NA, num_runs),

                  app_TP = rep (NA, num_runs),
                  app_TN = rep (NA, num_runs),
                  app_FP = rep (NA, num_runs),
                  app_FN = rep (NA, num_runs),

                  app_cSe = rep (NA, num_runs),
                  app_cSp = rep (NA, num_runs),
                  app_cPPV = rep (NA, num_runs),
                  app_cNPV = rep (NA, num_runs),

                  app_acc_frac = rep (NA, num_runs),
                  app_acc_err_frac = rep (NA, num_runs),
                  app_cost_savings = rep (NA, num_runs),

                  app_opt_cost_savings = rep (NA, num_runs),

                  app_TSS = rep (NA, num_runs),
                  app_max_cSe_cSp = rep (NA, num_runs),
                  app_min_cSe_cSp = rep (NA, num_runs),
                  app_mean_cSe_cSp = rep (NA, num_runs),
                  app_prod_cSe_cSp = rep (NA, num_runs),
                  app_euc_cSe_cSp = rep (NA, num_runs),
                  app_acc_err_mag = rep (NA, num_runs),

                      #  Apparent results as computed by Marxan
                  app_spp_rep_shortfall__fromMarxan = rep (NA, num_runs),
                  app_solution_NUM_spp_covered__fromMarxan = rep (NA, num_runs),
                  app_solution_FRAC_spp_covered__fromMarxan = rep (NA, num_runs),

                      #  Error generation parameters
                  add_error = rep (NA, num_runs),
                  FP_const_rate = rep (NA, num_runs),
                  FN_const_rate = rep (NA, num_runs),
                  match_error_counts = rep (NA, num_runs),
                  original_FP_const_rate = rep (NA, num_runs),
                  original_FN_const_rate = rep (NA, num_runs),

                      #  Derived options
                  num_nodes_per_group = rep (NA, num_runs),
                  tot_num_nodes = rep (NA, num_runs),
                  num_independent_set_nodes = rep (NA, num_runs),
                  num_dependent_set_nodes = rep (NA, num_runs),
                  num_rounds_of_linking_between_groups = rep (NA, num_runs),
                  target_num_links_between_2_groups_per_round = rep (NA, num_runs),
                  num_links_within_one_group = rep (NA, num_runs),
                  tot_num_links_inside_groups = rep (NA, num_runs),
                  max_possible_num_links_between_groups = rep (NA, num_runs),
                  max_possible_tot_num_links = rep (NA, num_runs),

                      #  Marxan options
                  marxan_spf_const = rep (NA, num_runs),
                  marxan_PROP = rep (NA, num_runs),
                  marxan_RANDSEED = rep (NA, num_runs),
                  marxan_NUMREPS = rep (NA, num_runs),

                      #  Marxan Annealing Parameters
                  marxan_NUMITNS = rep (NA, num_runs),
                  marxan_STARTTEMP = rep (NA, num_runs),
                  marxan_NUMTEMP = rep (NA, num_runs),

                      #  Marxan Cost Threshold
                  marxan_COSTTHRESH = rep (NA, num_runs),
                  marxan_THRESHPEN1 = rep (NA, num_runs),
                  marxan_THRESHPEN2 = rep (NA, num_runs),

                      #  Marxan Program control
                  marxan_RUNMODE = rep (NA, num_runs),
                  marxan_MISSLEVEL = rep (NA, num_runs),
                  marxan_ITIMPTYPE = rep (NA, num_runs),
                  marxan_HEURTYPE = rep (NA, num_runs),
                  marxan_CLUMPTYPE = rep (NA, num_runs),

                      #  Full runset name
                  runset_name = rep (NA, num_runs)
                  )

    return (results_list)
    }

#===============================================================================

save_rsrun_results_data_for_one_rsrun <- function (parameters,
                                                  rsrun,

                                                        #rsprob,
                                                  COR_bd_prob,
                                                  APP_bd_prob    #,

                                                    #rs_output_dir_path,    #marxan_output_dir_path,
                                                    #rs_plot_output_dir,    #plot_output_dir,
                                                    #rs_top_dir             #,            #marxan_top_dir,

                                                        #num_spp,

                                                        #num_PUs,
                                                        #all_PU_IDs,

                                                        #cor_PU_costs,

                                                        #correct_solution_cost,
                                                    #cor_num_patches_in_solution,

                                                        #spp_rep_targets,
                                                        #targets,

                                                        #bpm,
                                                        #cor_bpm,
                                                        #app_bpm,

                                                        #marxan_ssoln_df_sorted_by_PU,
                                                        #marxan_best_num_patches_in_solution,

                                                        #FP_const_rate,
                                                        #FN_const_rate
                                                  )
    {
    tzar_run_ID  = parameters$run_id
    exp_root_dir = parameters$fullOutputDir_NO_slash
    out_dir      = get_RSrun_path_topdir (rsrun, exp_root_dir)

        #-----------------------------------------------------------------------
        #  Build or read a list for each aspect of the run.
        #  Make a NULL list for any section that doesn't apply in this run,
        #  e.g., if a type of network metric was not computed for this problem.
        #-----------------------------------------------------------------------

    tzar_run_ID_list          = list (rsr_tzar_run_ID = tzar_run_ID,
                                      rsr_UUID = rsrun@UUID,
                                      rsr_checksum = rsrun@checksum,
                                      rsr_rand_seed = rsrun@rand_seed)

    prob_characteristics_list = read_prob_characteristics_list (APP_bd_prob,     #rsprob,
                                                                exp_root_dir,
                                                                parameters)
    bipartite_measures_list   = read_bipartite_measures_list (APP_bd_prob,    #rsprob,
                                                              exp_root_dir)
    igraph_measures_list      = read_igraph_measures_list (APP_bd_prob,    #rsprob,
                                                           exp_root_dir)


        #-----------------------------------------------------------------------

    marxan_output_values =
        read_marxan_output_files (get_RSrun_path_output (rsrun, exp_root_dir),     #rs_output_dir_path,
                                  COR_bd_prob@all_PU_IDs  #all_correct_node_IDs
                                  )

        #-----------------------------------------------------------------------

# 2016 07 16 - nodes$dependent_set_member ONLY HAS THE NUMBER OF PLANNING UNITS
#           THAT WERE IN THE ORIGINAL XU PROBLEM, NOT THE WRAPPED PROBLEM.
#           MEANWHILE, THE MARXAN SOLUTION DOES HAVE THE WRAPPED PROBLEM PUs SO
#           THE LENGTHS DO NOT MATCH.  NEED TO MAKE SURE THAT EVERYTHING IN THE
#           WRAPPED PROBLEM HAS THE CORRECT DIMENSIONS AND VALUES.
#             This is part of a larger problem of making sure that the problem
#             returned by wrapping is correctly sized in every way to allow
#             subsequent operations to act on it exactly as they would act on
#             a base Xu problem.  One test of that is to make sure that all of
#             the dimensions of the object elements include all planning units
#             of the wrapped problem.  This may also be complicated by the
#             application of error to generate an apparent problem.  That means
#             you will also need to verify the problem dimensions and values
#             again, after you have generated the apparent version.
#
#           ANOTHER PROBLEM HERE IS THAT THE XU SOLUTION IS NOT NECESSARILY
#           THE ONLY CORRECT SOLUTION.  THIS MATCHING OF NODES TO A SOLUTION CAN
#           BE WRONG IF MARXAN HAS FOUND A DIFFERENT CORRECT SOLUTION.
#           NEED TO AT LEAST CHECK WHETHER
#             A) MARXAN SOLUTION IS THE CORRECT SIZE (I.E., COST)
#             AND
#             B) IF IT IS THE CORRECT SIZE, THEN YOU ALSO NEED TO CHECK THAT
#                IT REALLY DOES COVER THE SET, I.E., IT IS A CORRECT SOLUTION.


#####    cor_solution_vector = nodes$dependent_set_member
nodes = COR_bd_prob@nodes
cor_solution_vector = nodes$dependent_set_member

###2017 08 22 - BTL - I don't think this is valid anymore, so commenting in out for now.
###cat ("\n\nJUST BEFORE ERROR OCCURS:\n\n")
    cor_signed_difference         = marxan_output_values$marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
    cor_abs_val_signed_difference = abs (cor_signed_difference)

cor_num_patches_in_solution = sum (cor_solution_vector)

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

    rs_best_solution_PU_IDs = which (marxan_output_values$marxan_best_df_sorted$SOLUTION > 0)

    rs_best_num_patches_in_solution = length (rs_best_solution_PU_IDs)
    cat ("\nrs_best_num_patches_in_solution =", rs_best_num_patches_in_solution)

    app_rs_solution_summary_scores_list =
        summarize_RS_solution_scores (    #rs_best_solution_PU_IDs,
                                      cor_solution_vector,
                                      rs_best_num_patches_in_solution)

        #-----------------------------------------------------------------------

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

    cor_scores_list = build_and_write_scores_list (rsrun,
                COR_bd_prob@bpm,                         #cor_bpm,
                rs_best_solution_PU_IDs,
                rsrun@targets,                           #spp_rep_targets,
                COR_bd_prob@num_spp,
                rs_best_num_patches_in_solution,         #marxan_best_num_patches_in_solution,     #num_PUs_in_cand_solution,
                COR_bd_prob@num_PUs,
        cor_num_patches_in_solution,             #num_PUs_in_optimal_solution,
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
        cor_num_patches_in_solution,             #num_PUs_in_optimal_solution,
            FP_const_rate,
            FN_const_rate
                )

        #----------------------------------------------------------------
        #  Concatenate all of the lists and write the full list to file
        #  as a data frame.
        #----------------------------------------------------------------

    results_list = c (tzar_run_ID_list,
                      prob_characteristics_list,
                      igraph_measures_list,
                      bipartite_measures_list,

                      app_rs_solution_summary_scores_list,
                      app_rep_scores_list_according_to_RS,

                      cor_scores_list,
                      app_scores_list
                    )

    write_results_to_files ("rsrun_results.csv",
                            as.data.frame (results_list),
                            parameters,
                            out_dir,
                            "rsr_tzar_run_ID")
    }

#===============================================================================

