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

# #  2017 02 17 - BTL
# #  I don't think that this "Xu read from file" works anymore.
# #  When you go down and look at the loading of the final results dataframe,
# #  none of these variables are referenced.  In fact, none of them are ever
# #  referenced from here on, except for the 3 cor_... entries and they're only used
# #  for calculations related to when not reading from a benchmark file.
# #  The results data frame is built using the Xu parameters structures that
# #  are attached to the problem itself, not from the variables below.
# #  So, either the building of the data frame needs to change or else the
# #  creation of the problem object from a benchmark file needs to do the
# #  empty assignment stuff below as part of its initialization.
# #  However, either way leaves the 3 cor_... entries in the if section wrong.
# #  They look like they were only used in comparisons of marxan solution
# #  vectors with Xu solution vectors and that stuff never gets to the final
# #  results because the Xu solution vector might not be the only correct one
# #  and I think that all of the code and output that referenced that stuff
# #  has been removed.
# #  So, I'm going to remove this whole clause but leave it in the dead code
# #  section at the bottom of this file until I've got it all worked out.
# #
# #  Regardless of that, one important thing is to check to see how building
# #  the results data frame behaves when given a Xu problem that's read from
# #  a benchmark file.  For that matter, how does it behave when given ANY
# #  problem that lacks the Xu parameters that are currently being written
# #  to the data frame?

#---------------------------------------------------------------------

save_rsprob_results_data_for_Xu_read_from_file <-
    function (num_PUs, num_spp,
              cor_optimum_cost,
              parameters
              )
    {
    results_list = list()

    #---------------------------------------------------------------------------

    results_list$num_PUs                                 = num_PUs
    results_list$num_spp                                 = num_spp
    results_list$num_spp_per_PU                          = num_spp / num_PUs

    results_list$tot_num_nodes                           = num_PUs
    results_list$num_independent_set_nodes               = tot_num_nodes - cor_optimum_cost
    results_list$num_dependent_set_nodes                 = cor_optimum_cost

    results_list$opt_solution_as_frac_of_tot_num_nodes   = cor_optimum_cost / tot_num_nodes

    #---------------------------------------------------------------------------

    write_results_to_files (as.data.frame (results_list),
                            parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

#===============================================================================

save_rsprob_results_data_for_Xu_NOT_read_from_file <-
    function (num_PUs, num_spp,
              base_Xu_params, derived_Xu_params,
              add_error, FP_const_rate, FN_const_rate, match_error_counts,
              original_FP_const_rate, original_FN_const_rate,
              parameters
              )
    {
    results_list = list()

    #---------------------------------------------------------------------------

    results_list$num_PUs                                                           = num_PUs
    results_list$num_spp                                                           = num_spp
    results_list$num_spp_per_PU                                                    = num_spp / num_PUs
    results_list$seed                                                              = parameters$seed

        #  Xu options
    results_list$n__num_groups                                                     = base_Xu_params@n__num_groups
    results_list$alpha__                                                           = base_Xu_params@alpha__
    results_list$p__prop_of_links_between_groups                                   = base_Xu_params@p__prop_of_links_between_groups
    results_list$r__density                                                        = base_Xu_params@r__density

        #  Derived Xu options
    results_list$num_nodes_per_group                                              = derived_Xu_params@num_nodes_per_group
    results_list$tot_num_nodes                                                    = derived_Xu_params@tot_num_nodes
    results_list$num_independent_set_nodes                                        = derived_Xu_params@num_independent_set_nodes
    results_list$num_dependent_set_nodes                                          = derived_Xu_params@num_dependent_set_nodes
    results_list$num_rounds_of_linking_between_groups                             = derived_Xu_params@num_rounds_of_linking_between_groups
    results_list$target_num_links_between_2_groups_per_round                      = derived_Xu_params@target_num_links_between_2_groups_per_round
    results_list$num_links_within_one_group                                       = derived_Xu_params@num_links_within_one_group
    results_list$tot_num_links_inside_groups                                      = derived_Xu_params@tot_num_links_inside_groups
    results_list$max_possible_num_links_between_groups                            = derived_Xu_params@max_possible_num_links_between_groups
    results_list$max_possible_tot_num_links                                       = derived_Xu_params@max_possible_tot_num_links

    results_list$opt_solution_as_frac_of_tot_num_nodes                            = derived_Xu_params@opt_solution_as_frac_of_tot_num_nodes

    #---------------------------------------------------------------------------

        #  For APPARENT problem only:
            #  Error generation parameters
    results_list$add_error                                                        = add_error
    results_list$FP_const_rate                                                    = FP_const_rate
    results_list$FN_const_rate                                                    = FN_const_rate
    results_list$match_error_counts                                               = match_error_counts
    results_list$original_FP_const_rate                                           = original_FP_const_rate
    results_list$original_FN_const_rate                                           = original_FN_const_rate

    #---------------------------------------------------------------------------

    write_results_to_files (as.data.frame (results_list),
                            parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

#===============================================================================

compute_and_verify_APP_scores_according_to_RS <-
    function ()
    {
    results_list = list()

      # data.frame (
      #                 #  Apparent results as computed by bdpg
      #             app_spp_rep_shortfall = rep (NA, num_runs),
      #             app_solution_NUM_spp_covered = rep (NA, num_runs),
      #             app_solution_FRAC_spp_covered = rep (NA, num_runs)
      #             )

    #---------------------------------------------------------------------------
    #               Apparent scores as computed by marxan
    #---------------------------------------------------------------------------

    app_solution_NUM_spp_covered__fromMarxan = sum (marxan_mvbest_df$MPM)

    app_solution_FRAC_spp_covered__fromMarxan = app_solution_NUM_spp_covered__fromMarxan / num_spp
    app_spp_rep_shortfall__fromMarxan = 1 - app_solution_FRAC_spp_covered__fromMarxan

      cat ("\n\n----------------------------------")
      cat ("\nAPP_ VALUES AS COMPUTED BY MARXAN:")
      cat ("\n----------------------------------")
      cat ("\napp_solution_NUM_spp_covered__fromMarxan =", app_solution_NUM_spp_covered__fromMarxan)
      cat ("\napp_solution_FRAC_spp_covered__fromMarxan =", app_solution_FRAC_spp_covered__fromMarxan)
      cat ("\napp_spp_rep_shortfall__fromMarxan =", app_spp_rep_shortfall__fromMarxan)

    #---------------------------------------------------------------------------

        #  Apparent results as computed by Marxan
    results_list$app_spp_rep_shortfall__fromMarxan                                 = app_spp_rep_shortfall__fromMarxan
    results_list$app_solution_NUM_spp_covered__fromMarxan                          = app_solution_NUM_spp_covered__fromMarxan
    results_list$app_solution_FRAC_spp_covered__fromMarxan                         = app_solution_FRAC_spp_covered__fromMarxan

    #---------------------------------------------------------------------------

    write_results_to_files (as.data.frame (results_list),
                            parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

#===============================================================================

compute_and_verify_APP_scores_according_to_bdpg <- function ()
    {
    num_runs = 1    #  Vestigial?  Not sure it will ever be anything but 1.
                    #  2015 05 09 - BTL.

    results_list = list()
      # data.frame (
      #                 #  Apparent results as computed by bdpg
      #             app_spp_rep_shortfall = rep (NA, num_runs),
      #             app_solution_NUM_spp_covered = rep (NA, num_runs),
      #             app_solution_FRAC_spp_covered = rep (NA, num_runs),
      #
      #             app_TP = rep (NA, num_runs),
      #             app_TN = rep (NA, num_runs),
      #             app_FP = rep (NA, num_runs),
      #             app_FN = rep (NA, num_runs),
      #
      #             app_cSe = rep (NA, num_runs),
      #             app_cSp = rep (NA, num_runs),
      #             app_cPPV = rep (NA, num_runs),
      #             app_cNPV = rep (NA, num_runs),
      #
      #             app_acc_frac = rep (NA, num_runs),
      #             app_acc_err_frac = rep (NA, num_runs),
      #             app_cost_savings = rep (NA, num_runs),
      #
      #             app_opt_cost_savings = rep (NA, num_runs),
      #
      #             app_TSS = rep (NA, num_runs),
      #             app_max_cSe_cSp = rep (NA, num_runs),
      #             app_min_cSe_cSp = rep (NA, num_runs),
      #             app_mean_cSe_cSp = rep (NA, num_runs),
      #             app_prod_cSe_cSp = rep (NA, num_runs),
      #             app_euc_cSe_cSp = rep (NA, num_runs),
      #             app_acc_err_mag = rep (NA, num_runs),
      #
      #                 #  Apparent results as computed by Marxan
      #             app_spp_rep_shortfall__fromMarxan = rep (NA, num_runs),
      #             app_solution_NUM_spp_covered__fromMarxan = rep (NA, num_runs),
      #             app_solution_FRAC_spp_covered__fromMarxan = rep (NA, num_runs)
      #             )

    #---------------------------------------------------------------------------
    #               Apparent scores as computed by bdpg...
    #---------------------------------------------------------------------------

    app_results_list = compute_solution_vector_scores (bpm,
                                                     num_PUs,
                                                      marxan_best_solution_PU_IDs,
                                                      marxan_best_num_patches_in_solution,
                                                      cor_num_patches_in_solution,
                                                      spp_rep_targets,
                                                      num_spp,
                                                      DEBUG_LEVEL,
                                                      FP_const_rate,
                                                      FN_const_rate)

    app_solution_spp_rep_fracs              = app_results_list$spp_rep_fracs
    app_solution_unmet_spp_rep_frac_indices = app_results_list$indices_of_spp_with_unmet_rep_frac

    # app_solution_NUM_spp_covered            = app_results_list$num_spp_covered
    # app_solution_FRAC_spp_covered           = app_results_list$frac_spp_covered
    # app_spp_rep_shortfall                   = app_results_list$spp_rep_shortfall

    #         #  These should be the same as those computed via Marxan.
    #     app_solution_spp_rep_fracs =
    #         compute_rep_fraction (bpm, marxan_best_solution_PU_IDs, DEBUG_LEVEL, spp_rep_targets)
    #     app_solution_unmet_spp_rep_frac_indices = which (app_solution_spp_rep_fracs < 1)
    #     app_solution_NUM_spp_covered = num_spp - length (app_solution_unmet_spp_rep_frac_indices)
    #
    #     app_solution_FRAC_spp_covered = app_solution_NUM_spp_covered / num_spp
    #     app_spp_rep_shortfall = 1 - app_solution_FRAC_spp_covered

      cat ("\n\n-----------------------------------------")
      cat ("\nAPP_ VALUES AS COMPUTED BY bdpg:")
      cat ("\n-----------------------------------------")
      cat ("\nlength (app_solution_unmet_spp_rep_frac_indices) = ",
         length (app_solution_unmet_spp_rep_frac_indices))
    #     cat ("\napp_solution_NUM_spp_covered =", app_solution_NUM_spp_covered)
    #     cat ("\napp_solution_FRAC_spp_covered =", app_solution_FRAC_spp_covered)
    #     cat ("\napp_spp_rep_shortfall =", app_spp_rep_shortfall)

    #---------------------------------------------------------------------------

        #  Apparent

        #  Apparent results as computed by bdpg

    results_list$app_spp_rep_shortfall                                 = app_results_list$spp_rep_shortfall
    results_list$app_solution_NUM_spp_covered                          = app_results_list$num_spp_covered
    results_list$app_solution_FRAC_spp_covered                         = app_results_list$frac_spp_covered

    results_list$app_TP                                                = app_results_list$TP
    results_list$app_TN                                                = app_results_list$TN
    results_list$app_FP                                                = app_results_list$FP
    results_list$app_FN                                                = app_results_list$FN

    results_list$app_cSe                                               = app_results_list$cSe
    results_list$app_cSp                                               = app_results_list$cSp
    results_list$app_cPPV                                              = app_results_list$cPPV
    results_list$app_cNPV                                              = app_results_list$cNPV

    results_list$app_acc_frac                                          = app_results_list$acc_frac
    results_list$app_acc_err_frac                                      = app_results_list$acc_err_frac
    results_list$app_cost_savings                                      = app_results_list$cost_savings

    results_list$app_opt_cost_savings                                  = app_results_list$opt_cost_savings

    results_list$app_TSS                                               = app_results_list$TSS
    results_list$app_max_cSe_cSp                                       = app_results_list$max_cSe_cSp
    results_list$app_min_cSe_cSp                                       = app_results_list$min_cSe_cSp
    results_list$app_mean_cSe_cSp                                      = app_results_list$mean_cSe_cSp
    results_list$app_prod_cSe_cSp                                      = app_results_list$prod_cSe_cSp
    results_list$app_euc_cSe_cSp                                       = app_results_list$euc_cSe_cSp
    results_list$app_acc_err_mag                                       = app_results_list$acc_err_mag

    #---------------------------------------------------------------------------

    write_results_to_files (results_list, parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

#===============================================================================

compute_and_verify_COR_scores_according_to_bdpg <- function ()
    {
    results_list = list()
      # data.frame (
      #                   #  Correct results as computed by bdpg
      #               opt_solution_as_frac_of_tot_num_nodes = rep (NA, num_runs),
      #               cor_num_patches_in_solution = rep (NA, num_runs),
      #               marxan_best_num_patches_in_solution = rep (NA, num_runs),
      #               abs_marxan_best_solution_cost_err_frac = rep (NA, num_runs),
      #               marxan_best_solution_cost_err_frac = rep (NA, num_runs),
      #
      #             cor_spp_rep_shortfall = rep (NA, num_runs),
      #             cor_NUM_spp_covered = rep (NA, num_runs),
      #             cor_FRAC_spp_covered = rep (NA, num_runs),
      #
      #             cor_TP = rep (NA, num_runs),
      #             cor_TN = rep (NA, num_runs),
      #             cor_FP = rep (NA, num_runs),
      #             cor_FN = rep (NA, num_runs),
      #
      #             cor_cSe = rep (NA, num_runs),
      #             cor_cSp = rep (NA, num_runs),
      #             cor_cPPV = rep (NA, num_runs),
      #             cor_cNPV = rep (NA, num_runs),
      #
      #             cor_acc_frac = rep (NA, num_runs),
      #             cor_acc_err_frac = rep (NA, num_runs),
      #             cor_cost_savings = rep (NA, num_runs),
      #
      #             cor_opt_cost_savings = rep (NA, num_runs),
      #
      #             cor_TSS = rep (NA, num_runs),
      #             cor_max_cSe_cSp = rep (NA, num_runs),
      #             cor_min_cSe_cSp = rep (NA, num_runs),
      #             cor_mean_cSe_cSp = rep (NA, num_runs),
      #             cor_prod_cSe_cSp = rep (NA, num_runs),
      #             cor_euc_cSe_cSp = rep (NA, num_runs),
      #             cor_acc_err_mag = rep (NA, num_runs)
      #             )

    cat ("\n\nJust before things requiring major fix in gscp_15:\n")
#browser()
# biodivprobgen_utilities.R:235:    #  like nodes$dependent_set_member.
# biodivprobgen_utilities.R:238:    #       Error in marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member :
# gen_bdprob.R:738:largest_PU_ID = max (Xu_nodes$node_ID)
# generateSetCoverProblem.R:242:#all_correct_node_IDs = cor_nodes$node_ID
# generateSetCoverProblem.R:243:all_correct_node_IDs = 1:max(cor_nodes$node_ID)
# gscp_15_create_master_output_structure.R:131:2016 07 16 - nodes$dependent_set_member ONLY HAS THE NUMBER OF PLANNING UNITS
# gscp_15_create_master_output_structure.R:159:      cor_solution_vector = nodes$dependent_set_member
# gscp_15_create_master_output_structure.R:162:      cor_signed_difference = marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
#
#
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


    cor_solution_vector = nodes$dependent_set_member

cat ("\n\nJUST BEFORE ERROR OCCURS:\n\n")
    cor_signed_difference         = marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
    cor_abs_val_signed_difference = abs (cor_signed_difference)

    #---------------------------------------------------------------------------
    #               Correct scores as computed by bdpg...
    #---------------------------------------------------------------------------

    cor_results_list = compute_solution_vector_scores (cor_bpm,
                                                     num_PUs,
                                                      marxan_best_solution_PU_IDs,
                                                      marxan_best_num_patches_in_solution,
                                                      cor_num_patches_in_solution,
                                                      spp_rep_targets,
                                                      num_spp,
                                                      DEBUG_LEVEL,
                                                      FP_const_rate,
                                                      FN_const_rate)

    cor_spp_rep_fracs              = cor_results_list$spp_rep_fracs
    cor_unmet_spp_rep_frac_indices = cor_results_list$indices_of_spp_with_unmet_rep_frac

    # cor_NUM_spp_covered            = cor_results_list$num_spp_covered
    # cor_FRAC_spp_covered           = cor_results_list$frac_spp_covered
    # cor_spp_rep_shortfall                   = cor_results_list$spp_rep_shortfall

    #     cor_spp_rep_fracs =
    #         compute_rep_fraction (cor_bpm, marxan_best_solution_PU_IDs, DEBUG_LEVEL, spp_rep_targets)
    #     cor_unmet_spp_rep_frac_indices = which (cor_spp_rep_fracs < 1)
    #     cor_NUM_spp_covered = num_spp - length (cor_unmet_spp_rep_frac_indices)
    #
    #     cor_FRAC_spp_covered = cor_NUM_spp_covered / num_spp
    #     cor_spp_rep_shortfall = 1 - cor_FRAC_spp_covered

      cat ("\n\n-----------------------------------------")
      cat ("\nCOR_ VALUES AS COMPUTED BY bdpg:")
      cat ("\n-----------------------------------------")
      cat ("\nlength (cor_unmet_spp_rep_frac_indices) = ",
         length (cor_unmet_spp_rep_frac_indices))
    #     cat ("\ncor_NUM_spp_covered =", cor_NUM_spp_covered)
    #     cat ("\ncor_FRAC_spp_covered =", cor_FRAC_spp_covered)
    #     cat ("\ncor_spp_rep_shortfall =", cor_spp_rep_shortfall)

    #---------------------------------------------------------------------------

        #  Correct

        #  Correct results as computed by bdpg
    results_list$opt_solution_as_frac_of_tot_num_nodes                             = opt_solution_as_frac_of_tot_num_nodes
    results_list$cor_num_patches_in_solution                                       = cor_num_patches_in_solution

    results_list$marxan_best_num_patches_in_solution                               = marxan_best_num_patches_in_solution
    results_list$abs_marxan_best_solution_cost_err_frac                            = abs_marxan_best_solution_cost_err_frac
    results_list$marxan_best_solution_cost_err_frac                                = marxan_best_solution_cost_err_frac

    results_list$cor_spp_rep_shortfall                                 = cor_results_list$spp_rep_shortfall
    results_list$cor_NUM_spp_covered                                   = cor_results_list$num_spp_covered
    results_list$cor_FRAC_spp_covered                                  = cor_results_list$frac_spp_covered

    results_list$cor_TP                                                = cor_results_list$TP
    results_list$cor_TN                                                = cor_results_list$TN
    results_list$cor_FP                                                = cor_results_list$FP
    results_list$cor_FN                                                = cor_results_list$FN

    results_list$cor_cSe                                               = cor_results_list$cSe
    results_list$cor_cSp                                               = cor_results_list$cSp
    results_list$cor_cPPV                                              = cor_results_list$cPPV
    results_list$cor_cNPV                                              = cor_results_list$cNPV

    results_list$cor_acc_frac                                          = cor_results_list$acc_frac
    results_list$cor_acc_err_frac                                      = cor_results_list$acc_err_frac
    results_list$cor_cost_savings                                      = cor_results_list$cost_savings

    results_list$cor_opt_cost_savings                                  = cor_results_list$opt_cost_savings

    results_list$cor_TSS                                               = cor_results_list$TSS
    results_list$cor_max_cSe_cSp                                       = cor_results_list$max_cSe_cSp
    results_list$cor_min_cSe_cSp                                       = cor_results_list$min_cSe_cSp
    results_list$cor_mean_cSe_cSp                                      = cor_results_list$mean_cSe_cSp
    results_list$cor_prod_cSe_cSp                                      = cor_results_list$prod_cSe_cSp
    results_list$cor_euc_cSe_cSp                                       = cor_results_list$euc_cSe_cSp
    results_list$cor_acc_err_mag                                       = cor_results_list$acc_err_mag

    #---------------------------------------------------------------------------

    write_results_to_files (results_list, parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

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

    write_results_to_files (results_list, parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

#===============================================================================

document_solutions_df <- function ()
    {
    #  2017 05 13 - BTL
    #  solutions_df is no longer used at all, so commenting all of this part out,
    #  but don't want to get rid of it completely yet because it's useful
    #  information about what these various vectors represent.
    #  Should probably just make a more explicit bit of documentation about that.

    #  2017 05 01 - BTL
    #  Why was I creating the solutions_df?  Is this vestigial?
    #  Right now, it looks like the only column used in it is the optimal solution
    #  column and it's just a copy of cor_solution_vector.

    # {
    #   #browser()
    #   solutions_df = data.frame (puid                 = marxan_best_df_sorted$PUID,
    #                              optimal_solution     = cor_solution_vector,
    #                              marxan_best_solution = marxan_best_df_sorted$SOLUTION, #  assumes already sorted by PU_ID
    #                              marxan_votes         = marxan_ssoln_df$number,
    #                              cor_signed_diff      = cor_signed_difference,
    #                              cor_abs_val_diff     = cor_abs_val_signed_difference,
    #           #  2015 06 19 - BTL
    #           #  Until now (3:04 pm), this said:
    #           #       cor_num_spp_on_patch = final_link_counts_for_each_node$freq
    #           #  That means that it was using the apparent, not the correct,
    #           #  final link counts for each node.
    #           #  I have now changed this to say cor_... instead.
    #           #  Not sure if this was a source of unknown bugs before or will be
    #           #  a source now.  Need to look more closely at this to see the
    #           #  consequences where solutions_df$cor_num_spp_on_patch is used
    #           #  downstream of here.  However, since this is the only place it
    #           #  appears in this file and this file (gscp_15) is at the end of
    #           #  the whole program, maybe it doesn't matter at all except being
    #           #  echoed in some output file.
    #                              cor_num_spp_on_patch = cor_final_link_counts_for_each_node$freq
    #                              )
    #
    # }
    }

#===============================================================================

summarize_RS_solution_features <- function ()
    {

    #---------------------------------------------------------------------------
    #               Summarize marxan solution features.
    #---------------------------------------------------------------------------

    cor_num_patches_in_solution = sum (cor_solution_vector)

      #cor_num_patches_in_solution = cor_optimum_cost    #  assuming cost = number of patches
      cat ("\n\ncor_num_patches_in_solution =", cor_num_patches_in_solution)
      #  Find which PUs marxan chose for its best solution.
    marxan_best_solution_PU_IDs = which (marxan_best_df_sorted$SOLUTION > 0)

      #  Compute error in cost of best marxan solution.
      #  Assumes equal cost for all patches, i.e., cost per patch = 1.
    marxan_best_num_patches_in_solution = length (marxan_best_solution_PU_IDs)
      cat ("\nmarxan_best_num_patches_in_solution =", marxan_best_num_patches_in_solution)
    marxan_best_solution_cost_err_frac = (marxan_best_num_patches_in_solution - cor_num_patches_in_solution) / cor_num_patches_in_solution
    abs_marxan_best_solution_cost_err_frac = abs (marxan_best_solution_cost_err_frac)
      cat ("\nmarxan_best_solution_cost_err_frac =", marxan_best_solution_cost_err_frac)
      cat ("\nabs_marxan_best_solution_cost_err_frac =", abs_marxan_best_solution_cost_err_frac)
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

create_master_output_structure <-
        function (read_Xu_problem_from_Xu_file,

                    Xu_parameters,  #  most param values for problem

                    num_PUs,
                    num_spp,
                    cor_optimum_cost,
                    nodes,
                    cor_final_link_counts_for_each_node,
                    bpm,
                    DEBUG_LEVEL,
                    cor_bpm,
                    parameters,
                    add_error,
                    match_error_counts,
                    FP_const_rate,
                    FN_const_rate,
                    original_FP_const_rate,
                    original_FN_const_rate,
                    spf_const,
                    bipartite_metrics_from_bipartite_package,
                    bipartite_metrics_from_igraph_package_df,

                    marxan_best_df_sorted,
                    marxan_ssoln_df,
                    marxan_mvbest_df,
                    marxan_PROP,
                    marxan_RANDSEED,
                    marxan_NUMREPS,
                    marxan_NUMITNS,
                    marxan_STARTTEMP,
                    marxan_NUMTEMP,
                    marxan_COSTTHRESH,
                    marxan_THRESHPEN1,
                    marxan_THRESHPEN2,
                    marxan_RUNMODE,
                    marxan_MISSLEVEL,
                    marxan_ITIMPTYPE,
                    marxan_HEURTYPE,
                    marxan_CLUMPTYPE
                    )
{
derived_Xu_params    = Xu_parameters@derived_params
base_Xu_params       = Xu_parameters@base_params
bdpg_extended_params = Xu_parameters@bdpg_extended_params

#-------------------------------------------------------------------------------
#      Initialize the data frame holding correct and apparent solutions.
#-------------------------------------------------------------------------------

      #---------------------------------------------------------------------
      #  Need to separate the case of reading a Xu problem from one of his
      #  his benchmark files vs. generating a problem from scratch.
      #  When you read it from a benchmark file, the correct solution cost
      #  is known, but not the correct solution vector.
      #  So, when reading a problem from a benchmark file, initialize all
      #  kinds of things to NA.
      #---------------------------------------------------------------------

  if (read_Xu_problem_from_Xu_file)
      {
      save_rsprob_results_data_for_Xu_read_from_file ()

      } else  #  generated the problem
      {
      save_rsprob_results_data_for_Xu_NOT_read_from_file ()
      }

  #===============================================================================
  #       Compute correct and apparent scores for marxan solution.
  #===============================================================================

  cat ("\n\nnum_spp =", num_spp)
  spp_rep_targets = rep (1, num_spp)    #  Seems like this should already have been set long before now.
                                        #  In fact, they are now a variable (called "targets") inside of the RSrun class structure.
                                        #  Are they ever set there?
                                        #  ***  Actually, shouldn't targets be part of the RSprob instead of RSrun,
                                        #       i.e., the targets are part of what makes a problem hard or easy.
                                        #       They're also a basic structuring assumption in building the Xu
                                        #       problems.  ***

    document_solutions_df ()
    summarize_RS_solution_features ()
    compute_and_verify_APP_scores_according_to_RS ()
    compute_and_verify_APP_scores_according_to_bdpg ()
    compute_and_verify_COR_scores_according_to_bdpg ()

  #===============================================================================

  #  Supporting data not in binding
  #   species vs planning units (database?) to allow computation of performance
  #   measures related to which species are covered in solutions
  #   (e.g., SELECT species WHERE planning unit ID == curPlanningUnitID)
  #   (e.g., SELECT planning unit ID WHERE species == curSpeciesID))
  #       - planning unit IDs
  #       - set of species on planning unit

  #     ALSO STILL NEED TO ADD THE FP AND FN RATES AND OTHER ERROR MODEL
  #     ATTRIBUTES TO THE OUTPUT DATA FRAME AND CSV FILE.
  #             2017 05 01 - Is this still true?

  #-------------------------------------------------------------------------------

  num_runs = 1    #  Vestigial?  Not sure it will ever be anything but 1.
                  #  2015 05 09 - BTL.

  results_list <- initialize_results_list ()

  cur_result_row = 0

  #-------------------------------------------------------------------------------

  cur_result_row = cur_result_row + 1

      #  Filling in the runset_abbrev with the full runset name for the moment,
      #  because it's more reliably correct since it's automatically captured
      #  by tzar.  Not sure what I'll do in the long run.
      #  2015 03 09 - BTL
  results_list$runset_abbrev                                                     = parameters$runset_name    #  parameters$runset_abbrev
  results_list$runset_name                                                       = parameters$runset_name

  results_list$exceeded_thresh_for_num_spp                                       = FALSE

  #-------------------------------------------------------------------------------

    compute_and_verify_APP_scores_according_to_bdpg ()
    compute_and_verify_COR_scores_according_to_bdpg ()
    compute_and_verify_APP_scores_according_to_RS ()

    save_RS_options ()

  #-------------------------------------------------------------------------------

  #  Getting an error.  Not sure why...  Is it because the free variable names
  #  like num_PUs, are the same as the list element names, like results_list$num_PUs?
  #
  #  Error in `$<-.data.frame`(`*tmp*`, "num_PUs", value = c(NA, 12L)) :
  #    replacement has 2 rows, data has 1
  #  Calls: source ... withVisible -> eval -> eval -> $<- -> $<-.data.frame
  #  Execution halted


  if (parameters$compute_network_metrics)
      {
          #  Need to bind the network measures to the data frame too.
#  2017 05 01 - BTL
#  Are these graph metrics only computed over the correct problem or
#  only over the apparent problem?
#  Seems like both may be important to know for prediction.
      results_list = cbind (results_list,
                          bipartite_metrics_from_bipartite_package,
                          bipartite_metrics_from_igraph_package_df
                          )
      }

  write_results_to_files (results_list, parameters,
                          cur_result_row)    #  Added 2016 03 28 - BTL.
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
#===============================================================================
#===============================================================================

{

        #  OLD CODE FOR COR - FROM BDPROBDIFF.
        #  This is just here to give context for how create_master...()
        #  was called, particularly, in terms of what the arguments were.

# #===============================================================================
# #                   Save the values for the "correct" problem.
# #===============================================================================
#
#     #  The problem structures built so far represent the correct values.
#     #  Adding error to the problem structure will create an apparent
#     #  problem structure that is probably different from the correct
#     #  structure.
#     #  When we compute scores at the end of all this, we need to compute
#     #  them with respect to the correct problem rather than the apparent.
#     #  So, before we add error, we need to save the values defining the
#     #  correct structure.
#
# cor_PU_spp_pair_indices = bdprob@PU_spp_pair_indices
# cor_bpm                 = bdprob@bpm
# cor_num_PUs             = bdprob@num_PUs
# cor_num_spp             = bdprob@num_spp
# cor_nodes               = bdprob@nodes
# cor_optimum_cost        = bdprob@cor_optimum_cost  #  BUG?  HOW IS THIS LOADED FOR XU FROM FILE?
# cor_PU_costs            = bdprob@PU_costs
# PU_col_name             = bdprob@PU_col_name
# spp_col_name            = bdprob@spp_col_name
#
# cor_PU_IDs              = bdprob@all_PU_IDs
# cor_spp_IDs             = bdprob@all_spp_IDs
#
# cat ("\n\nAbout to set all_correct_node_IDs.\n")
# #browser()
#     #  Some nodes may be unusued in cor_nodes, particularly if building a
#     #  compound problem, e.g., if wrapping a distribution around a Xu problem.
#     #  Need to add them into this list since it will be used to index array
#     #  entries, you can't have any missing indices.
# #all_correct_node_IDs = cor_nodes$node_ID
# all_correct_node_IDs = 1:max(cor_nodes$node_ID)
#
# presences_col_name = "freq"
#
# #===============================================================================
#
# cor_or_app_subdir_name = "cor"
#
#         #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
#         #                       reading Xu file, e.g., prob_generator_params_known.
#         #                       This is because learning alg downstream needs to
#         #                       know things like whether the generator's params
#         #                       are even known, so that it doesn't try to learn
#         #                       something from missing data.
#
# CALL TO do_graph...().  DEFN OF do_graph...() IS GIVEN BELOW.
# do_graph_and_marxan_analysis (cor_or_app_subdir_name,
#
#                                             #  input parameters
#                                           parameters,
#                                           emulatingTzar,
#                                           DEBUG_LEVEL,
#                                           #derived_bdpg_parameters,   #  BUG?  UNKNOWN FOR XU FROM FILE?
#                                           derived_bdpg_parameters$current_os,
#
#                                             #  parameters from gen prob.
# #                                          bdprob$derived_Xu_params,
#                                           bdprob@Xu_parameters,
#                                           bdprob@read_Xu_problem_from_Xu_file,
#
# #            PU_spp_pair_names,  #NO
#
#                                             #  From bdprob structure, i.e., results of gen prob routine
#                                           cor_num_spp,
#                                           cor_num_PUs,
#                                           cor_PU_spp_pair_indices,
#     cor_PU_IDs, #####!!!!!#####
#     cor_spp_IDs,  #####!!!!!#####
#                                           cor_bpm,
#
#                             cor_PU_costs,
#                                           cor_optimum_cost,
#                                           cor_nodes,
#                                           spp_col_name,
#                                           PU_col_name,
#
#                                             #  Immediately after bdprob struct vars above.
#                                           presences_col_name, #  hard-coded as "freq"
# #####!!!!!#####                                          all_correct_node_IDs,
#
#                                             #  Results of adding error.
#                                             cor_num_spp,
#                                             cor_num_PUs,
#                                           cor_PU_spp_pair_indices,
#                                           cor_bpm,
#
#                                             #  input parameters for error model.
#                                           add_error=FALSE,
#                                           match_error_counts=FALSE,
#                                           FP_const_rate=0,
#                                           FN_const_rate=0,
#                                           original_FP_const_rate=0,
#                                           original_FN_const_rate=0
#                                           )


# DEFN OF do_graph...().  CALL TO do_graph...() IS GIVEN ABOVE.
# #===============================================================================
#
#         #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
#         #                       reading Xu file, e.g., prob_generator_params_known.
#         #                       This is because learning alg downstream needs to
#         #                       know things like whether the generator's params
#         #                       are even known, so that it doesn't try to learn
#         #                       something from missing data.
#
# do_graph_and_marxan_analysis <- function (cor_or_app_subdir_name,
#
#                                             #  input parameters
#                                           parameters,
#                                           emulatingTzar,
#                                           DEBUG_LEVEL,
#
#                                           current_os,   #### derived_bdpg_parameters$current_os
#                                           #derived_bdpg_parameters,  # only used to get dirs and current_os, so should separate those out !!
#
#                                             #  parameters from gen prob.
#                                             #  They're only used in creating
#                                             #  the master output structure.
#                                           Xu_parameters,
#                                           read_Xu_problem_from_Xu_file,
#
#                                             #  From bdprob structure, i.e., results of gen prob routine
#                                           cor_num_spp,
#                                           cor_num_PUs,
#                                           cor_PU_spp_pair_indices,
#     cor_PU_IDs, #####!!!!!#####
#     cor_spp_IDs,  #####!!!!!#####
#                                           cor_bpm,
#
#                             cor_PU_costs,
#                                           cor_optimum_cost,
#                                           cor_nodes,
#                                           spp_col_name,
#                                           PU_col_name,
#
#                                             #  Immediately after bdprob struct vars above.
#                                           presences_col_name, #  hard-coded as "freq"
# #####!!!!!#####                                          all_correct_node_IDs,
#
#                                             #  Results of adding error.
#                                             app_num_spp,
#                                             app_num_PUs,
#                                           app_PU_spp_pair_indices,
#                                           app_bpm,
#
#                                             #  input parameters for error model.
#                                             #  Only used to create master output structure?
#                                           add_error,
#                                           match_error_counts,
#                                           FP_const_rate,
#                                           FN_const_rate,
#                                           original_FP_const_rate,
#                                           original_FN_const_rate
#                                           )
# {
# derived_bdpg_dir_names = create_dir_structure (cor_or_app_subdir_name)
# ...
# }




create_COR_master_output_structure <- function (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        COR_marxan_run
                                        )
{
#xxxxx
#===============================================================================
#                   Dump all of the different kinds of results.
#===============================================================================


        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to
        #                       know things like whether the generator's params
        #                       are even known, so that it doesn't try to learn
        #                       something from missing data.

  create_master_output_structure (
                          read_Xu_problem_from_Xu_file,
                          Xu_parameters,

              #  These used to say just num_spp and num_PUs.
              #  For the moment, I'm going to make them cor_...
              #  since there is neither num_spp nor app_num_spp, etc.
              #  This needs fixing though.
                                        #num_PUs,  #  cor_num_PUs?  app_num_PUs?
                                        #num_spp,  #  cor_num_spp?  app_num_spp?
                          cor_num_PUs,  #  cor_num_PUs?  app_num_PUs?
                          cor_num_spp,  #  cor_num_spp?  app_num_spp?

                          cor_optimum_cost,

              #  Using cor_nodes for now
                                      #nodes,  #  cor_nodes?  app_nodes?
                          cor_nodes,

                          cor_final_link_counts_for_each_node,
                          app_bpm,
                          DEBUG_LEVEL,
                          cor_bpm,
                          parameters,
                          add_error,
                          match_error_counts,
                          FP_const_rate,
                          FN_const_rate,
                          original_FP_const_rate,
                          original_FN_const_rate,

                          marxan_control_values$spf_const,
  #                        spf_const,

                          app_bipartite_metrics_from_bipartite_package,
                          app_bipartite_metrics_from_igraph_package_df,

                          app_marxan_output_values$marxan_best_df_sorted,
                          app_marxan_output_values$marxan_ssoln_df,
                          app_marxan_output_values$marxan_mvbest_df,

                          marxan_control_values$marxan_PROP,
                          marxan_control_values$marxan_RANDSEED,
                          marxan_control_values$marxan_NUMREPS,
                          marxan_control_values$marxan_NUMITNS,
                          marxan_control_values$marxan_STARTTEMP,
                          marxan_control_values$marxan_NUMTEMP,
                          marxan_control_values$marxan_COSTTHRESH,
                          marxan_control_values$marxan_THRESHPEN1,
                          marxan_control_values$marxan_THRESHPEN2,
                          marxan_control_values$marxan_RUNMODE,
                          marxan_control_values$marxan_MISSLEVEL,
                          marxan_control_values$marxan_ITIMPTYPE,
                          marxan_control_values$marxan_HEURTYPE,
                          marxan_control_values$marxan_CLUMPTYPE
                                              )


#xxxxx
    }

#===============================================================================

        #  OLD CODE FOR APP - FROM BDPROBDIFF.
        #  This is just here to give context for how create_master...()
        #  was called, particularly, in terms of what the arguments were.

# #===============================================================================
# #                   Add error to the species occupancy data.
# #===============================================================================
#
# add_error = FALSE
# if (! is.null (parameters$add_error_to_spp_occupancy_data))
#     add_error = parameters$add_error_to_spp_occupancy_data
#
# if (add_error)
#     {
#     ret_vals_from_add_errors =
#         add_error_to_spp_occupancy_data (parameters, cor_bpm,
#                                          cor_num_PU_spp_pairs,
#                                          cor_num_PUs, cor_num_spp,
#                                          bdpg_error_codes)
#
#         #  Save the chosen error parameters to output later with results.
#     original_FP_const_rate = ret_vals_from_add_errors$original_FP_const_rate
#     original_FN_const_rate = ret_vals_from_add_errors$original_FN_const_rate
#     match_error_counts     = ret_vals_from_add_errors$match_error_counts
#     FP_const_rate          = ret_vals_from_add_errors$FP_const_rate
#     FN_const_rate          = ret_vals_from_add_errors$FN_const_rate
#     app_num_spp            = ret_vals_from_add_errors$app_num_spp
#     app_num_PUs            = ret_vals_from_add_errors$app_num_PUs
#
#         #  Set the values for the apparent problem structure.
#     app_PU_spp_pair_indices      = ret_vals_from_add_errors$app_PU_spp_pair_indices
#     app_bpm                      = ret_vals_from_add_errors$app_spp_occupancy_data
#
# #=================================
#
#         #  Create subdirectory name for this apparent problem.
#         #  In the future, we may allow more than 1 app per cor, so
#         #  I'll add an app count to the end of the subdirectory name and
#         #  nest it under a more general "app" directory that corresponds to
#         #  the "cor" directory.
#
#     cur_app_num = 1
#     cor_or_app_subdir_name = paste0 ("app", .Platform$file.sep, "app.", cur_app_num)
#
#     do_graph_and_marxan_analysis (cor_or_app_subdir_name,
#
#                                                 #  input parameters
#                                               parameters,
#                                               emulatingTzar,
#                                               DEBUG_LEVEL,
#                                               #derived_bdpg_parameters,   #  BUG?  UNKNOWN FOR XU FROM FILE?
#                                               derived_bdpg_parameters$current_os,
#
#                                                 #  parameters from gen prob.
# #                                              bdprob$derived_Xu_params,
#                                               bdprob@Xu_parameters,
#                                               bdprob@read_Xu_problem_from_Xu_file,
#
#     #            PU_spp_pair_names,  #NO
#
#                                                 #  From bdprob structure, i.e., results of gen prob routine
#                                               cor_num_spp,
#                                               cor_num_PUs,
#                                               cor_PU_spp_pair_indices,
#     cor_PU_IDs, #####!!!!!#####
#     cor_spp_IDs,  #####!!!!!#####
#                                               cor_bpm,
#
#                             cor_PU_costs,
#                                               cor_optimum_cost,
#                                               cor_nodes,
#                                               spp_col_name,
#                                               PU_col_name,
#
#                                                 #  Immediately after bdprob struct vars above.
#                                               presences_col_name, #  hard-coded as "freq"
# #####!!!!!#####                                              all_correct_node_IDs,
#
#                                                 #  Results of adding error.
#                                                 app_num_spp,
#                                                 app_num_PUs,
#                                               app_PU_spp_pair_indices,
#                                               app_bpm,
#
#                                                 #  input parameters for error model.
#                                               add_error,
#                                               match_error_counts,
#                                               FP_const_rate,
#                                               FN_const_rate,
#                                               original_FP_const_rate,
#                                               original_FN_const_rate
#                                               )
#     } #####else    #  Don't add error.
# #     {         #  Since no error is being added, correct and apparent are the same.
# #
# #     app_PU_spp_pair_indices      = cor_PU_spp_pair_indices
# #     app_bpm                      = cor_bpm
# #     }

#-------------------------------------------------------------------------------

    #----------------------------
    #  Inputs to create master:
    #----------------------------

        #  Problem generator parameters

# Xu_parameters

        #  Reserve selector inputs and outputs

    #  Need to make these work for something other than just marxan_sa,
    #  so that it's easy to plug in zonation emulator and/or linear programming.
    #  To do that, need to look more closely at exactly the questions that
    #  you want to ask at the end, so that you know which values are crucial.
    #  You'll still have to learn separate predictors for each kind of
    #  reserve selector because input controls like marxan's number of of
    #  iterations affect output error.
    #  Still, another interesting question could be what you're able to predict
    #  across all different reserve selectors, i.e., are some problems harder
    #  than others for everyone?  That might be phrased as error(prob_A) >
    #  error(prob_B) for all reserve selectors...

# marxan_control_values
# app_marxan_output_values

        #  Network measures

# app_bipartite_metrics_from_bipartite_package
# app_bipartite_metrics_from_igraph_package_dfy

        #  Output scores

# ???

#-------------------------------------------------------------------------------

                                        #  Guessing at these args for now...
create_APP_master_output_structure <- function (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        APP_bd_prob,
                                        APP_marxan_run
                                        )
{
}
}

#===============================================================================




