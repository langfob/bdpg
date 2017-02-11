#===============================================================================

                #  gscp_15_create_master_output_structure.R

#===============================================================================

create_COR_master_output_structure <- function (COR_bd_prob,
                                                marxan_control_values,
                                                marxan_output_values
                                                )
    {
    return (
            create_master_output_structure (

                    read_Xu_problem_from_Xu_file        = COR_bd_prob@prob_gen_info@read_Xu_problem_from_Xu_file,
                    Xu_parameters                       = COR_bd_prob@prob_gen_info@Xu_parameters,
                    num_PUs                             = COR_bd_prob@num_PUs,
                    num_spp                             = COR_bd_prob@num_spp,
                    correct_solution_cost               = COR_bd_prob@correct_solution_cost,
                    nodes                               = COR_bd_prob@nodes,
                    cor_link_counts_for_each_node       = COR_bd_prob@final_link_counts_for_each_node,
                    bpm                                 = COR_bd_prob@bpm,
                    cor_bpm                             = COR_bd_prob@bpm,
                    parameters                          = parameters,

                        #  input parameters for error model.
                    apply_error             = FALSE,
                    match_error_counts      = FALSE,
                    FP_const_rate           = 0,
                    FN_const_rate           = 0,
                    original_FP_const_rate  = 0,
                    original_FN_const_rate  = 0,

                    spf_const = marxan_control_values$spf_const,

                    bipartite_metrics_from_bipartite_package = COR_bd_prob@bipartite_metrics_from_bipartite_package,
                    bipartite_metrics_from_igraph_package_df = COR_bd_prob@bipartite_metrics_from_igraph_package_df,

                    marxan_best_df_sorted = marxan_output_values$marxan_best_df_sorted,
                    marxan_ssoln_df       = marxan_output_values$marxan_ssoln_df,
                    marxan_mvbest_df      = marxan_output_values$marxan_mvbest_df,

                    marxan_PROP       = marxan_control_values$marxan_PROP,
                    marxan_RANDSEED   = marxan_control_values$marxan_RANDSEED,
                    marxan_NUMREPS    = marxan_control_values$marxan_NUMREPS,
                    marxan_NUMITNS    = marxan_control_values$marxan_NUMITNS,
                    marxan_STARTTEMP  = marxan_control_values$marxan_STARTTEMP,
                    marxan_NUMTEMP    = marxan_control_values$marxan_NUMTEMP,
                    marxan_COSTTHRESH = marxan_control_values$marxan_COSTTHRESH,
                    marxan_THRESHPEN1 = marxan_control_values$marxan_THRESHPEN1,
                    marxan_THRESHPEN2 = marxan_control_values$marxan_THRESHPEN2,
                    marxan_RUNMODE    = marxan_control_values$marxan_RUNMODE,
                    marxan_MISSLEVEL  = marxan_control_values$marxan_MISSLEVEL,
                    marxan_ITIMPTYPE  = marxan_control_values$marxan_ITIMPTYPE,
                    marxan_HEURTYPE   = marxan_control_values$marxan_HEURTYPE,
                    marxan_CLUMPTYPE  = marxan_control_values$marxan_CLUMPTYPE
                    )
            )
    }

#===============================================================================

create_APP_master_output_structure <- function (APP_bd_prob,
                                                COR_bd_prob,
                                                marxan_control_values,
                                                marxan_output_values
                                                )
    {
    return (
            create_master_output_structure (

                    read_Xu_problem_from_Xu_file        = COR_bd_prob@prob_gen_info@read_Xu_problem_from_Xu_file,
                    Xu_parameters                       = COR_bd_prob@prob_gen_info@Xu_parameters,
                    num_PUs                             = COR_bd_prob@num_PUs,
                    num_spp                             = COR_bd_prob@num_spp,
                    correct_solution_cost               = COR_bd_prob@correct_solution_cost,
                    nodes                               = COR_bd_prob@nodes,

                    cor_link_counts_for_each_node = COR_bd_prob@final_link_counts_for_each_node,  #  See 3:04 pm note below
                    bpm                                 = APP_bd_prob@bpm,
                    cor_bpm                             = COR_bd_prob@bpm,
                    parameters                          = parameters,

                        #  input parameters for error model.
                    apply_error             = TRUE,    # FALSE,
                    match_error_counts      = APP_bd_prob@APP_prob_info@match_error_counts,    # FALSE,
                    FP_const_rate           = APP_bd_prob@APP_prob_info@FP_const_rate,    # 0,
                    FN_const_rate           = APP_bd_prob@APP_prob_info@FN_const_rate,    # 0,
                    original_FP_const_rate  = APP_bd_prob@APP_prob_info@original_FP_const_rate,    # 0,
                    original_FN_const_rate  = APP_bd_prob@APP_prob_info@original_FN_const_rate,    # 0,

                    spf_const = marxan_control_values$spf_const,

        bipartite_metrics_from_bipartite_package = APP_bd_prob@bipartite_metrics_from_bipartite_package,
        bipartite_metrics_from_igraph_package_df = APP_bd_prob@bipartite_metrics_from_igraph_package_df,

                    marxan_best_df_sorted = marxan_output_values$marxan_best_df_sorted,
                    marxan_ssoln_df       = marxan_output_values$marxan_ssoln_df,
                    marxan_mvbest_df      = marxan_output_values$marxan_mvbest_df,

                    marxan_PROP       = marxan_control_values$marxan_PROP,
                    marxan_RANDSEED   = marxan_control_values$marxan_RANDSEED,
                    marxan_NUMREPS    = marxan_control_values$marxan_NUMREPS,
                    marxan_NUMITNS    = marxan_control_values$marxan_NUMITNS,
                    marxan_STARTTEMP  = marxan_control_values$marxan_STARTTEMP,
                    marxan_NUMTEMP    = marxan_control_values$marxan_NUMTEMP,
                    marxan_COSTTHRESH = marxan_control_values$marxan_COSTTHRESH,
                    marxan_THRESHPEN1 = marxan_control_values$marxan_THRESHPEN1,
                    marxan_THRESHPEN2 = marxan_control_values$marxan_THRESHPEN2,
                    marxan_RUNMODE    = marxan_control_values$marxan_RUNMODE,
                    marxan_MISSLEVEL  = marxan_control_values$marxan_MISSLEVEL,
                    marxan_ITIMPTYPE  = marxan_control_values$marxan_ITIMPTYPE,
                    marxan_HEURTYPE   = marxan_control_values$marxan_HEURTYPE,
                    marxan_CLUMPTYPE  = marxan_control_values$marxan_CLUMPTYPE
                    )
            )
    }

#===============================================================================

#  Build a master table containing:
    #  planning unit ID
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

create_master_output_structure <- function (
                                            read_Xu_problem_from_Xu_file,
                                            Xu_parameters,
                                            num_PUs,
                                            num_spp,
                                            correct_solution_cost,
                                            nodes,
                                            cor_link_counts_for_each_node,
                                            bpm,
                                            cor_bpm,
                                            parameters,
                                            apply_error,
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
      correct_solution_vector = rep (NA, num_PUs)
      cor_signed_difference = rep (NA, num_PUs)
      cor_abs_val_signed_difference = rep (NA, num_PUs)

              #  Xu options
      n__num_groups = NA
      alpha__ = NA
      p__prop_of_links_between_groups = NA
      r__density = NA

          #  Derived Xu options
      num_nodes_per_group = NA
      tot_num_nodes = num_PUs
      num_independent_set_nodes = tot_num_nodes - correct_solution_cost
      num_dependent_set_nodes = correct_solution_cost
      num_rounds_of_linking_between_groups = NA
      target_num_links_between_2_groups_per_round = NA
      num_links_within_one_group = NA
      tot_num_links_inside_groups = NA
      max_possible_num_links_between_groups = NA
      max_possible_tot_num_links = NA

      opt_solution_as_frac_of_tot_num_nodes = correct_solution_cost / tot_num_nodes

      } else  #  generated the problem
      {



cat ("\n\nJust before things requiring major fix in gscp_15:\n")
#browser()
# biodivprobgen_utilities.R:235:    #  like nodes$dependent_set_member.
# biodivprobgen_utilities.R:238:    #       Error in marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member :
# gen_bdprob.R:738:largest_PU_ID = max (Xu_nodes$node_ID)
# generateSetCoverProblem.R:242:#all_correct_node_IDs = cor_nodes$node_ID
# generateSetCoverProblem.R:243:all_correct_node_IDs = 1:max(cor_nodes$node_ID)
# gscp_15_create_master_output_structure.R:131:2016 07 16 - nodes$dependent_set_member ONLY HAS THE NUMBER OF PLANNING UNITS
# gscp_15_create_master_output_structure.R:159:      correct_solution_vector = nodes$dependent_set_member
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





      correct_solution_vector = nodes$dependent_set_member
cat ("\n\nJUST BEFORE ERROR OCCURS:\n\n")
#browser()
      cor_signed_difference = marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
      cor_abs_val_signed_difference = abs (cor_signed_difference)

#      opt_solution_as_frac_of_tot_num_nodes = Xu_parameters$opt_solution_as_frac_of_tot_num_nodes
      opt_solution_as_frac_of_tot_num_nodes = derived_Xu_params@opt_solution_as_frac_of_tot_num_nodes

      }
  #browser()
  solutions_df = data.frame (puid = marxan_best_df_sorted$PUID,
                             optimal_solution = correct_solution_vector,
                             marxan_best_solution = marxan_best_df_sorted$SOLUTION, #  assumes already sorted by PU_ID
                             marxan_votes = marxan_ssoln_df$number,
                             cor_signed_diff = cor_signed_difference,
                             cor_abs_val_diff = cor_abs_val_signed_difference,
          #  2015 06 19 - BTL
          #  Until now (3:04 pm), this said:
          #       cor_num_spp_on_patch = final_link_counts_for_each_node$freq
          #  That means that it was using the apparent, not the correct,
          #  final link counts for each node.
          #  I have now changed this to say cor_... instead.
          #  Not sure if this was a source of unknown bugs before or will be
          #  a source now.  Need to look more closely at this to see the
          #  consequences where solutions_df$cor_num_spp_on_patch is used
          #  downstream of here.  However, since this is the only place it
          #  appears in this file and this file (gscp_15) is at the end of
          #  the whole program, maybe it doesn't matter at all except being
          #  echoed in some output file.
          #  2017 02 11 - BTL
          #  cor_final_counts_for_each_node seems to be the wrong name all over.
          #  Trying cor_link_counts_for_each_node now.
          #  All of this definitely needs some checking into...
#                             cor_num_spp_on_patch = cor_final_counts_for_each_node$freq
                             cor_num_spp_on_patch = cor_link_counts_for_each_node$freq
                             )

  cor_num_patches_in_solution = sum (solutions_df$optimal_solution)
      #cor_num_patches_in_solution = correct_solution_cost    #  assuming cost = number of patches
      cat ("\n\ncor_num_patches_in_solution =", cor_num_patches_in_solution)

  #---------------------------------------------------------------------------
  #               Summarize marxan solution features.
  #---------------------------------------------------------------------------

      #  Find which PUs marxan chose for its best solution.
  marxan_best_solution_PU_IDs = which (marxan_best_df_sorted$SOLUTION > 0)
  marxan_best_num_patches_in_solution = length (marxan_best_solution_PU_IDs)
      cat ("\nmarxan_best_num_patches_in_solution =", marxan_best_num_patches_in_solution)

      #  Compute error in cost of best marxan solution.
      #  Assumes equal cost for all patches, i.e., cost per patch = 1.
  marxan_best_solution_cost_err_frac = (marxan_best_num_patches_in_solution - cor_num_patches_in_solution) / cor_num_patches_in_solution
  abs_marxan_best_solution_cost_err_frac = abs (marxan_best_solution_cost_err_frac)
      cat ("\nmarxan_best_solution_cost_err_frac =", marxan_best_solution_cost_err_frac)
      cat ("\nabs_marxan_best_solution_cost_err_frac =", abs_marxan_best_solution_cost_err_frac)

  #===============================================================================
  #       Compute correct and apparent scores for marxan solution.
  #===============================================================================

  cat ("\n\nnum_spp =", num_spp)
  spp_rep_targets = rep (1, num_spp)    #  Seems like this should already have been set long before now.

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
  #               Apparent scores as computed by biodivprobgen...
  #---------------------------------------------------------------------------

  app_results_list = compute_solution_vector_scores (bpm,
                                                     num_PUs,
                                                      marxan_best_solution_PU_IDs,
                                                      marxan_best_num_patches_in_solution,
                                                      cor_num_patches_in_solution,
                                                      spp_rep_targets,
                                                      num_spp,
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
      cat ("\nAPP_ VALUES AS COMPUTED BY BIODIVPROBGEN:")
      cat ("\n-----------------------------------------")
      cat ("\nlength (app_solution_unmet_spp_rep_frac_indices) = ",
         length (app_solution_unmet_spp_rep_frac_indices))
  #     cat ("\napp_solution_NUM_spp_covered =", app_solution_NUM_spp_covered)
  #     cat ("\napp_solution_FRAC_spp_covered =", app_solution_FRAC_spp_covered)
  #     cat ("\napp_spp_rep_shortfall =", app_spp_rep_shortfall)

  #---------------------------------------------------------------------------
  #               Correct scores as computed by biodivprobgen...
  #---------------------------------------------------------------------------

  cor_results_list = compute_solution_vector_scores (cor_bpm,
                                                     num_PUs,
                                                      marxan_best_solution_PU_IDs,
                                                      marxan_best_num_patches_in_solution,
                                                      cor_num_patches_in_solution,
                                                      spp_rep_targets,
                                                      num_spp,
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
      cat ("\nCOR_ VALUES AS COMPUTED BY BIODIVPROBGEN:")
      cat ("\n-----------------------------------------")
      cat ("\nlength (cor_unmet_spp_rep_frac_indices) = ",
         length (cor_unmet_spp_rep_frac_indices))
  #     cat ("\ncor_NUM_spp_covered =", cor_NUM_spp_covered)
  #     cat ("\ncor_FRAC_spp_covered =", cor_FRAC_spp_covered)
  #     cat ("\ncor_spp_rep_shortfall =", cor_spp_rep_shortfall)

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

  #-------------------------------------------------------------------------------

  num_runs = 1    #  Vestigial?  Not sure it will ever be anything but 1.
                  #  2015 05 09 - BTL.

  results_df =
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

                      #  Correct results as computed by biodivprobgen
                  opt_solution_as_frac_of_tot_num_nodes = rep (NA, num_runs),
                  cor_num_patches_in_solution = rep (NA, num_runs),
                  marxan_best_num_patches_in_solution = rep (NA, num_runs),
                  abs_marxan_best_solution_cost_err_frac = rep (NA, num_runs),
                  marxan_best_solution_cost_err_frac = rep (NA, num_runs),

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

                      #  Apparent results as computed by biodivprobgen
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
                  apply_error = rep (NA, num_runs),
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

  cur_result_row = 0

  #-------------------------------------------------------------------------------

  cur_result_row = cur_result_row + 1

      #  Filling in the runset_abbrev with the full runset name for the moment,
      #  because it's more reliably correct since it's automatically captured
      #  by tzar.  Not sure what I'll do in the long run.
      #  2015 03 09 - BTL
  results_df$runset_abbrev [cur_result_row]                                    = parameters$runset_name    #  parameters$runset_abbrev

  results_df$exceeded_thresh_for_num_spp                                       = FALSE

  results_df$num_PUs [cur_result_row]                                          = num_PUs
  results_df$num_spp [cur_result_row]                                          = num_spp
  results_df$num_spp_per_PU [cur_result_row]                                   = num_spp / num_PUs
  results_df$seed [cur_result_row]                                             = parameters$seed

        #  Xu options
    results_df$n__num_groups [cur_result_row]                                    = base_Xu_params@n__num_groups
    results_df$alpha__ [cur_result_row]                                          = base_Xu_params@alpha__
    results_df$p__prop_of_links_between_groups [cur_result_row]                  = base_Xu_params@p__prop_of_links_between_groups
    results_df$r__density [cur_result_row]                                       = base_Xu_params@r__density

        #  Correct results as computed by biodivprobgen
    results_df$opt_solution_as_frac_of_tot_num_nodes [cur_result_row]            = opt_solution_as_frac_of_tot_num_nodes
    results_df$cor_num_patches_in_solution [cur_result_row]                      = cor_num_patches_in_solution
    results_df$marxan_best_num_patches_in_solution [cur_result_row]              = marxan_best_num_patches_in_solution
    results_df$abs_marxan_best_solution_cost_err_frac [cur_result_row]           = abs_marxan_best_solution_cost_err_frac
    results_df$marxan_best_solution_cost_err_frac [cur_result_row]               = marxan_best_solution_cost_err_frac

    results_df$cor_spp_rep_shortfall [cur_result_row]                = cor_results_list$spp_rep_shortfall
    results_df$cor_NUM_spp_covered [cur_result_row]                  = cor_results_list$num_spp_covered
    results_df$cor_FRAC_spp_covered [cur_result_row]                 = cor_results_list$frac_spp_covered

    results_df$cor_TP [cur_result_row]                               = cor_results_list$TP
    results_df$cor_TN [cur_result_row]                               = cor_results_list$TN
    results_df$cor_FP [cur_result_row]                               = cor_results_list$FP
    results_df$cor_FN [cur_result_row]                               = cor_results_list$FN

    results_df$cor_cSe [cur_result_row]                              = cor_results_list$cSe
    results_df$cor_cSp [cur_result_row]                              = cor_results_list$cSp
    results_df$cor_cPPV [cur_result_row]                             = cor_results_list$cPPV
    results_df$cor_cNPV [cur_result_row]                             = cor_results_list$cNPV

    results_df$cor_acc_frac [cur_result_row]                         = cor_results_list$acc_frac
    results_df$cor_acc_err_frac [cur_result_row]                     = cor_results_list$acc_err_frac
    results_df$cor_cost_savings [cur_result_row]                     = cor_results_list$cost_savings

    results_df$cor_opt_cost_savings [cur_result_row]                 = cor_results_list$opt_cost_savings

    results_df$cor_TSS [cur_result_row]                              = cor_results_list$TSS
    results_df$cor_max_cSe_cSp [cur_result_row]                      = cor_results_list$max_cSe_cSp
    results_df$cor_min_cSe_cSp [cur_result_row]                      = cor_results_list$min_cSe_cSp
    results_df$cor_mean_cSe_cSp [cur_result_row]                     = cor_results_list$mean_cSe_cSp
    results_df$cor_prod_cSe_cSp [cur_result_row]                     = cor_results_list$prod_cSe_cSp
    results_df$cor_euc_cSe_cSp [cur_result_row]                      = cor_results_list$euc_cSe_cSp
    results_df$cor_acc_err_mag [cur_result_row]                      = cor_results_list$acc_err_mag

        #  Apparent results as computed by biodivprobgen
    results_df$app_spp_rep_shortfall [cur_result_row]                = app_results_list$spp_rep_shortfall
    results_df$app_solution_NUM_spp_covered [cur_result_row]         = app_results_list$num_spp_covered
    results_df$app_solution_FRAC_spp_covered [cur_result_row]        = app_results_list$frac_spp_covered

    results_df$app_TP [cur_result_row]                               = app_results_list$TP
    results_df$app_TN [cur_result_row]                               = app_results_list$TN
    results_df$app_FP [cur_result_row]                               = app_results_list$FP
    results_df$app_FN [cur_result_row]                               = app_results_list$FN

    results_df$app_cSe [cur_result_row]                              = app_results_list$cSe
    results_df$app_cSp [cur_result_row]                              = app_results_list$cSp
    results_df$app_cPPV [cur_result_row]                             = app_results_list$cPPV
    results_df$app_cNPV [cur_result_row]                             = app_results_list$cNPV

    results_df$app_acc_frac [cur_result_row]                         = app_results_list$acc_frac
    results_df$app_acc_err_frac [cur_result_row]                     = app_results_list$acc_err_frac
    results_df$app_cost_savings [cur_result_row]                     = app_results_list$cost_savings

    results_df$app_opt_cost_savings [cur_result_row]                 = app_results_list$opt_cost_savings

    results_df$app_TSS [cur_result_row]                              = app_results_list$TSS
    results_df$app_max_cSe_cSp [cur_result_row]                      = app_results_list$max_cSe_cSp
    results_df$app_min_cSe_cSp [cur_result_row]                      = app_results_list$min_cSe_cSp
    results_df$app_mean_cSe_cSp [cur_result_row]                     = app_results_list$mean_cSe_cSp
    results_df$app_prod_cSe_cSp [cur_result_row]                     = app_results_list$prod_cSe_cSp
    results_df$app_euc_cSe_cSp [cur_result_row]                      = app_results_list$euc_cSe_cSp
    results_df$app_acc_err_mag [cur_result_row]                      = app_results_list$acc_err_mag

        #  Apparent results as computed by Marxan
    results_df$app_spp_rep_shortfall__fromMarxan [cur_result_row]                = app_spp_rep_shortfall__fromMarxan
    results_df$app_solution_NUM_spp_covered__fromMarxan [cur_result_row]         = app_solution_NUM_spp_covered__fromMarxan
    results_df$app_solution_FRAC_spp_covered__fromMarxan [cur_result_row]        = app_solution_FRAC_spp_covered__fromMarxan

        #  Error generation parameters
    results_df$apply_error [cur_result_row]                                       = apply_error
    results_df$FP_const_rate [cur_result_row]                                   = FP_const_rate
    results_df$FN_const_rate [cur_result_row]                                   = FN_const_rate
    results_df$match_error_counts [cur_result_row]                              = match_error_counts
    results_df$original_FP_const_rate [cur_result_row]                          = original_FP_const_rate
    results_df$original_FN_const_rate [cur_result_row]                          = original_FN_const_rate

        #  Derived Xu options
    results_df$num_nodes_per_group [cur_result_row]                             = derived_Xu_params@num_nodes_per_group
    results_df$tot_num_nodes [cur_result_row]                                   = derived_Xu_params@tot_num_nodes
    results_df$num_independent_set_nodes [cur_result_row]                       = derived_Xu_params@num_independent_set_nodes
    results_df$num_dependent_set_nodes [cur_result_row]                         = derived_Xu_params@num_dependent_set_nodes
    results_df$num_rounds_of_linking_between_groups [cur_result_row]            = derived_Xu_params@num_rounds_of_linking_between_groups
    results_df$target_num_links_between_2_groups_per_round [cur_result_row]     = derived_Xu_params@target_num_links_between_2_groups_per_round
    results_df$num_links_within_one_group [cur_result_row]                      = derived_Xu_params@num_links_within_one_group
    results_df$tot_num_links_inside_groups [cur_result_row]                     = derived_Xu_params@tot_num_links_inside_groups
    results_df$max_possible_num_links_between_groups [cur_result_row]           = derived_Xu_params@max_possible_num_links_between_groups
    results_df$max_possible_tot_num_links [cur_result_row]                      = derived_Xu_params@max_possible_tot_num_links

        #  Marxan options
    results_df$marxan_spf_const [cur_result_row]                                 = spf_const
    results_df$marxan_PROP [cur_result_row]                                      = marxan_PROP
    results_df$marxan_RANDSEED [cur_result_row]                                  = marxan_RANDSEED
    results_df$marxan_NUMREPS [cur_result_row]                                   = marxan_NUMREPS

        #  Marxan Annealing Parameters
    results_df$marxan_NUMITNS [cur_result_row]                                   = marxan_NUMITNS
    results_df$marxan_STARTTEMP [cur_result_row]                                 = marxan_STARTTEMP
    results_df$marxan_NUMTEMP [cur_result_row]                                   = marxan_NUMTEMP

        #  Marxan Cost Threshold
    results_df$marxan_COSTTHRESH [cur_result_row]                                = marxan_COSTTHRESH
    results_df$marxan_THRESHPEN1 [cur_result_row]                                = marxan_THRESHPEN1
    results_df$marxan_THRESHPEN2 [cur_result_row]                                = marxan_THRESHPEN2

        #  Marxan Program control
    results_df$marxan_RUNMODE [cur_result_row]                                   = marxan_RUNMODE
    results_df$marxan_MISSLEVEL [cur_result_row]                                 = marxan_MISSLEVEL
    results_df$marxan_ITIMPTYPE [cur_result_row]                                 = marxan_ITIMPTYPE
    results_df$marxan_HEURTYPE [cur_result_row]                                  = marxan_HEURTYPE
    results_df$marxan_CLUMPTYPE [cur_result_row]                                 = marxan_CLUMPTYPE

      #  Full runset name
  results_df$runset_name [cur_result_row]                                      = parameters$runset_name



  #  Getting an error.  Not sure why...  Is it because the free variable names
  #  like num_PUs, are the same as the list element names, like results_df$num_PUs?
  #
  #  Error in `$<-.data.frame`(`*tmp*`, "num_PUs", value = c(NA, 12L)) :
  #    replacement has 2 rows, data has 1
  #  Calls: source ... withVisible -> eval -> eval -> $<- -> $<-.data.frame
  #  Execution halted


  if (parameters$compute_network_metrics)
      {
          #  Need to bind the network measures to the data frame too.
      results_df = cbind (results_df,
                          bipartite_metrics_from_bipartite_package,
                          bipartite_metrics_from_igraph_package_df
                          )
      }

  write_results_to_files (results_df, parameters,
                          cur_result_row)    #  Added 2016 03 28 - BTL.
}

#===============================================================================

