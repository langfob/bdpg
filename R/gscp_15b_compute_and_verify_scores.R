#===============================================================================

                #  gscp_15b_compute_and_verify_scores.R

#===============================================================================

#  2017 08 22 - BTL
#  None of the routines in this file seem to be called anymore, but
#  they were all a part of that giant gscp_15_create_master... file a long time
#  ago, I think.
#  They may all have some diagnostic use or have been accidentally left out
#  when I did some overhaul of gscp_15_create_master_output_structure in the
#  past, so I'm going to leave them here until I know for sure.
#  However, I do know for sure that at the moment, no one calls any of them.

#===============================================================================

compute_and_verify_APP_rep_scores_according_to_RS_marxan_sa <-
    function (marxan_mvbest_df, num_spp)
    {
    results_list = list()

      # data.frame (
      #                 #  Apparent results as computed by bdpg
      #             app_spp_rep_shortfall = rep (NA, num_runs),
      #             app_solution_NUM_spp_covered = rep (NA, num_runs),
      #             app_solution_FRAC_spp_covered = rep (NA, num_runs)
      #             )

    #---------------------------------------------------------------------------
    #           Apparent representation scores as computed by marxan
    #---------------------------------------------------------------------------

    app_solution_NUM_spp_covered__fromMarxan  = sum (marxan_mvbest_df$MPM)

    app_solution_FRAC_spp_covered__fromMarxan = app_solution_NUM_spp_covered__fromMarxan / num_spp
    app_spp_rep_shortfall__fromMarxan         = 1 - app_solution_FRAC_spp_covered__fromMarxan

      cat ("\n\n--------------------------------------")
      cat ("\nAPP_ REP VALUES AS COMPUTED BY MARXAN:")
      cat ("\n--------------------------------------")
      cat ("\napp_solution_NUM_spp_covered__fromMarxan =", app_solution_NUM_spp_covered__fromMarxan)
      cat ("\napp_solution_FRAC_spp_covered__fromMarxan =", app_solution_FRAC_spp_covered__fromMarxan)
      cat ("\napp_spp_rep_shortfall__fromMarxan =", app_spp_rep_shortfall__fromMarxan)

    #---------------------------------------------------------------------------

        #  Apparent results as computed by Marxan
    results_list$rsr_app_spp_rep_shortfall__fromMarxan                          = app_spp_rep_shortfall__fromMarxan
    results_list$rsr_app_solution_NUM_spp_covered__fromMarxan                   = app_solution_NUM_spp_covered__fromMarxan
    results_list$rsr_app_solution_FRAC_spp_covered__fromMarxan                  = app_solution_FRAC_spp_covered__fromMarxan

    return (results_list)
    }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

    #  This routine is a placeholder for something more generic to be
    #  developed later when there is more than one reserve selector.
    #  It's just a reminder that the work will need to be done at some point.
    #  2017 06 01 - BTL

#---------------------------------------------------------------------

compute_and_verify_APP_rep_scores_according_to_RS <-
    function (marxan_mvbest_df, num_spp)
        {
        results_list =
            compute_and_verify_APP_rep_scores_according_to_RS_marxan_sa (
                marxan_mvbest_df, num_spp)

        return (results_list)
        }

#===============================================================================

compute_and_verify_APP_scores_according_to_bdpg <-
    function (app_bpm,

            app_num_PUs,    #  Is "app" right for this?
                marxan_best_solution_PU_IDs,
                marxan_best_num_patches_in_solution,
                cor_num_patches_in_solution,
                spp_rep_targets,
            app_num_spp,    #  Is "app" right for this?
                DEBUG_LEVEL,
                FP_const_rate,
                FN_const_rate)
    {
    # num_runs = 1    #  Vestigial?  Not sure it will ever be anything but 1.
    #                 #  2015 05 09 - BTL.

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

    app_results_list = compute_solution_vector_scores (app_bpm,

                                                app_num_PUs,    #  Is "app" right for this?
                                                      marxan_best_solution_PU_IDs,
                                                      marxan_best_num_patches_in_solution,
                                                      cor_num_patches_in_solution,
                                                      spp_rep_targets,
                                                app_num_spp,    #  Is "app" right for this?
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

    results_list$rsr_app_solution_spp_rep_fracs                            = app_solution_spp_rep_fracs
    results_list$rsr_app_solution_unmet_spp_rep_frac_indices               = app_solution_unmet_spp_rep_frac_indices

    results_list$rsr_app_spp_rep_shortfall                                 = app_results_list$spp_rep_shortfall
    results_list$rsr_app_solution_NUM_spp_covered                          = app_results_list$num_spp_covered
    results_list$rsr_app_solution_FRAC_spp_covered                         = app_results_list$frac_spp_covered

    results_list$rsr_app_TP                                                = app_results_list$TP
    results_list$rsr_app_TN                                                = app_results_list$TN
    results_list$rsr_app_FP                                                = app_results_list$FP
    results_list$rsr_app_FN                                                = app_results_list$FN

    results_list$rsr_app_cSe                                               = app_results_list$cSe
    results_list$rsr_app_cSp                                               = app_results_list$cSp
    results_list$rsr_app_cPPV                                              = app_results_list$cPPV
    results_list$rsr_app_cNPV                                              = app_results_list$cNPV

    results_list$rsr_app_acc_frac                                          = app_results_list$acc_frac
    results_list$rsr_app_acc_err_frac                                      = app_results_list$acc_err_frac
    results_list$rsr_app_cost_savings                                      = app_results_list$cost_savings

    results_list$rsr_app_opt_cost_savings                                  = app_results_list$opt_cost_savings

    results_list$rsr_app_TSS                                               = app_results_list$TSS
    results_list$rsr_app_max_cSe_cSp                                       = app_results_list$max_cSe_cSp
    results_list$rsr_app_min_cSe_cSp                                       = app_results_list$min_cSe_cSp
    results_list$rsr_app_mean_cSe_cSp                                      = app_results_list$mean_cSe_cSp
    results_list$rsr_app_prod_cSe_cSp                                      = app_results_list$prod_cSe_cSp
    results_list$rsr_app_euc_cSe_cSp                                       = app_results_list$euc_cSe_cSp
    results_list$rsr_app_acc_err_mag                                       = app_results_list$acc_err_mag

    #---------------------------------------------------------------------------

    # write_results_to_files (results_list, parameters,
    #                         cur_result_row)    #  Added 2016 03 28 - BTL.

    return (results_list)
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

#cat ("\n\nJUST BEFORE ERROR OCCURS:\n\n")
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

browser()    #  Should this call have been commented out when the APP version
             #  of it in the previous routine was commented out?
             #  Should it return (results_list)?
    write_results_to_files (results_list,
                            parameters,
                            cur_result_row)    #  Added 2016 03 28 - BTL.
    }

#===============================================================================

