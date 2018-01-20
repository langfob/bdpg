#===============================================================================

                #  gscp_15b_compute_and_verify_scores.R

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
    # results_list$rsr_app_spp_rep_shortfall__fromMarxan                          = app_spp_rep_shortfall__fromMarxan
    # results_list$rsr_app_solution_NUM_spp_covered__fromMarxan                   = app_solution_NUM_spp_covered__fromMarxan
    # results_list$rsr_app_solution_FRAC_spp_covered__fromMarxan                  = app_solution_FRAC_spp_covered__fromMarxan
    results_list$rsr_app_spp_rep_shortfall__fromRS                          = app_spp_rep_shortfall__fromMarxan
    results_list$rsr_app_solution_NUM_spp_covered__fromRS                   = app_solution_NUM_spp_covered__fromMarxan
    results_list$rsr_app_solution_FRAC_spp_covered__fromRS                  = app_solution_FRAC_spp_covered__fromMarxan

    return (results_list)
    }

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

