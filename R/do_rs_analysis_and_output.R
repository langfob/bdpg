#===============================================================================
#
#                       do_rs_analysis_and_output.R
#
#  Run code to run reserve selector and dump results to file.
#
#  Used to do graph analysis here too, but no longer.
#
#===============================================================================

#' Run reserve selector(s) on bdproblem and write output from all analyses
#'
#' Note that the COR and APP arguments are the same if running on a correct
#' problem.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns named list containing top directory for each reserve
#' selector that was run
#' @export

#-------------------------------------------------------------------------------

do_rs_analysis_and_output <- function (APP_bd_prob,
                                           COR_bd_prob,
                                           parameters,
                                       starting_dir,
                                           src_rds_file_dir = NULL,
                                           spp_rep_targets =
                                               rep(1,COR_bd_prob@num_spp))
    {
    RS_topDirs_list = list ()

    #-----------------------------------

    do_simple_richness_forward = vb (parameters$do_simple_richness_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_simple_richness_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "SR_Forward"

        srforward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = simple_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$srforward = srforward_topDir
        }

    #-----------------------------------

    do_simple_richness_backward = vb (parameters$do_simple_richness_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_simple_richness_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "SR_Backward"

        srbackward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = simple_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$srbackward = srbackward_topDir
        }

    #---------------------------------------------------------------------------

    do_unprotected_richness_forward = vb (parameters$do_unprotected_richness_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_unprotected_richness_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "UR_Forward"

        urforward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = unprotected_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$urforward = urforward_topDir
        }

    #-----------------------------------

    do_unprotected_richness_backward = vb (parameters$do_unprotected_richness_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_unprotected_richness_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "UR_Backward"

        urbackward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = unprotected_richness,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$urbackward = urbackward_topDir
        }

    #---------------------------------------------------------------------------

    do_zonation_like_forward = vb (parameters$do_zonation_like_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_zonation_like_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "ZL_Forward"

        zlforward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = zonation_like,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$zlforward = zlforward_topDir
        }

    #-----------------------------------

    do_zonation_like_backward = vb (parameters$do_zonation_like_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_zonation_like_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "ZL_Backward"

        zlbackward_topDir =
            do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                           COR_bd_prob,    #  <<<<<-----------------
                                           parameters,
                                           starting_dir,

                                           rs_method_name,
                                           resSel_func     = zonation_like,
                                           # run_ResSel_func = run_greedy_ResSel,
                                           RS_specific_params,

                                           src_rds_file_dir,
                                           spp_rep_targets)

        RS_topDirs_list$zlbackward = zlbackward_topDir
        }

    #---------------------------------------------------------------------------

    marxan_elapsed_time = NA
    run_marxan = vb (parameters$run_marxan, def_on_empty = TRUE, def = FALSE)
    if (run_marxan)
        {
        rs_method_name = "Marxan_SA"
        # marxan_elapsed_time =
        marxan_ret_vals =
            do_marxan_analysis_and_output (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                           starting_dir,
                                               rs_method_name,
                                               src_rds_file_dir,
                                               spp_rep_targets)

        marxan_elapsed_time = marxan_ret_vals$marxan_elapsed_time
        RS_topDirs_list$marxan = marxan_ret_vals$marxan_topdir
        }

    #---------------------------------------------------------------------------

    do_gurobi = vb (parameters$do_gurobi, def_on_empty = TRUE, def = FALSE)
    if (do_gurobi)
        {
        rs_method_name = "Gurobi"
        gurobi_topDir =
            do_gurobi_analysis_and_output (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                           starting_dir,
                                               rs_method_name,
                                               marxan_elapsed_time,
                                               src_rds_file_dir,
                                               spp_rep_targets)

        RS_topDirs_list$gurobi = gurobi_topDir
        }

    #---------------------------------------------------------------------------

        #  NOTE:  Ensemble must come last in this list of reserve selectors
        #         so that it can be passed the marxan_run_dir (or the run_dir
        #         of whatever selector is being used in the ensemble).

    do_ensemble = vb (parameters$do_ensemble,
                      def_on_empty = TRUE, def = FALSE)

    if (do_ensemble)
        {
        num_probs_in_ensemble = vn (parameters$num_probs_in_ensemble,
                                    range_lo=2)

        RS_specific_params = list (ensemble_sub_rs_list = c ("marxan"),
                                   num_probs_in_ensemble = num_probs_in_ensemble,
                                   marxan_run_dir = RS_topDirs_list$marxan)
        rs_method_name = "Ensemble"

        ensemble_topDir =
            do_ensemble (APP_bd_prob,    #  <<<<<-----------------
                         COR_bd_prob,    #  <<<<<-----------------
                         parameters,
                         starting_dir,

                         rs_method_name,
                         resSel_func     = ensemble,

                         RS_specific_params,

                         src_rds_file_dir,
                         spp_rep_targets)

        RS_topDirs_list$ensemble = ensemble_topDir
        }

    #---------------------------------------------------------------------------

    return (RS_topDirs_list)
    }

#===============================================================================



