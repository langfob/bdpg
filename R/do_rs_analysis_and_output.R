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
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_rs_analysis_and_output <- function (APP_bd_prob,
                                           COR_bd_prob,
                                           parameters,
                                           src_rds_file_dir = NULL,
                                           spp_rep_targets =
                                               rep(1,COR_bd_prob@num_spp))
    {
    do_simple_richness_forward = vb (parameters$do_simple_richness_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_simple_richness_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "SR_Forward"

        do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = simple_richness,
                                       # run_ResSel_func = run_greedy_ResSel,
                                       RS_specific_params,

                                       src_rds_file_dir,
                                       spp_rep_targets)
        }

    do_simple_richness_backward = vb (parameters$do_simple_richness_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_simple_richness_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "SR_Backward"

        do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = simple_richness,
                                       # run_ResSel_func = run_greedy_ResSel,
                                       RS_specific_params,

                                       src_rds_file_dir,
                                       spp_rep_targets)
        }

    #---------------------------------------------------------------------------

    do_unprotected_richness_forward = vb (parameters$do_unprotected_richness_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_unprotected_richness_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "UR_Forward"

        do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = unprotected_richness,
                                       # run_ResSel_func = run_greedy_ResSel,
                                       RS_specific_params,

                                       src_rds_file_dir,
                                       spp_rep_targets)
        }

    do_unprotected_richness_backward = vb (parameters$do_unprotected_richness_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_unprotected_richness_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "UR_Backward"

        do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = unprotected_richness,
                                       # run_ResSel_func = run_greedy_ResSel,
                                       RS_specific_params,

                                       src_rds_file_dir,
                                       spp_rep_targets)
        }

    #---------------------------------------------------------------------------

    do_zonation_like_forward = vb (parameters$do_zonation_like_forward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_zonation_like_forward)
        {
        forward = TRUE
        RS_specific_params = list (forward = forward)
        rs_method_name = "ZL_Forward"

        do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = zonation_like,
                                       # run_ResSel_func = run_greedy_ResSel,
                                       RS_specific_params,

                                       src_rds_file_dir,
                                       spp_rep_targets)
        }

    do_zonation_like_backward = vb (parameters$do_zonation_like_backward,
                                     def_on_empty = TRUE, def = FALSE)

    if (do_zonation_like_backward)
        {
        forward = FALSE
        RS_specific_params = list (forward = forward)
        rs_method_name = "ZL_Backward"

        do_Greedy_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = zonation_like,
                                       # run_ResSel_func = run_greedy_ResSel,
                                       RS_specific_params,

                                       src_rds_file_dir,
                                       spp_rep_targets)
        }

    #---------------------------------------------------------------------------

    marxan_elapsed_time = NA
    run_marxan = vb (parameters$run_marxan, def_on_empty = TRUE, def = FALSE)
    if (run_marxan)
        {
        rs_method_name = "Marxan_SA"
        marxan_elapsed_time =
            do_marxan_analysis_and_output (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                               rs_method_name,
                                               src_rds_file_dir,
                                               spp_rep_targets)
        }

    #---------------------------------------------------------------------------

    do_gurobi = vb (parameters$do_gurobi, def_on_empty = TRUE, def = FALSE)
    if (do_gurobi)
        {
        rs_method_name = "Gurobi"
        do_gurobi_analysis_and_output (APP_bd_prob,
                                           COR_bd_prob,
                                           parameters,
                                           rs_method_name,
                                           marxan_elapsed_time,
                                           src_rds_file_dir,
                                           spp_rep_targets)
        }
    }

#===============================================================================



