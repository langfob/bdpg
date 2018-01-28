#===============================================================================
#
#                       do_rs_analysis_and_output.R
#
#  Run code to run reserve selector and dump results to file.
#
#  Used to do graph analysis here too, but no longer.
#
#===============================================================================

#' Create an RSrun
#'
#' Create a run of a reserve selector
#'
#-------------------------------------------------------------------------------

#' @param prob_UUID UUID for the biodiversity problem the reserve selector is
#'     run over
#' @param cor_or_app_str character string
#' @param basic_or_wrapped_or_comb_str character string
#' @inheritParams std_param_defns
#'
#' @return Returns an RSrun object

#-------------------------------------------------------------------------------

create_RSrun <- function (prob_UUID,
                          spp_rep_targets,
                          parameters,
                          cor_or_app_str,
                          basic_or_wrapped_or_comb_str,
                          rs_method_name
                          )
    {
    location_string = paste0 ("Start of create_RSrun(),",
                              cor_or_app_str, ",", basic_or_wrapped_or_comb_str)
    new_seed_list =
        set_new_or_forced_rand_seed_if_necessary (is_rsrun = TRUE,
                                                  is_rsprob = FALSE,
                                                  parameters,
                                                  cor_or_app_str,
                                                  basic_or_wrapped_or_comb_str,
                                                  location_string)

    #------------------------------------------------------------------

    rsrun <- new ("RSrun")

    rsrun@UUID             <- uuid::UUIDgenerate()
    rsrun@run_on_prob_UUID <- prob_UUID

    rsrun@rand_seed             = new_seed_list$seed_value
    rsrun@R_internal_seed_array = new_seed_list$R_internal_seed_array

    rsrun@targets  <- spp_rep_targets

    rsrun@obj_type_str   = "RSrun_"
    rsrun@rs_method_name = rs_method_name
    rsrun@cor_or_app_str = cor_or_app_str
    rsrun@basic_or_wrapped_or_comb_str = basic_or_wrapped_or_comb_str


    rsrun@file_name_prefix =
                            paste (rsrun@obj_type_str,
                                   rsrun@cor_or_app_str,
                                   rsrun@basic_or_wrapped_or_comb_str,
                                   rsrun@rs_method_name,
                                   sep='-')

    starting_dir = parameters$fullOutputDir_NO_slash

    create_RSrun_dir_and_subdirs (rsrun, starting_dir)

    rsrun <- save_rsrun (rsrun, starting_dir)

    return (rsrun)
    }

#===============================================================================

#' Run reserve selector(s) on COR problem and write output from all analyses
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_COR_rs_analysis_and_output <- function (COR_bd_prob,
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

        do_ResSel_analysis_and_output (COR_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = simple_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (COR_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = simple_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (COR_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = unprotected_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (COR_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = unprotected_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (COR_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = zonation_like,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (COR_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = zonation_like,
                                       run_ResSel_func = run_greedy_ResSel,
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
            do_COR_marxan_analysis_and_output (COR_bd_prob,
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
        do_COR_gurobi_analysis_and_output (COR_bd_prob,
                                           parameters,
                                           rs_method_name,
                                           marxan_elapsed_time,
                                           src_rds_file_dir,
                                           spp_rep_targets)
        }
    }

#===============================================================================

#' Run reserve selector(s) on APP problem and write output from all analyses
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_APP_rs_analysis_and_output <- function (APP_bd_prob,
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

        do_ResSel_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = simple_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = simple_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = unprotected_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = unprotected_richness,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = zonation_like,
                                       run_ResSel_func = run_greedy_ResSel,
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

        do_ResSel_analysis_and_output (APP_bd_prob,    #  <<<<<-----------------
                                       COR_bd_prob,    #  <<<<<-----------------
                                       parameters,

                                       rs_method_name,
                                       resSel_func     = zonation_like,
                                       run_ResSel_func = run_greedy_ResSel,
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
            do_APP_marxan_analysis_and_output (APP_bd_prob,
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
        do_APP_gurobi_analysis_and_output (APP_bd_prob,
                                           COR_bd_prob,
                                           parameters,
                                           rs_method_name,
                                           marxan_elapsed_time,
                                           src_rds_file_dir,
                                           spp_rep_targets)
        }
    }

#===============================================================================



