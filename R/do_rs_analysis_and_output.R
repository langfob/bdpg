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
                  # set_rand_seed_at_creation_of_all_new_major_objects,
                  #         starting_dir,

                          cor_or_app_str,
                          basic_or_wrapped_or_comb_str,
                          rs_method_name
                          )
    {
    # forced_seed =
    #     get_forced_seed_value_if_necessary (is_rsrun = TRUE,
    #                                         is_rsprob = FALSE,
    #                                         parameters,
    #                                         cor_or_app_str,
    #                                         basic_or_wrapped_or_comb_str)
    #
    # new_seed =
    #     set_new_or_forced_rand_seed_if_necessary (value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
    #                                               paste0 ("Start of create_RSrun(),",
    #                                                       cor_or_app_str, ",",
    #                                                       basic_or_wrapped_or_comb_str),
    #                                               forced_seed)

    new_seed_list =
        set_new_or_forced_rand_seed_if_necessary (is_rsrun = TRUE,
                                                  is_rsprob = FALSE,
                                                  parameters,
                                                  cor_or_app_str,
                                                  basic_or_wrapped_or_comb_str,
                                                  location_string = paste0 ("Start of create_RSrun(),",
                                                                            cor_or_app_str, ",",
                                                                            basic_or_wrapped_or_comb_str))

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

#' Run reserve selector(s) on COR problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_COR_rs_analysis_and_output <- function (COR_bd_prob,
                                               parameters,
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp))
    {
    marxan_elapsed_time = NA
    run_marxan = vb (parameters$run_marxan, def_on_empty = TRUE, def = FALSE)
    if (run_marxan)
        {
        rs_method_name = "Marxan_SA"
        marxan_elapsed_time =
            do_COR_marxan_analysis_and_output (COR_bd_prob,
                                               parameters,
                                               rs_method_name,
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp))
        }

    do_gurobi = vb (parameters$do_gurobi, def_on_empty = TRUE, def = FALSE)
    if (do_gurobi)
        {
        rs_method_name = "Gurobi"
        do_COR_gurobi_analysis_and_output (COR_bd_prob,
                                           parameters,
                                           rs_method_name,
                                           marxan_elapsed_time,
                                           src_rds_file_dir=NULL,
                                           spp_rep_targets=rep(1,COR_bd_prob@num_spp))
        }
    }

#===============================================================================

#' Run reserve selector(s) on APP problem and write output from all analysis
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
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp)
                                               )
    {
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
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp))
        }

    do_gurobi = vb (parameters$do_gurobi, def_on_empty = TRUE, def = FALSE)
    if (do_gurobi)
        {
        rs_method_name = "Gurobi"
        do_APP_gurobi_analysis_and_output (APP_bd_prob,
                                           COR_bd_prob,
                                           parameters,
                                           rs_method_name,
                                           marxan_elapsed_time,
                                           src_rds_file_dir=NULL,
                                           spp_rep_targets=rep(1,COR_bd_prob@num_spp))
        }
    }

#===============================================================================



