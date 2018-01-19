#===============================================================================
#
#                       do_marxan_analysis_and_output.R
#
#  Run code to run marxan and dump results to file.
#
#  Used to do graph analysis here too, but no longer.
#
#===============================================================================

#' #' Create an RSrun
#' #'
#' #' Create a run of a reserve selector
#' #'
#' #-------------------------------------------------------------------------------
#'
#' #' @param prob_UUID UUID for the biodiversity problem the reserve selector is
#' #'     run over
#' #' @param targets numeric vector
#' #' @param cor_or_app_str character string
#' #' @param basic_or_wrapped_or_comb_str character string
#' #' @param method_name character string for reserve selection method, e.g.,
#' #'     "marxan_sa"
#' #' @inheritParams std_param_defns
#' #'
#' #' @return Returns an RSrun object
#'
#' #-------------------------------------------------------------------------------
#'
#' create_RSrun <- function (prob_UUID,
#'                           targets,
#'
#'                           parameters,
#'                   # set_rand_seed_at_creation_of_all_new_major_objects,
#'                   #         starting_dir,
#'
#'                           cor_or_app_str,
#'                           basic_or_wrapped_or_comb_str,
#'                           method_name
#'                           )
#'     {
#'     # forced_seed =
#'     #     get_forced_seed_value_if_necessary (is_rsrun = TRUE,
#'     #                                         is_rsprob = FALSE,
#'     #                                         parameters,
#'     #                                         cor_or_app_str,
#'     #                                         basic_or_wrapped_or_comb_str)
#'     #
#'     # new_seed =
#'     #     set_new_or_forced_rand_seed_if_necessary (value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
#'     #                                               paste0 ("Start of create_RSrun(),",
#'     #                                                       cor_or_app_str, ",",
#'     #                                                       basic_or_wrapped_or_comb_str),
#'     #                                               forced_seed)
#'
#'     new_seed_list =
#'         set_new_or_forced_rand_seed_if_necessary (is_rsrun = TRUE,
#'                                                   is_rsprob = FALSE,
#'                                                   parameters,
#'                                                   cor_or_app_str,
#'                                                   basic_or_wrapped_or_comb_str,
#'                                                   location_string = paste0 ("Start of create_RSrun(),",
#'                                                                             cor_or_app_str, ",",
#'                                                                             basic_or_wrapped_or_comb_str))
#'
#'     #------------------------------------------------------------------
#'
#'     rsrun <- new ("RSrun")
#'
#'     rsrun@UUID             <- uuid::UUIDgenerate()
#'     rsrun@run_on_prob_UUID <- prob_UUID
#'
#'     rsrun@rand_seed             = new_seed_list$seed_value
#'     rsrun@R_internal_seed_array = new_seed_list$R_internal_seed_array
#'
#'     rsrun@targets  <- targets
#'
#'     rsrun@obj_type_str   = "RSrun_"
#'     rsrun@rs_method_name = method_name
#'     rsrun@cor_or_app_str = cor_or_app_str
#'     rsrun@basic_or_wrapped_or_comb_str = basic_or_wrapped_or_comb_str
#'
#'
#'     rsrun@file_name_prefix =
#'                             paste (rsrun@obj_type_str,
#'                                    rsrun@cor_or_app_str,
#'                                    rsrun@basic_or_wrapped_or_comb_str,
#'                                    rsrun@rs_method_name,
#'                                    sep='-')
#'
#'     starting_dir = parameters$fullOutputDir_NO_slash
#'
#'     create_RSrun_dir_and_subdirs (rsrun, starting_dir)
#'
#'     rsrun <- save_rsrun (rsrun, starting_dir)
#'
#'     return (rsrun)
#'     }

#===============================================================================

#' Run marxan on COR problem and write output from all analysis
#'
#' Run marxan on COR problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @param COR_bd_prob a Xu_bd_problem
#' @param parameters list
#' @param src_rds_file_dir character string
#' @param targets numeric vector
#'
#' @return Returns nothing
#' @export
#'
#-------------------------------------------------------------------------------

do_COR_marxan_analysis_and_output <- function (COR_bd_prob,
                                               parameters,
                                               src_rds_file_dir=NULL,
                                               targets=rep(1,COR_bd_prob@num_spp))
    {
    rs_method_name = "Marxan_SA"

        #---------------
        #  Run marxan.
        #---------------

    COR_marxan_run <- create_RSrun (COR_bd_prob@UUID,
                                    targets,

                            parameters,
                            # value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
                            # parameters$rsrun_rand_seed,
                            #         parameters$fullOutputDir_NO_slash,

                                    COR_bd_prob@cor_or_app_str,
                                    COR_bd_prob@basic_or_wrapped_or_comb_str,
#                                    method_name = "Marxan_SA"
                                    rs_method_name = "Marxan_SA"
                                    )
    marxan_control_values = set_up_for_and_run_marxan_COR (COR_bd_prob,
                                                           COR_marxan_run,
                                                           parameters)

        #---------------------------
        #  Collect marxan results.
        #---------------------------

    save_rsrun_results_data_for_one_rsrun (parameters,
                                              COR_marxan_run,
                                              COR_bd_prob,
                                              COR_bd_prob,
                                           rs_method_name,
                                           marxan_control_values,
                                           src_rds_file_dir)

    }  #  end function - do_COR_marxan_analysis_and_output

#===============================================================================
#===============================================================================
#===============================================================================

#' Run marxan on APP problem and write output from all analysis
#'
#' Run marxan on APP problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @param APP_bd_prob a Xu_bd_problem
#' @param COR_bd_prob a Xu_bd_problem
#' @param parameters list
#' @param src_rds_file_dir character string
#' @param targets numeric vector
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_APP_marxan_analysis_and_output <- function (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                               src_rds_file_dir=NULL,
                                               targets=rep(1,COR_bd_prob@num_spp)
                                               )
    {
    rs_method_name = "Marxan_SA"

        #---------------
        #  Run marxan.
        #---------------

    APP_marxan_run <- create_RSrun (APP_bd_prob@UUID,
                                    targets,

                            parameters,
                            # value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
                            # parameters$rsrun_rand_seed,
                            #         parameters$fullOutputDir_NO_slash,

                                    APP_bd_prob@cor_or_app_str,
                                    APP_bd_prob@basic_or_wrapped_or_comb_str,
#                                    method_name = "Marxan_SA"
                                    rs_method_name = "Marxan_SA"
                                    )

    marxan_control_values = set_up_for_and_run_marxan_APP (APP_bd_prob,
                                                           COR_bd_prob,
                                                           APP_marxan_run,
                                                           parameters)

        #---------------------------
        #  Collect marxan results.
        #---------------------------

    save_rsrun_results_data_for_one_rsrun (parameters,
                                              APP_marxan_run,
                                              COR_bd_prob,
                                              APP_bd_prob,
                                           rs_method_name,
                                           marxan_control_values,
                                           src_rds_file_dir)

    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



