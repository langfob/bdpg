#===============================================================================
#
#                       do_graph_and_marxan_analysis.R
#
#  Run code to run marxan and dump results to file.
#
#  Used to do graph analysis here too, but no longer.  Should rename file.
#
#===============================================================================

create_RSrun <- function (prob_UUID,
                          targets,
                          starting_dir,
                          cor_or_app_str,
                          basic_or_wrapped_or_comb_str,
                          method_name
                          )
    {
    rsrun <- new ("RSrun")

    rsrun@UUID             <- uuid::UUIDgenerate()
    rsrun@run_on_prob_UUID <- prob_UUID

    rsrun@targets  <- targets

    rsrun@obj_type_str   = "RSrun_"
    rsrun@rs_method_name = method_name
    rsrun@cor_or_app_str = cor_or_app_str
    rsrun@basic_or_wrapped_or_comb_str = basic_or_wrapped_or_comb_str


    rsrun@file_name_prefix =
                            paste (rsrun@obj_type_str,
                                   rsrun@cor_or_app_str,
                                   rsrun@basic_or_wrapped_or_comb_str,
                                   rsrun@rs_method_name,
                                   sep='-')

    create_RSrun_dir_and_subdirs (rsrun, starting_dir)

    rsrun <- save_rsprob (rsrun, starting_dir)

doc_vars_in_this_func ()

    return (rsrun)
    }

#===============================================================================

#' Run marxan on COR problem and write output from all analysis
#'
#' @param COR_bd_prob
#' @param parameters
#' @param targets
#'
#' @return
#' @export
#'
do_COR_marxan_analysis_and_output <- function (COR_bd_prob, parameters,
                                               targets=rep(1,COR_bd_prob@num_spp))
    {
        #---------------
        #  Run marxan.
        #---------------

    COR_marxan_run <- create_RSrun (COR_bd_prob@UUID,
                                    targets,
                                    parameters$fullOutputDir_NO_slash,
                                    COR_bd_prob@cor_or_app_str,
                                    COR_bd_prob@basic_or_wrapped_or_comb_str,
                                    method_name = "Marxan_SA"
                                    )

    marxan_control_values = set_up_for_and_run_marxan_COR (COR_bd_prob,
                                                           COR_marxan_run,
                                                           parameters)

        #---------------------------
        #  Collect marxan results.
        #---------------------------

                                            #  Guessing at these args for now...
    marxan_output_values = read_COR_marxan_output_files (COR_marxan_run,
                                                         COR_bd_prob,
                                                         parameters)

doc_vars_in_this_func ()

        #-----------------------------------------------
        #  Dump all of the different kinds of results.
        #-----------------------------------------------

    if (FALSE)  #  2017 02 23 - BTL - rebuilding this code, so don't run it yet
    {
    create_COR_master_output_structure (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        COR_marxan_run
                                        )
    }

    }  #  end function - do_COR_marxan_analysis_and_output

#===============================================================================
#===============================================================================
#===============================================================================

#' Run marxan on APP problem and write output from all analysis
#'
#' @param APP_bd_prob
#' @param COR_bd_prob
#' @param parameters
#' @param targets
#'
#' @return
#' @export

do_APP_marxan_analysis_and_output <- function (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                               targets=rep(1,COR_bd_prob@num_spp)
                                               )
    {
        #---------------
        #  Run marxan.
        #---------------

    APP_marxan_run <- create_RSrun (APP_bd_prob@UUID,
                                    targets,
                                    parameters$fullOutputDir_NO_slash,
                                    APP_bd_prob@cor_or_app_str,
                                    APP_bd_prob@basic_or_wrapped_or_comb_str,
                                    method_name = "Marxan_SA"
                                    )

    marxan_control_values = set_up_for_and_run_marxan_APP (APP_bd_prob,
                                                           COR_bd_prob,
                                                           APP_marxan_run,
                                                           parameters)

        #---------------------------
        #  Collect marxan results.
        #---------------------------

    marxan_output_values = read_APP_marxan_output_files (APP_marxan_run,
                                                         APP_bd_prob,
                                                         COR_bd_prob,
                                                         parameters)

doc_vars_in_this_func ()

            #-----------------------------------------------
        #  Dump all of the different kinds of results.
        #-----------------------------------------------

    if (FALSE)  #  2017 02 23 - BTL - rebuilding this code, so don't run it yet
    {
                                        #  Guessing at these args for now...
    create_APP_master_output_structure (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        APP_bd_prob,
                                        APP_marxan_run
                                        )
    }
    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



