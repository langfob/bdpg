#===============================================================================
#
#                       do_graph_and_marxan_analysis.R
#
#  Run code to run marxan and dump results to file.
#
#  Used to do graph analysis here too, but no longer.  Should rename file.
#
#===============================================================================

create_RSrun <- function (prob_UUID, parameters, targets)
    {
    rsrun <- new ("RSrun")

    rsrun@UUID             <- uuid::UUIDgenerate()
    rsrun@run_on_prob_UUID <- prob_UUID

    rsrun@targets  <- targets

    create_RSrun_dir_and_subdirs (rsrun, parameters$fullOutputDir_NO_slash)

    return (rsrun)
    }

#===============================================================================

create_marxan_run <- function (prob_UUID, parameters, targets)
    {
    return (create_RSrun (prob_UUID, parameters, targets))
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



    COR_marxan_run <- create_marxan_run (COR_bd_prob@UUID, parameters,
                                         targets)

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

        #-----------------------------------------------
        #  Dump all of the different kinds of results.
        #-----------------------------------------------

    create_COR_master_output_structure (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        COR_marxan_run
                                        )

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

    APP_marxan_run <- create_marxan_run (APP_bd_prob@UUID, parameters,
                                         targets)

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

        #-----------------------------------------------
        #  Dump all of the different kinds of results.
        #-----------------------------------------------

                                        #  Guessing at these args for now...
    create_APP_master_output_structure (marxan_control_values,
                                        marxan_output_values,
                                        parameters,

                                        COR_bd_prob,
                                        APP_bd_prob,
                                        APP_marxan_run
                                        )

    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



