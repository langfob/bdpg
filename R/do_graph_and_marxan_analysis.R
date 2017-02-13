#===============================================================================
#
#                       do_graph_and_marxan_analysis.R
#
#   Run code to compute graph metrics and to run marxan and dump results
#   to file.
#
#===============================================================================

#' Run marxan on COR problem and write output from all analysis
#'
#' @param COR_bd_prob
#' @param parameters
#'
#' @return
#' @export
#'
do_COR_marxan_analysis_and_output <- function (COR_bd_prob, parameters)
    {
        #---------------
        #  Run marxan.
        #---------------

    COR_marxan_ret_values = set_up_for_and_run_marxan_COR (COR_bd_prob,
                                                           parameters)

    marxan_control_values  = COR_marxan_ret_values$marxan_control_values
    COR_bd_prob            = COR_marxan_ret_values$COR_bd_prob  #  COR_bd_prob has new dirs

        #---------------------------
        #  Collect marxan results.
        #---------------------------

                                            #  Guessing at these args for now...
    marxan_output_values = read_COR_marxan_output_files (COR_bd_prob,
                                                         parameters)

        #-----------------------------------------------
        #  Dump all of the different kinds of results.
        #-----------------------------------------------

                                        #  Guessing at these args for now...
    create_COR_master_output_structure (COR_bd_prob,
                                        marxan_control_values,
                                        marxan_output_values)

    }  #  end function - do_COR_marxan_analysis_and_output

#===============================================================================
#===============================================================================
#===============================================================================

#' Run marxan on APP problem and write output from all analysis
#'
#' @param APP_bd_prob
#' @param COR_bd_prob
#' @param parameters
#'
#' @return
#' @export

do_APP_marxan_analysis_and_output <- function (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters)
    {
        #---------------
        #  Run marxan.
        #---------------

    APP_marxan_ret_values = set_up_for_and_run_marxan_APP (APP_bd_prob,
                                                           COR_bd_prob,
                                                           parameters)

    marxan_control_values  = APP_marxan_ret_values$marxan_control_values
    APP_bd_prob            = APP_marxan_ret_values$APP_bd_prob  #  APP_bd_prob has new dirs

        #---------------------------
        #  Collect marxan results.
        #---------------------------

                                            #  Guessing at these args for now...
    marxan_output_values = read_APP_marxan_output_files (APP_bd_prob,
                                                         COR_bd_prob,
                                                         parameters)

        #-----------------------------------------------
        #  Dump all of the different kinds of results.
        #-----------------------------------------------

                                        #  Guessing at these args for now...
    create_APP_master_output_structure (APP_bd_prob,
                                        COR_bd_prob,
                                        marxan_control_values,
                                        marxan_output_values)

    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



