#===============================================================================
#
#                       do_marxan_analysis_and_output.R
#
#  Run code to run marxan and dump results to file.
#
#  Used to do graph analysis here too, but no longer.
#
#===============================================================================

#' Run marxan on COR problem and write output from all analysis
#'
#' Run marxan on COR problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export
#'
#-------------------------------------------------------------------------------

do_COR_marxan_analysis_and_output <- function (COR_bd_prob,
                                               parameters,
                                               rs_method_name,
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp))
    {
        #---------------
        #  Run marxan.
        #---------------

    COR_marxan_run <- create_RSrun (COR_bd_prob@UUID,
                                    spp_rep_targets,
                                    parameters,
                                    COR_bd_prob@cor_or_app_str,
                                    COR_bd_prob@basic_or_wrapped_or_comb_str,
                                    rs_method_name)

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

    return (marxan_control_values$marxan_elapsed_time)
    }  #  end function - do_COR_marxan_analysis_and_output

#===============================================================================
#===============================================================================
#===============================================================================

#' Run marxan on APP problem and write output from all analysis
#'
#' Run marxan on APP problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_APP_marxan_analysis_and_output <- function (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                               rs_method_name,
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp)
                                               )
    {
        #---------------
        #  Run marxan.
        #---------------

    APP_marxan_run <- create_RSrun (APP_bd_prob@UUID,
                                    spp_rep_targets,
                                    parameters,
                                    APP_bd_prob@cor_or_app_str,
                                    APP_bd_prob@basic_or_wrapped_or_comb_str,
                                    rs_method_name)

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

    return (marxan_control_values$marxan_elapsed_time)
    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



