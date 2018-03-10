#===============================================================================
#
#                       do_marxan_analysis_and_output.R
#
#  Run code to run marxan and dump results to file.
#
#  Used to do graph analysis here too, but no longer.
#
#===============================================================================
#===============================================================================
#===============================================================================

#' Run marxan on bd problem and write output from all analysis
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

do_marxan_analysis_and_output <- function (APP_bd_prob,
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
browser()
    # rs_best_solution_PU_IDs = get_rs_best_solution_PU_IDs (rs_method_name,
    #                                                        rsrun,
    #                                                        exp_root_dir,
    #                                                        COR_bd_prob,
    #                                                        APP_bd_prob,
    #                                                        rs_control_values)
    rs_best_and_summed_solution_PU_IDs =
            get_marxan_best_and_summed_solution_PU_IDs (APP_marxan_run,
                   exp_root_dir = parameters$fullOutputDir_NO_slash,
                                                        COR_bd_prob,
                                                        APP_bd_prob)

        #-------------------------
        #  Best OVERALL solution
        #-------------------------

    rs_best_solution_PU_IDs =
        rs_best_and_summed_solution_PU_IDs$marxan_best_solution_PU_IDs

    save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs (
                                                rs_best_solution_PU_IDs,
                        tzar_run_ID = parameters$run_ID,
                                           exp_root_dir = parameters$fullOutputDir_NO_slash,
                                              APP_marxan_run,
                                            COR_bd_prob,
                                            APP_bd_prob,
                                            rs_method_name,
                        csv_outfile_name = "rsrun_results.csv",
                                            marxan_control_values,
                                            src_rds_file_dir)

        #---------------------------------------------------
        #  Write out results for the best SUMMED solution.
        #---------------------------------------------------

    rs_best_summed_solution_PU_IDs =
        rs_best_and_summed_solution_PU_IDs$marxan_best_summed_solution_PU_IDs

    rs_method_name = "Marxan_SA_SS"
    save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs (
                                                rs_best_solution_PU_IDs,
                        tzar_run_ID = parameters$run_ID,
                        exp_root_dir = parameters$fullOutputDir_NO_slash,
                                                APP_marxan_run,
                                                COR_bd_prob,
                                                APP_bd_prob,
                                                rs_method_name,
        csv_outfile_name = "rsrun_results_summed_solution.csv",
                                                marxan_control_values,
                                                src_rds_file_dir)

    # save_rsrun_results_data_for_one_rsrun (tzar_run_ID = parameters$run_ID,
    #                                        exp_root_dir = parameters$fullOutputDir_NO_slash,
    #                                           APP_marxan_run,
    #                                         COR_bd_prob,
    #                                         APP_bd_prob,
    #                                         rs_method_name,
    #                                         marxan_control_values,
    #                                         src_rds_file_dir)

#    return (marxan_control_values$marxan_elapsed_time)
    return (marxan_control_values$RS_elapsed_time)
    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



