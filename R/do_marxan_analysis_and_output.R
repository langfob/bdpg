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
                                           starting_dir,
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
                                    starting_dir,
                                    APP_bd_prob@cor_or_app_str,
                                    APP_bd_prob@basic_or_wrapped_or_comb_str,
                                    rs_method_name)

    marxan_run_dir = get_RSrun_path_topdir (APP_marxan_run, starting_dir)

    marxan_control_values = set_up_for_and_run_marxan_APP (APP_bd_prob,
                                                           COR_bd_prob,
                                                           APP_marxan_run,
                                                           parameters,
                                                           starting_dir)

        #---------------------------
        #  Collect marxan results.
        #---------------------------

    # rs_best_solution_PU_IDs = get_rs_best_solution_PU_IDs (rs_method_name,
    #                                                        rsrun,
    #                                                        exp_root_dir,
    #                                                        COR_bd_prob,
    #                                                        APP_bd_prob,
    #                                                        rs_control_values)
    rs_best_and_summed_solution_PU_IDs =
            get_marxan_best_and_summed_solution_PU_IDs (APP_marxan_run,

#                   exp_root_dir = parameters$fullOutputDir_NO_slash,
                   exp_root_dir = starting_dir,

                                                        COR_bd_prob,
                                                        APP_bd_prob)

        #-------------------------
        #  Best OVERALL solution
        #-------------------------

    rs_best_solution_PU_IDs =
#        rs_best_and_summed_solution_PU_IDs$marxan_best_solution_PU_IDs
        rs_best_and_summed_solution_PU_IDs$rs_best_solution_PU_IDs

            #---------------------------------------------------------
            #  Save the best solution vector to disk in its own file
            #  to make it easier to find and retrieve.
            #---------------------------------------------------------

    Marxan_SA_best_solution_file_name = "Marxan_SA_best_solution_PU_IDs.csv"
    Marxan_SA_best_solution_file_path =
        file.path (marxan_run_dir, Marxan_SA_best_solution_file_name)
    write (rs_best_solution_PU_IDs,
           Marxan_SA_best_solution_file_path, sep=",")

            #---------------------------------------------------------
            #  Save all of the input variables and output scores etc
            #  to disk as one big one line table with headers.
            #---------------------------------------------------------

    save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs (
                                                rs_best_solution_PU_IDs,
                        tzar_run_ID = parameters$run_id,

                   # exp_root_dir = parameters$fullOutputDir_NO_slash,
                   exp_root_dir = starting_dir,

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

            #-----------------------------------------------------------
            #  Save the summed solution vector to disk in its own file
            #  to make it easier to find and retrieve.
            #-----------------------------------------------------------

    Marxan_SA_SS_summed_solution_file_name = "Marxan_SA_SS_summed_solution_PU_IDs.csv"
    Marxan_SA_SS_summed_solution_file_path =
        file.path (marxan_run_dir, Marxan_SA_SS_summed_solution_file_name)
    write (rs_best_summed_solution_PU_IDs,
           Marxan_SA_SS_summed_solution_file_path, sep=",")

            #---------------------------------------------------------
            #  Save all of the input variables and output scores etc
            #  to disk as one big one line table with headers.
            #---------------------------------------------------------

    rs_method_name = "Marxan_SA_SS"
    save_rsrun_results_data_for_one_rsrun_given_solution_PU_IDs (

                                                # rs_best_solution_PU_IDs,
                                                rs_best_summed_solution_PU_IDs,

                        tzar_run_ID = parameters$run_id,

                   # exp_root_dir = parameters$fullOutputDir_NO_slash,
                   exp_root_dir = starting_dir,

                                                APP_marxan_run,
                                                COR_bd_prob,
                                                APP_bd_prob,
                                                rs_method_name,
        csv_outfile_name = "rsrun_results_summed_solution.csv",
                                                marxan_control_values,
                                                src_rds_file_dir)

    # save_rsrun_results_data_for_one_rsrun (tzar_run_ID = parameters$run_id,

    #                                        #exp_root_dir = parameters$fullOutputDir_NO_slash,
    #                                        exp_root_dir = starting_dir,

    #                                           APP_marxan_run,
    #                                         COR_bd_prob,
    #                                         APP_bd_prob,
    #                                         rs_method_name,
    #                                         marxan_control_values,
    #                                         src_rds_file_dir)


    RSrun_topdir = get_RSrun_path_topdir (APP_marxan_run, starting_dir)
#    return (marxan_control_values$marxan_elapsed_time)

    return (list (marxan_topdir = RSrun_topdir,
                  marxan_elapsed_time = marxan_control_values$RS_elapsed_time))

    }  #  end function - do_APP_marxan_analysis_and_output

#===============================================================================



