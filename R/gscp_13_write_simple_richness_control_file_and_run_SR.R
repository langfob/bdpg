#===============================================================================

        #  gscp_13_write_simple_richness_control_file_and_run_SR.R

#===============================================================================

#' Run simple_richness on COR problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export
#'
#-------------------------------------------------------------------------------

do_COR_simple_richness_analysis_and_output <-
                        function (COR_bd_prob,
                                  parameters,
                                  starting_dir,
                                  rs_method_name,
                                  forward = TRUE,
                                  src_rds_file_dir = NULL,
                                  spp_rep_targets = rep (1,COR_bd_prob@num_spp))
    {
        #-------------------------
        #  Run reserve selector.
        #-------------------------

    COR_rs_run <- create_RSrun (COR_bd_prob@UUID,
                                spp_rep_targets,
                                parameters,
                                starting_dir,
                                COR_bd_prob@cor_or_app_str,
                                COR_bd_prob@basic_or_wrapped_or_comb_str,
                                rs_method_name)

    rs_control_values = set_up_for_and_run_simple_richness_COR (COR_bd_prob,
                                                                COR_rs_run,
                                                                parameters,
                                                                starting_dir,
                                                                forward)

        #-------------------------------------
        #  Collect reserve selector results.
        #-------------------------------------

    save_rsrun_results_data_for_one_rsrun (
                            tzar_run_ID  = parameters$run_id,

#                            exp_root_dir = parameters$fullOutputDir_NO_slash,
                            exp_root_dir = starting_dir,

                            COR_rs_run,
                            COR_bd_prob,
                            COR_bd_prob,
                            rs_method_name,
                            rs_control_values,
                            src_rds_file_dir)

    }  #  end function - do_COR_rs_analysis_and_output

#===============================================================================

#' Run reserve selector on APP problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_APP_simple_richness_analysis_and_output <-
                        function (APP_bd_prob,
                                  COR_bd_prob,
                                  parameters,
                                  starting_dir,
                                  rs_method_name,
                                  forward = TRUE,
                                  src_rds_file_dir = NULL,
                                  spp_rep_targets = rep (1,COR_bd_prob@num_spp))
    {
        #-------------------------
        #  Run reserve selector.
        #-------------------------

    APP_rs_run <- create_RSrun (APP_bd_prob@UUID,
                                spp_rep_targets,
                                parameters,
                                starting_dir,
                                APP_bd_prob@cor_or_app_str,
                                APP_bd_prob@basic_or_wrapped_or_comb_str,
                                rs_method_name)

    rs_control_values = set_up_for_and_run_simple_richness_APP (APP_bd_prob,
                                                                COR_bd_prob,
                                                                APP_rs_run,
                                                                parameters,
                                                                starting_dir,
                                                                forward)

        #-------------------------------------
        #  Collect reserve selector results.
        #-------------------------------------

    save_rsrun_results_data_for_one_rsrun (
                            tzar_run_ID  = parameters$run_id,

#                            exp_root_dir = parameters$fullOutputDir_NO_slash,
                            exp_root_dir = starting_dir,

                            APP_rs_run,
                            COR_bd_prob,
                            APP_bd_prob,
                            rs_method_name,
                            rs_control_values,
                            src_rds_file_dir)

    }  #  end function - do_APP_rs_analysis_and_output

#===============================================================================

#' Set up for and run simple_richness for COR problem
#'
#' Convenience function to call set_up_for_and_run_simple_richness with proper
#' arguments for a correct Xu_bd_problem.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns named list of simple_richness control and return values
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_simple_richness_COR <- function (COR_bd_prob,
                                                    rsrun,
                                                    parameters,
                                                    starting_dir,
                                                    forward = TRUE)
    {
    simple_richness_control_values =
        set_up_for_and_run_simple_richness (num_spp = COR_bd_prob@num_spp,
                                            num_PUs = COR_bd_prob@num_PUs,
                                            bpm     = COR_bd_prob@bpm,

                                            PU_costs = COR_bd_prob@PU_costs,
                                            spp_rep_targets = rsrun@targets,

                                            forward,

                                            rsrun,

#                                            top_dir = parameters$fullOutputDir_NO_slash,
                                            top_dir = starting_dir,

                                            save_inputs = TRUE,
                                            save_outputs = TRUE)

    return (simple_richness_control_values)
    }

#===============================================================================

#' Set up for and run simple_richness for APP problem
#'
#' Convenience function to call set_up_for_and_run_simple_richness with proper
#' arguments for an apparent Xu_bd_problem
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns named list of simple_richness control and return values
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_simple_richness_APP <- function (APP_bd_prob,
                                                    COR_bd_prob,
                                                    rsrun,
                                                    parameters,
                                                    starting_dir,
                                                    forward = TRUE)
    {
    simple_richness_control_values =
        set_up_for_and_run_simple_richness (num_spp = COR_bd_prob@num_spp,
                                            num_PUs = COR_bd_prob@num_PUs,
                                            bpm     = APP_bd_prob@bpm,

                                            PU_costs = APP_bd_prob@PU_costs,
                                            spp_rep_targets = rsrun@targets,

                                            forward,

                                            rsrun,

#                                            top_dir = parameters$fullOutputDir_NO_slash,
                                            top_dir = starting_dir,

                                            save_inputs = TRUE,
                                            save_outputs = TRUE)

    return (simple_richness_control_values)
    }

#===============================================================================

#' Set up for and run simple_richness
#'
#' Set up for and run simple_richness for either COR or APP
#'
#-------------------------------------------------------------------------------

#' @param save_inputs boolean indicating whether input model and input params
#'     should be saved to disk
#' @param save_outputs boolean indicating whether results and
#'     simple_richness_controls_and_results params should be saved to disk

#' @inheritParams std_param_defns
#'
#' @return Returns named list of simple_richness control and return values

#-------------------------------------------------------------------------------

set_up_for_and_run_simple_richness <- function (num_spp,
                                                num_PUs,
                                                bpm,
                                                PU_costs,
                                                spp_rep_targets,

                                                forward,

                                                rsrun,
                                                top_dir,
                                                save_inputs,
                                                save_outputs)
    {
    simple_richness_controls_and_results =
                    run_simple_richness (num_spp,
                                         num_PUs,
                                         bpm,

                                         forward = TRUE,
                                         spp_rep_targets,

                                         rsrun,
                                         top_dir,            #= parameters$fullOutputDir_NO_slash
                                         save_inputs = TRUE,
                                         save_outputs = TRUE)

        #---------------------------------------------------------------------
        #  Need to strip the solution vector out of the result.
        #  The list returned from here will be added to the bdpg results
        #  as part of a single line in a data frame and therefore,
        #  can only contain single scalar values.
        #  If the list contains a vector, then each element is added on a
        #  new line in the rsrun results file with all scalar values in the
        #  list copied on the new line.  In other words, if there are 100
        #  PUs in the solution vector, there will be 100 identical lines
        #  (plus a header line) in the resulting output file.
        #
        #  We don't want to remove the solution vector from the return in
        #  the run_simple_richness() function itself because it can be useful to
        #  have the returned solution vector directly available rather than
        #  having to read it back in from disk, e.g., in testing.
        #---------------------------------------------------------------------

    simple_richness_control_values = simple_richness_controls_and_results
    simple_richness_control_values$simple_richness_solution_vector = NULL

    return (simple_richness_control_values)
    }

#===============================================================================

