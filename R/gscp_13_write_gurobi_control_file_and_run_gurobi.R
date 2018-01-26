#===============================================================================

            #  gscp_13_write_gurobi_control_file_and_run_gurobi.R

#===============================================================================

#' Set up for and run gurobi for COR problem
#'
#' Convenience function to call set_up_for_and_run_gurobi with proper
#' arguments for a correct Xu_bd_problem.
#'
#-------------------------------------------------------------------------------

#' @param rsrun an RSrun object (or subclass)
#' @inheritParams std_param_defns
#'
#' @return Returns named list of gurobi control and return values
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_gurobi_COR <- function (COR_bd_prob,
                                           rsrun,
                                           parameters,
                                           marxan_elapsed_time=NA)
    {
    gurobi_control_values =
        set_up_for_and_run_gurobi (num_spp = COR_bd_prob@num_spp,
                                   num_PUs = COR_bd_prob@num_PUs,
                                   bpm     = COR_bd_prob@bpm,

                                   PU_costs     = COR_bd_prob@PU_costs,
                                   spp_rep_targets = rsrun@targets,

                                   use_gap_limit = parameters$use_gap_limit,
                                   gap_limit     = parameters$gap_limit,

                                   use_given_time_as_limit = parameters$use_given_time_as_limit,
                                   time_limit     = parameters$time_limit,

                                   parameters$use_marxan_time_as_limit,
                                   marxan_elapsed_time,

                                   rsrun,
                                   top_dir = parameters$fullOutputDir_NO_slash,
                                   save_inputs = TRUE,
                                   save_outputs = TRUE
                                   )

    return (gurobi_control_values)
    }

#===============================================================================

#' Set up for and run gurobi for APP problem
#'
#' Convenience function to call set_up_for_and_run_gurobi with proper
#' arguments for an apparent Xu_bd_problem
#'
#-------------------------------------------------------------------------------

#' @param rsrun an RSrun object (or subclass)
#' @inheritParams std_param_defns
#'
#' @return Returns named list of gurobi control and return values
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_gurobi_APP <- function (APP_bd_prob,
                                           COR_bd_prob,
                                           rsrun,
                                           parameters,
                                           marxan_elapsed_time=NA)
    {
    gurobi_control_values =
        set_up_for_and_run_gurobi (num_spp = COR_bd_prob@num_spp,
                                   num_PUs = COR_bd_prob@num_PUs,
                                   bpm     = APP_bd_prob@bpm,

                                   PU_costs        = APP_bd_prob@PU_costs,
                                   spp_rep_targets = rsrun@targets,

                                   use_gap_limit = parameters$use_gap_limit,
                                   gap_limit     = parameters$gap_limit,

                                   use_given_time_as_limit = parameters$use_given_time_as_limit,
                                   time_limit     = parameters$time_limit,

                                   parameters$use_marxan_time_as_limit,
                                   marxan_elapsed_time,

                                   rsrun,
                                   top_dir = parameters$fullOutputDir_NO_slash,
                                   save_inputs = TRUE,
                                   save_outputs = TRUE
                                   )

    return (gurobi_control_values)
    }

#===============================================================================

#' Set up for and run gurobi
#'
#' Set up for and run gurobi for either COR or APP
#'
#-------------------------------------------------------------------------------

#' @param use_given_time_as_limit boolean
#' @param time_limit numeric
#' @param use_gap_limit boolean
#' @param gap_limit numeric

#' @param save_inputs boolean indicating whether input model and input params
#'     should be saved to disk
#' @param save_outputs boolean indicating whether results and
#'     gurobi_controls_and_results params should be saved to disk

#' @inheritParams std_param_defns
#'
#' @return Returns named list of gurobi control and return values

#-------------------------------------------------------------------------------

set_up_for_and_run_gurobi <- function (num_spp,
                                       num_PUs,
                                       bpm,
                                       PU_costs,
                                       spp_rep_targets,
                                       use_gap_limit, gap_limit,
                                       use_given_time_as_limit,
                                       time_limit,
                                       use_marxan_time_as_limit,
                                       marxan_elapsed_time=NA,

                                       rsrun,
                                       top_dir,
                                       save_inputs,
                                       save_outputs
                                       )
    {
    gurobi_result =
        run_gurobi (num_spp, num_PUs, bpm, PU_costs, spp_rep_targets,
                    use_gap_limit, gap_limit,
                    use_given_time_as_limit, time_limit,
                    use_marxan_time_as_limit, marxan_elapsed_time,

                    rsrun,
                    top_dir,
                    save_inputs = TRUE,
                    save_outputs = TRUE
                    )

    return (gurobi_result)
    }

#===============================================================================

