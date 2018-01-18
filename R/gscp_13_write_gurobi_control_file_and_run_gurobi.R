#===============================================================================

            #  gscp_13_write_gurobi_control_file_and_run_gurobi.R

#===============================================================================

#' Set up for and run gurobi for COR problem
#'
#' Convenience function to call set_up_for_and_run_gurobi with proper
#' arguments for a correct Xu_bd_problem.
#'
#-------------------------------------------------------------------------------

#' @param COR_bd_prob a correct Xu_bd_problem (or subclass)
#' @param gurobi_run an RSrun object (or subclass)
#' @param parameters parameters list for the run, usually derived from project.yaml
#'
#' @return Returns named list of gurobi control and return values
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_gurobi_COR <- function (COR_bd_prob,
                                           gurobi_run,
                                           parameters)
    {
    gurobi_control_values =
        set_up_for_and_run_gurobi (num_spp = COR_bd_prob@num_spp,
                                   num_PUs = COR_bd_prob@num_PUs,
                                   bpm     = COR_bd_prob@bpm,

                                   PU_costs    = COR_bd_prob@PU_costs,
                                   spp_targets = gurobi_run@targets,

                                   use_time_limit = parameters$use_time_limit,
                                   time_limit     = parameters$time_limit,

                                   use_gap_limit = parameters$use_gap_limit,
                                   gap_limit     = parameters$gap_limit)

    return (gurobi_control_values)
    }

#===============================================================================

#' Set up for and run gurobi for APP problem
#'
#' Convenience function to call set_up_for_and_run_gurobi with proper
#' arguments for an apparent Xu_bd_problem
#'
#-------------------------------------------------------------------------------

#' @param APP_bd_prob an apparent Xu_bd_problem (or subclass)
#' @param COR_bd_prob the correct Xu_bd_problem (or subclass) that the apparent problem is derived from
#' @param gurobi_run an RSrun object (or subclass)
#' @param parameters parameters list for the run, usually derived from project.yaml
#'
#' @return Returns named list of gurobi control and return values
#' @export

#-------------------------------------------------------------------------------

set_up_for_and_run_gurobi_APP <- function (APP_bd_prob,
                                           COR_bd_prob,
                                           gurobi_run,
                                           parameters)
    {
    gurobi_control_values =
        set_up_for_and_run_gurobi (num_spp = COR_bd_prob@num_spp,
                                   num_PUs = COR_bd_prob@num_PUs,
                                   bpm     = APP_bd_prob@bpm,

                                   PU_costs    = APP_bd_prob@PU_costs,
                                   spp_targets = gurobi_run@targets,

                                   use_time_limit = parameters$use_time_limit,
                                   time_limit     = parameters$time_limit,

                                   use_gap_limit = parameters$use_gap_limit,
                                   gap_limit     = parameters$gap_limit)

    return (gurobi_control_values)
    }

#===============================================================================

#' Set up for and run gurobi
#'
#' Set up for and run gurobi for either COR or APP
#'
#-------------------------------------------------------------------------------

#' @param use_time_limit boolean
#' @param time_limit numeric
#' @param use_gap_limit boolean
#' @param gap_limit numeric
#' @inheritParams std_param_defns
#'
#' @return Returns named list of gurobi control and return values

#-------------------------------------------------------------------------------

set_up_for_and_run_gurobi <- function (num_spp,
                                       num_PUs,
                                       bpm,
                                       PU_costs,
                                       spp_targets,
                                       use_time_limit, time_limit,
                                       use_gap_limit, gap_limit)
    {
    gurobi_result =
        run_gurobi (num_spp, num_PUs, bpm, PU_costs, spp_targets,
                    use_time_limit, time_limit, use_gap_limit, gap_limit)

    return (gurobi_result)
    }

#===============================================================================

