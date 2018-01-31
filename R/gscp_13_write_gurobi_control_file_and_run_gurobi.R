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
    ResSel_timings = system.time (
        {
        gurobi_controls_and_results =
            run_gurobi (num_spp, num_PUs, bpm, PU_costs, spp_rep_targets,
                        use_gap_limit, gap_limit,
                        use_given_time_as_limit, time_limit,
                        use_marxan_time_as_limit, marxan_elapsed_time,

                        rsrun,
                        top_dir,
                        save_inputs = TRUE,
                        save_outputs = TRUE
                        )
        })

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
        #  the run_gurobi() function itself because it can be useful to
        #  have the returned solution vector directly available rather than
        #  having to read it back in from disk, e.g., in testing.
        #---------------------------------------------------------------------

    gurobi_control_values = gurobi_controls_and_results
    gurobi_control_values$gurobi_solution_vector = NULL

    gurobi_control_values$RS_user_time       = ResSel_timings["user.self"]
    gurobi_control_values$RS_system_time     = ResSel_timings["sys.self"]
    gurobi_control_values$RS_elapsed_time    = ResSel_timings["elapsed"]
    gurobi_control_values$RS_user_child_time = ResSel_timings["user.child"]
    gurobi_control_values$RS_sys_child_time  = ResSel_timings["sys.child"]

    return (gurobi_control_values)
    }

#===============================================================================

