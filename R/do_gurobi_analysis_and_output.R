#===============================================================================

                    #  do_gurobi_analysis_and_output.R

#===============================================================================

#' Run gurobi linear programming optimizer
#'
#' @param use_given_time_as_limit boolean indicating whether to set a time limit on the
#'     optimization computation; TRUE implies set the limit, FALSE implies no
#'     time limit
#' @param time_limit numeric value for time limit measured in seconds
#' @param use_gap_limit boolean indicating whether to set a maximum gap allowed
#'     between the result of the optimization computation and the correct
#'     optimum value; TRUE implies set the gap value, FALSE implies no gap
#'     enforced
#' @param gap_limit numeric value for gap, measured in same units as the
#'     objective function
#'
#' @param save_inputs boolean indicating whether input model and input params
#'     should be saved to disk
#' @param save_outputs boolean indicating whether results and
#'     gurobi_controls_and_results params should be saved to disk
#'
#' @inheritParams std_param_defns
#'
#' @return Returns a gurobi model result list with named elements that contains,
#'     among other things, an element x which is the solution vector
#' @importFrom gurobi gurobi
#' @export

#-------------------------------------------------------------------------------

run_gurobi <- function (num_spp,
                        num_PUs,
                        bpm,
                        PU_costs,
                        spp_rep_targets,

                        use_gap_limit = FALSE,
                        gap_limit = 0.005,

                        use_given_time_as_limit = TRUE,
                        time_limit = 60,

                        use_marxan_time_as_limit = FALSE,
                        marxan_elapsed_time = NA,

                        rsrun = NULL,
                        top_dir = NULL,            #= parameters$fullOutputDir_NO_slash
                        save_inputs = FALSE,
                        save_outputs = FALSE
                        )
    {
        # set up Gurobi model object in R
    gurobi_model <- list()

        #  2018 01 20 - BTL
        #  Gurobi is leaving a log file in the bdpg root area when I run
        #  test code and CHECK is complaining about that.
        #  Haven't found any good explanation of how to put it somewhere
        #  else but have found some flags.  However, nothing mentions
        #  where these flags go, i.e., do I just add them to the model
        #  structure or are they command line flags or ???
        #  I'm going to try adding them to the model parameters list and
        #  see what happens...
    gurobi_model$LogToConsole = TRUE
    #gurobi_model$LogFile = "anotherGurobiLog"

        #  Assign the constraints matrix object
    constr = bpm
    gurobi_model$A <- constr

        #----------------------------------------------------------------
        #  Assign the species target amounts as the right hand side.
        #----------------------------------------------------------------
        #  Deal with infeasibles resulting from adding error to inputs,
        #  i.e., FNs making it so that some species have no occurrences
        #  and therefore,have no possible way of meeting a target > 0.
        #  So, whenever there aren't enough occurrences of a species to
        #  possibly meet that species' target, reduce the target to
        #  match the number of occurrences that do exist, even if that
        #  number is 0.
        #  This should make it feasible to find a solution to any
        #  given problem.
        #----------------------------------------------------------------

#bpm = matrix (c (0, 0, 1, 1), nrow=2, ncol=2, byrow=TRUE)
#spp_rep_targets = c(1,1)

#spp_abundances = apply (bpm, 1, sum)
spp_abundances = rowSums (bpm)
if (length (spp_abundances) != length (spp_rep_targets))
    stop_bdpg (paste0 ("length (spp_abundances) = '",
                       length (spp_abundances),
                       "' NOT EQUAL TO length (spp_rep_targets) = '",
                       length (spp_rep_targets), "'"))
spp_with_subtarget_abundances = which (spp_abundances < spp_rep_targets)
num_subtarget_abundances = length (spp_with_subtarget_abundances)
cat ("\n\nlength (spp_abundances) = ", length (spp_abundances),
     "\n\nnum_subtarget_abundances = ", num_subtarget_abundances,
     "\n")
if (num_subtarget_abundances > 0)
    {
    cat ("\nspp_with_subtarget_abundances = \n")
    print (spp_with_subtarget_abundances)
    }

rhs = pmin (spp_rep_targets, spp_abundances)

    gurobi_model$rhs <- rhs

        #  Make sure that the solution must have all species amounts be
        #  greater than or equal to the species target amounts.
    sense <- rep(">=", num_spp)
    gurobi_model$sense <- sense

        #  Set this as a minimisation problem:
    gurobi_model$modelsense <- "min"

        #  Set all decision variables as binary:
    gurobi_model$vtype <- "B"

        #  Define the objective function to be over the PU costs.
    gurobi_model$obj <- as.vector (t (PU_costs))

        #  Set the parameters that control the algorithm.
    gurobi_params <- list (Presolve = 2)
        #  http://www.gurobi.com/support/faqs
        #  13.  How do you set multiple termination criteria for a model?
        #  When you set multiple termination parameters, Gurobi Optimizer
        #  will stop when it reaches the first one. For example, if you
        #  want to stop if you get to a 10% MIP gap or if 60 seconds have
        #  elapsed, then set MIPGap=0.1 and TimeLimit=60. If you want a
        #  more complex set of stopping criteria, you will need to use
        #  warm starts. For example, suppose you need a 1% MIP gap, but
        #  you would prefer a smaller MIP gap if it takes less than
        #  300 seconds. In this case, start with TimeLimit=300.
        #  If the value of the MIPGap attribute is greater than 0.01
        #  when the time limit is reached, then increase the TimeLimit
        #  parameter, set the MIPGap parameter to 0.01, and continue to
        #  solve the MIP.
    use_gap_limit = vb (use_gap_limit, def_on_empty = TRUE, def = FALSE)
    if (use_gap_limit)
        {
        gurobi_params$MIPGap = vn (gap_limit, range_lo = 0, bounds_types = "ii")
        cat ("\nrun_gurobi() setting MIPGap to '", gurobi_params$MIPGap,
             "'\n", sep='')
        }

        #-----------------------------------------------------------------------
        #  If the caller wants to use a time limit, then they can either
        #  specify the time limit directly or they can choose to use
        #  marxan's elapsed run time so that gurobi gets roughly the same
        #  amount of time as it took marxan to complete.
        #-----------------------------------------------------------------------

    use_given_time_as_limit = vb (use_given_time_as_limit,
                                  def_on_empty = TRUE, def = FALSE)
    use_marxan_time_as_limit = vb (use_marxan_time_as_limit,
                                   def_on_empty = TRUE, def = FALSE)

    if (use_given_time_as_limit && use_marxan_time_as_limit)
        {
        stop_bdpg (paste0 ("Can't set both use_given_time_as_limit and ",
                           "use_marxan_time_as_limit to TRUE."))
        }

    if (use_given_time_as_limit || use_marxan_time_as_limit)
        {
        if (use_given_time_as_limit)
            {
            gurobi_params$TimeLimit = vn (time_limit,
                                          range_lo = 0, bounds_types = "ei")

            } else if (use_marxan_time_as_limit)
            {
            gurobi_params$TimeLimit =
                ceiling (vn (marxan_elapsed_time,
                             range_lo = 0, bounds_types = "ei"))
            }

        cat ("\nrun_gurobi() setting TimeLimit to '", gurobi_params$TimeLimit,
             "'\n", sep='')
        }

    cat ("\n--------------------\n\n")

    if (save_inputs || save_outputs)
        {
                    #  Get paths to the gurobi IO subdirectories.

#        gurobi_IO_dir     = get_RSrun_path_IO (rsrun, top_dir)
        gurobi_input_dir  = get_RSrun_path_input (rsrun, top_dir)
        gurobi_output_dir = get_RSrun_path_output (rsrun, top_dir)
        }

    if (save_inputs)
        {
        saveRDS (gurobi_model,
                 file.path (gurobi_input_dir, "gurobi_mode.rds"))
        saveRDS (gurobi_params,
                 file.path (gurobi_input_dir, "gurobi_params.rds"))
        }

    #--------------------

        # solve the problem
    # result <- gurobi::gurobi (gurobi_model_and_params$gurobi_model,
    #                           gurobi_model_and_params$gurobi_params)

    result <- gurobi::gurobi (gurobi_model, gurobi_params)

    #--------------------

        # the result object contains several data objects including the objective
        # value achieved (result$objval) and the vector of decision variable
        # values (0 or 1 in our example because the variables are binary).

    cat ("\n--------------------\n\n")

    cat ("Gurobi result:\n")
    print (result)

if(FALSE)
{
    solution = result$x
    solution_node_IDs = which (solution == 1)

    is_solution =
        verify_that_generated_solution_really_is_a_solution (bpm,
                                                             solution_node_IDs,
                                                             num_spp,
                                                             num_PUs,
                                                             PU_costs)

    cat ("\n\nVerifying that Gurobi solution really is a solution: '",
         is_solution, "'\n")
}

# gurobi_output_scalars =
#     list (
#           gurobi_status       = result$status,
#           gurobi_objval       = result$objval,
#           gurobi_objbound     = result$objbound,
#           gurobi_runtime      = result$runtime,
#           gurobi_itercount    = result$itercount,
#           gurobi_baritercount = result$baritercount,
#           gurobi_nodecount    = result$nodecount
#           )

# gurobi_input_scalars =
#     list (
#           gurobi_num_spp                   = num_spp,
#           gurobi_num_PUs                   = num_PUs,
#
#           gurobi_use_gap_limit             = use_gap_limit,
#           gurobi_gap_limit_input           = gap_limit,
#           gurobi_gap_limit_used            = gurobi_params$MIPGap,
#
#           gurobi_use_given_time_as_limit   = use_given_time_as_limit,
#           gurobi_time_limit_input          = time_limit,
#
#           gurobi_use_marxan_time_as_limit  = use_marxan_time_as_limit,
#           gurobi_marxan_elapsed_time_input = marxan_elapsed_time,
#
#           gurobi_time_limit_used           = gurobi_params$TimeLimit
#           )

# gurobi_controls_and_results =
#     list (gurobi_solution_vector = result$x,
#           gurobi_output_scalars  = gurobi_output_scalars,
#           gurobi_input_scalars   = gurobi_input_scalars)

gurobi_controls_and_results =
    list (gurobi_solution_vector = result$x,

            #  Output scalars
          gurobi_status       = result$status,
          gurobi_objval       = result$objval,
          gurobi_objbound     = result$objbound,
          gurobi_runtime      = result$runtime,
          gurobi_itercount    = result$itercount,
          gurobi_baritercount = result$baritercount,
          gurobi_nodecount    = result$nodecount,

            #  Input scalars
          gurobi_num_spp                   = num_spp,
          gurobi_num_PUs                   = num_PUs,

          gurobi_use_gap_limit             = use_gap_limit,
          gurobi_gap_limit_input           = gap_limit,
          gurobi_gap_limit_used            = gurobi_params$MIPGap,
#          gurobi_gap_limit_used            = gurobi_model_and_params$gurobi_params$MIPGap,

          gurobi_use_given_time_as_limit   = use_given_time_as_limit,
          gurobi_time_limit_input          = time_limit,

          gurobi_use_marxan_time_as_limit  = use_marxan_time_as_limit,
          gurobi_marxan_elapsed_time_input = marxan_elapsed_time,

          gurobi_time_limit_used           = gurobi_params$TimeLimit
#          gurobi_time_limit_used           = gurobi_model_and_params$gurobi_params$TimeLimit
          )

    if (save_outputs)
        {
        saveRDS (result,
                 file.path (gurobi_output_dir, "result.rds"))
        saveRDS (gurobi_controls_and_results,
                 file.path (gurobi_output_dir, "gurobi_controls_and_results.rds"))
        }

#browser()
    return (gurobi_controls_and_results)
    }

#===============================================================================

#' Run gurobi on COR problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export
#'
#-------------------------------------------------------------------------------

do_COR_gurobi_analysis_and_output <- function (COR_bd_prob,
                                               parameters,
                                               rs_method_name,
                                               marxan_elapsed_time,
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp))
    {
        #-------------------------
        #  Run reserve selector.
        #-------------------------

    COR_rs_run <- create_RSrun (COR_bd_prob@UUID,
                                    spp_rep_targets,

                            parameters,
                            # value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
                            # parameters$rsrun_rand_seed,
                            #         parameters$fullOutputDir_NO_slash,

                                    COR_bd_prob@cor_or_app_str,
                                    COR_bd_prob@basic_or_wrapped_or_comb_str,
                                    rs_method_name)    # = rs_method_name  #"Marxan_SA"

    rs_control_values = set_up_for_and_run_gurobi_COR (COR_bd_prob,
                                                           COR_rs_run,
                                                           parameters,
                                                       marxan_elapsed_time)

        #-------------------------------------
        #  Collect reserve selector results.
        #-------------------------------------

    save_rsrun_results_data_for_one_rsrun (tzar_run_ID = parameters$run_ID,
                                           exp_root_dir = parameters$fullOutputDir_NO_slash,
                                              COR_rs_run,
                                              COR_bd_prob,
                                              COR_bd_prob,
                                           rs_method_name,
                                           rs_control_values,
                                           src_rds_file_dir)

    }  #  end function - do_COR_rs_analysis_and_output

#===============================================================================
#===============================================================================
#===============================================================================

#' Run reserve selector on APP problem and write output from all analysis
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

do_APP_gurobi_analysis_and_output <- function (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                               rs_method_name,
                                               marxan_elapsed_time,
                                               src_rds_file_dir=NULL,
                                               spp_rep_targets=rep(1,COR_bd_prob@num_spp)
                                               )
    {
        #-------------------------
        #  Run reserve selector.
        #-------------------------

    APP_rs_run <- create_RSrun (APP_bd_prob@UUID,
                                    spp_rep_targets,

                            parameters,
                            # value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
                            # parameters$rsrun_rand_seed,
                            #         parameters$fullOutputDir_NO_slash,

                                    APP_bd_prob@cor_or_app_str,
                                    APP_bd_prob@basic_or_wrapped_or_comb_str,
                                    rs_method_name)    # = rs_method_name  #"Marxan_SA"

    rs_control_values = set_up_for_and_run_gurobi_APP (APP_bd_prob,
                                                           COR_bd_prob,
                                                           APP_rs_run,
                                                           parameters,
                                                       marxan_elapsed_time)

        #-------------------------------------
        #  Collect reserve selector results.
        #-------------------------------------

    save_rsrun_results_data_for_one_rsrun (tzar_run_ID = parameters$run_ID,
                                           exp_root_dir = parameters$fullOutputDir_NO_slash,
                                              APP_rs_run,
                                              COR_bd_prob,
                                              APP_bd_prob,
                                           rs_method_name,
                                           rs_control_values,
                                           src_rds_file_dir)

    }  #  end function - do_APP_rs_analysis_and_output

#===============================================================================

