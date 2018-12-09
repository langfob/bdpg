#===============================================================================

                    #  do_gurobi_analysis_and_output.R

#  2018 02 08 - BTL
#  Temporarily removed this line from run_gurobi() docs below.
#  importFrom gurobi gurobi


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
        #------------------------------------
        #  Set up Gurobi model object in R.
        #------------------------------------

    gurobi_model <- list()

        #-----------------------------------------------------------------
        #  2018 01 20 - BTL
        #  Gurobi is leaving a log file in the bdpg root area when I run
        #  test code and CHECK is complaining about that.
        #  Haven't found any good explanation of how to put it somewhere
        #  else but have found some flags.  However, nothing mentions
        #  where these flags go, i.e., do I just add them to the model
        #  structure or are they command line flags or ???
        #  I'm going to try adding them to the model parameters list and
        #  see what happens...
        #-----------------------------------------------------------------

    gurobi_model$LogToConsole = TRUE
    #gurobi_model$LogFile = "anotherGurobiLog"

        #----------------------------------------
        #  Assign the constraints matrix object
        #----------------------------------------

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

spp_abundances = rowSums (bpm)
if (length (spp_abundances) != length (spp_rep_targets))
    stop_bdpg (paste0 ("length (spp_abundances) = '",
                       length (spp_abundances),
                       "' NOT EQUAL TO length (spp_rep_targets) = '",
                       length (spp_rep_targets), "'"))
spp_with_subtarget_abundances = which (spp_abundances < spp_rep_targets)
num_subtarget_abundances = length (spp_with_subtarget_abundances)

if (num_subtarget_abundances > 0)
    {
    cat ("\nspp_with_subtarget_abundances = \n")
    print (spp_with_subtarget_abundances)
    }

rhs = pmin (spp_rep_targets, spp_abundances)

    gurobi_model$rhs <- rhs

        #----------------------------------------------------------------
        #  Make sure that the solution must have all species amounts be
        #  greater than or equal to the species target amounts.
        #----------------------------------------------------------------

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

        #------------------------------------------------------------------
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
        #------------------------------------------------------------------

    use_gap_limit = vb (use_gap_limit, def_on_empty = TRUE, def = FALSE)
    if (use_gap_limit)
        {
        gurobi_params$MIPGap = vn (gap_limit, range_lo = 0, bounds_types = "ii")
        cat ("\nrun_gurobi() setting MIPGap to '", gurobi_params$MIPGap,
             "'\n", sep='')
        }

        #------------------------------------------------------------------
        #  If the caller wants to use a time limit, then they can either
        #  specify the time limit directly or they can choose to use
        #  marxan's elapsed run time so that gurobi gets roughly the same
        #  amount of time as it took marxan to complete.
        #------------------------------------------------------------------

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

    if (save_inputs)
        {
        gurobi_input_dir  = get_RSrun_path_input (rsrun, top_dir)

        saveRDS (gurobi_model,
                 file.path (gurobi_input_dir, "gurobi_model.rds"))
        saveRDS (gurobi_params,
                 file.path (gurobi_input_dir, "gurobi_params.rds"))
        }

        #--------------------
        # solve the problem
        #--------------------

    result <- gurobi::gurobi (gurobi_model, gurobi_params)

        #-----------------------------------------------------------------
        #  The result object contains several data objects including the
        #  objective value achieved (result$objval) and the vector of
        #  decision variable values (0 or 1 in our example because the
        #  variables are binary).
        #-----------------------------------------------------------------

    cat ("\n--------------------\n\n")
    cat ("Gurobi result:\n")
    print (result)

                #-----------------------------------------------------------
                #  Here's an example of what gurobi result structure looks
                #  like when printed:
                #-----------------------------------------------------------

                # Gurobi result:
                # $status
                # [1] "OPTIMAL"
                #
                # $runtime
                # [1] 0.005516052
                #
                # $itercount
                # [1] 0
                #
                # $baritercount
                # [1] 0
                #
                # $nodecount
                # [1] 0
                #
                # $objval
                # [1] 79
                #
                # $x
                #   [1] 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
                #  [38] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
                #  [75] 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
                # [112] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
                # [149] 0 1 0 1 0 1 0 1 0 1
                #
                # $pool
                # $pool[[1]]
                # $pool[[1]]$objval
                # [1] 79
                #
                # $pool[[1]]$xn
                #   [1] 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
                #  [38] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
                #  [75] 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0
                # [112] 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
                # [149] 0 1 0 1 0 1 0 1 0 1
                #
                #
                #
                # $poolobjbound
                # [1] 79
                #
                # $slack
                #   [1]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
                #  [26]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
                #  [51]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
                #  [76]  0  0  0  0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [101] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [126] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [151] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [176] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [201] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [226] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [251] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [276] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [301] -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                # [326] -1 -1 -1 -1 -1 -1 -1
                #
                # $objbound
                # [1] 79
                #
                # $objboundc
                # [1] 79
                #
                # $mipgap
                # [1] 0


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

    gurobi_controls =
        list (
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

              gurobi_use_given_time_as_limit   = use_given_time_as_limit,
              gurobi_time_limit_input          = time_limit,

              gurobi_use_marxan_time_as_limit  = use_marxan_time_as_limit,
              gurobi_marxan_elapsed_time_input = marxan_elapsed_time,

              gurobi_time_limit_used           = gurobi_params$TimeLimit,

                    #-----------------------------------------------------------
                    #  2018 07 02 - BTL
                    #  Late additions of more scalar outputs.
                    #
                    #  Adding them at the end of the list so that maybe in
                    #  the output csv file, they will show up beyond the
                    #  end of existing values from earlier runs and therefore,
                    #  not mess up combined spreadsheets.
                    #
                    #  Mostly adding these to get the mipgap variable;
                    #  it's 0 when gurobi has enough time to finish but
                    #  can be non-zero when it has a time or gap limit that
                    #  is met, e.g., on a hard problem.
                    #  I doubt that any of the other values are useful, but
                    #  I hadn't realized mipgap would be useful either, so
                    #  I'm just adding every scalar value that gurobi has
                    #  returned.
                    #-----------------------------------------------------------

              gurobi_runtime      = result$runtime,
              gurobi_poolobjbound = result$poolobjbound,
              gurobi_objbound     = result$objbound,
              gurobi_objboundc    = result$objboundc,
              gurobi_mipgap       = result$mipgap
              )

        #----------------------------------------------------------------------
        #  Gurobi returns a solution as a set of 0s and 1s rather than as
        #  the list of PU_IDs.  Convert the 0/1 set to PU_IDs now since
        #  that is how the rest of bdpg expects a solution to be represented.
        #----------------------------------------------------------------------

    solution_PU_IDs = which (result$x > 0)

        #----------------------------------------------------------------------
        #  Save the results and the solution to separate output files so that
        #  they don't have to be separated from each other in downstream uses.
        #----------------------------------------------------------------------

    if (save_outputs)
        {
        gurobi_output_dir = get_RSrun_path_output (rsrun, top_dir)

        saveRDS (solution_PU_IDs,
                 file.path (gurobi_output_dir, "solution_PU_IDs.rds"))
        saveRDS (gurobi_controls,
                 file.path (gurobi_output_dir, "gurobi_controls.rds"))
        }

        #----------------------------------------------------------------------
        #  Testing of the call to gurobi is easier if both the controls and
        #  the results are returned in one list from this routine, so combine
        #  them now for return.
        #----------------------------------------------------------------------

    gurobi_controls_and_results = gurobi_controls
    gurobi_controls_and_results$gurobi_solution_vector = solution_PU_IDs

    return (gurobi_controls_and_results)
    }

#===============================================================================
#===============================================================================
#===============================================================================

#' Run reserve selector on bd problem and write output from all analysis
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

do_gurobi_analysis_and_output <- function (APP_bd_prob,
                                               COR_bd_prob,
                                               parameters,
                                           starting_dir,
                                               rs_method_name,
                                               marxan_elapsed_time,
                                               src_rds_file_dir = NULL,
                                               spp_rep_targets =
                                                   rep (1,COR_bd_prob@num_spp)
                                               )
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

    rs_control_values = set_up_for_and_run_gurobi_APP (APP_bd_prob,
                                                       COR_bd_prob,
                                                       APP_rs_run,
                                                       parameters,
                                                       marxan_elapsed_time)

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

