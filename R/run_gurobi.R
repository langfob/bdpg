#===============================================================================

                            #  run_gurobi.R

#===============================================================================

#' Run gurobi linear programming optimizer
#'
#' @param use_time_limit boolean indicating whether to set a time limit on the
#'     optimization computation; TRUE implies set the limit, FALSE implies no
#'     time limit
#' @param time_limit numeric value for time limit measured in seconds
#' @param use_gap_limit boolean indicating whether to set a maximum gap allowed
#'     between the result of the optimization computation and the correct
#'     optimum value; TRUE implies set the gap value, FALSE implies no gap
#'     enforced
#' @param gap_limit numeric value for gap, measured in same units as the
#'     objective function
#' @inheritParams std_param_defns
#'
#' @return Returns a gurobi model result list with named elements that contains,
#'     among other things, an element x which is the solution vector
#' @export

run_gurobi <- function (num_spp, num_PUs,
                        bpm,
                        PU_costs, spp_targets,
                        use_time_limit = FALSE, time_limit = 60,
                        use_gap_limit = TRUE, gap_limit = 0.005
                        )
    {
        # set up Gurobi model object in R
    gurobi_model <- list()

        #  Assign the constraints matrix object
    constr = bpm
    gurobi_model$A <- constr

        #  Assign the species target amounts as the right hand side.
    rhs = spp_targets
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
    if (use_gap_limit && use_time_limit)
    {
    stop_bdpg ("Can't set both use_gap_limit and use_time_limit to TRUE.")

    } else if (use_gap_limit)
    {
    gurobi_params$MIPGap = gap_limit

    } else if (use_time_limit)
    {
    gurobi_params$TimeLimit = time_limit

    } else  #  no limits
    {
    cat ("\n\nNeither gap nor time limit will be used in gurobi call.  ",
         "May take a long time.\n")
    }

    #--------------------

        # solve the problem
    gurobi_result <- gurobi::gurobi (gurobi_model, gurobi_params)

    #--------------------

        # the result object contains several data objects including the objective
        # value achieved (result$objval) and the vector of decision variable
        # values (0 or 1 in our example because the variables are binary).

    cat ("\n\nGurobi result:\n")
    print (gurobi_result)

    solution = gurobi_result$x
    solution_node_IDs = which (solution == 1)

    is_solution =
        verify_that_generated_solution_really_is_a_solution (bpm,
                                                             solution_node_IDs,
                                                             num_spp,
                                                             num_PUs,
                                                             PU_costs)

    cat ("\n\nVerifying that Gurobi solution really is a solution: '",
         is_solution, "'\n")

    return (gurobi_result)
    }

#===============================================================================

gen_dummy_bpm <- function (num_spp, num_PUs)
    {
    bpm = matrix (0, nrow=num_spp, ncol=num_PUs)

    cat ("\ndim(bpm) = ", dim(bpm))
    for (cur_spp in 1:num_spp)
        {
        occ_PUs_for_this_spp = sample (1:num_PUs, 2, replace=FALSE)

        bpm [cur_spp, occ_PUs_for_this_spp] = 1
        }

    return (bpm)
    }

#===============================================================================

#' Title
#'
#' @param seed numeric
#' @param num_spp numeric
#' @param num_PUs numeric
#'
#' @return Returns a gurobi model result list with named elements that contains,
#'     among other things, an element x which is the solution vector
#' @export

test_gurobi <- function (seed = 456,
                         num_spp = 10,
                         num_PUs = 5)
    {
    set.seed (seed)

    PU_costs    = rep (1, num_PUs)
    spp_targets = rep (1, num_spp)

    bpm_with_spp_rows_PU_cols = gen_dummy_bpm (num_spp, num_PUs)

    result = run_gurobi (num_spp,
                         num_PUs,
                         bpm_with_spp_rows_PU_cols,
                         PU_costs,
                         spp_targets,
                         use_time_limit = FALSE, time_limit = 60,
                         use_gap_limit = TRUE, gap_limit = 0.005
                        )

    return (result)
    }

#===============================================================================

