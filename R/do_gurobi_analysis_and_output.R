#===============================================================================

                    #  do_gurobi_analysis_and_output.R

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
#' @importFrom gurobi gurobi
#' @export

run_gurobi <- function (num_spp, num_PUs,
                        bpm,
                        PU_costs, spp_rep_targets,
                        use_time_limit = TRUE, time_limit = 60,
                        use_gap_limit = FALSE, gap_limit = 0.005
                        )
    {
        # set up Gurobi model object in R
    gurobi_model <- list()

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
     "\nnum_subtarget_abundances = ", num_subtarget_abundances)
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
    # if (use_gap_limit && use_time_limit)
    # {
    # stop_bdpg ("Can't set both use_gap_limit and use_time_limit to TRUE.")
    #
    # } else
    use_gap_limit = vb (use_gap_limit, def_on_empty = TRUE, def = FALSE)
    if (use_gap_limit)
        {
        gap_limit = vn (gap_limit)
        gurobi_params$MIPGap = gap_limit
        cat ("\nrun_gurobi() setting MIPGap to '", gurobi_params$MIPGap, "'")
        }

    use_time_limit = vb (use_time_limit, def_on_empty = TRUE, def = FALSE)
    if (use_time_limit)
        {
        time_limit = vn (time_limit)
        gurobi_params$TimeLimit = time_limit
        cat ("\nrun_gurobi() setting TimeLimit to '", gurobi_params$TimeLimit,
             "'")
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

if(FALSE)
{
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
}

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
#' @inheritParams std_param_defns
#'
#' @return Returns a gurobi model result list with named elements that contains,
#'     among other things, an element x which is the solution vector
#' @export

test_gurobi <- function (seed = 456,
                         num_spp = 10,
                         num_PUs = 5)
    {
    set.seed (seed)

    PU_costs        = rep (1, num_PUs)
    spp_rep_targets = rep (1, num_spp)

    bpm_with_spp_rows_PU_cols = gen_dummy_bpm (num_spp, num_PUs)

    result = run_gurobi (num_spp,
                         num_PUs,
                         bpm_with_spp_rows_PU_cols,
                         PU_costs,
                         spp_rep_targets,
                         use_time_limit = FALSE, time_limit = 60,
                         use_gap_limit = TRUE, gap_limit = 0.005
                        )

    return (result)
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
                                                           parameters)

        #-------------------------------------
        #  Collect reserve selector results.
        #-------------------------------------

    save_rsrun_results_data_for_one_rsrun (parameters,
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
                                                           parameters)

        #-------------------------------------
        #  Collect reserve selector results.
        #-------------------------------------

    save_rsrun_results_data_for_one_rsrun (parameters,
                                              APP_rs_run,
                                              COR_bd_prob,
                                              APP_bd_prob,
                                           rs_method_name,
                                           rs_control_values,
                                           src_rds_file_dir)

    }  #  end function - do_APP_rs_analysis_and_output

#===============================================================================

