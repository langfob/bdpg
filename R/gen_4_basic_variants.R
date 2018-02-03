#===============================================================================

                        #  gen_4_basic_variants.R

#===============================================================================

#' Create 1 of each of the 4 types of problems and run reserve selector on each
#'
#' This is what used to be the mainline for bdpgxupaper.  It creates a correct
#' biodiversity problem, then from that correct, it creates a wrapped problem.
#' It also creates an apparent problem for the original correct problem
#' and another for the wrapped problem.  It also runs marxan on each of these
#' four problems.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns nothing.
#' @export

#-------------------------------------------------------------------------------

gen_4_basic_variants <- function (parameters, integerize)
    {
cat ("\n\nAT START OF gen_4_basic_variants().\n\n")

    #===============================================================================
    #       Generate a base problem, i.e, create the Xu graph nodes and edge_list.
    #===============================================================================

    base_COR_bd_prob = gen_single_bdprob_COR (parameters,
                                              integerize,
                                              base_prob_name_stem = "base_prob",
                                              cor_dir_name_stem = "cor"
                                              )

cat ("\n\n-----  base_COR_bd_prob@UUID = '", base_COR_bd_prob@UUID,
     "', checksum = '", base_COR_bd_prob@checksum, "'  -----\n\n")

    #do_COR_marxan_analysis_and_output (base_COR_bd_prob, parameters)
    do_COR_rs_analysis_and_output (base_COR_bd_prob, parameters)

    cat("\n\njust after set_up_for_and_run_marxan() for Base COR problem")
    cat ("\n\n================================================================================")
    cat ("\n================================================================================\n\n")

    #===============================================================================
    #  Generate an APPARENT problem from the base problem, i.e., apply errors.
    #===============================================================================

#    if (value_or_FALSE_if_null (parameters$apply_error_to_spp_occupancy_data))
    if (value_or_FALSE_if_null (parameters$apply_error_to_COR))
        {
        base_APP_bd_prob =
            bdpg::gen_single_bdprob_APP (base_COR_bd_prob,
                                         #parameters$compute_network_metrics_APP,
                                         parameters)

        # do_APP_marxan_analysis_and_output (base_APP_bd_prob,
        #                                    base_COR_bd_prob,
        #                                    parameters)
        do_APP_rs_analysis_and_output (base_APP_bd_prob,
                                       base_COR_bd_prob,
                                       parameters)

        cat("\n\njust after set_up_for_and_run_marxan() for Base APP problem")
        cat ("\n\n================================================================================")
        cat ("\n================================================================================\n\n")
        }

    #===============================================================================
    #       Generate a wrapped problem around the base problem.
    #===============================================================================

    if (value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu) &
        ! value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file))
        {
        wrapped_COR_bd_prob =
            gen_bdprob (parameters,
                        #parameters$compute_network_metrics_wrapped_COR,
                        integerize,
                        base_COR_bd_prob)

        #do_COR_marxan_analysis_and_output (wrapped_COR_bd_prob, parameters)
        do_COR_rs_analysis_and_output (wrapped_COR_bd_prob, parameters)

        cat("\n\njust after set_up_for_and_run_marxan() for Wrapped COR problem")
        cat ("\n\n================================================================================")
        cat ("\n================================================================================\n\n")

        #-------------------------------------------------------------------------------

        #     #  Try running marxan on the base cor problem since we know that
        #     #  this particular test has created a wrapped problem.
        #     #
        #     #  Note that a different way around this issue is to create the
        #     #  cor base problem and do the analysis on it before creating
        #     #  the wrapped problem, but creating the wrap by directly passing
        #     #  the cor to it without going through gen_bdprob's superstructure.
        #
        # get_base_cor_prob_of_wrap_prob <- function ()
        #     {
        #     cat ("not implemented yet""
        #     }
        # base_COR_bd_prob = get_base_cor_prob_of_wrap_prob (COR_bd_prob)
        #
        # base_COR_marxan_ret_values = bdpg::set_up_for_and_run_marxan_COR (base_COR_bd_prob, parameters)
        #
        # marxan_control_values  = base_COR_marxan_ret_values$marxan_control_values
        # base_COR_bd_prob       = base_COR_marxan_ret_values$base_COR_bd_prob  #  COR_bd_prob has new dirs
        #
        # cat("\n\njust after set_up_for_and_run_marxan() for base cor problem")

    #===============================================================================
    #  Generate an APPARENT problem from the wrapped problem, i.e., apply errors.
    #===============================================================================

        if (value_or_FALSE_if_null (parameters$apply_error_to_spp_occupancy_data))
            {
            wrapped_APP_bd_prob =
                gen_single_bdprob_APP (wrapped_COR_bd_prob,
                                       #parameters$compute_network_metrics_wrapped_APP,
                                       parameters)

            # do_APP_marxan_analysis_and_output (wrapped_APP_bd_prob,
            #                                    wrapped_COR_bd_prob,
            #                                    parameters)
            do_APP_rs_analysis_and_output (wrapped_APP_bd_prob,
                                           wrapped_COR_bd_prob,
                                           parameters)

            cat("\n\njust after set_up_for_and_run_marxan() for Wrapped APP problem")
            cat ("\n\n================================================================================")
            cat ("\n================================================================================\n\n")
            }
        }  #  end if - wrap_lognormal_dist_around_Xu

    }  #  end function - gen_4_basic_variants()

#===============================================================================

