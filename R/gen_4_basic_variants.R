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

    do_COR_rs_analysis_and_output (base_COR_bd_prob, parameters)

                    cat("\n\njust after set_up_for_and_run_marxan() for Base COR problem")
                    cat ("\n\n================================================================================")
                    cat ("\n================================================================================\n\n")

    #===============================================================================
    #  Generate an APPARENT problem from the base problem, i.e., apply errors.
    #===============================================================================

    base_APP_bd_prob =
        bdpg::gen_single_bdprob_APP (base_COR_bd_prob,
                                     parameters)

    do_APP_rs_analysis_and_output (base_APP_bd_prob,
                                   base_COR_bd_prob,
                                   parameters)

                    cat("\n\njust after set_up_for_and_run_marxan() for Base APP problem")
                    cat ("\n\n================================================================================")
                    cat ("\n================================================================================\n\n")

    #===============================================================================
    #       Generate a wrapped problem around the base problem.
    #===============================================================================

    if (value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu) &
        ! value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file))
        {
        wrapped_COR_bd_prob =
            gen_bdprob (parameters,
                        integerize,
                        base_COR_bd_prob)

        do_COR_rs_analysis_and_output (wrapped_COR_bd_prob, parameters)

                        cat("\n\njust after set_up_for_and_run_marxan() for Wrapped COR problem")
                        cat ("\n\n================================================================================")
                        cat ("\n================================================================================\n\n")

    #===============================================================================
    #  Generate an APPARENT problem from the wrapped problem, i.e., apply errors.
    #===============================================================================

        wrapped_APP_bd_prob =
            gen_single_bdprob_APP (wrapped_COR_bd_prob,
                                   parameters)

        do_APP_rs_analysis_and_output (wrapped_APP_bd_prob,
                                       wrapped_COR_bd_prob,
                                       parameters)

                        cat("\n\njust after set_up_for_and_run_marxan() for Wrapped APP problem")
                        cat ("\n\n================================================================================")
                        cat ("\n================================================================================\n\n")
        }  #  end if - wrap_lognormal_dist_around_Xu

    }  #  end function - gen_4_basic_variants()

#===============================================================================

