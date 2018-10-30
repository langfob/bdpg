#===============================================================================

                        #  gen_6_basic_variants.R

#  2018 10 28 - BTL
#  CLoned from gen_20_basic_variants.R

#===============================================================================

#' Generate 6 basic variants of a problem
#'
#' Create 1 correct problem and 1 wrap of that correct problem, then from the
#' wrap problem, create 4 apparent variants using the following errors: FP only,
#' FN only, FP and FN, and finally, FP and FN with matching counts.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#' @param err_amt numeric amount of error to add in creating apparent problems
#'
#' @return Returns nothing.
#' @export

#-------------------------------------------------------------------------------

gen_6_basic_variants <- function (parameters, integerize, err_amt = NA)
    {
                    cat ("\n\nAT START OF gen_6_basic_variants().\n\n")

    #===========================================================================
    #            Determine amount of error to add in apparent problems.
    #===========================================================================

    default_err_amt = 0.05
    if (is.na (err_amt))
        {
        use_unif_rand_err_amt =vb (parameters$use_unif_rand_err_amt,
                                   def_on_empty = TRUE, def = FALSE)

        if (use_unif_rand_err_amt)
            {
            default_err_amt_lower_bound = 0
            err_amt_lower_bound = vn (parameters$err_amt_lower_bound,
                                      range_lo = 0,
                                      bounds_types = "ii",
                                      def_on_empty = TRUE,
                                      def = default_err_amt_lower_bound,
                                      treat_NULL_as_empty = TRUE,
                                      treat_NA_as_empty = FALSE)

            default_err_amt_upper_bound = 0.10
            err_amt_upper_bound = vn (parameters$err_amt_upper_bound,
                                      range_lo = err_amt_lower_bound,
                                      bounds_types = "ii",
                                      def_on_empty = TRUE,
                                      def = default_err_amt_upper_bound,
                                      treat_NULL_as_empty = TRUE,
                                      treat_NA_as_empty = FALSE)

            err_amt = runif (1, err_amt_lower_bound, err_amt_upper_bound)

            }  else
            {
            err_amt = vn (parameters$gen_basic_variants_err_amt,
                          range_lo = 0, bounds_types = "ii",
                          def_on_empty = TRUE, def = default_err_amt,
                          treat_NULL_as_empty = TRUE, treat_NA_as_empty = FALSE)
            }
        }

                    cat ("\n\nIn gen_6_basic_variants(), chosen err_amt = ", err_amt, "\n", sep='')

    #===========================================================================
    #  Generate a base problem, i.e, create the Xu graph nodes and edge_list.
    #===========================================================================

    base_COR_bd_prob = gen_single_bdprob_COR (parameters,
                                              integerize,
                                              base_prob_name_stem = "base_prob",
                                              cor_dir_name_stem = "cor"
                                              )

                    cat ("\n\n-----  base_COR_bd_prob@UUID = '", base_COR_bd_prob@UUID,
                         "', checksum = '", base_COR_bd_prob@checksum, "'  -----\n\n")

    do_rs_analysis_and_output (base_COR_bd_prob,
                               base_COR_bd_prob,
                               parameters)

                    cat("\n\njust after do_rs_analysis_and_output() for Base COR problem")
                    cat ("\n\n================================================================================")
                    cat ("\n================================================================================\n\n")

    #===========================================================================
    #       Generate a wrapped problem around the BASE problem.
    #===========================================================================

    if (value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu) &
        ! value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file))
        {
        wrapped_COR_bd_prob =
#            gen_bdprob (parameters,
            gen_multi_bdprob (parameters,
                              integerize,
                              base_COR_bd_prob)

        do_rs_analysis_and_output (wrapped_COR_bd_prob,
                                   wrapped_COR_bd_prob,
                                   parameters)

                        cat("\n\njust after do_COR_rs_analysis_and_output() for Wrapped COR problem")
                        cat ("\n\n================================================================================")
                        cat ("\n================================================================================\n\n")

        #==========================================================================
        #  Generate 4 APPARENT problems from the WRAP problem WITHOUT cost error.
        #==========================================================================

        gen_4_or_5_app_variants (wrapped_COR_bd_prob,
                                 parameters,
                                 gen_combined_cost_and_FP_FN_errors = FALSE,
                                 err_amt)

                       cat("\n\njust after gen_4_or_5_app_variants() WITHOUT cost error for Wrapped problem")
                        cat ("\n\n================================================================================")
                        cat ("\n================================================================================\n\n")

        }  #  end if - wrap_lognormal_dist_around_Xu

    }  #  end function - gen_6_basic_variants()

#===============================================================================

