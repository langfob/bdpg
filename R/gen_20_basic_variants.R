#===============================================================================

                        #  gen_20_basic_variants.R

#===============================================================================

get_compound_err_name <- function (gen_cost_errors,
                                   spp_occ_FP_const_rate,
                                   spp_occ_FN_const_rate,
                                   match_error_counts
                                   )
    {
    has_FP = (spp_occ_FP_const_rate != 0)
    has_FN = (spp_occ_FN_const_rate != 0)

    value = NULL

    if (! gen_cost_errors)
        {
        if (match_error_counts)
            {
            value = "04-FP_and_FN_matched_NO_cost_err"

            } else  #  unmatched FP/FN
            {
            if (has_FP)
                {
                if (has_FN)
                    {
                    value = "05-FP_and_FN_not_matched_NO_cost_err"

                    } else  #  unmatched and FP but no FN
                    {
                    value = "02-FP_only_NO_cost_err"

                    }
                } else  #  unmatched and FN but no FP
                {
                if (has_FN)  value = "03-FN_only_NO_cost_err"
                }
            }  #  end else - unmatched FP/FN

        } else  #  HAS COST ERROR
        {
        if (match_error_counts)
            {
            value = "08-FP_and_FN_matched_WITH_cost_err"

            } else  #  unmatched FP/FN
            {
            if (has_FP)
                {
                if (has_FN)
                    {
                    value = "09-FP_and_FN_not_matched_WITH_cost_err"

                    } else  #  unmatched and FP but no FN
                    {
                    value = "06-FP_only_WITH_cost_err"

                    }
                } else  #  unmatched and FN but no FP
                {
                if (has_FN)
                    {
                    value = "07-FN_only_WITH_cost_err"

                    } else  #  unmatched and no FP and no FN, so cost only
                    {
                    value = "10-Cost_err_only"
                    }
                }
            }  #  end else - unmatched FP/FN

        }  #  end else - has cost err

    if (is.null (value))  stop (paste0 ("No compound error name was assigned"))

    return (value)
    }

#===============================================================================

    #  Generate an APPARENT problem from the base problem, i.e., apply errors.

gen_1_app_variant <- function (base_bd_prob,
                               parameters,

                               gen_cost_errors,
                               cost_error_frac_bound,

                               gen_FP_FN_errors,

                               spp_occ_FP_const_rate = 0,
                               spp_occ_FN_const_rate = 0,

                               match_error_counts = FALSE,

                               spp_occ_FP_error_type       = "CONSTANT",
                               spp_occ_FP_rate_lower_bound = NA,
                               spp_occ_FP_rate_upper_bound = NA,

                               spp_occ_FN_error_type       = "CONSTANT",
                               spp_occ_FN_rate_lower_bound = NA,
                               spp_occ_FN_rate_upper_bound = NA
                               )
    {
        #--------------------------------------------------------
        #  Compute PU cost errors and create apparent PU_costs.
        #--------------------------------------------------------

    if (gen_cost_errors)
        {
        ret_vals_from_apply_cost_errors =
            apply_unif_rand_error_to_PU_costs (base_bd_prob@PU_costs,
                                               cost_error_frac_bound)

        } else { ret_vals_from_apply_cost_errors = NULL }

        #-----------------------------------------------------
        #  Compute FP and FN errors and create apparent bpm.
        #-----------------------------------------------------

    if (gen_FP_FN_errors)
        {
        ret_vals_from_build_const_err =
            build_const_err_FP_and_FN_matrices (
                    sum (base_bd_prob@bpm),       #  num_TPs
                    length (base_bd_prob@bpm),    #  num_TPs_and_TNs

                    base_bd_prob@num_PUs,
                    base_bd_prob@num_spp,

                    spp_occ_FP_error_type,
                    spp_occ_FP_const_rate,
                    spp_occ_FP_rate_lower_bound,
                    spp_occ_FP_rate_upper_bound,

                    spp_occ_FN_error_type,
                    spp_occ_FN_const_rate,
                    spp_occ_FN_rate_lower_bound,
                    spp_occ_FN_rate_upper_bound,

                    match_error_counts)

        } else { ret_vals_from_build_const_err = NULL }

        #-----------------------------------------------------------------------
        #  Ready to generate the apparent problem and run reserve selector(s).
        #-----------------------------------------------------------------------

    compound_err_name = get_compound_err_name (gen_cost_errors,
                                               spp_occ_FP_const_rate,
                                               spp_occ_FN_const_rate,
                                               match_error_counts)


    APP_bd_prob = gen_single_bdprob_APP (base_bd_prob,
                                         parameters,
                                         compound_err_name,
                                         ret_vals_from_build_const_err,
                                         ret_vals_from_apply_cost_errors)

    do_rs_analysis_and_output (APP_bd_prob,
                                   base_bd_prob,
                                   parameters)

                    cat("\n\njust after do_APP_rs_analysis_and_output() APP problem")
                    cat ("\n\n================================================================================")
                    cat ("\n================================================================================\n\n")
    }

#===============================================================================

gen_4_or_5_app_variants <- function (base_bd_prob,
                                     parameters,
                                     gen_combined_cost_and_FP_FN_errors,
                                     err_amt)
    {
                    cat ("\n\nAT START OF gen_4_or_5_app_variants().\n\n")

        #----------------------------------------------------------
        #  If requested, generate 1 variant with only cost error.
        #----------------------------------------------------------

    if (gen_combined_cost_and_FP_FN_errors)
        {
        gen_1_app_variant (base_bd_prob,
                           parameters,
                           gen_cost_errors = TRUE,
                           cost_error_frac_bound = err_amt,
                           gen_FP_FN_errors = FALSE)
        }

        #---------------------------------------------
        #  Generate 4 variants of FP/FN error.
        #  Also add cost error to them if requested.
        #---------------------------------------------

            #-----------------------
            #  Variant 1:  FP only
            #-----------------------

    gen_1_app_variant (base_bd_prob,
                       parameters,

                       gen_cost_errors = gen_combined_cost_and_FP_FN_errors,
                       cost_error_frac_bound = err_amt,

                       gen_FP_FN_errors = TRUE,

                       spp_occ_FP_const_rate = err_amt,
                       spp_occ_FN_const_rate = 0)

            #-----------------------
            #  Variant 2:  FN only
            #-----------------------

    gen_1_app_variant (base_bd_prob,
                       parameters,

                       gen_cost_errors = gen_combined_cost_and_FP_FN_errors,
                       cost_error_frac_bound = err_amt,

                       gen_FP_FN_errors = TRUE,

                       spp_occ_FP_const_rate = 0,
                       spp_occ_FN_const_rate = err_amt,

                       match_error_counts = FALSE)

            #-------------------------------------------
            #  Variant 3:  FP & FN, counts NOT matched
            #-------------------------------------------

    gen_1_app_variant (base_bd_prob,
                       parameters,

                       gen_cost_errors = gen_combined_cost_and_FP_FN_errors,
                       cost_error_frac_bound = err_amt,

                       gen_FP_FN_errors = TRUE,

                       spp_occ_FP_const_rate = 0,
                       spp_occ_FN_const_rate = err_amt)

            #---------------------------------------
            #  Variant 4:  FP & FN, counts MATCHED
            #---------------------------------------

    gen_1_app_variant (base_bd_prob,
                       parameters,

                       gen_cost_errors = gen_combined_cost_and_FP_FN_errors,
                       cost_error_frac_bound = err_amt,

                       gen_FP_FN_errors = TRUE,

                       spp_occ_FP_const_rate = 0,
                       spp_occ_FN_const_rate = err_amt,

                       match_error_counts = TRUE)
    }

#===============================================================================

#' Generate 20 basic variants of a problem
#'
#' Create 1 correct problem and 1 wrap of that correct problem, then from each
#' of those, create 4 apparent variants using the following errors: FP only,
#' FN only, FP and FN, and finally, FP and FN with matching counts.  Based on
#' each of these 10 variants (2 correct and 4 apparent for each of the 2), then
#' create another apparent version of each problem that includes cost error too.
#' This gives a total of 20 problems.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#' @param err_amt numeric amount of error to add in creating apparent problems
#'
#' @return Returns nothing.
#' @export

#-------------------------------------------------------------------------------

gen_20_basic_variants_including_cost_error <- function (parameters,
                                                        integerize,
                                                        err_amt = 0.05)
    {
                    cat ("\n\nAT START OF gen_20_basic_variants_including_cost_error().\n\n")

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

    do_rs_analysis_and_output (base_COR_bd_prob,
                               base_COR_bd_prob,
                               parameters)

                    cat("\n\njust after do_rs_analysis_and_output() for Base COR problem")
                    cat ("\n\n================================================================================")
                    cat ("\n================================================================================\n\n")

    #==========================================================================
    #  Generate 4 APPARENT problems from the BASE problem WITHOUT cost error.
    #==========================================================================

    gen_4_or_5_app_variants (base_COR_bd_prob,
                             parameters,
                             gen_combined_cost_and_FP_FN_errors = FALSE,
                             err_amt)

                   cat("\n\njust after gen_4_or_5_app_variants() WITH cost error for BASE problem")
                    cat ("\n\n================================================================================")
                    cat ("\n================================================================================\n\n")

    #=======================================================================
    #  Generate 5 APPARENT problems from the BASE problem WITH cost error.
    #  1 problem has only cost error, no FP/FN error.
    #=======================================================================

    gen_4_or_5_app_variants (base_COR_bd_prob,
                             parameters,
                             gen_combined_cost_and_FP_FN_errors = TRUE,
                             err_amt)

                   cat("\n\njust after gen_4_or_5_app_variants() WITHOUT cost error for BASE problem")
                    cat ("\n\n================================================================================")
                    cat ("\n================================================================================\n\n")


cat ("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^")
cat ("\n---  Just after all generation for BASE problem, about to do WRAP problems  ---")
cat (  "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n\n")

    #===============================================================================
    #       Generate a wrapped problem around the BASE problem.
    #===============================================================================

    if (value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu) &
        ! value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file))
        {
        wrapped_COR_bd_prob =
            gen_bdprob (parameters,
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

        #=======================================================================
        #  Generate 5 APPARENT problems from the WRAP problem WITH cost error.
        #  1 problem has only cost error, no FP/FN error.
        #=======================================================================

        gen_4_or_5_app_variants (wrapped_COR_bd_prob,
                                 parameters,
                                 gen_combined_cost_and_FP_FN_errors = TRUE,
                                 err_amt)

                       cat("\n\njust after gen_4_or_5_app_variants() WITH cost error for Wrapped problem")
                        cat ("\n\n================================================================================")
                        cat ("\n================================================================================\n\n")


        }  #  end if - wrap_lognormal_dist_around_Xu

    }  #  end function - gen_20_basic_variants()

#===============================================================================

