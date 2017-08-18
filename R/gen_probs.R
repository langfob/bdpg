#===============================================================================

                            #  gen_probs.R

#===============================================================================

#' Generate a single correct Xu biodiversity problem
#'
#' @param base_prob_name_stem a character string
#' @param cor_dir_name_stem a character string
#' @inheritParams std_param_defns
#'
#' @return Returns a correct Xu biodiversity problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_COR <- function (parameters,
                                   bdpg_error_codes,
                                   integerize,
                                   base_prob_name_stem = "base_prob",
                                   cor_dir_name_stem = "cor"
                                   )
    {
    exp_root_dir = file.path (normalizePath (parameters$full_output_dir_with_slash))
    Xu_bench_infile_name = parameters$infile_name
    if (is.null (Xu_bench_infile_name)) Xu_bench_infile_name = ""

    COR_Xu_bdprob =
        gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (
                                exp_root_dir,
                                parameters$compute_network_metrics_COR,
                                parameters,
                                parameters$read_Xu_problem_from_Xu_bench_file,
                                Xu_bench_infile_name,
                                parameters$given_correct_solution_cost,
                                parameters$max_allowed_num_spp,
                                bdpg_error_codes,
                                integerize,
                                base_prob_name_stem = "base_prob",
                                cor_dir_name_stem = "cor"
                                )

    return (COR_Xu_bdprob)
    }

#===============================================================================

#' Generate a single wrapped Xu biodiversity problem
#'
#' @param bdprob_to_wrap a bdproblem to wrap a distribution around
#' @inheritParams std_param_defns
#'
#' @return Returns a wrapped Xu biodiversity problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_WRAP <- function (bdprob_to_wrap,
                                    parameters,
                                    bdpg_error_codes)
    {
    wrap_lognormal_dist_around_Xu =
        value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu)

    read_Xu_problem_from_Xu_bench_file =
        value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file)

    #---------------------------------------------------------------------------

        #----------------------------------------------------------------------
        #  Make sure that the base problem for the multiproblem is not one of
        #  Xu's benchmark problems read in from a file, since they do not
        #  contain the correct solution set.  They only contain the correct
        #  solution cost.
        #----------------------------------------------------------------------

    if (wrap_lognormal_dist_around_Xu &   #(parameters$wrap_lognormal_around_Xu &
        read_Xu_problem_from_Xu_bench_file)   # parameters$read_Xu_problem_from_Xu_file)
        {
        stop (paste0 ("\n\nParameters wrap_lognormal_dist_around_Xu and ",
                    "read_Xu_problem_from_Xu_file ",
                    "\nare both true.",
                    "\nCannot wrap around Xu problem read from file ",
                    "because dependent node IDs ",
                    "\nare never given with the file.",
                    "\nQuitting.\n\n")
            )
        }

    #---------------------------------------------------------------------------

    if (wrap_lognormal_dist_around_Xu)  #parameters$wrap_lognormal_around_Xu)
        {
        starting_dir =
            file.path (normalizePath (parameters$full_output_dir_with_slash))
            # ,
            # "wrap_prob.1")

        compute_network_metrics_for_this_prob =
            value_or_FALSE_if_null (parameters$compute_network_metrics_for_this_prob)

        WRAP_prob =
            bdpg::gen_wrapped_bdprob_COR (starting_dir,
                                          compute_network_metrics_for_this_prob,
                                          parameters,
                                          bdprob_to_wrap,
                                          bdpg_error_codes)
        } else
        {
        stop (paste0 ("\n\nwrap_lognormal_dist_around_Xu is not set to TRUE.  ",
                    "\n    It is currently the only defined wrap function.\n")
            )
        }

    return (WRAP_prob)
    }

#===============================================================================

