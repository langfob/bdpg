#===============================================================================
#                   Generate a biodiversity problem.
#===============================================================================

#' Generate a biodiversity problem
#'
#'Generate a biodiversity problem.
#'
#-------------------------------------------------------------------------------

#' @param base_bdprob NULL or an existing bdprob to use in creating multi_bdproblem
#' @inheritParams std_param_defns
#'
#' @return biodiversity problems
#' @export

#-------------------------------------------------------------------------------

gen_bdprob  = function (parameters,
                                    compute_network_metrics_for_this_prob,
                        integerize,
                        base_bdprob = NULL)
    {
        #  May need to test these parameters to see if they even exist
        #  and if they don't for this particular run, then set them to
        #  something like NULL.

    read_Xu_problem_from_Xu_bench_file = value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file)
    infile_name                        = parameters$infile_name
    if (is.null (infile_name)) infile_name = ""
    given_correct_solution_cost        = parameters$given_correct_solution_cost
    max_allowed_num_spp                = parameters$max_allowed_num_spp

##    wrap_lognormal_dist_around_Xu    = TRUE    #  temp for testing only...
    wrap_lognormal_dist_around_Xu  = parameters$wrap_lognormal_dist_around_Xu

##    parameters$gen_multi_bdproblem      = TRUE    #  temp for testing only...
    gen_multi_bdproblem            = parameters$gen_multi_bdproblem

    if (gen_multi_bdproblem)
        {
        bdprob = gen_multi_bdprob (parameters,
                                    #compute_network_metrics_for_this_prob,
                                   wrap_lognormal_dist_around_Xu,
                                   read_Xu_problem_from_Xu_bench_file,
                                   infile_name,
                                   given_correct_solution_cost,
                                   max_allowed_num_spp,
                                   integerize,
                                   base_bdprob)
        } else
        {
        # starting_dir =
        #     file.path (normalizePath (parameters$full_output_dir_with_slash),
        #                "base_prob.1")

        # bdprob = gen_single_bdprob_COR (parameters$full_output_dir_with_slash,  #starting_dir,
        #                             compute_network_metrics_for_this_prob,
        #                                 parameters,
        #                                 read_Xu_problem_from_Xu_bench_file,
        #                                 infile_name,
        #                                 given_correct_solution_cost,
        #                                 max_allowed_num_spp,
        #                                 integerize)
        bdprob = gen_single_bdprob_COR (parameters,
                                        integerize,
                                        base_prob_name_stem = "base_prob",
                                        cor_dir_name_stem = "cor"
                                        )

        }

    return (bdprob)
    }

#===============================================================================


