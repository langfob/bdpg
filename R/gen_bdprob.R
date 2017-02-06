#===============================================================================
#                   Generate a biodiversity problem.
#===============================================================================

#' Generate a biodiversity problem
#'
#' @param parameters parameter list, generally from project.yaml
#' @param bdpg_error_codes list of error codes and corresponding error numbers
#' @param integerize function to use in converting floats to integers
#'
#' @return biodiversity problems
#' @export

gen_bdprob  = function (parameters, bdpg_error_codes, integerize)
    {
        #  May need to test these parameters to see if they even exist
        #  and if they don't for this particular run, then set them to
        #  something like NULL.

    read_Xu_problem_from_Xu_file   = parameters$read_Xu_problem_from_Xu_file
    infile_name                    = parameters$infile_name
    given_correct_solution_cost    = parameters$given_correct_solution_cost
    max_allowed_num_spp            = parameters$max_allowed_num_spp

##    wrap_lognormal_dist_around_Xu    = TRUE    #  temp for testing only...
    wrap_lognormal_dist_around_Xu  = parameters$wrap_lognormal_dist_around_Xu

##    parameters$gen_multi_bdproblem      = TRUE    #  temp for testing only...
    gen_multi_bdproblem            = parameters$gen_multi_bdproblem

    if (gen_multi_bdproblem)
        {
        bdprob = gen_multi_bdprob (parameters,
                                   wrap_lognormal_dist_around_Xu,
                                   read_Xu_problem_from_Xu_file,
                                   infile_name,
                                   given_correct_solution_cost,
                                   max_allowed_num_spp,
                                   bdpg_error_codes,
                                   integerize)

        } else
        {
        bdprob = gen_single_bdprob_cor (parameters,
                                    read_Xu_problem_from_Xu_file,
                                    infile_name,
                                    given_correct_solution_cost,
                                    max_allowed_num_spp,
                                    bdpg_error_codes,
                                    integerize)
        }

    return (bdprob)
    }

#===============================================================================


