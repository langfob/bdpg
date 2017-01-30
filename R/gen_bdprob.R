#===============================================================================
#                   Generate a biodiversity problem.
#===============================================================================

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param parameters DESCRIPTION.
#' @param bdpg_error_codes DESCRIPTION.
#' @param integerize DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
gen_bdprob  = function (parameters, bdpg_error_codes, integerize)
    {
        #  May need to test these parameters to see if they even exist
        #  and if they don't for this particular run, then set them to
        #  something like NULL.

    read_Xu_problem_from_Xu_file   = parameters$read_Xu_problem_from_Xu_file
    infile_name                    = parameters$infile_name
    given_correct_solution_cost    = parameters$given_correct_solution_cost
    max_allowed_num_spp            = parameters$max_allowed_num_spp

    wrap_lognormal_dist_around_Xu    = TRUE    #  temp for testing only...

    parameters$gen_multi_bdproblem      = TRUE    #  temp for testing only...

    if (parameters$gen_multi_bdproblem)
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
        bdprob = gen_single_bdprob (parameters,
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


