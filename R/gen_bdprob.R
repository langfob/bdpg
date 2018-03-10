#===============================================================================
#
#                            gen_bdprob.R
#
#  Generate a biodiversity problem.
#
#===============================================================================

#' Generate a base or wrapped biodiversity problem
#'
#' Problem can be a base problem or a wrapped problem.  Generally, the control
#' for this decision is contained in the parameters list.  However, in some
#' functions, you may want to set the value yourself while looping through
#' lots of different combinations.  To allow that override, you can pass the
#' controlling value for gen_multi_bdproblem as a non-NULL boolean argument.
#'
#-------------------------------------------------------------------------------

#' @param gen_multi_bdproblem NULL or boolean indicating whether to generate
#'     a multi_bdproblem or not; if NULL then derive the value from the
#'     parameters list
#' @param base_bdprob NULL or an existing bdprob to use in creating multi_bdproblem
#' @inheritParams std_param_defns
#'
#' @return biodiversity problems
#' @export

#-------------------------------------------------------------------------------

gen_bdprob  = function (parameters,
                        integerize,
                        gen_multi_bdproblem = NULL,
                        base_bdprob = NULL)
    {
        #-----------------------------------------------------------------------
        #  Decide whether to generate a multi problem or not.
        #  Generally, you derive this from the parameters list.
        #  When that is the case, then set the gen_multi_bdproblem argument
        #  to this function to be NULL and a value will be derived in here.
        #  In some functions, you may want to
        #  set the value yourself while looping through lots of different
        #  combinations.  To allow that override, you can pass the controlling
        #  value for gen_multi_bdproblem as a non-NULL boolean argument.
        #-----------------------------------------------------------------------

    if (is.null (gen_multi_bdproblem))
        {
        gen_multi_bdproblem         = vb (parameters$gen_multi_bdproblem,
                                      def_on_empty = TRUE)
        }

    if (gen_multi_bdproblem)
        {
        bdprob = gen_multi_bdprob (parameters,
                                   integerize,
                                   base_bdprob)
        } else
        {
        bdprob = gen_single_bdprob_COR (parameters,
                                        integerize,
                                        base_prob_name_stem = "base_prob",
                                        cor_dir_name_stem = "cor"
                                        )

        }

    return (bdprob)
    }

#===============================================================================

#' Generate a new bd problem by modifying or combining existing problem(s)
#'
#'  Sometimes it may be useful to combine 2 or more bd problems into
#'  one bigger problem since it may have the potential to produce a
#'  more difficult compound problem.
#'  If nothing else, it bears some resemblance to having a larger
#'  landscape with subregion characteristics.
#'
#'  This function is intended to allow either wrapping a distribution around
#'  an existing problem or combining two problems.  However, at the moment,
#'  it only allows wrapping.  Combining two problems is pretty straightforward
#'  but has not been implemented yet and probably won't be implemented unless
#'  there is a demand for it.
#'
#'  Dummy code that was in here to demonstrate high-level parts of that
#'  combination (i.e., combine_2_bdprobs()) have been removed but can be
#'  found in github versions of the code up until around commit 1c0fbba6
#'  on Feb 4, 2017.
#'
#-------------------------------------------------------------------------------

#'
#' @param bdprob_1 a Xu biodiversity problem object
#' @inheritParams std_param_defns
#'
#' @return Returns a multi-BD_Prob
#' @export

#-------------------------------------------------------------------------------

gen_multi_bdprob <- function (parameters,
                              integerize,
                              bdprob_1 = NULL)
    {
        #----------------------------------------------------------------------
        #  Make sure that the base problem for the multiproblem is not one of
        #  Xu's benchmark problems read in from a file, since they do not
        #  contain the correct solution set.  They only contain the correct
        #  solution cost.
        #----------------------------------------------------------------------

    read_Xu_problem_from_Xu_bench_file = vb (parameters$read_Xu_problem_from_Xu_bench_file,
                                             def_on_empty = TRUE)

    wrap_lognormal_dist_around_Xu =
        vb (parameters$wrap_lognormal_dist_around_Xu,
            def_on_empty = TRUE)

    if (wrap_lognormal_dist_around_Xu && read_Xu_problem_from_Xu_bench_file)
        {
        stop_bdpg (paste0 ("\n\nParameters wrap_lognormal_dist_around_Xu and ",
                      "read_Xu_problem_from_Xu_file ",
                      "\nare both true.",
                      "\nCannot wrap around Xu problem read from file ",
                      "because dependent node IDs ",
                      "\nare never given with the file.",
                      "\nQuitting.\n\n")
              )
        }

        #--------------------------------------------------------------------
        #  Base problem is not a Xu benchmark read from a file, so go ahead
        #  and generate the problem.
        #--------------------------------------------------------------------

    cat ("\n\n>>>>>>>>>>>>>>>>>>>>>>  ABOUT TO build base Xu problem for multi-problem  <<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n")

    if (is.null (bdprob_1))
        {
        bdprob_1 = gen_single_bdprob_COR (parameters,
                                          integerize,
                                          base_prob_name_stem = "base_prob",
                                          cor_dir_name_stem = "cor")
        }

    cat ("\n\n>>>>>>>>>>>>>>>>>>>>>>  FINISHED build base Xu problem for multi-problem  <<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n")

    if (! bdprob_1@prob_is_ok)
        stop_bdpg ("\n\nGenerating base BD_Problem for multi-problem failed.\n\n")

        #--------------------------------------------------------------
        #  Base problem generation worked, so build multiproblem now.
        #  Currently only generating multiproblems by wrapping.
        #  Future work may allow for merging 2 problems into 1.
        #--------------------------------------------------------------

    if (wrap_lognormal_dist_around_Xu)
        {
        starting_dir =
            file.path (normalizePath (parameters$full_output_dir_with_slash))

        combined_bdprob = gen_wrapped_bdprob_COR (starting_dir,
                                                  parameters,
                                                  bdprob_1)
        }

    return (combined_bdprob)
    }

#===============================================================================

#===============================================================================


