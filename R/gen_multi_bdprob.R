#===============================================================================
#       Generate a multi problem, i.e, wrap or merge problems.
#===============================================================================

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param bdprob_1 a BD_Prob to be combined
#' @param bdprob_2 a BD_Prob to be combined
#' @inheritParams std_param_defns

combine_2_bdprobs = function (bdprob_1, bdprob_2,
                              parameters, bdpg_error_codes, integerize)
    {
    stop ("\n\ncombine_2_bdprobs() is NOT IMPLEMENTED yet.\n\n")
    }

#===============================================================================

#' Generate a new biodiversity problem by modifying or combining existing
#' problem(s)
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
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{bdpg_error_codes}{
#' \preformatted{
#' bdpg_error_codes : List of 6
#'  $ ERROR_STATUS_num_inside_or_within_group_links_less_than_one: num 1001
#'  $ ERROR_STATUS_optimal_solution_is_not_optimal               : num 1002
#'  $ ERROR_STATUS_num_nodes_per_group_must_be_at_least_2        : num 1003
#'  $ ERROR_STATUS_duplicate_spp_in_Xu_input_file                : num 1004
#'  $ ERROR_STATUS_unknown_spp_occ_FP_error_type                 : num 1005
#'  $ ERROR_STATUS_unknown_spp_occ_FN_error_type                 : num 1006
#' }}
#' \subsection{bdprob_1}{
#' \preformatted{
#' bdprob_1 : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{combined_bdprob}{
#' \preformatted{
#' combined_bdprob : Formal class 'Xu_wrapped_bd_problem' [package "bdpg"] with 36 slots
#' }}
#' \subsection{given_correct_solution_cost}{
#' \preformatted{
#' given_correct_solution_cost :  num 420
#' }}
#' \subsection{infile_name}{
#' \preformatted{
#' infile_name :  chr ""
#' }}
#' \subsection{integerize}{
#' \preformatted{
#' integerize : function (x, digits = 0)
#' }}
#' \subsection{max_allowed_num_spp}{
#' \preformatted{
#' max_allowed_num_spp :  num 2000
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{read_Xu_problem_from_Xu_file}{
#' \preformatted{
#' read_Xu_problem_from_Xu_file :  logi FALSE
#' }}
#' \subsection{starting_dir}{
#' \preformatted{
#' starting_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{wrap_lognormal_dist_around_Xu}{
#' \preformatted{
#' wrap_lognormal_dist_around_Xu :  logi TRUE
#' }}

#'
#' @param wrap_lognormal_dist_around_Xu boolean indicating whether to wrap a
#'     lognormal distribution around a base Xu problem; TRUE implies wrapping
#'     should be done; FALSE implies not
#' @param read_Xu_problem_from_Xu_file boolean indicating whether to read a
#'     Xu problem from one of Xu's benchmark problem files; TRUE implies that
#'     the problem should be read from one of those files; FALSE implies that
#'     the problem should be generated from scratch
#' @param infile_name string containing the name of the Xu benchmark file to
#'     read a problem from if reading from a Xu benchmark file
#' @param given_correct_solution_cost boolean indicating whether the correct
#'     cost of the correct optimum solution is known; TRUE implies that it is
#'     known
#' @param max_allowed_num_spp maximum number of species allowed in generating
#'     a problem from scratch (particularly of use when trying to do smaller,
#'     faster tests in development)
#' @inheritParams std_param_defns
#'
#' @return Returns a multi-BD_Prob
#' @export

gen_multi_bdprob = function (parameters,
                                    compute_network_metrics_for_this_prob,
                             wrap_lognormal_dist_around_Xu,
                             read_Xu_problem_from_Xu_bench_file,
                             infile_name,
                             given_correct_solution_cost,
                             max_allowed_num_spp,
                             bdpg_error_codes,
                             integerize,
                             bdprob_1 = NULL)
    {
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

        #--------------------------------------------------------------------
        #  Base problem is not a Xu benchmark read from a file, so go ahead
        #  and generate the problem.
        #--------------------------------------------------------------------

    cat ("\n\n>>>>>>>>>>>>>>>>>>>>>>  ABOUT TO build base Xu problem for multi-problem  <<<<<<<<<<<<<<<<<<<<<<<<<<<<\n\n")
    starting_dir =
        file.path (normalizePath (parameters$full_output_dir_with_slash))
                   # ,
                   # "base_prob.1")

    if (is.null (bdprob_1))
        {
        bdprob_1 = gen_single_bdprob_COR (starting_dir,
                                          parameters,
                                          read_Xu_problem_from_Xu_file,
                                          infile_name,
                                          given_correct_solution_cost,
                                          max_allowed_num_spp,
                                          bdpg_error_codes,
                                          integerize)
        }

    if (! bdprob_1@prob_is_ok)
        {
        stop ("\n\nGenerating base BD_Problem for multi-problem failed.\n\n")

                #--------------------------------------------------------------
        } else  #  Base problem generation worked, so build multiproblem now.
                #--------------------------------------------------------------
        {
        if (wrap_lognormal_dist_around_Xu)  #parameters$wrap_lognormal_around_Xu)
            {
            starting_dir =
                file.path (normalizePath (parameters$full_output_dir_with_slash))
                           # ,
                           # "wrap_prob.1")

            combined_bdprob = gen_wrapped_bdprob_COR (starting_dir,
                                    compute_network_metrics_for_this_prob,
                                                      parameters,
                                                      bdprob_1,
                                                      bdpg_error_codes)

            #     #---------------------------------
            #     #  Control parameters from user.
            #     #---------------------------------
            #
            # desired_Xu_spp_frac_of_all_spp  = parameters$desired_Xu_spp_frac_of_all_spp
            # solution_frac_of_landscape      = parameters$solution_frac_of_landscape
            # desired_max_abundance_frac      = parameters$desired_max_abundance_frac
            # dep_set_PUs_eligible            = parameters$dep_set_PUs_eligible
            # add_one_to_lognormal_abundances = parameters$add_one_to_lognormal_abundances
            # seed_value_for_search           = parameters$seed_value_for_search
            # max_search_iterations           = parameters$max_search_iterations
            #
            #     #-----------------------
            #     #  Derived parameters.
            #     #-----------------------
            #
            # tot_num_PUs_in_landscape = round (get_num_nodes (bdprob_1@nodes) /
            #                                   solution_frac_of_landscape)
            # search_outfile_name      = paste0 (parameters$full_output_dir_with_slash,
            #                                    "wrap_search_outfile.csv")
            #
            #     #-----------------------------------------------------------
            #     #  Search for a set of lognormal parameters that fit the
            #     #  user's constraints while including the base Xu problem.
            #     #-----------------------------------------------------------
            #
            # rounded_abundances =
            #     find_lognormal_to_wrap_around_Xu (bdprob_1,
            #                                       parameters,
            #                                       desired_Xu_spp_frac_of_all_spp,
            #                                       solution_frac_of_landscape,
            #                                       desired_max_abundance_frac,
            #                                       seed_value_for_search,
            #                                       max_search_iterations,
            #                                       add_one_to_lognormal_abundances,
            #                                       search_outfile_name)
            #
            #
            #     #--------------------------------------------------------------
            #     #  Wrap the generated lognormal around the Xu base problem.
            #     #
            #     #  Note that the wrap_abundance_dist_around_Xu_problem()
            #     #  function doesn't care where you got the abundances, i.e.,
            #     #  they don't have to have come from the lognormal generator.
            #     #
            #     #  It can be any abundance set that you want, as long as it
            #     #  contains at least as many species sitting on exactly
            #     #  2 patches as the base Xu problem has.
            #     #
            #     #  It can have more species that sit on exactly 2 patches
            #     #  than the base Xu problem, but not less.
            #     #--------------------------------------------------------------
            #
            # combined_bdprob =
            #     wrap_abundance_dist_around_Xu_problem (rounded_abundances,
            #                                            bdprob_1,
            #                                            dep_set_PUs_eligible,
            #                                            tot_num_PUs_in_landscape)

            }
        }

#docaids::doc_vars_in_this_func_once ()
    return (combined_bdprob)
    }

#===============================================================================

