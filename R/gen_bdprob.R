#===============================================================================
#                   Generate a biodiversity problem.
#===============================================================================

#' Generate a biodiversity problem
#'
#'Generate a biodiversity problem.
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
#' \subsection{bdprob}{
#' \preformatted{
#' bdprob : Formal class 'Xu_wrapped_bd_problem' [package "bdpg"] with 36 slots
#' }}
#' \subsection{gen_multi_bdproblem}{
#' \preformatted{
#' gen_multi_bdproblem :  logi TRUE
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
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1842_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1842_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{read_Xu_problem_from_Xu_file}{
#' \preformatted{
#' read_Xu_problem_from_Xu_file :  logi FALSE
#' }}
#' \subsection{wrap_lognormal_dist_around_Xu}{
#' \preformatted{
#' wrap_lognormal_dist_around_Xu :  logi TRUE
#' }}
#'
#' @param base_bdprob NULL or an existing bdprob to use in creating multi_bdproblem
#' @inheritParams std_param_defns
#'
#' @return biodiversity problems
#' @export

gen_bdprob  = function (parameters,
                                    compute_network_metrics_for_this_prob,
                        bdpg_error_codes, integerize,
                        base_bdprob = NULL)
    {
        #  May need to test these parameters to see if they even exist
        #  and if they don't for this particular run, then set them to
        #  something like NULL.

    read_Xu_problem_from_Xu_file   = parameters$read_Xu_problem_from_Xu_file
    infile_name                    = parameters$infile_name
    if (is.null (infile_name)) infile_name = ""
    given_correct_solution_cost    = parameters$given_correct_solution_cost
    max_allowed_num_spp            = parameters$max_allowed_num_spp

##    wrap_lognormal_dist_around_Xu    = TRUE    #  temp for testing only...
    wrap_lognormal_dist_around_Xu  = parameters$wrap_lognormal_dist_around_Xu

##    parameters$gen_multi_bdproblem      = TRUE    #  temp for testing only...
    gen_multi_bdproblem            = parameters$gen_multi_bdproblem

    if (gen_multi_bdproblem)
        {
        bdprob = gen_multi_bdprob (parameters,
                                    compute_network_metrics_for_this_prob,
                                   wrap_lognormal_dist_around_Xu,
                                   read_Xu_problem_from_Xu_file,
                                   infile_name,
                                   given_correct_solution_cost,
                                   max_allowed_num_spp,
                                   bdpg_error_codes,
                                   integerize,
                                   base_bdprob)

        } else
        {
        # starting_dir =
        #     file.path (normalizePath (parameters$fullOutputDirWithSlash),
        #                "base_prob.1")
        bdprob = gen_single_bdprob_COR (parameters$fullOutputDirWithSlash,  #starting_dir,
                                    compute_network_metrics_for_this_prob,
                                        parameters,
                                        read_Xu_problem_from_Xu_file,
                                        infile_name,
                                        given_correct_solution_cost,
                                        max_allowed_num_spp,
                                        bdpg_error_codes,
                                        integerize)
        }

#docaids::doc_vars_in_this_func_once ()
    return (bdprob)
    }

#===============================================================================


