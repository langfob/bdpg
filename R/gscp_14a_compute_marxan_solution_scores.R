#===============================================================================

                #  gscp_14a_compute_marxan_solution_scores.R

#===============================================================================


#===============================================================================

dist_between_marxan_solutions = function (solution_1, solution_2)
    {
#browser()
    dist_between_solutions <- sum (abs (solution_1 - solution_2))
#docaids::doc_vars_in_this_func_once ()
    return (dist_between_solutions)
    }

#-------------------------------------------------------------------------------

#' Compute marxan solution scores
#'
#' Compute marxan solution scores
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{cor_PU_costs}{
#' \preformatted{
#' cor_PU_costs :  num [1:407] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{cur_cost}{
#' \preformatted{
#' cur_cost :  num 61
#' }}
#' \subsection{cur_frac_of_all_spp_meeting_their_target}{
#' \preformatted{
#' cur_frac_of_all_spp_meeting_their_target :  num 1
#' }}
#' \subsection{cur_rep_fractions}{
#' \preformatted{
#' cur_rep_fractions :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{cur_solution_num}{
#' \preformatted{
#' cur_solution_num :  int 1
#' }}
#' \subsection{cur_solution_PUs}{
#' \preformatted{
#' cur_solution_PUs :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{marxan_solution_PU_IDs}{
#' \preformatted{
#' marxan_solution_PU_IDs :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{marxan_solution_scores}{
#' \preformatted{
#' marxan_solution_scores : 'data.frame':	4 obs. of  3 variables:
#'  $ solution_num  : int  1 2 3 4
#'  $ representation: num  1 0 0 0
#'  $ cost          : num  0.15 0 0 0
#' }}
#' \subsection{marxan_solutions_matrix}{
#' \preformatted{
#' marxan_solutions_matrix :  num [1:4, 1:407] 0 0 0 0 1 1 1 1 0 0 ...
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU}{
#' \preformatted{
#' spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{targets}{
#' \preformatted{
#' targets :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{total_landscape_cost}{
#' \preformatted{
#' total_landscape_cost :  num 407
#' }}
#'
#' @param spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU matrix
#' @param marxan_solution_PU_IDs numeric vector
#' @param targets numeric vector
#' @param num_spp integer
#' @param marxan_solutions_matrix matrix
#' @param cur_solution_num integer
#' @param marxan_solution_scores data frame
#' @param cor_PU_costs numeric vector
#' @param total_landscape_cost numeric
#'
#' @return Returns marxan_solution_scores data frame

compute_marxan_solution_scores <- function (spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU,
                                            marxan_solution_PU_IDs,
                                            targets,
                                            num_spp,
                                            marxan_solutions_matrix,
                                            cur_solution_num,
                                            marxan_solution_scores,
                                            cor_PU_costs,
                                            total_landscape_cost
                                            )
    {
    cur_rep_fractions =
        compute_rep_fraction (spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU,
                              marxan_solution_PU_IDs,
                              targets)
        #  How best to give a tolerance here?
        #  Could say cur_rep_fractions >= (1.0 - epsilon).
        #  This might be a good thing to compute in general, rather than
        #  just for this one routine, since it might be a more general
        #  way for someone to be playing with the results to see how they
        #  change as you loosen up the epsilon.  Still, they can already
        #  do that by just changing the targets in their inputs.
        #  Providing an epsilon in my code would still require re-running
        #  everything in the same way that having the user do it in the
        #  marxan input file, so there's no gain for having an epsilon here...
#*****
#  2017 02 18 - BTL
#  The column names in marxan_solution_scores are misleading.
#  Should rename them to "frac_of_all_spp_meeting_their_tgt" and
#  "frac_of_total_landscape_cost".
#  I would do this immediately, but I'm not sure how far that would propagate,
#  i.e., how many different places is this structure referenced?
#  Should really be doing this with a variable name, in the same way I do it
#  for most of the other data frames.
#  This in turn suggests that I need someplace to store global, shared constants.
#  Maybe I really should violate all the rules and make a global constant object
#  that is universally available but never written to other than when it is
#  created at the start of the run.  That would save all kinds of noise in the
#  argument lists that would need it passed in and/or passed down.
#*****
    cur_frac_of_all_spp_meeting_their_target = sum (cur_rep_fractions >= 1.0) / num_spp
    marxan_solution_scores [cur_solution_num, "representation"] = cur_frac_of_all_spp_meeting_their_target

    cur_solution_PUs = which (marxan_solutions_matrix [cur_solution_num,] > 0)
    cur_cost = compute_solution_cost (cur_solution_PUs, cor_PU_costs)
    marxan_solution_scores [cur_solution_num, "cost"] = cur_cost / total_landscape_cost

#docaids::doc_vars_in_this_func_once ()
    return (marxan_solution_scores)
    }


#-------------------------------------------------------------------------------

#' Get marxan solution choice string
#'
#' Get marxan solution choice string
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{marxan_best_cost}{
#' \preformatted{
#' marxan_best_cost :  num 0.15
#' }}
#' \subsection{marxan_best_rep}{
#' \preformatted{
#' marxan_best_rep :  num 1
#' }}
#' \subsection{solution_choice_string}{
#' \preformatted{
#' solution_choice_string :  chr "OK_marxan_solution_IS_apparent_best"
#' }}
#' \subsection{sorted_best_cost}{
#' \preformatted{
#' sorted_best_cost :  num 0.15
#' }}
#' \subsection{sorted_best_rep}{
#' \preformatted{
#' sorted_best_rep :  num 1
#' }}
#'
#' @param marxan_best_cost numeric
#' @param marxan_best_rep numeric
#' @param sorted_best_cost numeric
#' @param sorted_best_rep numeric
#'
#' @return Returns character string

get_marxan_solution_choice_string = function (marxan_best_cost,
                                              marxan_best_rep,
                                              sorted_best_cost,
                                              sorted_best_rep)
    {
    solution_choice_string = "OK__marxan_solution_IS_apparent_best"
    if (marxan_best_cost > sorted_best_cost)
        {
            #  marxan's chosen best is NOT the best cost
        if (marxan_best_rep < sorted_best_rep)
            {
                #  marxan's chosen best is also NOT the best representation
            solution_choice_string = "BAD__marxan_solution_NEITHER_best"
            } else
            {
                #  marxan's chosen best is not best score but is best representation
            solution_choice_string = "BAD__HALF_of_marxan_solution_NOT_apparent_best_cost_and_IS_apparent_best_rep"
            }
        } else if (marxan_best_rep < sorted_best_rep)
        {
            #  marxan's chosen best is best score but is NOT best representation
        solution_choice_string = "BAD__HALF_of_marxan_solution_IS_apparent_best_cost_and_NOT_apparent_best_rep"
        }

#docaids::doc_vars_in_this_func_once ()
    return (solution_choice_string)
    }

#-------------------------------------------------------------------------------

#' Verify whether the solution marxan chose as best was actually its best
#'
#' A comment in the marxan mailing list said that sometimes in the unix version
#' of marxan, the solution that marxan marked as its best was not actually its
#' best.  This function looks at all of marxan's solutions and compares them to
#' marxan's chosen best solution to verify that it really was the best.
#'
#' Writes an empty file in the marxan output directory and the name of the file
#' indicates whether marxan really did return its best guess. This will make it
#' easy to quickly search a bunch of runs to see whether any of them had a bad
#' marxan choice for best run by just looking for the existance of any files
#' whose names begin with "BAD_".  This should never happen, but this function
#' is run to make sure.
#'
#' The empty file that is written has one of the following names:
#'
#' - OK_marxan_solution_IS_apparent_best:  If marxan's chosen best cost solution
#' really is its best cost solution
#'
#' - BAD_marxan_solution_NEITHER_best:  If marxan's chosen best is also neither
#' the best cost nor the best representation
#'
#' - BAD_HALF_marxan_solution_NOT_apparent_best_cost_and_IS_apparent_best_rep:
#' If marxan's chosen best is not best cost but is best representation
#'
#' - BAD_HALF_marxan_solution_IS_apparent_best_cost_and_NOT_apparent_best_rep:
#' If marxan's chosen best is best cost but is NOT best representation
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{app_marxan_solution_scores}{
#' \preformatted{
#' app_marxan_solution_scores : 'data.frame':	4 obs. of  3 variables:
#'  $ solution_num  : int  1 2 3 4
#'  $ representation: num  1 1 1 1
#'  $ cost          : num  0.15 0.15 0.15 0.15
#' }}
#' \subsection{best_solution_ID_according_to_marxan}{
#' \preformatted{
#' best_solution_ID_according_to_marxan :  int 1
#' }}
#' \subsection{flag_file_name}{
#' \preformatted{
#' flag_file_name :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/OK_marxan_solution_IS_appar"| __truncated__
#' }}
#' \subsection{marxan_best_cost}{
#' \preformatted{
#' marxan_best_cost :  num 0.15
#' }}
#' \subsection{marxan_best_rep}{
#' \preformatted{
#' marxan_best_rep :  num 1
#' }}
#' \subsection{marxan_solution_choice_check_string}{
#' \preformatted{
#' marxan_solution_choice_check_string :  chr "OK_marxan_solution_IS_apparent_best"
#' }}
#' \subsection{out_dir}{
#' \preformatted{
#' out_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/"
#' }}
#' \subsection{sorted_best_cost}{
#' \preformatted{
#' sorted_best_cost :  num 0.15
#' }}
#' \subsection{sorted_best_rep}{
#' \preformatted{
#' sorted_best_rep :  num 1
#' }}
#'
#' @param best_solution_ID_according_to_marxan integer
#' @param app_marxan_solution_scores data frame
#' @param out_dir character string
#'
#' @return Returns nothing

see_if_marxan_best_was_actually_best <-
                            function (best_solution_ID_according_to_marxan,
                                      app_marxan_solution_scores,
                                      out_dir)
    {
    marxan_best_cost = app_marxan_solution_scores [best_solution_ID_according_to_marxan, "cost"]
    marxan_best_rep  = app_marxan_solution_scores [best_solution_ID_according_to_marxan, "representation"]
    sorted_best_cost = app_marxan_solution_scores [1, "cost"]
    sorted_best_rep  = app_marxan_solution_scores [1, "representation"]

    marxan_solution_choice_check_string =
        get_marxan_solution_choice_string (marxan_best_cost, marxan_best_rep,
                                           sorted_best_cost, sorted_best_rep)
    cat ("\n\n=====>  ", marxan_solution_choice_check_string, "     <=====\n", sep='')

      #  Write an empty file whose name indicates whether marxan really did
      #  return its best guess.
      #  This will make it easy to quickly search a bunch of runs to see
      #  whether any of them had a bad marxan choice for best run by
      #  just looking for the existance of any files whose names begin
      #  with "BAD_".

    flag_file_name = paste0 (out_dir, marxan_solution_choice_check_string)

    #    system (paste ("touch", flag_file_name), wait=FALSE)
    touch (flag_file_name)
#docaids::doc_vars_in_this_func_once ()
    }

#-------------------------------------------------------------------------------


#' Plot incrementatl marxan summed solution representations
#'
#'  For each step in order by Marxan summed solution PU ID:
#'  Want the fraction of all species who have met or exceeded their target
#'  when all PUs with the same number of votes or more are included in the
#'  solution.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{bpm}{
#' \preformatted{
#' bpm :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{cor_app_prefix_string}{
#' \preformatted{
#' cor_app_prefix_string :  chr "cor"
#' }}
#' \subsection{cor_PU_costs}{
#' \preformatted{
#' cor_PU_costs :  num [1:407] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{correct_optimum_landscape_frac_cost}{
#' \preformatted{
#' correct_optimum_landscape_frac_cost :  num 0.15
#' }}
#' \subsection{cost}{
#' \preformatted{
#' cost :  num [1:2] 61 407
#' }}
#' \subsection{cost_thresh_for_all_spp_meeting_targets}{
#' \preformatted{
#' cost_thresh_for_all_spp_meeting_targets :  num 61
#' }}
#' \subsection{cur_cost}{
#' \preformatted{
#' cur_cost :  num 407
#' }}
#' \subsection{cur_frac_of_all_spp_meeting_their_target}{
#' \preformatted{
#' cur_frac_of_all_spp_meeting_their_target :  num 1
#' }}
#' \subsection{cur_frac_rep_met_over_optimal_frac_cost}{
#' \preformatted{
#' cur_frac_rep_met_over_optimal_frac_cost :  num 0.15
#' }}
#' \subsection{cur_landscape_frac_cost}{
#' \preformatted{
#' cur_landscape_frac_cost :  num 1
#' }}
#' \subsection{cur_num_spp_meeting_their_target}{
#' \preformatted{
#' cur_num_spp_meeting_their_target :  int 1277
#' }}
#' \subsection{cur_optimal_frac_cost}{
#' \preformatted{
#' cur_optimal_frac_cost :  num 6.67
#' }}
#' \subsection{cur_rep_fractions}{
#' \preformatted{
#' cur_rep_fractions :  num [1:1277] 2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{cur_run_end_idx_in_PU_IDs}{
#' \preformatted{
#' cur_run_end_idx_in_PU_IDs :  num 407
#' }}
#' \subsection{cur_run_index}{
#' \preformatted{
#' cur_run_index :  num 2
#' }}
#' \subsection{cur_run_indices}{
#' \preformatted{
#' cur_run_indices :  int [1:346] 62 63 64 65 66 67 68 69 70 71 ...
#' }}
#' \subsection{cur_run_length}{
#' \preformatted{
#' cur_run_length :  int 346
#' }}
#' \subsection{cur_run_start_idx}{
#' \preformatted{
#' cur_run_start_idx :  num 408
#' }}
#' \subsection{cur_solution_PUs}{
#' \preformatted{
#' cur_solution_PUs :  int [1:407] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{frac_of_all_spp_meeting_their_target}{
#' \preformatted{
#' frac_of_all_spp_meeting_their_target :  num [1:2] 1 1
#' }}
#' \subsection{frac_rep_met_over_optimal_frac_cost}{
#' \preformatted{
#' frac_rep_met_over_optimal_frac_cost :  num [1:2] 1 0.15
#' }}
#' \subsection{landscape_frac_cost}{
#' \preformatted{
#' landscape_frac_cost :  num [1:2] 0.15 1
#' }}
#' \subsection{landscape_frac_cost_thresh_for_all_spp_meeting_targets}{
#' \preformatted{
#' landscape_frac_cost_thresh_for_all_spp_meeting_targets :  num 0.15
#' }}
#' \subsection{marxan_ssoln_df}{
#' \preformatted{
#' marxan_ssoln_df : 'data.frame':	407 obs. of  2 variables:
#'  $ planning_unit: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ number       : int  0 4 0 4 0 4 0 4 0 4 ...
#' }}
#' \subsection{marxan_ssoln_PUs_ranked_by_votes_df}{
#' \preformatted{
#' marxan_ssoln_PUs_ranked_by_votes_df : 'data.frame':	407 obs. of  2 variables:
#'  $ planning_unit: int  2 4 6 8 10 12 14 16 18 20 ...
#'  $ number       : int  4 4 4 4 4 4 4 4 4 4 ...
#' }}
#' \subsection{num_runs}{
#' \preformatted{
#' num_runs :  int 2
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{optimal_frac_cost}{
#' \preformatted{
#' optimal_frac_cost :  num [1:2] 1 6.67
#' }}
#' \subsection{optimal_frac_cost_thresh_for_all_spp_meeting_targets}{
#' \preformatted{
#' optimal_frac_cost_thresh_for_all_spp_meeting_targets :  num 1
#' }}
#' \subsection{optimum_cost}{
#' \preformatted{
#' optimum_cost :  num 61
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{rle_lengths_and_values}{
#' \preformatted{
#' rle_lengths_and_values : List of 2
#'  $ lengths: int [1:2] 61 346
#'  $ values : int [1:2] 4 0
#' }}
#' \subsection{thresh_found}{
#' \preformatted{
#' thresh_found :  logi TRUE
#' }}
#' \subsection{total_landscape_cost}{
#' \preformatted{
#' total_landscape_cost :  num 407
#' }}
#'
#' @param marxan_ssoln_df data frame
#' @param cor_PU_costs numeric vector
#' @param optimum_cost numeric
#' @param bpm matrix
#' @param cor_app_prefix_string character string
#' @param num_spp integer
#' @param plot_output_dir character string
#'
#' @return Returns nothing

plot_incremental_marxan_summed_solution_representations =
    function (marxan_ssoln_df,
              cor_PU_costs,
              optimum_cost,
              bpm,
              cor_app_prefix_string,
              num_spp,
              plot_output_dir
              )
    {
    marxan_ssoln_PUs_ranked_by_votes_df = plyr::arrange (marxan_ssoln_df, plyr::desc (number))

    total_landscape_cost = sum (cor_PU_costs)
    correct_optimum_landscape_frac_cost = optimum_cost / total_landscape_cost

    rle_lengths_and_values = rle (marxan_ssoln_PUs_ranked_by_votes_df [, "number"])
    num_runs = length (rle_lengths_and_values$values)

    cur_run_start_idx = 1
    cur_run_index = 0
    cur_solution_PUs = c()
    frac_of_all_spp_meeting_their_target = rep (0.0, num_runs)
    cost = rep (0, num_runs)
    landscape_frac_cost = rep (0, num_runs)
    optimal_frac_cost = rep (0, num_runs)
    frac_rep_met_over_optimal_frac_cost = rep (0, num_runs)
    thresh_found = FALSE
    cost_thresh_for_all_spp_meeting_targets = total_landscape_cost
    landscape_frac_cost_thresh_for_all_spp_meeting_targets = 1.0
    optimal_frac_cost_thresh_for_all_spp_meeting_targets =
        total_landscape_cost / optimum_cost

    for (cur_run_length in rle_lengths_and_values$lengths)
        {
        cur_run_index = cur_run_index + 1

        cur_run_end_idx_in_PU_IDs = cur_run_start_idx + cur_run_length - 1
        cur_run_indices = cur_run_start_idx : cur_run_end_idx_in_PU_IDs

        cur_solution_PUs =
        c(cur_solution_PUs,
        marxan_ssoln_PUs_ranked_by_votes_df [cur_run_indices, "planning_unit"])

        cur_rep_fractions = compute_rep_fraction (bpm,
                                        cur_solution_PUs,
                                        rep (1, num_spp))
        cur_num_spp_meeting_their_target = sum (cur_rep_fractions >= 1.0)  #  How best to give a tolerance here?
        cur_frac_of_all_spp_meeting_their_target =
            cur_num_spp_meeting_their_target / num_spp
        frac_of_all_spp_meeting_their_target [cur_run_index] =
            cur_frac_of_all_spp_meeting_their_target

        #--------------------

        cur_cost = compute_solution_cost (cur_solution_PUs, cor_PU_costs)
        cost [cur_run_index] = cur_cost

        cur_landscape_frac_cost = cur_cost / total_landscape_cost
        landscape_frac_cost [cur_run_index] = cur_landscape_frac_cost

        cur_optimal_frac_cost = cur_cost / optimum_cost
        optimal_frac_cost [cur_run_index] = cur_optimal_frac_cost

        #--------------------

        cur_frac_rep_met_over_optimal_frac_cost =
            cur_frac_of_all_spp_meeting_their_target / cur_optimal_frac_cost
        frac_rep_met_over_optimal_frac_cost [cur_run_index] =
            cur_frac_rep_met_over_optimal_frac_cost

        #--------------------

        if (!thresh_found)
            {
            if (cur_frac_of_all_spp_meeting_their_target >= 1.0)
                {
                thresh_found = TRUE

                cost_thresh_for_all_spp_meeting_targets = cur_cost
                landscape_frac_cost_thresh_for_all_spp_meeting_targets =
                    cur_landscape_frac_cost
                optimal_frac_cost_thresh_for_all_spp_meeting_targets =
                    cur_optimal_frac_cost

                cat ("\n\n>>>>> For marxan summed solution:")
                cat ("\n", cor_app_prefix_string, "_", "cost_thresh_for_all_spp_meeting_targets = ",
                   cost_thresh_for_all_spp_meeting_targets, sep='')
                cat ("\n", cor_app_prefix_string, "_", "landscape_frac_cost_thresh_for_all_spp_meeting_targets = ",
                   landscape_frac_cost_thresh_for_all_spp_meeting_targets, sep='')
                cat ("\n", cor_app_prefix_string, "_", "optimal_frac_cost_thresh_for_all_spp_meeting_targets = ",
                   optimal_frac_cost_thresh_for_all_spp_meeting_targets, sep='')

                }  #  end if - all targets met
            }  #  end if - no threshold found yet

        cat ("\n")

        #--------------------

        cur_run_start_idx = cur_run_end_idx_in_PU_IDs + 1
        }

    #-------------------------------------------
    #-------------------------------------------

###  2015 05 14 - BTL
###  Try converting this to "natural" spline instead of using loess().
###  It may be better-behaved.
###  See the code I saved in running notes file today for an example.

# splined_data <- stats::splinefun (cost, frac_of_all_spp_meeting_their_target,
#                                     method="natural")

# curve (splined_data, 0.5, 5,
#         lwd=6,
#         col="darkblue",
#         xlab="Experience",
#         ylab="Perceived difficulty",
#          axes=FALSE,
#         xlim=c(0.4,5)
#         )

    #--------------------

        #  2015 04 10 - BTL
        #  NOTE:  In the following code, the call to loess() is used to make
        #         a smoother looking approximation to the curve.
        #         It occasionally generates a
        #         warning and that causes everything to fail when I have options(warn=2)
        #         set to turn all warnings into errors.
        #         Since this is unimportant code that is only aimed at giving a bit
        #         more visualization of the results, I'm going to wrap all of the
        #         loess() calls in suppressWarnings() to keep things from failing.

        #  See http://www.statmethods.net/advgraphs/axes.html and
        #  http://www.statmethods.net/advgraphs/parameters.html
        #  for help on plot labelling if I need to modify these plots.

    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_ssoln_frac_rep_vs_raw_cost.pdf")))
    plot (cost, frac_of_all_spp_meeting_their_target,
          main="Marxan summed solutions\nFraction of spp meeting targets vs. Raw costs",
          xlab="Solution cost",
          ylab="Fraction of spp meeting target")

    lines (suppressWarnings (loess (frac_of_all_spp_meeting_their_target ~ cost)))    #  good fit
    #lines (lowess (cost, frac_of_all_spp_meeting_their_target))    #  terrible fit

# splined_data <- stats::splinefun (cost, frac_of_all_spp_meeting_their_target,
#                                   method="natural")
# curve (splined_data, #0.5, 5,
#         lwd=6,
#         col="darkblue"
# #        xlab="Experience",
# #        ylab="Perceived difficulty",
# #         axes=FALSE,
# #        xlim=c(0.4,5)
#         )

    abline (v=optimum_cost, lty=2)
    abline (h=1.0, lty=2)
    dev.off()

    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_ssoln_frac_rep_vs_normalized_cost.pdf")))
    plot (landscape_frac_cost, frac_of_all_spp_meeting_their_target,
          main="Marxan summed solutions\nFraction of spp meeting targets vs. Normalized costs",
          xlab="Solution cost as fraction of total landscape cost",
          ylab="Fraction of spp meeting target")
    lines (suppressWarnings (loess (frac_of_all_spp_meeting_their_target ~ landscape_frac_cost)))    #  good fit
    abline (v=correct_optimum_landscape_frac_cost, lty=4)
    abline (h=1.0, lty=4)
    dev.off()

    #--------------------

        #  These two seemed like they should be useful, but don't seem to tell
        #  much.  They just say that you're always getting less bang for your
        #  buck as you add more planning units.
        #  The plots above seem to tell more about that in that you can see the
        #  inflection point where the plot starts to bend.
        #  I'll leave them in for now, but could probably chuck them.

    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_ssoln_frac_rep_vs_frac_optimal_cost.pdf")))
    plot (optimal_frac_cost, frac_of_all_spp_meeting_their_target,
          main="Marxan summed solutions\nFraction of spp meeting targets vs. Fraction of optimal cost",
          xlab="Solution cost as fraction of optimal cost",
          ylab="Fraction of spp meeting target")
    lines (suppressWarnings (loess (frac_of_all_spp_meeting_their_target ~ optimal_frac_cost)))    #  good fit
    abline (v=1, lty=5)
    abline (h=1.0, lty=5)
    dev.off()

    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_ssoln_frac_rep_over_frac_optimal_cost.pdf")))
    plot (optimal_frac_cost, frac_rep_met_over_optimal_frac_cost,
          main="Marxan summed solutions\nRatio: sppFrac/optCostFrac vs. optCostFrac",
          xlab="Solution cost as fraction of optimal cost",
          ylab="Fraction of spp meeting target / fraction of optimal cost")
    #lines (suppressWarnings (loess (frac_of_all_spp_meeting_their_target ~ optimal_frac_cost)))    #  good fit
    abline (v=1, lty=6)
    abline (h=1.0, lty=6)
    dev.off()

#docaids::doc_vars_in_this_func_once ()
    }

#-------------------------------------------------------------------------------

#' Plot how marxan is actually doing vs. how marxan things it's doing
#'
#'  Evaluate apparent summed solutions as a function of the correct
#'  problem structure and the apparent problem structure, i.e.,
#'  how marxan is really doing vs. how marxan thinks it's doing.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{app_bpm}{
#' \preformatted{
#' app_bpm :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{app_optimum_cost}{
#' \preformatted{
#' app_optimum_cost :  int 61
#' }}
#' \subsection{cor_bpm}{
#' \preformatted{
#' cor_bpm :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{cor_PU_costs}{
#' \preformatted{
#' cor_PU_costs :  num [1:407] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{correct_solution_cost}{
#' \preformatted{
#' correct_solution_cost :  num 61
#' }}
#' \subsection{marxan_ssoln_df}{
#' \preformatted{
#' marxan_ssoln_df : 'data.frame':	407 obs. of  2 variables:
#'  $ planning_unit: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ number       : int  0 4 0 4 0 4 0 4 0 4 ...
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#'
#' @param marxan_ssoln_df data frame
#' @param cor_PU_costs numeric vector
#' @param correct_solution_cost numeric
#' @param cor_bpm matrix
#' @param app_optimum_cost numeric
#' @param app_bpm matrix
#' @param num_spp integer
#' @param plot_output_dir character string
#'
#' @return Returns nothing

plot_incremental_marxan_summed_solution_reps_for_COR_and_APP <-
    function (marxan_ssoln_df,
                cor_PU_costs,
                correct_solution_cost,
                app_optimum_cost,
                cor_bpm,
                app_bpm,
                num_spp,
                plot_output_dir
                )
    {
        #  Using correct scores...
    plot_incremental_marxan_summed_solution_representations (marxan_ssoln_df,
                                                                cor_PU_costs,
                                                                correct_solution_cost,
                                                                cor_bpm,
                                                                "cor",
                                                                num_spp,
                                                                plot_output_dir
                                                             )

        #  Using apparent scores...
    plot_incremental_marxan_summed_solution_representations (marxan_ssoln_df,
                                                                cor_PU_costs,
                                                                app_optimum_cost,
                                                                app_bpm,
                                                                "app",
                                                                num_spp,
                                                                plot_output_dir
                                                             )

#docaids::doc_vars_in_this_func_once ()
    }

#-------------------------------------------------------------------------------

#' Plot marxan best soltion scores COR and APP
#'
#' Plot marxan best solution scores for both CORRECT and APPARENT
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{app_marxan_solution_scores}{
#' \preformatted{
#' app_marxan_solution_scores : 'data.frame':	4 obs. of  3 variables:
#'  $ solution_num  : int  1 2 3 4
#'  $ representation: num  1 1 1 1
#'  $ cost          : num  0.15 0.15 0.15 0.15
#' }}
#' \subsection{best_solution_ID_according_to_marxan}{
#' \preformatted{
#' best_solution_ID_according_to_marxan :  int 1
#' }}
#' \subsection{cor_app_prefix_string}{
#' \preformatted{
#' cor_app_prefix_string :  chr "app"
#' }}
#' \subsection{cor_marxan_solution_scores}{
#' \preformatted{
#' cor_marxan_solution_scores : 'data.frame':	4 obs. of  3 variables:
#'  $ solution_num  : int  1 2 3 4
#'  $ representation: num  1 1 1 1
#'  $ cost          : num  0.15 0.15 0.15 0.15
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#'
#' @param plot_output_dir character string
#' @param cor_marxan_solution_scores data frame
#' @param best_solution_ID_according_to_marxan integer
#' @param app_marxan_solution_scores data frame
#'
#' @return Returns nothing

plot_marxan_best_solution_scores_COR_and_APP <- function (plot_output_dir,
                                                            cor_marxan_solution_scores,
                                                            best_solution_ID_according_to_marxan,
                                                            app_marxan_solution_scores
                                                            )
    {
    cor_app_prefix_string = "cor"
    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_all_solutions_frac_rep_vs_raw_cost.pdf")))
      plot (cor_marxan_solution_scores [, "cost"],
              cor_marxan_solution_scores [, "representation"],
              main="Marxan solutions\nCORRECT - Fraction of spp meeting targets vs. costs",
              xlab="Solution cost",
              ylab="Fraction of spp meeting target",
              col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
              )

    text(cor_marxan_solution_scores [, "cost"],
         cor_marxan_solution_scores [, "representation"],
         labels = cor_marxan_solution_scores [, "solution_num"],
         cex= 0.7, pos=4)

        #  Color marxan's chosen solution red.
    points (cor_marxan_solution_scores [best_solution_ID_according_to_marxan, "cost"],
            cor_marxan_solution_scores [best_solution_ID_according_to_marxan, "representation"],
            col= "red", pch = 19, cex = 1, lty = "solid", lwd = 2)
    dev.off()

    cor_app_prefix_string = "app"
    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_all_solutions_frac_rep_vs_raw_cost.pdf")))
    plot (app_marxan_solution_scores [, "cost"],
          app_marxan_solution_scores [, "representation"],
          main="Marxan solutions\nAPPARENT - Fraction of spp meeting targets vs. costs",
          xlab="Solution cost",
          ylab="Fraction of spp meeting target",
          col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
          )

    text(app_marxan_solution_scores [, "cost"],
         app_marxan_solution_scores [, "representation"],
         labels = cor_marxan_solution_scores [, "solution_num"],
         cex= 0.7, pos=4)

        #  Color marxan's chosen solution red.
    points (app_marxan_solution_scores [best_solution_ID_according_to_marxan, "cost"],
            app_marxan_solution_scores [best_solution_ID_according_to_marxan, "representation"],
            col= "red", pch = 19, cex = 1, lty = "solid", lwd = 2)
    dev.off()

#docaids::doc_vars_in_this_func_once ()
    }

#-------------------------------------------------------------------------------

# Variables and their structures
#
# app_marxan_solution_scores: data frame:  columns: [same as for cor_marxan_solution_scores]
# cor_marxan_solution_scores: data frame:  columns:
#     - solution_num
#     - representation
#     - cost
# cur_marxan_solution_PU_IDs:  vector of integers
# marxan_solutions_matrix:  data frame:  columns:
#     - ???  see load_marxan_solutionsmatrix_from_file_and_sort_and_add_missing_PUs()
# marxan_solutions_matrix_and_num_solutions:  list:  slots:
#     - marxan_solutions_matrix
#     - num_marxan_solutions
# num_marxan_solutions:  integer
# total_landscape_cost:  integer

#-------------------------------------------------------------------------------

#' Find best marxan solutions
#'
#' Find best marxan soltuions
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{app_bpm}{
#' \preformatted{
#' app_bpm :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{app_marxan_solution_scores}{
#' \preformatted{
#' app_marxan_solution_scores : 'data.frame':	4 obs. of  3 variables:
#'  $ solution_num  : int  1 2 3 4
#'  $ representation: num  1 1 1 1
#'  $ cost          : num  0.15 0.15 0.15 0.15
#' }}
#' \subsection{best_solution_ID_according_to_marxan}{
#' \preformatted{
#' best_solution_ID_according_to_marxan :  int 1
#' }}
#' \subsection{cor_bpm}{
#' \preformatted{
#' cor_bpm :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{cor_marxan_solution_scores}{
#' \preformatted{
#' cor_marxan_solution_scores : 'data.frame':	4 obs. of  3 variables:
#'  $ solution_num  : int  1 2 3 4
#'  $ representation: num  1 1 1 1
#'  $ cost          : num  0.15 0.15 0.15 0.15
#' }}
#' \subsection{cor_PU_costs}{
#' \preformatted{
#' cor_PU_costs :  num [1:407] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{cur_col}{
#' \preformatted{
#' cur_col :  int 4
#' }}
#' \subsection{cur_dist_from_marxan_best_df_sorted_as_vector}{
#' \preformatted{
#' cur_dist_from_marxan_best_df_sorted_as_vector :  num 0
#' }}
#' \subsection{cur_marxan_solution_PU_IDs}{
#' \preformatted{
#' cur_marxan_solution_PU_IDs :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{cur_row}{
#' \preformatted{
#' cur_row :  int 4
#' }}
#' \subsection{cur_solution_num}{
#' \preformatted{
#' cur_solution_num :  int 4
#' }}
#' \subsection{distances_between_marxan_solutions}{
#' \preformatted{
#' distances_between_marxan_solutions :  num [1:4, 1:4] 0 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{IDs_of_vectors_matching_marxan_best_solution_choice}{
#' \preformatted{
#' IDs_of_vectors_matching_marxan_best_solution_choice :  int [1:4] 1 2 3 4
#' }}
#' \subsection{largest_PU_ID}{
#' \preformatted{
#' largest_PU_ID :  num 407
#' }}
#' \subsection{largest_spp_ID}{
#' \preformatted{
#' largest_spp_ID :  int 1277
#' }}
#' \subsection{marxan_best_df_sorted_as_vector}{
#' \preformatted{
#' marxan_best_df_sorted_as_vector :  int [1:407] 0 1 0 1 0 1 0 1 0 1 ...
#' }}
#' \subsection{marxan_output_dir_path}{
#' \preformatted{
#' marxan_output_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{marxan_solutions_matrix}{
#' \preformatted{
#' marxan_solutions_matrix :  num [1:4, 1:407] 0 0 0 0 1 1 1 1 0 0 ...
#' }}
#' \subsection{marxan_solutions_matrix_and_num_solutions}{
#' \preformatted{
#' marxan_solutions_matrix_and_num_solutions : List of 2
#'  $ marxan_solutions_matrix: num [1:4, 1:407] 0 0 0 0 1 1 1 1 0 0 ...
#'  $ num_marxan_solutions   : int 4
#' }}
#' \subsection{num_marxan_solutions}{
#' \preformatted{
#' num_marxan_solutions :  int 4
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{short_range}{
#' \preformatted{
#' short_range :  num 4
#' }}
#' \subsection{targets}{
#' \preformatted{
#' targets :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{total_landscape_cost}{
#' \preformatted{
#' total_landscape_cost :  num 407
#' }}
#'
#' @param marxan_output_dir_path character string
#' @param num_spp integer
#' @param cor_PU_costs numeric vector
#' @param cor_bpm matrix
#' @param app_bpm matrix
#' @param marxan_best_df_sorted_as_vector data frame
#' @param plot_output_dir character string
#' @param largest_PU_ID integer
#' @param largest_spp_ID integer
#' @param targets numeric vector
#'
#' @return Returns nothing

find_best_marxan_solutions <- function (marxan_output_dir_path,
                                        #num_PUs,     #  should this be largest_PU_ID?
                                        num_spp,     #  should this be largest_spp_ID?
                                        cor_PU_costs,
                                        cor_bpm,
                                        app_bpm,
                                        marxan_best_df_sorted_as_vector,
                                        plot_output_dir,

                                        largest_PU_ID,
                                        largest_spp_ID,

                                        targets
                                        )
    {
    marxan_solutions_matrix_and_num_solutions <-
        load_marxan_solutionsmatrix_from_file_and_sort_and_add_missing_PUs (
            marxan_output_dir_path,
            largest_PU_ID)

    marxan_solutions_matrix = marxan_solutions_matrix_and_num_solutions$marxan_solutions_matrix
    num_marxan_solutions    = marxan_solutions_matrix_and_num_solutions$num_marxan_solutions

  #-----------------------------------------------------------------------------

        #  Ready now to go through the matrix and compute the representation and
        #  cost values for each solution row.

    cor_marxan_solution_scores = data.frame (solution_num=1:num_marxan_solutions,
                                            representation=0,
                                            cost=0)

    app_marxan_solution_scores = cor_marxan_solution_scores

    total_landscape_cost = sum (cor_PU_costs)

    cat("\njust before for() to compute marxan solutions scores for each solution.")
    for (cur_solution_num in 1:num_marxan_solutions)
        {
        cor_marxan_solution_scores [cur_solution_num, "solution_num"] = cur_solution_num
        app_marxan_solution_scores [cur_solution_num, "solution_num"] = cur_solution_num

        cur_marxan_solution_PU_IDs = which (marxan_solutions_matrix [cur_solution_num,] > 0)

        cat("\njust before compute_marxan_solution_scores() for cur_solution_num = ", cur_solution_num, ".")

        cor_marxan_solution_scores = compute_marxan_solution_scores (cor_bpm,
                                                                   cur_marxan_solution_PU_IDs,
                                                                   targets,
                                                                   num_spp,
                                                                   marxan_solutions_matrix,
                                                                   cur_solution_num,
                                                                   cor_marxan_solution_scores,
                                                                   cor_PU_costs,
                                                                   total_landscape_cost)

        app_marxan_solution_scores = compute_marxan_solution_scores (app_bpm,
                                                                   cur_marxan_solution_PU_IDs,
                                                                   targets,
                                                                   num_spp,
                                                                   marxan_solutions_matrix,
                                                                   cur_solution_num,
                                                                   app_marxan_solution_scores,
                                                                   cor_PU_costs,
                                                                   total_landscape_cost)
        }

    # cor_sorted_marxan_solution_scores = plyr::arrange (cor_marxan_solution_scores, -representation, -cost)
    # app_sorted_marxan_solution_scores = plyr::arrange (app_marxan_solution_scores, -representation, -cost)

    distances_between_marxan_solutions = matrix (0,
                                                 nrow  = num_marxan_solutions,
                                                 ncol  = num_marxan_solutions,
                                                 byrow = TRUE)

    IDs_of_vectors_matching_marxan_best_solution_choice = c()
    for (cur_row in 1:num_marxan_solutions)
        {
                            cat ("\n\ncur_row = ", cur_row, ", just before first dist_between_marxan_solutions()")
        cur_dist_from_marxan_best_df_sorted_as_vector =
            dist_between_marxan_solutions (marxan_solutions_matrix [cur_row, ],
                                           marxan_best_df_sorted_as_vector)

        if (cur_dist_from_marxan_best_df_sorted_as_vector == 0)
            IDs_of_vectors_matching_marxan_best_solution_choice =
                c (IDs_of_vectors_matching_marxan_best_solution_choice, cur_row)

        for (cur_col in 1:num_marxan_solutions)
            {
                                cat ("\n\ncur_col = ", cur_col, ", just before second dist_between_marxan_solutions()")
            distances_between_marxan_solutions [cur_row, cur_col] =
                dist_between_marxan_solutions (marxan_solutions_matrix [cur_row, ],
                                                marxan_solutions_matrix [cur_col, ])
            }
        }

                        short_range = min (num_marxan_solutions, 5)
                        cat ("\n\ndistances_between_marxan_solutions [1:short_range,1:short_range] = \n")
                        print (distances_between_marxan_solutions [1:short_range,1:short_range])

                        cat ("\n\nIDs_of_vectors_matching_marxan_best_solution_choice = ",
                           IDs_of_vectors_matching_marxan_best_solution_choice)
                        cat ("\nnumber of vectors matching marxan best solution choice = ",
                           length (IDs_of_vectors_matching_marxan_best_solution_choice))

        #-----------------------------------------------------------------------
        #  Marxan returns a best solution, but I have not been able to find
        #  anyplace where it tells you what solution number it was.
        #  Since you can have multiple identical solutions, there may be
        #  multiple solution IDs that could be identified as the best solution.
        #  I need any one of them to use to get the corresponding solution
        #  vector.
        #  I will arbitrarily choose the first one in the list of vectors that
        #  selected the same PUs as the vector that marxan returned.
        #-----------------------------------------------------------------------

    best_solution_ID_according_to_marxan =
        IDs_of_vectors_matching_marxan_best_solution_choice [1]

    plot_marxan_best_solution_scores_COR_and_APP (plot_output_dir,
                                                    cor_marxan_solution_scores,
                                                    best_solution_ID_according_to_marxan,
                                                    app_marxan_solution_scores
                                                    )

  #--------------------

    see_if_marxan_best_was_actually_best (best_solution_ID_according_to_marxan,
                                          app_marxan_solution_scores,
                                          parameters$fullOutputDirWithSlash)

#docaids::doc_vars_in_this_func_once ()

#browser()
    }

#===============================================================================

