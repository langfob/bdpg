#===============================================================================

                #  gscp_14a_compute_marxan_solution_scores.R

#===============================================================================

dist_between_marxan_solutions = function (solution_1, solution_2)
    {
#browser()
    dist_between_solutions <- sum (abs (solution_1 - solution_2))
#docaids::doc_vars_in_this_func_once ()
    return (dist_between_solutions)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Compute marxan solution scores
#'
#' Compute marxan solution scores
#'
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

compute_marxan_solution_scores <-   #_wrt_COR_reps_and_costs_or_APP_reps_and_costs <-
    function (spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU,
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

    return (marxan_solution_scores)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Choose a best solution ID according to marxan
#'
#'  Marxan returns a best solution, but I have not been able to find
#'  anyplace where it tells you what solution number it was.
#'  Since you can have multiple identical solutions, there may be
#'  multiple solution IDs that could be identified as the best solution.
#'  I need any one of them to use to get the corresponding solution
#'  vector.
#'  I will arbitrarily choose the first one in the list of vectors that
#'  selected the same PUs as the vector that marxan returned.

#-------------------------------------------------------------------------------

#' @param num_marxan_solutions integer
#' @param marxan_solutions_matrix matrix
#' @param marxan_best_df_sorted_as_vector vector
#'
#' @return integer ID of best solution according to marxan

#-------------------------------------------------------------------------------

choose_a_best_solution_ID_according_to_marxan <- function (num_marxan_solutions,
                                                           marxan_solutions_matrix,
                                                           marxan_best_df_sorted_as_vector)
    {
    distances_between_marxan_solutions = matrix (0,
                                           nrow  = num_marxan_solutions,
                                           ncol  = num_marxan_solutions,
                                           byrow = TRUE)

    IDs_of_vectors_matching_marxan_best_solution_choice = c()
    for (cur_row in 1:num_marxan_solutions)
        {
#                          cat ("\n\ncur_row = ", cur_row, ", just before first dist_between_marxan_solutions()")
        cur_dist_from_marxan_best_df_sorted_as_vector =
            dist_between_marxan_solutions (marxan_solutions_matrix [cur_row, ],
                                           marxan_best_df_sorted_as_vector)

        if (cur_dist_from_marxan_best_df_sorted_as_vector == 0)
            IDs_of_vectors_matching_marxan_best_solution_choice =
                c (IDs_of_vectors_matching_marxan_best_solution_choice,
                   cur_row)

        for (cur_col in 1:num_marxan_solutions)
            {
#                          cat ("\n\ncur_col = ", cur_col, ", just before second dist_between_marxan_solutions()")
            distances_between_marxan_solutions [cur_row, cur_col] =
                dist_between_marxan_solutions (marxan_solutions_matrix [cur_row, ],
                                               marxan_solutions_matrix [cur_col, ])
            }
        }

                    short_range = min (num_marxan_solutions, 5)
#                    cat ("\n\ndistances_between_marxan_solutions [1:short_range,1:short_range] = \n")
#                    print (distances_between_marxan_solutions [1:short_range,1:short_range])

                    cat ("\n\nIDs_of_vectors_matching_marxan_best_solution_choice = ",
                         IDs_of_vectors_matching_marxan_best_solution_choice)
                    cat ("\nnumber of vectors matching marxan best solution choice = ",
                         length (IDs_of_vectors_matching_marxan_best_solution_choice))

    a_best_solution_ID_according_to_marxan =
          IDs_of_vectors_matching_marxan_best_solution_choice [1]

    return (a_best_solution_ID_according_to_marxan)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#'  Compute rep and cost values of marxan solutions with respect to COR or APP
#'
#'  Go through the solutions matrix and compute the representation
#'  and cost values for each solution that marxan found in its different
#'  restarts.

#-------------------------------------------------------------------------------

#' @param given_bpm blah
#' @param given_PU_costs blah
#' @param num_marxan_solutions blah
#' @param marxan_solutions_matrix blah
#' @param targets blah
#' @param num_spp blah
#'
#' @return blah

#-------------------------------------------------------------------------------

compute_marxan_solution_scores_wrt_COR_or_APP_reps_and_costs <-
    function (given_bpm,
              given_PU_costs,
              num_marxan_solutions,
              marxan_solutions_matrix,
              targets,
              num_spp
              )
    {
    marxan_solution_scores_wrt_GIVEN_reps_and_costs =
                            data.frame (solution_num=1:num_marxan_solutions,
                                        representation=0,
                                        cost=0)

    total_landscape_cost = sum (given_PU_costs)

    for (cur_solution_num in 1:num_marxan_solutions)
        {
        marxan_solution_scores_wrt_GIVEN_reps_and_costs [cur_solution_num,
                                                         "solution_num"] =
            cur_solution_num

        cur_marxan_solution_PU_IDs =
            which (marxan_solutions_matrix [cur_solution_num,] > 0)

            #------------------------------------------------------------
            #  Compute various scores of Current Marxan Solution
            #  (e.g., fraction of species meeting their target,
            #  solution cost, etc.) with respect to the
            #  GIVEN species representation values (either COR or APP).
            #------------------------------------------------------------

        marxan_solution_scores_wrt_GIVEN_reps_and_costs =
            compute_marxan_solution_scores (given_bpm,
                                            cur_marxan_solution_PU_IDs,
                                            targets,
                                            num_spp,
                                            marxan_solutions_matrix,
                                            cur_solution_num,
                                            marxan_solution_scores_wrt_GIVEN_reps_and_costs,
                                            given_PU_costs,
                                            total_landscape_cost)
        }

    return (marxan_solution_scores_wrt_GIVEN_reps_and_costs)
    }

#===============================================================================

# Variables and their structures
#
# marxan_solution_scores_wrt_APP_reps_and_costs: data frame:  columns: [same as for marxan_solution_scores_wrt_COR_reps_and_costs]
# marxan_solution_scores_wrt_COR_reps_and_costs: data frame:  columns:
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

#===============================================================================

#-------------------------------------------------------------------------------

#' Find best marxan solutions
#'
#' Find best marxan soltuions
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
#' @param marxan_top_dir character string
#'
#' @return Returns nothing

#-------------------------------------------------------------------------------

find_best_marxan_solutions <- function (marxan_output_dir_path,
                                        #num_PUs,     #  should this be largest_PU_ID?
                                        num_spp,     #  should this be largest_spp_ID?
                                #cor_PU_costs,
                                        cor_bpm,
                                        app_bpm,
                                        marxan_best_df_sorted_as_vector,
                                        plot_output_dir,

                                        largest_PU_ID,
                                        largest_spp_ID,

                                        targets,

                                        marxan_top_dir
                                        )
    {
    marxan_solutions_matrix_and_num_solutions <-
        load_marxan_solutionsmatrix_and_add_missing_PUs (
#        load_marxan_solutionsmatrix_from_file_and_sort_and_add_missing_PUs (
            marxan_output_dir_path,
            largest_PU_ID)

    marxan_solutions_matrix = marxan_solutions_matrix_and_num_solutions$marxan_solutions_matrix
    num_marxan_solutions    = marxan_solutions_matrix_and_num_solutions$num_marxan_solutions

    #---------------------------------------------------------------------------

    # cor_sorted_marxan_solution_scores = plyr::arrange (marxan_solution_scores_wrt_COR_reps_and_costs, -representation, -cost)
    # app_sorted_marxan_solution_scores = plyr::arrange (marxan_solution_scores_wrt_APP_reps_and_costs, -representation, -cost)

    best_solution_ID_according_to_marxan =
        choose_a_best_solution_ID_according_to_marxan (num_marxan_solutions,
                                                       marxan_solutions_matrix,
                                                       marxan_best_df_sorted_as_vector)

    #---------------------------------------------------------------------------

    marxan_solution_scores_wrt_COR_reps_and_costs =
        compute_marxan_solution_scores_wrt_COR_or_APP_reps_and_costs (cor_bpm,
                                                                      cor_PU_costs,
                                                                      num_marxan_solutions,
                                                                      marxan_solutions_matrix,
                                                                      targets,
                                                                      num_spp)

app_PU_costs = cor_PU_costs    #  for the moment, since no app costs stored yet?  2017 11 30 - BTL

    marxan_solution_scores_wrt_APP_reps_and_costs =
        compute_marxan_solution_scores_wrt_COR_or_APP_reps_and_costs (app_bpm,
                                                                      app_PU_costs,
                                                                      num_marxan_solutions,
                                                                      marxan_solutions_matrix,
                                                                      targets,
                                                                      num_spp)

    #---------------------------------------------------------------------------

    plot_marxan_best_solution_scores_COR_and_APP (plot_output_dir,
                                                    marxan_solution_scores_wrt_COR_reps_and_costs,
                                                    best_solution_ID_according_to_marxan,
                                                    marxan_solution_scores_wrt_APP_reps_and_costs
                                                    )

    #---------------------------------------------------------------------------
    #  2017 11 30 - BTL
    #  Removing this code for now because I don't think that it was
    #  answering the question correctly and it's not important to bdpg.
    #  It was only here to satisfy my curiosity about something that I
    #  had read on the marxan mailing list.
    #---------------------------------------------------------------------------
    if (FALSE)
        {
            #  Verify that marxan's choice of best solution was internally
            #  consistent, i.e., that it was best with respect to the APP costs and
            #  representation values that it used in its computations.
            #
            #  I'm doing this because I've seen some things on the marxan mailing
            #  list that suggest that sometimes marxan returns a "best solution"
            #  that isn't actually its best solution when you go through and
            #  compare all of its solutions.

        see_if_marxan_best_was_actually_best (best_solution_ID_according_to_marxan,
                                              marxan_solution_scores_wrt_APP_reps_and_costs,
    #                                          parameters$full_output_dir_with_slash
                                              marxan_top_dir
                                              )
        }
    }

#===============================================================================

