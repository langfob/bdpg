#===============================================================================

            #  gscp_14d_see_if_marxan_best_was_actually_best.R

#  Verify that marxan's choice of best solution was internally
#  consistent, i.e., that it was best with respect to the APP costs and
#  representation values that it used in its computations.

#  I'm doing this because I've seen some things on the marxan mailing
#  list that suggest that sometimes marxan returns a "best solution"
#  that isn't actually its best solution when you go through and
#  compare all of its solutions.

#-------------------------------------------------------------------------------

#  2017 11 30 - BTL

#  This code was called in find_best_marxan_solutions(), but I'm removing it
#  for now because I don't think that it was answering the question correctly
#  and it's not important to bdpg.
#  It was only here to satisfy my curiosity about something that I
#  had read on the marxan mailing list.

#  The reason that I think it was incorrect code is that marxan only has to
#  find a solution that meets all of the representation targets, so it doesn't
#  matter whether it has the best representations or not.  As long as it
#  meets the targets, then the lowest cost alternative that does that should
#  be returned as a solution.  My code was asking whether something was
#  best representation AND best cost, but that's not required.  I don't know
#  whether marxan would rank on cost or on representation first if there
#  were multiple solutions that were at or below the lowest cost that met
#  all representation goals.

#-------------------------------------------------------------------------------

#  NOTE that I first started looking into this code today because it was
#  somehow generating not one, but two touch files for some marxan runs;
#  one said HALF_BAD and the other said OK.  I suspect that it was being
#  called by two different times in the same run (since the time stamps
#  differed by two minutes), but I never found out exactly where.
#  I'm not bothering to pursue that further because I don't think I'm going
#  to use the code.

#===============================================================================

#-------------------------------------------------------------------------------

#' Get marxan solution choice string
#'
#' Get marxan solution choice string
#'
#-------------------------------------------------------------------------------

#' @param marxan_best_cost numeric
#' @param marxan_best_rep numeric
#' @param sorted_best_cost numeric
#' @param sorted_best_rep numeric
#'
#' @return Returns character string

#-------------------------------------------------------------------------------

get_marxan_solution_choice_string = function (marxan_best_cost,
                                              marxan_best_rep,
                                              sorted_best_cost,
                                              sorted_best_rep)
    {



#  2017 11 30 - BTL
# THIS IS NOT QUITE RIGHT, SINCE YOU WANT LOWEST COST FOR _ANY_ REP THAT
# MEETS ALL TARGETS.
# NEED A FUNCTION TO TEST WHETHER A SOLUTION MEETS ALL TARGETS.
# OR IS THAT ALREADY DONE SOMEWHERE UNDER A DIFFERENT NAME?
# IF NOT, THEN SOMETHING THAT JUST EXAMINES THE ERROR MEASURE I HAVE FOR
# MEASURING REPRESENTATION SHORTFALL AND IF IT IS ZERO, THEN MEETS() IS TRUE.
# MIGHT WANT TO USE THE EXISTING FUNCTION compute_frac_spp_covered() in
# biodivrprobgen_utilities.R:
#
# compute_frac_spp_covered =
#         function (spp_occ_matrix,               -->       bpm?
#                   candidate_solution_PU_IDs,    -->       marxan_best_solution_vector?
#                   num_spp,                      -->       cor_num_spp?
#                   spp_rep_targets               -->       cor_spp_targets?
#                   )
#
# all_legal_solutions = select all solutions where (compute_frac_spp_covered (best_solution) >= 1)
# if ((marxan_best_solution in all_legal_solutions) &
#     (cost (marxan_best_solution) == min (costs of all solutions in all_legal_solutions)))
#  then marxan best solution is consistent with its set of solutions
# ...




    solution_choice_string = "OK__marxan_solution_IS_apparent_best_cost_and_best_rep"
    if (marxan_best_cost > sorted_best_cost)
        {
            #  marxan's chosen best is NOT the best cost
        if (marxan_best_rep < sorted_best_rep)
            {
                #  marxan's chosen best is also NOT the best representation
            solution_choice_string = "BAD__marxan_solution_NEITHER_best_cost_nor_best_rep"
            } else
            {
                #  marxan's chosen best is not best score but is best representation
            solution_choice_string = "HALF_BAD__marxan_solution_NOT_apparent_best_cost_but_IS_apparent_best_rep"
            }
        } else if (marxan_best_rep < sorted_best_rep)
        {
            #  marxan's chosen best is best score but is NOT best representation
        solution_choice_string = "HALF_BAD__marxan_solution_IS_apparent_best_cost_but_NOT_apparent_best_rep"
        }

    return (solution_choice_string)
    }

#===============================================================================

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
#' whose names begin with "BAD__" or "HALF_BAD__.  This should never happen,
#' but this function is run to make sure.
#'
#' The empty file that is written has one of the following names:
#'
#' - OK__marxan_solution_IS_apparent_best_cost_and_best_rep:  If marxan's chosen best cost solution
#' really is its best cost solution
#'
#' - BAD__marxan_solution_NEITHER_best_cost_nor_best_rep:  If marxan's chosen best is also neither
#' the best cost nor the best representation
#'
#' - HALF_BAD__marxan_solution_NOT_apparent_best_cost_but_IS_apparent_best_rep:
#' If marxan's chosen best is not best cost but is best representation
#'
#' - HALF_BAD__marxan_solution_IS_apparent_best_cost_but_NOT_apparent_best_rep:
#' If marxan's chosen best is best cost but is NOT best representation
#'
#-------------------------------------------------------------------------------

#' @param best_solution_ID_according_to_marxan integer
#' @param marxan_solution_scores_wrt_APP_reps_and_costs data frame
#' @param marxan_output_dir character string
#'
#' @return Returns nothing

#-------------------------------------------------------------------------------

see_if_marxan_best_was_actually_best <-
                            function (best_solution_ID_according_to_marxan,
                                      marxan_solution_scores_wrt_APP_reps_and_costs,
#                                      out_dir
                                      marxan_output_dir
                                      )
    {
    marxan_best_cost = marxan_solution_scores_wrt_APP_reps_and_costs [best_solution_ID_according_to_marxan, "cost"]
    marxan_best_rep  = marxan_solution_scores_wrt_APP_reps_and_costs [best_solution_ID_according_to_marxan, "representation"]
    sorted_best_cost = marxan_solution_scores_wrt_APP_reps_and_costs [1, "cost"]
    sorted_best_rep  = marxan_solution_scores_wrt_APP_reps_and_costs [1, "representation"]

    marxan_solution_choice_check_string =
        get_marxan_solution_choice_string (marxan_best_cost, marxan_best_rep,
                                           sorted_best_cost, sorted_best_rep)
    cat ("\n\n=====>  ", marxan_solution_choice_check_string, "     <=====\n", sep='')

      #  Write an empty file whose name indicates whether marxan really did
      #  return its best guess.
      #  This will make it easy to quickly search a bunch of runs to see
      #  whether any of them had a bad marxan choice for best run by
      #  just looking for the existance of any files whose names begin
      #  with "BAD__" or "HALF_BAD__".

    flag_file_name = file.path (marxan_output_dir, marxan_solution_choice_check_string)

    #    system (paste ("touch", flag_file_name), wait=FALSE)
    touch (flag_file_name)
    }

