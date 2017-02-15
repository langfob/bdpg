#===============================================================================

                    #  gscp_14_read_marxan_output_files.R

#===============================================================================

convert_name_str_to_ID_num = function (str, lead_str="P")
    {
    return (as.numeric (stringr::str_replace (str, lead_str, "")))
    }

#-------------------------------------------------------------------------------

dist_between_marxan_solutions = function (solution_1, solution_2)
    {
#browser()
    return (sum (abs (solution_1 - solution_2)))
    }

#-------------------------------------------------------------------------------

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
    cur_frac_of_all_spp_meeting_their_target = sum (cur_rep_fractions >= 1.0) / num_spp    #  How best to give a tolerance here?
    marxan_solution_scores [cur_solution_num, "representation"] = cur_frac_of_all_spp_meeting_their_target

    cur_solution_PUs = which (marxan_solutions_matrix [cur_solution_num,] > 0)
    cur_cost = compute_solution_cost (cur_solution_PUs, cor_PU_costs)
    marxan_solution_scores [cur_solution_num, "cost"] = cur_cost / total_landscape_cost

    return (marxan_solution_scores)
    }


#-------------------------------------------------------------------------------

get_marxan_solution_choice_string = function (marxan_best_cost,
                                              marxan_best_rep,
                                              sorted_best_cost,
                                              sorted_best_rep)
    {
    solution_choice_string = "OK_marxan_solution_IS_apparent_best"
    if (marxan_best_cost > sorted_best_cost)
        {
            #  marxan's chosen best is NOT the best cost
        if (marxan_best_rep < sorted_best_rep)
            {
                #  marxan's chosen best is also NOT the best representation
            solution_choice_string = "BAD_marxan_solution_NEITHER_best"
            } else
            {
                #  marxan's chosen best is not best score but is best representation
            solution_choice_string = "BAD_HALF_marxan_solution_NOT_apparent_best_cost_and_IS_apparent_best_rep"
            }
        } else if (marxan_best_rep < sorted_best_rep)
        {
            #  marxan's chosen best is best score but is NOT best representation
        solution_choice_string = "BAD_HALF_marxan_solution_IS_apparent_best_cost_and_NOT_apparent_best_rep"
        }

    return (solution_choice_string)
    }

#-------------------------------------------------------------------------------

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
    }

#-------------------------------------------------------------------------------

    #  For each step in order by Marxan summed solution PU ID:
    #  Want the fraction of all species who have met or exceeded their target
    #  when all PUs with the same number of votes or more are included in the
    #  solution.

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
    }

#-------------------------------------------------------------------------------

#' Plot how marxan is actually doing vs. how marxan things it's doing
#'
#'  Evaluate apparent summed solutions as a function of the correct
#'  problem structure and the apparent problem structure, i.e.,
#'  how marxan is really doing vs. how marxan thinks it's doing.
#'
#' @param marxan_ssoln_df
#' @param cor_PU_costs
#' @param correct_solution_cost
#' @param cor_bpm
#' @param app_optimum_cost
#' @param app_bpm
#' @param num_spp
#' @param plot_output_dir
#'
#' @return

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
    }

#-------------------------------------------------------------------------------

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
}

#-------------------------------------------------------------------------------

load_marxan_mvbest_df_from_file_and_sort_by_CF <- function (marxan_output_dir_path)
    {
        #-----------------------------------------------------------------------
        #  Read the file containing marxan's mvbest.
        #  Can't remember what it is, but don't do anything other than read it
        #  in here and return its values.
        #-----------------------------------------------------------------------

    marxan_output_mvbest_file_name   = "output_mvbest.csv"

    marxan_mvbest_df =
        read.csv (file.path (marxan_output_dir_path,
                             marxan_output_mvbest_file_name),
                  header=TRUE)

        #  The call to "arrange()" below gives a weird error when run on the
        #  data frame because the column names have spaces in them (e.g.,
        #  "Conservation Feature").  Renaming them seems to fix the problem

    names (marxan_mvbest_df) =  c ("ConservationFeature",
                                   "FeatureName",
                                   "Target",
                                   "AmountHeld",
                                   "OccurrenceTarget",
                                   "OccurrencesHeld",
                                   "SeparationTarget",
                                   "SeparationAchieved",
                                   "TargetMet",
                                   "MPM"
                                  )

    marxan_mvbest_df = plyr::arrange (marxan_mvbest_df, ConservationFeature)

    return (marxan_mvbest_df)
    }

#-------------------------------------------------------------------------------

#'  Load marxan's summed solutions vector from its marxan output file.
#'
#'  For each PU ID, it shows the number of solution vectors that
#'  included that PU in the solution.
#'
#' @param marxan_output_dir_path
#' @param all_correct_node_IDs

load_marxan_ssoln_df_from_file_and_sort_by_PU <- function (marxan_output_dir_path,
                                                           all_correct_node_IDs)
    {
    marxan_output_ssoln_file_name    = "output_ssoln.csv"

    marxan_ssoln_df_unsorted_without_missing_rows =
        read.csv (file.path (marxan_output_dir_path, marxan_output_ssoln_file_name),
                    header=TRUE)

    marxan_ssoln_df_unsorted =
        add_missing_PU_rows_to_PU_Count_dataframe (marxan_ssoln_df_unsorted_without_missing_rows,
                                                    all_correct_node_IDs,
                                                    "planning_unit", "number")

        #  Sort by planning unit.
    marxan_ssoln_df = plyr::arrange (marxan_ssoln_df_unsorted, planning_unit)

    return (marxan_ssoln_df)
    }

#-------------------------------------------------------------------------------

load_marxan_best_df_from_file_and_sort_and_add_missing_PUs <- function (marxan_output_dir_path,
                                                                        all_correct_node_IDs)
    {
    marxan_output_best_file_name     = "output_best.csv"

        #-----------------------------------------------------------------------
        #  Read the file containing marxan's best solution vector.
        #
        #  Make sure marxan_best data frames are both sorted in
        #  increasing order of planning unit ID.
        #  Use "arrange()" syntax taken from Wickham comment in:
        #  http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r
        #
        #  2015 05 15 - BTL
        #  The marxan mailing list digest has had some entries in the last few
        #  days saying that the *nix version of marxan does not always return
        #  its best vector in the vector it has labelled as the best.
        #  So, I need to change this to go through all of the vectors marxan
        #  generates and figure out which one is the best myself.
        #-----------------------------------------------------------------------

    marxan_best_df_unsorted_without_missing_rows =
        read.csv (file.path (marxan_output_dir_path, marxan_output_best_file_name),
                  header=TRUE)

    marxan_best_df_unsorted =
        add_missing_PU_rows_to_PU_Count_dataframe (marxan_best_df_unsorted_without_missing_rows,
                                                    all_correct_node_IDs,
                                                    "PUID", "SOLUTION")

    marxan_best_df_sorted = plyr::arrange (marxan_best_df_unsorted, PUID)

    return (marxan_best_df_sorted)
    }

#-------------------------------------------------------------------------------

load_marxan_solutionsmatrix_from_file_and_sort_and_add_missing_PUs <-
    function (marxan_output_dir_path,
              largest_PU_ID)    #  should pass in largest_PU_ID instead?
    {
      #  Load output_solutionsmatrix.csv

    output_solutionsmatrix_file_name = "output_solutionsmatrix.csv"

    marxan_output_solutionsmatrix_df_unsorted_without_missing_rows =
        read.csv (file.path (marxan_output_dir_path,
                             output_solutionsmatrix_file_name),
                    as.is=TRUE, header=TRUE)

#-------------------------------------------------------------------------------
#  The marxan output solutions matrix has a column for each
#  PU that occurs in some solution, but that leaves out some
#  PUs since they don't occur in any solution.
#  Also, each column header is a string like "P105" and they
#  are not in sorted order.
#  Convert this data into a matrix that has a column for each
#  PU in the problem and the order corresponds to the PU ID
#  so that values for each PU can be addressed directly by
#  ID number.
#-------------------------------------------------------------------------------

        #  Get the planning unit names as they appear as solution column headings.
    PU_names = names (marxan_output_solutionsmatrix_df_unsorted_without_missing_rows)
    PU_names = PU_names [-1]  #  Get rid of first column's label, i.e., "solutionsmatrix"

    num_marxan_solutions = dim (marxan_output_solutionsmatrix_df_unsorted_without_missing_rows)[1]

    num_PU_IDs_in_solutionsmatrix = length (PU_names)
    PU_IDs_in_solutions = rep (NA, num_PU_IDs_in_solutionsmatrix)
    cur_idx = 0
    for (cur_PU_name in PU_names)
        {
        cur_idx = cur_idx + 1

        cur_PU_ID = convert_name_str_to_ID_num (cur_PU_name, "P")

        PU_IDs_in_solutions [cur_idx] =  cur_PU_ID
        }

###    largest_PU_ID = num_PUs    #max (PU_IDs_in_solutions)    #num_PUs
        cat ("\nlargest_PU_ID in marxan solutions matrix = ", largest_PU_ID)
    marxan_solutions_matrix = matrix (0, nrow = num_marxan_solutions,
                                    ncol = largest_PU_ID,
                                    byrow=TRUE)
    for (cur_PU_name in PU_names)
        {
        cur_PU_ID = convert_name_str_to_ID_num (cur_PU_name, "P")

        marxan_solutions_matrix [,cur_PU_ID] =
            marxan_output_solutionsmatrix_df_unsorted_without_missing_rows [,cur_PU_name]
        }

    return (list (marxan_solutions_matrix = marxan_solutions_matrix,
                  num_marxan_solutions    = num_marxan_solutions))
    }

#-------------------------------------------------------------------------------

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

    distances_between_marxan_solutions = matrix (0, nrow=num_marxan_solutions,
                                               ncol=num_marxan_solutions, byrow=TRUE)

    IDs_of_vectors_matching_marxan_best_solution_choice = c()
    for (cur_row in 1:num_marxan_solutions)
        {
        cat ("\n\ncur_row = ", cur_row, ", just before first dist_between_marxan_solutions()")

        cur_dist_from_marxan_best_df_sorted_as_vector =
        #        distances_between_marxan_solutions [cur_row, cur_col] =
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

        #  Marxan returns a best solution, but I have not been able to find
        #  anyplace where it tells you what solution number it was.
        #  Since you can have multiple identical solutions, there may be
        #  multiple solution IDs that could be identified as the best solution.
        #  I need any one of them to use to get the corresponding solution vector.
        #  I will arbitrarily choose the first one in the list of vectors that
        #  selected the same PUs as the vector that marxan returned.
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
    }

#===============================================================================
#===============================================================================

read_COR_marxan_output_files <- function (rsrun, COR_bd_prob, parameters)
    {
    top_dir = parameters$fullOutputDir_NO_slash
    marxan_output_dir = get_RSrun_path_output (rsrun, top_dir)

    return (read_marxan_output_files (
                                        marxan_output_dir,
                                        COR_bd_prob@all_PU_IDs,                                                #all_correct_node_IDs
                                #COR_bd_prob@num_PUs,                                                   #num_PUs
                                        COR_bd_prob@num_spp,                                                   #num_spp
                                        COR_bd_prob@bpm,                                                       #cor_bpm
                                        get_RSrun_path_plots (rsrun, top_dir),

                                        parameters,
                                        COR_bd_prob@bpm,                                                       #app_bpm
                                        COR_bd_prob@PU_costs,                                                  #cor_PU_costs
                                        COR_bd_prob@correct_solution_cost,                                       #correct_solution_cost

                                        largest_PU_ID = COR_bd_prob@num_PUs,
                                        largest_spp_ID = COR_bd_prob@num_spp,

                                        rsrun@targets
                                      )
            )
    }

#===============================================================================

read_APP_marxan_output_files <- function (rsrun,
                                          APP_bd_prob,
                                          COR_bd_prob,
                                          parameters
                                          )
    {
    top_dir = parameters$fullOutputDir_NO_slash
    marxan_output_dir = get_RSrun_path_output (rsrun, top_dir)

    return (read_marxan_output_files (
                                        marxan_output_dir,
                                        COR_bd_prob@all_PU_IDs,                                                #all_correct_node_IDs
                                #COR_bd_prob@num_PUs,                                                   #num_PUs
                                        COR_bd_prob@num_spp,                                                   #num_spp
                                        COR_bd_prob@bpm,                                                       #cor_bpm
                                        get_RSrun_path_plots (rsrun, top_dir),

                                        parameters,
                                        APP_bd_prob@bpm,                                                       #app_bpm
                                        COR_bd_prob@PU_costs,                                                  #cor_PU_costs
                                        COR_bd_prob@correct_solution_cost,                                      #correct_solution_cost

                                        largest_PU_ID = COR_bd_prob@num_PUs,
                                        largest_spp_ID = COR_bd_prob@num_spp,

                                        rsrun@targets
                                      )
            )
    }

#===============================================================================

#'  Read various marxan outputs into this program.
#'
#'  Should include these in the marxan package.
#'  Marxan's best solution.
#'  Marxan's votes for including each planning unit.
#'  Marxan output files to read from the Marxan output directory
#'  (need to look these file names up in the manual):
#'  output_best.csv
#'  output_mvbest.csv
#'  output_ssoln.csv
#'  output_solutionsmatrix.csv
#'  output_sum.csv
#'  output_penalty_planning_units.csv
#'  output_penalty.csv
#'  output_sen.dat
#'
#' @param marxan_output_dir
#' @param all_correct_node_IDs
#' @param num_spp
#' @param cor_bpm
#' @param plot_output_dir
#' @param parameters
#' @param app_bpm
#' @param cor_PU_costs
#' @param correct_solution_cost
#' @param largest_PU_ID
#' @param largest_spp_ID

read_marxan_output_files <- function (marxan_output_dir_path,
                                        all_correct_node_IDs,
                                #num_PUs,
                                        num_spp,
                                        cor_bpm,
                                        plot_output_dir,
                                        parameters,
                                        app_bpm,

                                        cor_PU_costs,
                                        correct_solution_cost,

                                        largest_PU_ID,
                                        largest_spp_ID,

                                        targets
                                      )
    {
    marxan_best_df_sorted <-
        load_marxan_best_df_from_file_and_sort_and_add_missing_PUs (marxan_output_dir_path,
                                                                    all_correct_node_IDs)

    marxan_best_df_sorted_as_vector = as.vector (t(marxan_best_df_sorted [,"SOLUTION"]))
    app_optimum_cost = sum (marxan_best_df_sorted$SOLUTION)

    find_best_marxan_solutions (marxan_output_dir_path,
#                                num_PUs,     #  should this be largest_PU_ID?
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

    marxan_mvbest_df_sorted_by_ConservationFeature <-
        load_marxan_mvbest_df_from_file_and_sort_by_CF (marxan_output_dir_path)

    marxan_ssoln_df_sorted_by_PU  <-
        load_marxan_ssoln_df_from_file_and_sort_by_PU (marxan_output_dir_path,
                                                       all_correct_node_IDs)

        # Plot how marxan is actually doing vs. how it thinks it's doing
    plot_incremental_marxan_summed_solution_reps_for_COR_and_APP (marxan_ssoln_df_sorted_by_PU,
                                                                    cor_PU_costs,
                                                                    correct_solution_cost,
                                                                    app_optimum_cost,
                                                                    cor_bpm,
                                                                    app_bpm,
                                                                    num_spp,
                                                                    plot_output_dir
                                                                    )

  #---------------------------------

    retVal = list ()
    retVal$marxan_best_df_sorted = marxan_best_df_sorted
    retVal$marxan_ssoln_df       = marxan_ssoln_df_sorted_by_PU
    retVal$marxan_mvbest_df      = marxan_mvbest_df_sorted_by_ConservationFeature

    return (retVal)
    }

#===============================================================================

