#===============================================================================

        #  gscp_14c_plot_incremental_marxan_summed_solution_reps.R

#===============================================================================

#-------------------------------------------------------------------------------

#' Plot incremental marxan summed solution representations
#'
#'  For each step in order by Marxan summed solution PU ID:
#'  Want the fraction of all species who have met or exceeded their target
#'  when all PUs with the same number of votes or more are included in the
#'  solution.
#'
#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

#' @param marxan_ssoln_df data frame
#' @param cor_PU_costs numeric vector
#' @param optimum_cost numeric
#' @param bpm matrix
#' @param cor_app_prefix_string character string
#' @param num_spp integer
#' @param plot_output_dir character string
#'
#' @return Returns nothing

#-------------------------------------------------------------------------------

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

#===============================================================================

#-------------------------------------------------------------------------------

#' Plot how marxan is actually doing vs. how marxan thinks it's doing
#'
#'  Evaluate apparent summed solutions as a function of the correct
#'  problem structure and the apparent problem structure, i.e.,
#'  how marxan is really doing vs. how marxan thinks it's doing.
#'
#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

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

#===============================================================================

#-------------------------------------------------------------------------------

#' Plot marxan best solution scores COR and APP
#'
#' Plot marxan best solution scores for both CORRECT and APPARENT
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{marxan_solution_scores_wrt_APP_reps_and_costs}{
#' \preformatted{
#' marxan_solution_scores_wrt_APP_reps_and_costs : 'data.frame':	4 obs. of  3 variables:
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
#' \subsection{marxan_solution_scores_wrt_COR_reps_and_costs}{
#' \preformatted{
#' marxan_solution_scores_wrt_COR_reps_and_costs : 'data.frame':	4 obs. of  3 variables:
#'  $ solution_num  : int  1 2 3 4
#'  $ representation: num  1 1 1 1
#'  $ cost          : num  0.15 0.15 0.15 0.15
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#'
#-------------------------------------------------------------------------------

#' @param plot_output_dir character string
#' @param marxan_solution_scores_wrt_COR_reps_and_costs data frame
#' @param best_solution_ID_according_to_marxan integer
#' @param marxan_solution_scores_wrt_APP_reps_and_costs data frame
#'
#' @return Returns nothing

#-------------------------------------------------------------------------------

plot_marxan_best_solution_scores_COR_and_APP <- function (plot_output_dir,
                                                            marxan_solution_scores_wrt_COR_reps_and_costs,
                                                            best_solution_ID_according_to_marxan,
                                                            marxan_solution_scores_wrt_APP_reps_and_costs
                                                            )
    {
    cor_app_prefix_string = "cor"
    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_all_solutions_frac_rep_vs_raw_cost.pdf")))
      plot (marxan_solution_scores_wrt_COR_reps_and_costs [, "cost"],
              marxan_solution_scores_wrt_COR_reps_and_costs [, "representation"],
              main="Marxan solutions\nCORRECT - Fraction of spp meeting targets vs. costs",
              xlab="Solution cost",
              ylab="Fraction of spp meeting target",
              col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
              )

    text(marxan_solution_scores_wrt_COR_reps_and_costs [, "cost"],
         marxan_solution_scores_wrt_COR_reps_and_costs [, "representation"],
         labels = marxan_solution_scores_wrt_COR_reps_and_costs [, "solution_num"],
         cex= 0.7, pos=4)

        #  Color marxan's chosen solution red.
    points (marxan_solution_scores_wrt_COR_reps_and_costs [best_solution_ID_according_to_marxan, "cost"],
            marxan_solution_scores_wrt_COR_reps_and_costs [best_solution_ID_according_to_marxan, "representation"],
            col= "red", pch = 19, cex = 1, lty = "solid", lwd = 2)
    dev.off()

    cor_app_prefix_string = "app"
    pdf (file.path (plot_output_dir, paste0 (cor_app_prefix_string, "_", "marxan_all_solutions_frac_rep_vs_raw_cost.pdf")))
    plot (marxan_solution_scores_wrt_APP_reps_and_costs [, "cost"],
          marxan_solution_scores_wrt_APP_reps_and_costs [, "representation"],
          main="Marxan solutions\nAPPARENT - Fraction of spp meeting targets vs. costs",
          xlab="Solution cost",
          ylab="Fraction of spp meeting target",
          col= "blue", pch = 19, cex = 1, lty = "solid", lwd = 2
          )

    text(marxan_solution_scores_wrt_APP_reps_and_costs [, "cost"],
         marxan_solution_scores_wrt_APP_reps_and_costs [, "representation"],
         labels = marxan_solution_scores_wrt_COR_reps_and_costs [, "solution_num"],
         cex= 0.7, pos=4)

        #  Color marxan's chosen solution red.
    points (marxan_solution_scores_wrt_APP_reps_and_costs [best_solution_ID_according_to_marxan, "cost"],
            marxan_solution_scores_wrt_APP_reps_and_costs [best_solution_ID_according_to_marxan, "representation"],
            col= "red", pch = 19, cex = 1, lty = "solid", lwd = 2)
    dev.off()

#docaids::doc_vars_in_this_func_once ()
    }

#===============================================================================

