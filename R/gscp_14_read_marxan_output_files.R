#===============================================================================

                    #  gscp_14_read_marxan_output_files.R

#===============================================================================

convert_name_str_to_ID_num = function (str, lead_str="P")
    {
    return (as.numeric (stringr::str_replace (str, lead_str, "")))
    }

#===============================================================================

#  Hack to quiet CHECK command for data frame column names that CHECK flags
#  as "no visible binding for global variable".
#  This is the officially sanctioned hack for doing this, e.g., see
#  https://github.com/STAT545-UBC/Discussion/issues/451
#  https://github.com/tidyverse/magrittr/issues/29
#  http://r.789695.n4.nabble.com/globalVariables-td4593980.html

if(getRversion() >= "2.15.1")  utils::globalVariables(c("ConservationFeature"))

#===============================================================================

#' Load marxan mvbest df from file and sort by CF
#'
#' Load marxan's best solution data frame from a file and sort it by
#' conservation features (i.e., species).
#'
#-------------------------------------------------------------------------------

#' @param marxan_output_dir_path character string
#'
#' @return Returns marxan mvbest data frame

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

#===============================================================================

#  Hack to quiet CHECK command for data frame column names that CHECK flags
#  as "no visible binding for global variable".
#  This is the officially sanctioned hack for doing this, e.g., see
#  https://github.com/STAT545-UBC/Discussion/issues/451
#  https://github.com/tidyverse/magrittr/issues/29
#  http://r.789695.n4.nabble.com/globalVariables-td4593980.html

if(getRversion() >= "2.15.1")  utils::globalVariables(c("planning_unit"))

#===============================================================================

#'  Load marxan's summed solutions vector from its marxan output file.
#'
#'  For each PU ID, it shows the number of solution vectors that
#'  included that PU in the solution.
#'
#-------------------------------------------------------------------------------

#' @param marxan_output_dir_path character string
#' @param all_correct_node_IDs integer vector
#'
#' @return Returns marxan_ssoln_df data frame

#-------------------------------------------------------------------------------

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

#===============================================================================

#  Hack to quiet CHECK command for data frame column names that CHECK flags
#  as "no visible binding for global variable".
#  This is the officially sanctioned hack for doing this, e.g., see
#  https://github.com/STAT545-UBC/Discussion/issues/451
#  https://github.com/tidyverse/magrittr/issues/29
#  http://r.789695.n4.nabble.com/globalVariables-td4593980.html

if(getRversion() >= "2.15.1")  utils::globalVariables(c("PUID"))

#===============================================================================

#' Load marxan best data frame from file, sort, add missing PUs
#'
#' Load marxan best data from file and sort in increasing order by planning
#' unit ID.  Also add any missing planning units.
#'
#-------------------------------------------------------------------------------

#' @param marxan_output_dir_path character string
#' @param all_correct_node_IDs integer vector
#'
#' @return Returns marxan_best_df_sorted data frame

#-------------------------------------------------------------------------------

load_marxan_best_df_from_file_and_sort_and_add_missing_PUs <-
                                            function (marxan_output_dir_path,
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

#===============================================================================

#-------------------------------------------------------------------------------

#' Load marxan solutions matrix from file, sort, add missing PUs
#'
#' Load marxan solutions matrix from file, sort, add missing PUs
#'
#-------------------------------------------------------------------------------

#' @param marxan_output_dir_path character string
#' @param largest_PU_ID integer
#'
#' @return Returns marxan_solutions_matrix_and_num_solutions

#-------------------------------------------------------------------------------

#load_marxan_solutionsmatrix_from_file_and_sort_and_add_missing_PUs <-
load_marxan_solutionsmatrix_and_add_missing_PUs <-
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

    marxan_solutions_matrix_and_num_solutions <-
        list (marxan_solutions_matrix = marxan_solutions_matrix,
              num_marxan_solutions    = num_marxan_solutions)

    return (marxan_solutions_matrix_and_num_solutions)
    }

#===============================================================================

find_best_marxan_solutions_and_plot_incremental_summed_solution_reps <-
        function (rsrun,
                  exp_root_dir,
                  COR_bd_prob,
                  APP_bd_prob,
                  marxan_output_values)
    {
if (FALSE) {    #  I don't think this is needed anymore.  2018 03 04 - BTL
      find_best_marxan_solutions_and_plot_incremental_summed_solution_reps_for_COR_and_APP (
              get_RSrun_path_output (rsrun, exp_root_dir),
              COR_bd_prob@num_spp,
COR_bd_prob@PU_costs,
COR_bd_prob@PU_costs,
              COR_bd_prob@bpm,
          COR_bd_prob@bpm,
              marxan_output_values$marxan_best_df_sorted,
              get_RSrun_path_plots (rsrun, exp_root_dir),
              COR_bd_prob@num_PUs,
              COR_bd_prob@num_spp,
              rsrun@targets,
              get_RSrun_path_output (rsrun, exp_root_dir),
              marxan_output_values$marxan_ssoln_df,
              COR_bd_prob@correct_solution_cost)
}

marxan_best_summed_solution_PU_IDs =
      find_best_marxan_solutions_and_plot_incremental_summed_solution_reps_for_COR_and_APP (
              get_RSrun_path_output (rsrun, exp_root_dir),
              COR_bd_prob@num_spp,
COR_bd_prob@PU_costs,        #COR_bd_prob@PU_costs,
APP_bd_prob@PU_costs,        #COR_bd_prob@PU_costs,
              COR_bd_prob@bpm,
          APP_bd_prob@bpm,
              marxan_output_values$marxan_best_df_sorted,
              get_RSrun_path_plots (rsrun, exp_root_dir),
              COR_bd_prob@num_PUs,
              COR_bd_prob@num_spp,
              rsrun@targets,
              get_RSrun_path_output (rsrun, exp_root_dir),
              marxan_output_values$marxan_ssoln_df,
              COR_bd_prob@correct_solution_cost)
return (marxan_best_summed_solution_PU_IDs)

    }

#-------------------------------------------------------------------------------

    #  Moved this code out into a separate function because it doesn't
    #  return anything.  It's only used for some verification and
    #  plotting side effects.
    #  BTL - 2017 06 01

#-------------------------------------------------------------------------------

find_best_marxan_solutions_and_plot_incremental_summed_solution_reps_for_COR_and_APP <-
        function (marxan_output_dir_path,
                  num_spp,

#PU_costs,            #cor_PU_costs,
cor_PU_costs,
app_PU_costs,

                  cor_bpm,
                  app_bpm,
            marxan_best_df_sorted,
                  plot_output_dir,
                  largest_PU_ID,
                  largest_spp_ID,
                  targets,
                  marxan_top_dir,
            marxan_ssoln_df_sorted_by_PU,
                  correct_solution_cost
                  #   ,
                  # app_optimum_cost
                )
    {
    marxan_best_df_sorted_as_vector = as.vector (t(marxan_best_df_sorted [,"SOLUTION"]))
    find_best_marxan_solutions (marxan_output_dir_path,
  #                                num_PUs,     #  should this be largest_PU_ID?
                                  num_spp,     #  should this be largest_spp_ID?

#cor_bpm@PU_costs,            #cor_PU_costs,
cor_PU_costs,
app_PU_costs,

                                  cor_bpm,
                                  app_bpm,
                                  marxan_best_df_sorted_as_vector,
                                  plot_output_dir,

                                  largest_PU_ID,
                                  largest_spp_ID,

                                  targets,

                                  marxan_top_dir
                                  )


          # Plot how marxan is actually doing vs. how it thinks it's doing
# #app_optimum_cost = sum (marxan_best_df_sorted$SOLUTION)    #  This only works if all costs == 1.
# app_optimum_cost = sum (app_bpm@PU_costs [which (marxan_best_df_sorted$SOLUTION > 0)])
app_optimum_cost = sum (app_PU_costs [which (marxan_best_df_sorted$SOLUTION > 0)])

marxan_best_summed_solution_PU_IDs =
    plot_incremental_marxan_summed_solution_reps_for_COR_and_APP (marxan_ssoln_df_sorted_by_PU,

#PU_costs,            #cor_PU_costs,
cor_PU_costs,
app_PU_costs,

                                                                      correct_solution_cost,
                                                                      app_optimum_cost,
                                                                      cor_bpm,
                                                                      app_bpm,
                                                                      num_spp,
                                                                      plot_output_dir
                                                                      )

return (marxan_best_summed_solution_PU_IDs)
    }

#===============================================================================

#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

#' @param marxan_output_dir_path character string output directory for the
#'     marxan run
#' @param all_correct_node_IDs integer vector
#'
#' @return Returns list

#-------------------------------------------------------------------------------

read_marxan_output_files <- function (marxan_output_dir_path,
                                      all_correct_node_IDs)
    {
    marxan_best_df_sorted <-
        load_marxan_best_df_from_file_and_sort_and_add_missing_PUs (marxan_output_dir_path,
                                                                    all_correct_node_IDs)

    marxan_best_df_sorted_as_vector = as.vector (t(marxan_best_df_sorted [,"SOLUTION"]))
    app_optimum_cost = sum (marxan_best_df_sorted$SOLUTION)

  #---------------------------------

    marxan_mvbest_df_sorted_by_ConservationFeature <-
        load_marxan_mvbest_df_from_file_and_sort_by_CF (marxan_output_dir_path)

    marxan_ssoln_df_sorted_by_PU  <-
        load_marxan_ssoln_df_from_file_and_sort_by_PU (marxan_output_dir_path,
                                                       all_correct_node_IDs)

  #---------------------------------

    retVal = list ()
    retVal$marxan_best_df_sorted = marxan_best_df_sorted
    retVal$marxan_ssoln_df       = marxan_ssoln_df_sorted_by_PU
    retVal$marxan_mvbest_df      = marxan_mvbest_df_sorted_by_ConservationFeature

    return (retVal)
    }

#===============================================================================

