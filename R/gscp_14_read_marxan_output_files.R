#===============================================================================

                    #  gscp_14_read_marxan_output_files.R

#===============================================================================

convert_name_str_to_ID_num = function (str, lead_str="P")
    {
    return (as.numeric (stringr::str_replace (str, lead_str, "")))
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

docaids::doc_vars_in_this_func_once ()
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

docaids::doc_vars_in_this_func_once ()
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

docaids::doc_vars_in_this_func_once ()
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

    marxan_solutions_matrix_and_num_solutions <-
        list (marxan_solutions_matrix = marxan_solutions_matrix,
              num_marxan_solutions    = num_marxan_solutions)

docaids::doc_vars_in_this_func_once ()

    return (marxan_solutions_matrix_and_num_solutions)
    }

#===============================================================================

read_COR_marxan_output_files <- function (rsrun, COR_bd_prob, parameters)
    {
    top_dir = parameters$fullOutputDir_NO_slash
    marxan_output_dir = get_RSrun_path_output (rsrun, top_dir)

    marxan_output_values <- read_marxan_output_files (
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

docaids::doc_vars_in_this_func_once ()
    return (marxan_output_values)
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

    marxan_output_values <-read_marxan_output_files (
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

docaids::doc_vars_in_this_func_once ()
    return (marxan_output_values)
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

docaids::doc_vars_in_this_func_once ()
    return (retVal)
    }

#===============================================================================

