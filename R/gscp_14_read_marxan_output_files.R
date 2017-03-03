#===============================================================================

                    #  gscp_14_read_marxan_output_files.R

#===============================================================================

convert_name_str_to_ID_num = function (str, lead_str="P")
    {
    return (as.numeric (stringr::str_replace (str, lead_str, "")))
    }

#-------------------------------------------------------------------------------

#' Load marxan mvbest df from file and sort by CF
#'
#' Load marxan's best solution data frame from a file and sort it by
#' conservation features (i.e., species).
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{marxan_mvbest_df}{
#' \preformatted{
#' marxan_mvbest_df : 'data.frame':	1277 obs. of  10 variables:
#'  $ ConservationFeature: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ FeatureName        : logi  NA NA NA NA NA NA ...
#'  $ Target             : num  1 1 1 1 1 1 1 1 1 1 ...
#'  $ AmountHeld         : num  1 1 1 1 1 1 1 1 1 1 ...
#'  $ OccurrenceTarget   : int  0 0 0 0 0 0 0 0 0 0 ...
#'  $ OccurrencesHeld    : int  1 1 1 1 1 1 1 1 1 1 ...
#'  $ SeparationTarget   : int  0 0 0 0 0 0 0 0 0 0 ...
#'  $ SeparationAchieved : int  0 0 0 0 0 0 0 0 0 0 ...
#'  $ TargetMet          : Factor w/ 1 level "yes": 1 1 1 1 1 1 1 1 1 1 ...
#'  $ MPM                : num  1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{marxan_output_dir_path}{
#' \preformatted{
#' marxan_output_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{marxan_output_mvbest_file_name}{
#' \preformatted{
#' marxan_output_mvbest_file_name :  chr "output_mvbest.csv"
#' }}
#'
#' @param marxan_output_dir_path character string
#'
#' @return Returns marxan mvbest data frame

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

#docaids::doc_vars_in_this_func_once ()
    return (marxan_mvbest_df)
    }

#-------------------------------------------------------------------------------

#'  Load marxan's summed solutions vector from its marxan output file.
#'
#'  For each PU ID, it shows the number of solution vectors that
#'  included that PU in the solution.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{all_correct_node_IDs}{
#' \preformatted{
#' all_correct_node_IDs :  int [1:407] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{marxan_output_dir_path}{
#' \preformatted{
#' marxan_output_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{marxan_output_ssoln_file_name}{
#' \preformatted{
#' marxan_output_ssoln_file_name :  chr "output_ssoln.csv"
#' }}
#' \subsection{marxan_ssoln_df}{
#' \preformatted{
#' marxan_ssoln_df : 'data.frame':	407 obs. of  2 variables:
#'  $ planning_unit: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ number       : int  0 4 0 4 0 4 0 4 0 4 ...
#' }}
#' \subsection{marxan_ssoln_df_unsorted}{
#' \preformatted{
#' marxan_ssoln_df_unsorted : 'data.frame':	407 obs. of  2 variables:
#'  $ planning_unit: int  407 406 405 404 403 402 401 400 399 398 ...
#'  $ number       : int  0 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{marxan_ssoln_df_unsorted_without_missing_rows}{
#' \preformatted{
#' marxan_ssoln_df_unsorted_without_missing_rows : 'data.frame':	407 obs. of  2 variables:
#'  $ planning_unit: int  407 406 405 404 403 402 401 400 399 398 ...
#'  $ number       : int  0 0 0 0 0 0 0 0 0 0 ...
#' }}
#'
#' @param marxan_output_dir_path character string
#' @param all_correct_node_IDs integer vector
#'
#' @return Returns marxan_ssoln_df data frame

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

#docaids::doc_vars_in_this_func_once ()
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
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{all_correct_node_IDs}{
#' \preformatted{
#' all_correct_node_IDs :  int [1:407] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
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
#' \subsection{largest_PU_ID}{
#' \preformatted{
#' largest_PU_ID :  num 407
#' }}
#' \subsection{largest_spp_ID}{
#' \preformatted{
#' largest_spp_ID :  int 1277
#' }}
#' \subsection{marxan_best_df_sorted}{
#' \preformatted{
#' marxan_best_df_sorted : 'data.frame':	407 obs. of  2 variables:
#'  $ PUID    : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ SOLUTION: int  0 1 0 1 0 1 0 1 0 1 ...
#' }}
#' \subsection{marxan_best_df_sorted_as_vector}{
#' \preformatted{
#' marxan_best_df_sorted_as_vector :  int [1:407] 0 1 0 1 0 1 0 1 0 1 ...
#' }}
#' \subsection{marxan_mvbest_df_sorted_by_ConservationFeature}{
#' \preformatted{
#' marxan_mvbest_df_sorted_by_ConservationFeature : 'data.frame':	1277 obs. of  10 variables:
#'  $ ConservationFeature: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ FeatureName        : logi  NA NA NA NA NA NA ...
#'  $ Target             : num  1 1 1 1 1 1 1 1 1 1 ...
#'  $ AmountHeld         : num  1 1 1 1 1 1 1 1 1 1 ...
#'  $ OccurrenceTarget   : int  0 0 0 0 0 0 0 0 0 0 ...
#'  $ OccurrencesHeld    : int  1 1 1 1 1 1 1 1 1 1 ...
#'  $ SeparationTarget   : int  0 0 0 0 0 0 0 0 0 0 ...
#'  $ SeparationAchieved : int  0 0 0 0 0 0 0 0 0 0 ...
#'  $ TargetMet          : Factor w/ 1 level "yes": 1 1 1 1 1 1 1 1 1 1 ...
#'  $ MPM                : num  1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{marxan_output_dir_path}{
#' \preformatted{
#' marxan_output_dir_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{marxan_ssoln_df_sorted_by_PU}{
#' \preformatted{
#' marxan_ssoln_df_sorted_by_PU : 'data.frame':	407 obs. of  2 variables:
#'  $ planning_unit: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ number       : int  0 4 0 4 0 4 0 4 0 4 ...
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSrun_-COR-Wrap-Marxan_SA.9"| __truncated__
#' }}
#' \subsection{retVal}{
#' \preformatted{
#' retVal : List of 3
#'  $ marxan_best_df_sorted:'data.frame':	407 obs. of  2 variables:
#'  $ marxan_ssoln_df      :'data.frame':	407 obs. of  2 variables:
#'  $ marxan_mvbest_df     :'data.frame':	1277 obs. of  10 variables:
#' }}
#' \subsection{targets}{
#' \preformatted{
#' targets :  num [1:1277] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#'
#' @param marxan_output_dir character string
#' @param all_correct_node_IDs integer vector
#' @param num_spp integer
#' @param cor_bpm matrix
#' @param plot_output_dir character string
#' @param parameters list
#' @param app_bpm matrix
#' @param cor_PU_costs numeric vector
#' @param correct_solution_cost numeric
#' @param largest_PU_ID integer
#' @param largest_spp_ID integer
#'
#' @return Returns list

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

#docaids::doc_vars_in_this_func_once ()
    return (retVal)
    }

#===============================================================================

