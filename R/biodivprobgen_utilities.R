#===============================================================================

                        #  biodivprobgen_utilities.R

#===============================================================================

#  2015 04 27 - BTL
#       Created by extracting functions from generateSetCoverProblem.R.

#===============================================================================

#-------------------------------------------------------------------------------

#' Safe version of R sample function
#'
#' 2015 04 08 - BTL
#'  I just got bitten very badly by the incredibly annoying behavior of R's
#'  sample() function, so here is a replacement function that I need to
#'  use everywhere now.
#'  When I called sample with a vector that sometimes had length n=1,
#'  it sampled from 1:n instead of returning the single value.
#'  This majorly screwed all kinds of things in a very subtle, very hard
#'  to find way.
#'
#-------------------------------------------------------------------------------

#' @param x Either a vector of one or more elements from which to choose, or a positive integer.
#'
#' @return Returns a value that is probably an integer but is definitely not null
#' @export

#-------------------------------------------------------------------------------

safe_sample = function (x,...) { if (length (x) == 1) x else sample (x,...) }

#===============================================================================

#' Return NA for any given value whose length is 0
#'
#' This is originally built to be used on each element of a list inside an
#' lapply where you're trying to get rid of NULLs, numeric(0)s, etc. in a
#' list to be converted to a data frame.
#'
#' @param a_list a list
#'
#' @return NA or the input argument
#' @export

fix_0_length_list_elements <- function (value)
    {
        #  length == 0 should handle NULL, logical(0), numeric(0), etc.
        #  These are all things that as.data.frame (list) chokes on.
    if (length (value) == 0)
        return (NA) else return (value)
    }

#-------------------------------------------------------------------------------

#' Convert list to a new list with 0 length values replaced by NA
#'
#' @param a_list a list
#'
#' @return the input list with its 0 length values replaced by NA values and
#' non-0 length values left as they were
#' @export

list_with_0_length_vals_replaced_by_NA <- function (a_list)
    {
    if (! is.list (a_list))
        stop ("list_with_0_length_vals_replaced_by_NA: argument is not a list.")

    return (lapply (a_list, fix_0_length_list_elements))
    }

#-------------------------------------------------------------------------------

#' Convert list to data frame with 0 length values replaced by NA
#'
#' @param a_list a list
#'
#' @return a data frame whose columns correspond to elements of the list and
#' any 0 length data value in the input list has been replaced with NA
#' @export

list_as_data_frame_with_0_length_vals_replaced_by_NA <- function (a_list)
    {
    return (as.data.frame (list_with_0_length_vals_replaced_by_NA (a_list)))
    }

#===============================================================================

#' Get parameter value that should be integer and return 1 if value is null
#'
#' @param value a value that is probably a number, but could be anything
#'
#' @return Returns a value that is probably an integer but is definitely not null
#' @export

#-------------------------------------------------------------------------------

value_or_1_if_null <- function (value)
    {
    if (is.null (value)) 1 else value
    }

#-------------------------------------------------------------------------------

#' Get parameter value that should be boolean and return FALSE if value is null
#'
#' @param value a value that is probably a boolean, but could be anything
#'
#' @return Returns a value that is probably a boolean but is definitely not null
#' @export

#-------------------------------------------------------------------------------

value_or_FALSE_if_null <- function (value)
    {
    if (is.null (value)) FALSE else value
    }

#===============================================================================

#' Write results to files
#'
#' This function is used two different ways.
#' It's called when the program quits because there are too many species
#' or it's called when the program runs successfully.
#'
#-------------------------------------------------------------------------------

#' @param csv_outfile_name  Name of the csv file to write results to, not
#'        including csv extension but not including path
#' @param results_df  data frame containing the results
#' @param out_dir  character string telling what directory to put the results
#'                 file in
#' @param cur_result_row integer row number in data frame where run_ID should
#'                       be added
#'
#' @inheritParams std_param_defns
#'
#' @return Returns nothing.

#-------------------------------------------------------------------------------

write_results_to_files = function (csv_outfile_name,
                                   results_df,
                                   parameters,
                                   out_dir,
                                   tzar_run_id_field_name
                                   # ,
                                   # cur_result_row=1    #  Added 2016 03 28 - BTL.
                                  )
    {
        #  Write the results out to 2 separate and nearly identical files.
        #  The only difference between the two files is that the run ID in
        #  one of them is always set to 0 and in the other, it's the correct
        #  current run ID.  This is done to make it easier to automatically
        #  compare the output csv files of different runs when the only thing
        #  that should be different between the two runs is the run ID.
        #  Having different run IDs causes diff or any similar comparison to
        #  think that the run outputs don't match.  If they both have 0 run ID,
        #  then diff's output will correctly flag whether there are differences
        #  in the outputs.
        #
        #  File names currently in project.yaml:
        #      summary_filename: $$output_path$$prob_diff_results.csv
        #      summary_without_run_id_filename: $$output_path$$prob_diff_results_with_0_run_id.csv

            #  Build file name and path for auxiliary file where run_id is
            #  zeroed out.
    csv_outfile_name_base = tools::file_path_sans_ext (csv_outfile_name)
    csv_outfile_name_ext  = tools::file_ext (csv_outfile_name)    #  Probably "csv" but just being cautious here...
    csv_outfile_name_with_0_run_id = paste0 (csv_outfile_name_base,
                                             "_with_0_run_id.",
                                             csv_outfile_name_ext)
    summary_WITHOUT_run_id_path = file.path (out_dir, csv_outfile_name_with_0_run_id)
                                             ##parameters$summary_without_run_id_filename)

    results_df[[tzar_run_id_field_name]] = 0
    write.csv (results_df, file = summary_WITHOUT_run_id_path, row.names = FALSE)

    summary_WITH_run_id_path = file.path (out_dir, csv_outfile_name)
                                          ##parameters$summary_filename)
    results_df[[tzar_run_id_field_name]] = parameters$run_id
    write.csv (results_df, file = summary_WITH_run_id_path, row.names = FALSE)
    }

#===============================================================================

get_PU_costs = function (num_PUs) { return (rep (1, num_PUs)) }

#===============================================================================

#' See if there are any duplicate links
#'
#' I'm not certain, but I think that the original Xu algorithm doesn't allow
#' duplicate links.  I don't think that they would invalidate the solution,
#' but I suspect that they could make the problem easier by giving more
#' reasons to choose a pair of planning units.  Consequently, I'm looking for
#' duplicate links.
#'
#'  Note that you can only uniquely decode an edge list from an occurrence
#'  matrix if the species only occurs on 2 patches, i.e., the underlying
#'  assumption in the Xu problem generator.
#'  However, the only reason I'm building this routine now is to run it
#'  on the Xu benchmark problems to see if those allow any duplicate
#'  edges, i.e., more than one species occurring on the same pair of
#'  patches.
#'
#-------------------------------------------------------------------------------

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
#' \subsection{cur_spp_occurs_on_patches}{
#' \preformatted{
#' cur_spp_occurs_on_patches :  int [1:2] 54 112
#' }}
#' \subsection{cur_spp_row}{
#' \preformatted{
#' cur_spp_row :  int 814
#' }}
#' \subsection{edge_list}{
#' \preformatted{
#' edge_list :  num [1:814, 1:2] 1 3 5 7 9 11 13 15 17 19 ...
#' }}
#' \subsection{num_duplicates}{
#' \preformatted{
#' num_duplicates :  int 0
#' }}
#' \subsection{num_patches_for_cur_spp}{
#' \preformatted{
#' num_patches_for_cur_spp :  int 2
#' }}
#' \subsection{num_PU_spp_pairs}{
#' \preformatted{
#' num_PU_spp_pairs :  num 1628
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 814
#' }}
#' \subsection{num_spp_on_no_patches}{
#' \preformatted{
#' num_spp_on_no_patches :  num 0
#' }}
#' \subsection{num_spp_warnings}{
#' \preformatted{
#' num_spp_warnings :  num 0
#' }}
#' \subsection{occ_matrix}{
#' \preformatted{
#' occ_matrix :  num [1:814, 1:122] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{row_nums_of_duplicates}{
#' \preformatted{
#' row_nums_of_duplicates :  int(0)
#' }}
#' \subsection{rows_to_delete}{
#' \preformatted{
#' rows_to_delete :  list()
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns an edge list, a two column integer matrix of
#'     node IDs with one row for each edge and columns for the 2 ends of
#'     the edge; quits if any duplicate edges are found.

#-------------------------------------------------------------------------------

see_if_there_are_any_duplicate_links = function (occ_matrix,
                                                 num_spp,
                                                 bdpg_error_codes)
    {
    num_PU_spp_pairs = sum (occ_matrix)

    edge_list = matrix (0,
                        nrow = num_spp,
                        ncol = 2,
                        byrow = TRUE)

    colnames (edge_list) = c("from_node", "to_node")

    num_spp_warnings = 0
    num_spp_on_no_patches = 0
    rows_to_delete = list()

    for (cur_spp_row in 1:num_spp)
        {
        cur_spp_occurs_on_patches = which (occ_matrix [cur_spp_row,] == 1)
        num_patches_for_cur_spp = length (cur_spp_occurs_on_patches)

        if ((num_patches_for_cur_spp != 2) & (num_patches_for_cur_spp != 0))
            {
            num_spp_warnings = num_spp_warnings + 1
            cat ("\nWARNING ", num_spp_warnings, ": spp ", cur_spp_row, " is on ",
                 num_patches_for_cur_spp, " patches, PU_IDs ", sep='')
            print (cur_spp_occurs_on_patches)
            rows_to_delete [[length (rows_to_delete) + 1]] = cur_spp_row

            } else
            {
            if (num_patches_for_cur_spp == 0)
                {
                num_spp_on_no_patches = num_spp_on_no_patches + 1
                rows_to_delete [[length (rows_to_delete) + 1]] = cur_spp_row

                } else
                {
                edge_list [cur_spp_row, "from_node"] = cur_spp_occurs_on_patches [1]
                edge_list [cur_spp_row, "to_node"] = cur_spp_occurs_on_patches [2]

                }  #  end else - spp is on exactly 2 patches
            }  #  end else - spp is on either 0 or 2 patches
        }  #  end for - all spp rows

    if (num_spp_warnings > 0)
        cat ("\n----->  num_spp_warnings = ", num_spp_warnings, sep='')

    if (num_spp_on_no_patches > 0)
        {
        cat ("\n----->  num_spp_on_no_patches = ", num_spp_on_no_patches, sep='')
        }

    if (length (rows_to_delete) > 0)
        {
        rows_to_delete = unlist (rows_to_delete)
        cat ("\n----->  rows_to_delete = ", sep='')
        print (rows_to_delete)

        edge_list = edge_list [- rows_to_delete,,drop=FALSE]
        }

    row_nums_of_duplicates = which (duplicated (edge_list))
    num_duplicates = length (row_nums_of_duplicates)
    cat ("\n\nindices (duplicates) = ", row_nums_of_duplicates, "\n")
    cat ("\nduplicates = \n")
    print (edge_list[row_nums_of_duplicates,,drop=FALSE])
    cat ("\n\nnumber of duplicates = ", num_duplicates, "\n", sep='')

    if (num_duplicates > 0)
        {
        cat ("\n\nERROR: ", num_duplicates,
             " duplicate species in the Xu benchmark file.\n\n")

        quit (save="no", bdpg_error_codes$ERROR_STATUS_duplicate_spp_in_Xu_input_file)
        }

#docaids::doc_vars_in_this_func_once ()
    return (edge_list)
    }

#===============================================================================

#' Build PU spp pair indices from occ matrix
#'
#' Build planning unit vs. species indices from occupancy matrix.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{cur_PU_col}{
#' \preformatted{
#' cur_PU_col :  int 407
#' }}
#' \subsection{cur_PU_spp_row_idx}{
#' \preformatted{
#' cur_PU_spp_row_idx :  num 3022
#' }}
#' \subsection{cur_spp_row}{
#' \preformatted{
#' cur_spp_row :  int 1277
#' }}
#' \subsection{num_PU_spp_pairs}{
#' \preformatted{
#' num_PU_spp_pairs :  num 3022
#' }}
#' \subsection{num_PUs}{
#' \preformatted{
#' num_PUs :  num 407
#' }}
#' \subsection{num_spp}{
#' \preformatted{
#' num_spp :  int 1277
#' }}
#' \subsection{occ_matrix}{
#' \preformatted{
#' occ_matrix :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{PU_spp_pair_indices}{
#' \preformatted{
#' PU_spp_pair_indices : 'data.frame':	3022 obs. of  2 variables:
#'  $ PU_ID : int  1 3 5 6 7 8 9 10 209 335 ...
#'  $ spp_ID: int  1 2 3 3 4 4 5 5 5 5 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param occ_matrix matrix
#' @param num_PUs integer
#' @param num_spp integer
#'
#' @return Returns PU_spp_pair_indices data frame

#-------------------------------------------------------------------------------

build_PU_spp_pair_indices_from_occ_matrix = function (occ_matrix,
                                                      num_PUs, num_spp)
    {
    num_PU_spp_pairs = sum (occ_matrix)

        #****************************************************************
        #  Why is PU_spp_pair_indices a data frame instead of a matrix?
        #****************************************************************

    PU_spp_pair_indices = data.frame (PU_ID = rep (NA, num_PU_spp_pairs),
                                      spp_ID = rep (NA, num_PU_spp_pairs))

    cur_PU_spp_row_idx = 0

    for (cur_spp_row in 1:num_spp)
        {
        for (cur_PU_col in 1:num_PUs)
            {
            if (occ_matrix [cur_spp_row, cur_PU_col])
                {
                cur_PU_spp_row_idx = cur_PU_spp_row_idx + 1

                PU_spp_pair_indices [cur_PU_spp_row_idx, "PU_ID"] = cur_PU_col
                PU_spp_pair_indices [cur_PU_spp_row_idx, "spp_ID"] = cur_spp_row

                }  #  end if - cur spp occupies cur PU
            }  #  end for - all PU cols
        }  #  end for - all soo rows

#docaids::doc_vars_in_this_func_once ()
    return (PU_spp_pair_indices)
    }

#===============================================================================
#     Compute what fraction of species meet their representation targets.
#===============================================================================

find_indices_of_spp_with_unmet_rep = function (spp_occ_matrix,
                                               candidate_solution_PU_IDs,
                                               num_spp,
                                               spp_rep_targets
                                               )
    {
    spp_rep_fracs = compute_rep_fraction (spp_occ_matrix,
                                          candidate_solution_PU_IDs,
                                          spp_rep_targets
                                          )

#docaids::doc_vars_in_this_func_once ()
    return (which (spp_rep_fracs < 1))
    }

#-------------------------------------------------------------------------------

compute_frac_spp_covered =
        function (spp_occ_matrix,
                  candidate_solution_PU_IDs,
                  num_spp,
                  spp_rep_targets
                  )
    {
    indices_of_spp_with_unmet_rep =
        find_indices_of_spp_with_unmet_rep (spp_occ_matrix,
                                            candidate_solution_PU_IDs,
                                            num_spp,
                                            spp_rep_targets
                                            )

#docaids::doc_vars_in_this_func_once ()
    return (1 - (length (indices_of_spp_with_unmet_rep) / num_spp))
    }

#===============================================================================

#' Add missing PUs to PU_Count data frame
#'
#'  When error is added to the input data, it sometimes results in
#'  planning units that appear to have no species on them.  This function
#'  adds these planning units back into the PU_Count data frame so that the
#'  apparent problem has the same number of planning units as the correct
#'  problem.
#'
#'  When the pu_spp_pair_indices are written out in marxan's input
#'  format, there is no record of the empty planning units in those
#'  pairs since each pair is a planning unit ID followed by the ID
#'  of a species on that planning unit.
#'
#'  Consequently, marxan's output solutions will have fewer planning
#'  units than the problem generator generated and you will get size
#'  warnings (that should be errors) when comparing them to things
#'  like nodes$dependent_set_member.
#'  For example, here is the error that showed this was happening:
#'
#'       Error in marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member :
#'       (converted from warning) longer object length is not a multiple of shorter object length
#'
#'  To fix this, you need to add entries for each of the missing PUs.
#'
#' NOTE:  There could also be correct problems that have some patches with no
#' species on them.
#'
#' NOTE:  Not sure if matters, but I had commented in here that the old name
#' for this function was: add_missing_PUs_to_marxan_solutions().  Probably
#' don't need to track that anymoe, but will leave it here for the moment.

#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{all_correct_node_IDs}{
#' \preformatted{
#' all_correct_node_IDs :  int [1:122] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{marxan_solution}{
#' \preformatted{
#' marxan_solution : 'data.frame':	122 obs. of  2 variables:
#'  $ PU_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq : int  1 25 1 27 1 29 1 28 1 24 ...
#' }}
#' \subsection{missing_PU_IDs}{
#' \preformatted{
#' missing_PU_IDs :  int(0)
#' }}
#' \subsection{num_missing_PU_IDs}{
#' \preformatted{
#' num_missing_PU_IDs :  int 0
#' }}
#' \subsection{presences_col_name}{
#' \preformatted{
#' presences_col_name :  chr "freq"
#' }}
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns \code{marxan solution} data frame that includes all planning
#'     unit IDs from the correct problem rather than only the IDs that occurred
#'     in the apparent problem (see description of \code{marxan_solution} in
#'     input argument list for this function)

#-------------------------------------------------------------------------------

add_missing_PU_rows_to_PU_Count_dataframe = function (marxan_solution,
                                                all_correct_node_IDs,
                                                PU_col_name,
                                                presences_col_name)
    {
        #  Marxan solutions are data frames with one row for each planning unit.
        #  They have 2 columns, one for the planning unit IDs and the other
        #  for the count or indicator of presence/absence.
        #  The second column usually contains 0 or 1 to indicate presence
        #  or absence of that PU in the marxan solution.
        #  However, in the case of marxan's summed solution, the second
        #  column contains the number of iterations (restarts) where that
        #  planning unit appeared in marxan's solution.
        #
        #  Search for the missing planning unit IDs, then add one line
        #  to the table for each missing planning unit ID.
        #  Set the presences field for each of those lines to be 0.

    missing_PU_IDs = setdiff (all_correct_node_IDs, marxan_solution [ , PU_col_name])
    num_missing_PU_IDs = length (missing_PU_IDs)

    if (num_missing_PU_IDs > 0)
        {
        missing_rows = matrix (c(missing_PU_IDs, rep(0,num_missing_PU_IDs)),
                               nrow=num_missing_PU_IDs,
                               ncol=2,
                               dimnames=list(NULL,c(PU_col_name,presences_col_name)))

        marxan_solution = rbind (marxan_solution, missing_rows)
        }

#docaids::doc_vars_in_this_func_once ()
    return (marxan_solution)
    }

#-------------------------------------------------------------------------------

        #  Count the number of species on each PU.
        #  The initial count may have to be cleaned up because
        #  some nodes may not appear in the PU_spp_pair_indices
        #  table if they have no species occurrences on them.
        #  For example, if false negative errors were added to a
        #  Xu problem to simulate detectability issues, nodes that had
        #  species on them in the initial problem definition may
        #  no longer have any species on them and not appear in the table.
        #  Downstream processes may expect those nodes to still appear
        #  in the list of counts but have a zero count, so you need
        #  to add them back into the table.

clean_up_final_link_counts_for_each_node <- function (PU_spp_pair_indices,
                                                      all_correct_node_IDs,
                                                      PU_col_name,
                                                      presences_col_name)
    {
    final_link_counts_for_each_node_without_missing_rows =
                            plyr::count (PU_spp_pair_indices, vars=PU_col_name)

    final_link_counts_for_each_node =
        add_missing_PU_rows_to_PU_Count_dataframe (final_link_counts_for_each_node_without_missing_rows,
                                             all_correct_node_IDs,
                                             PU_col_name, presences_col_name)

    return (final_link_counts_for_each_node)
    }

#-------------------------------------------------------------------------------

#' Compute final PU and species counts and plot degree and abundance distributions.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{all_correct_node_IDs}{
#' \preformatted{
#' all_correct_node_IDs :  int [1:122] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{cor_or_app_label}{
#' \preformatted{
#' cor_or_app_label :  chr "COR"
#' }}
#' \subsection{final_link_counts_for_each_node}{
#' \preformatted{
#' final_link_counts_for_each_node : 'data.frame':	122 obs. of  2 variables:
#'  $ PU_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq : int  1 25 1 27 1 29 1 28 1 24 ...
#' }}
#' \subsection{final_node_counts_for_each_link}{
#' \preformatted{
#' final_node_counts_for_each_link : 'data.frame':	814 obs. of  2 variables:
#'  $ spp_ID: int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq  : int  2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{plot_output_dir}{
#' \preformatted{
#' plot_output_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{presences_col_name}{
#' \preformatted{
#' presences_col_name :  chr "freq"
#' }}
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#' \subsection{PU_spp_pair_indices}{
#' \preformatted{
#' PU_spp_pair_indices : 'data.frame':	1628 obs. of  2 variables:
#'  $ PU_ID : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: int  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return final_link_counts_for_each_node, i.e., species count for each node, i.e., 2 column data frame of PU_IDs vs. number of species on corresponding PU
#'
#' @details
#'  Count the number of species on each PU.
#'  If a PU has no species on it, it won't necessarily be in the
#'  PU_spp_pair_indices table, so this routine adds the PU back into the table
#'  with a zero count.
#'
#'  This means that even though PU_spp_pair_indices could be either a correct
#'  or an apparent set of PU-spp pairs, all_correct_node_IDs HAS
# ' to be the full set of CORRECT node IDs.
#' @examples
#' \dontrun{
#' tot_num_nodes = 6
#' PU_spp_pair_indices = data.frame (PU_ID=1:tot_num_nodes, spp_ID=c(1,1,2,2))
#' all_correct_node_IDs = 1:tot_num_nodes
#' final_link_counts =
#'     clean_up_final_link_counts_for_each_node (PU_spp_pair_indices,
#'                                               all_correct_node_IDs,
#'                                               "PU_ID",
#'                                               "freq")
#'          }
#'
#-------------------------------------------------------------------------------

summarize_and_plot_graph_and_distribution_structure_information =
    function (PU_spp_pair_indices,  #  either correct or apparent
              cor_or_app_label,     #  either correct or apparent
              all_correct_node_IDs,  #  MUST BE correct only
              plot_output_dir,
              spp_col_name,
              PU_col_name,
              presences_col_name)
    {
        #  Count the number of PUs each species occurs on.
        #  For the Xu problem generator, all species should occur on
        #  exactly 2
cat("\nstarting summarize_and_plot_graph_and_distribution_structure_information()")

    final_node_counts_for_each_link =
            plyr::count (PU_spp_pair_indices, vars=spp_col_name)
cat("\njust after count()")

        #  Count the number of species on each PU.
        #  If a PU has no species on it, it won't necessarily be in the
        #  PU_spp_pair_indices table, so add the PU back into the table
        #  with a zero count.
        #
        #  So, even though PU_spp_pair_indices could be either a correct
        #  or an apparent set of PU-spp pairs, all_correct_node_IDs HAS
        #  to be the full set of CORRECT node IDs.

    final_link_counts_for_each_node =
            clean_up_final_link_counts_for_each_node (PU_spp_pair_indices,
                                                      all_correct_node_IDs,
                                                      PU_col_name,
                                                      presences_col_name)
cat("\njust after clean_up_final_link_counts_for_each_node()")

    plot_degree_and_abundance_dists_for_node_graph (
                                final_link_counts_for_each_node,
                                final_node_counts_for_each_link,
                                PU_col_name,
                                plot_output_dir,
                                cor_or_app_label,
                                spp_col_name)
cat("\njust after plot_degree_and_abundance_dists_for_node_graph()")


#docaids::doc_vars_in_this_func_once ()
    return (final_link_counts_for_each_node)
    }

#===============================================================================

#' Save bd problem to disk
#'
#' After a problem is generated, its R representation is saved to disk so
#' that it can be archived and re-used in future experiments without having
#' to regenerate the problem.  Saving it is also useful for reproducibility
#' in that it allows re-creation of exactly the problem used in an experiment.
#'
#-------------------------------------------------------------------------------

#' @details
#' Writes to a file whose name contains:
#' \itemize{
#'  \item{the UUID of the problem}
#'  \item{whether it's a basic problem or a wrapped problem}
#'  \item{whether it's a correct or an apparent problem}
#' }
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{base_outdir}{
#' \preformatted{
#' base_outdir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{rsprob}{
#' \preformatted{
#' rsprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{exp_root_dir}{
#' \preformatted{
#' exp_root_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns input object with its checksum slot filled

#-------------------------------------------------------------------------------

save_rsprob <- function (rsprob, exp_root_dir)
    {
    base_outdir = get_RSprob_path_topdir (rsprob, exp_root_dir)
    rsprob      = save_obj_with_checksum (rsprob,
                                          #saved_rsprob_filename,
                                          base_outdir)

#docaids::doc_vars_in_this_func_once ()
    return (rsprob)
    }


#===============================================================================

save_rsrun <- function (rsrun, starting_dir)
    {
    # saved_rsrun_filename = paste0 ("saved_rsrun.",
    #                                 rsrun@file_name_prefix,
    #                                 rsrun@UUID,
    #                                 ".rds")

    base_outdir = get_RSrun_path_topdir (rsrun, starting_dir)
    rsrun       = save_obj_with_checksum (rsrun,
                                          #saved_rsrun_filename,
                                          base_outdir)

#docaids::doc_vars_in_this_func_once ()
    return (rsrun)
    }

#===============================================================================

load_saved_obj_from_file <- function (path_to_file)
    {
    return (readRDS (path_to_file))
    }

#===============================================================================

#' Strip Trailing Slash Off Of Path If There Is One
#'
#' Tzar puts a slash on the end of the output path and this causes problems
#' when using it to build full paths to file names.  This function is
#' primarily here to strip that off but can be used on any path.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{last_char}{
#' \preformatted{
#' last_char :  chr "/"
#' }}
#' \subsection{path}{
#' \preformatted{
#' path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#'
#-------------------------------------------------------------------------------

#' @param path character string containing a path that may or may not end in a
#'     slash
#'
#' @return character string containing original path argument with trailing
#'     slash removed if there was one, otherwise, same as original
#' @export

#-------------------------------------------------------------------------------

strip_trailing_slash <- function (path)
    {
    last_char = stringr::str_sub (path, nchar (path), nchar (path))

        #  Originally, this looked for the platform-specific file separator,
        #  but that may cause a problem, since tzar and R both seem to always
        #  use a slash and then translate back and forth to the
        #  platform-specific only when actually talking to the OS.
        #  So, changing back to just checking for a slash.

    if (last_char == "/")                                   #.Platform$file.sep)
        path = stringr::str_sub (path,1,nchar(path)-1)

#docaids::doc_vars_in_this_func_once ()
    return (path)
    }

#===============================================================================

#'  Imitate unix touch function to create an empty file.
#'
#'  I couldn't find an existing function for this, but did find a
#'  stack overflow question that suggested using write.table() on an
#'  empty table, so that's what I've done.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{file_path_to_touch}{
#' \preformatted{
#' file_path_to_touch :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/OK_marxan_solution_IS_appar"| __truncated__
#' }}
#'
#-------------------------------------------------------------------------------

#' @param file_path_to_touch character string
#'
#' @return Returns nothing
#' @export

#-------------------------------------------------------------------------------

touch <- function (file_path_to_touch)
    {
    file_path_to_touch = normalizePath (file_path_to_touch, mustWork=FALSE)
#    cat ("\nfile_path_to_touch = '", file_path_to_touch, "'\n")
    write.table (data.frame(),
                 file = file_path_to_touch,
                 col.names=FALSE)
#docaids::doc_vars_in_this_func_once ()
    }

#===============================================================================



