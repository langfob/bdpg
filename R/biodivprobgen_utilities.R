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
#' @param ... Arguments that can be passed on to base::sample()
#'
#' @return Returns a value that is probably an integer but is definitely not null
#' @export

#-------------------------------------------------------------------------------

safe_sample = function (x,...) { if (length (x) == 1) x else sample (x,...) }

#===============================================================================

#-------------------------------------------------------------------------------

#' Return NA for any given value whose length is 0
#'
#' This is originally built to be used on each element of a list inside an
#' lapply where you're trying to get rid of NULLs, numeric(0)s, etc. in a
#' list to be converted to a data frame.
#'
#-------------------------------------------------------------------------------

#' @param value any value
#'
#' @return NA or the input argument
#' @export

#-------------------------------------------------------------------------------

fix_0_length_list_elements <- function (value)
    {
        #  length == 0 should handle NULL, logical(0), numeric(0), etc.
        #  These are all things that as.data.frame (list) chokes on.
    if (length (value) == 0)
        return (NA) else return (value)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Convert list to a new list with 0 length values replaced by NA
#'
#-------------------------------------------------------------------------------

#' @param a_list a list
#'
#' @return the input list with its 0 length values replaced by NA values and
#' non-0 length values left as they were
#' @export

#-------------------------------------------------------------------------------

list_with_0_length_vals_replaced_by_NA <- function (a_list)
    {
    if (! is.list (a_list))
        stop ("list_with_0_length_vals_replaced_by_NA: argument is not a list.")

    return (lapply (a_list, fix_0_length_list_elements))
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Convert list to data frame with 0 length values replaced by NA
#'
#-------------------------------------------------------------------------------

#' @param a_list a list
#'
#' @return a data frame whose columns correspond to elements of the list and
#' any 0 length data value in the input list has been replaced with NA
#' @export

#-------------------------------------------------------------------------------

list_as_data_frame_with_0_length_vals_replaced_by_NA <- function (a_list)
    {
    return (as.data.frame (list_with_0_length_vals_replaced_by_NA (a_list)))
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Get parameter value that should be integer and return 1 if value is null
#'
#-------------------------------------------------------------------------------

#' @param value a value that is probably a number, but could be anything
#'
#' @return Returns a value that is probably an integer but is definitely not null
#' @export

#-------------------------------------------------------------------------------

value_or_1_if_null <- function (value)
    {
    if (is.null (value)) 1 else value
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Get parameter value that should be boolean and return FALSE if value is null
#'
#-------------------------------------------------------------------------------

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
#' @param tzar_run_id_field_name character string containing name of tzar run ID
#' field in output file
#'
#' @inheritParams std_param_defns
#'
#' @return Returns nothing.

#-------------------------------------------------------------------------------

write_results_to_files <- function (csv_outfile_name,
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

#-------------------------------------------------------------------------------

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

#' @inheritParams std_param_defns
#'
#' @return Returns an edge list, a two column integer matrix of
#'     node IDs with one row for each edge and columns for the 2 ends of
#'     the edge; quits if any duplicate edges are found.

#-------------------------------------------------------------------------------

see_if_there_are_any_duplicate_links = function (occ_matrix,
                                                 num_spp
                                                 # ,
                                                 # bdpg_error_codes
                                                 )
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
        # cat ("\n\nERROR: ", num_duplicates,
        #      " duplicate species in the Xu benchmark file.\n\n")

#        quit (save="no", bdpg_error_codes$ERROR_STATUS_duplicate_spp_in_Xu_input_file)
        stop (paste0 ("\n\nERROR: ", num_duplicates,
                      " duplicate species in the Xu benchmark file."))
        }

    return (edge_list)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Build PU spp pair indices from occ matrix
#'
#' Build planning unit vs. species indices from occupancy matrix.
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

    return (PU_spp_pair_indices)
    }

#===============================================================================
#     Compute what fraction of species meet their representation targets.
#===============================================================================

#-------------------------------------------------------------------------------

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

    return (which (spp_rep_fracs < 1))
    }

#===============================================================================

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

    return (marxan_solution)
    }

#===============================================================================

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

#===============================================================================

#-------------------------------------------------------------------------------

#' Compute final PU and species counts and plot degree and abundance distributions.
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
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Validate input as boolean and replace with default on empty input if desired
#'
#' This function is intended to be a more flexible replacement for calling
#' is.logical().  In particular, it makes it possible to replace empty values
#' with a default value (e.g., FALSE) and to define whether NULL and/or NA
#' are treated as empty values.  It also allows control over whether numeric
#' values are considered boolean by this validation function.
#' This is sometimes useful when validating an input option where a numeric
#' value would be suggestive of a mistake in the input file.
#'
#' Note that even if they are set to TRUE, the flags treat_NULL_as_empty and
#' treat_NA_as_empty are ignored if def_on_empty is FALSE.

#-------------------------------------------------------------------------------

#' @param var_value the value to be checked to see if it's a boolean
#' @param def_on_empty boolean flag indicating whether to return a default
#'     value instead of the input value when the input value is empty (where
#'     empty is defined by other flags below)
#' @param def a TRUE or FALSE default value to return instead of the input value
#'     when a default is requested
#' @param treat_NULL_as_empty a boolean flag set to TRUE if a NULL input is to
#'     be treated as an empty input; FALSE otherwise
#' @param treat_NA_as_empty a boolean flag set to TRUE if an NA input is to
#'     be treated as an empty input; FALSE otherwise
#' @param allow_num a boolean flag set to TRUE if a numeric input is to be
#'     allowed and returned as FALSE if 0 and TRUE if non-zero
#'
#' @return Returns the input value if it is boolean or, returns a specified
#'     boolean if other arguments force a valid converted or default boolean
#'     to return; otherwise, throws an error
#'
#' @export
#'
#' @examples
#' x <- TRUE
#' vb (x)
#' vb (-150, allow_num = TRUE)
#' vb (0, allow_num = TRUE)
#' vb (NULL, def_on_empty = TRUE)
#' vb (NULL, def_on_empty = TRUE, def = TRUE)
#' vb (NA, def_on_empty = TRUE, def = TRUE, treat_NA_as_empty = TRUE)
#' vb (NA, def_on_empty = TRUE, treat_NA_as_empty = TRUE)
#' vb (NULL, def_on_empty = TRUE, def = 10, treat_NULL_as_empty = TRUE, allow_num = TRUE)
#' \dontrun{
#'                 #  These all generate errors
#' vb (1)                     #  error - not boolean & allow_num not set to TRUE
#' vb (0)
#' vb (1000)
#' vb ("aString")                        #  error - not boolean
#' vb (NULL)                             #  error - def_on_empty not set
#' vb (NA)            #  error - def_on_empty not set, treat_NA_as_empty not set
#' vb (NULL, treat_NULL_as_empty = TRUE) #  error - def_on_empty not set
#' vb (NA, treat_NA_as_empty = TRUE)     #  error - def_on_empty not set to TRUE
#'         #  error - non-numeric default, but allow_num not TRUE
#' vb (NA, def_on_empty = TRUE, def = 10, treat_NA_as_empty = TRUE)
#'          }

#-------------------------------------------------------------------------------

vb <- function (var_value, def_on_empty = FALSE, def = FALSE,
                treat_NULL_as_empty = TRUE,
                treat_NA_as_empty = FALSE,
                allow_num = FALSE)
    {
        #  A little extra logic is required here when building error messages
        #  to keep error messages from being misleading when a bad input value
        #  invokes the use of a caller-provided default value and that value
        #  is also bad.
        #  Without the extra logic, the bad default value is reported as the
        #  name of the input value in the error message instead of giving
        #  the name of the original input variable.

    var_name = deparse (substitute (var_value))  #  Get var_name arg as string
    err_string_lead = "Value"

    err_string_lead_for_def = "Default value"
    if (def_on_empty &&
            ((treat_NULL_as_empty && is.null (var_value))
                    ||
#  Getting an error message when I use is.na() and have warnings set to errors.
#  is.na() documentation says:
#  anyNA(NULL) is false: is.na(NULL) is logical(0) with a warning.
#                 (treat_NA_as_empty && is.na (var_value))))
             (treat_NA_as_empty && anyNA (var_value))))
        {
        var_value = def
        err_string_lead = err_string_lead_for_def
        }

    if (is.numeric (var_value))
        {
        if (allow_num) var_value = (var_value != 0)  #  Set 0 FALSE, non-0 TRUE
        if (err_string_lead == err_string_lead_for_def)
            {
            err_string_lead = "Default numeric value"
            }
        else
            err_string_lead = "Numeric value"
        }

    if (!is.logical (var_value) ||
        (anyNA (var_value) && (!def_on_empty || !treat_NA_as_empty)))
        {
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      " must be boolean"))
        }

    return (var_value)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Validate input as boolean and replace with default on empty input if desired
#'
#' This function is intended to be a more flexible replacement for calling
#' is.numeric().  In particular, it makes it possible to replace empty values
#' with a default value (e.g., 0) and to define whether NULL and/or NA
#' are treated as empty values.  It also allows checking whether the input
#' (or a resulting default) value falls in a given range.  If no range is
#' specified, then any numeric value is allowed.  It also allows specification
#' of whether the bounds represent an open, closed, or semi-closed interval
#' through the bounds_type argument.  That argument is a 2 character string
#' composed of any combination of 'i' and 'e' to designate whether the bounds
#' are inclusive or exclusive.  The first character of the string is for the
#' lower bound and the second is for the upper bound, e.g., if the string is
#' "ei", then the function will check whether the value is strictly > the
#' lower bound and <= to the upper bound.
#'
#' Note that even if they are set to TRUE, the flags treat_NULL_as_empty and
#' treat_NA_as_empty are ignored if def_on_empty is FALSE.

#-------------------------------------------------------------------------------

#' @param var_value the value to be checked to see if it's a numeric in range
#' @param range_lo the lower bound of the range to see if input value falls in
#' @param range_hi the upper bound of the range to see if input value falls in
#' @param bounds_types a 2 character string indicating whether the lower and
#'     upper bounds are inclusive or exclusive bounds, with 'i' meaning
#'     inclusive and 'e' meaning exclusive; legal strings are "ii", "ie", "ei",
#'     and "ee"
#' @param def_on_empty boolean flag indicating whether to return a default
#'     value instead of the input value when the input value is empty (where
#'     empty is defined by other flags below)
#' @param def a TRUE or FALSE default value to return instead of the input value
#'     when a default is requested
#' @param treat_NULL_as_empty a boolean flag set to TRUE if a NULL input is to
#'     be treated as an empty input; FALSE otherwise
#' @param treat_NA_as_empty a boolean flag set to TRUE if an NA input is to
#'     be treated as an empty input; FALSE otherwise
#'
#' @return Returns the input value if it is numeric and in range or, returns a
#'     specified numeric value if other arguments force a valid default
#'     value to return; otherwise, throws an error
#'
#' @export
#'
#' @examples
#' x <- 0.7
#' vn (x)
#' vn (x, range_lo = 0, range_hi = 1)
#' vn (x, range_lo = 0.7, range_hi = 1, bounds_types = "ie")
#' vn (100, range_hi = 100)
#' vn (NULL, def_on_empty = TRUE, def = 0)
#' vn (NULL, def_on_empty = TRUE)
#' vn (NA, def_on_empty = TRUE, def = -999)
#' vn (NA, range_hi = 100, bounds_types = "ee",
#'     def_on_empty = TRUE, def=15, treat_NA_as_empty = TRUE)
#' \dontrun{
#' vn (1000, range_hi = 100, bounds_types = "ii")                       #  error
#' vn (0.7, range_lo = 0.7, bounds_types = "ei")                        #  error
#' vn (NULL)                                                            #  error
#' vn (NA)                                                              #  error
#' vn (NA, range_lo = -10, range_hi = 10, bounds_types = "ee",
#'     def_on_empty = TRUE, def = 15)                                   #  error
#' vn (NULL, range_lo = -10, range_hi = 10, bounds_types = "ee",
#'     def_on_empty = TRUE, def = 15, treat_NULL_as_empty = TRUE)       #  error
#'          }

#-------------------------------------------------------------------------------

vn <- function (var_value,
                range_lo=-Inf, range_hi=Inf, bounds_types = "ii",
                def_on_empty = FALSE,
                def = 0,
                treat_NULL_as_empty = TRUE,
                treat_NA_as_empty = TRUE)
    {
        #--------------------------------------------------------
        #  Get var_name arg as string to use in error messages.
        #--------------------------------------------------------

    var_name = deparse (substitute (var_value))
    err_string_lead = "Validating"

        #-------------------------------------------------------------------
        #  If caller wants to replace empty input with a default value,
        #  and the input is empty, then go ahead and make the replacement.
        #  Because the default value itself might not be numeric or
        #  in range, continue on to check that value as you would a
        #  value that was passed in normally.
        #-------------------------------------------------------------------

    if (def_on_empty &&
            ((treat_NULL_as_empty && is.null (var_value))
                    ||
             (treat_NA_as_empty && anyNA (var_value))))
        {
        var_value = def
        err_string_lead = "Default value"
        }

        #-----------------------------------------------------------------
        #  Check the value that will be returned to see if it's numeric,
        #  regardless of whether it is the value that was passed in or
        #  the default value.
        #-----------------------------------------------------------------

    if (! is.numeric (var_value))
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", must be numeric"))

        #-----------------------------------------------------------------
        #  Make sure that the range hi and lo are themselves numeric and
        #  are in the proper order to specify a range.
        #-----------------------------------------------------------------

    if (! is.numeric (range_lo))
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", range_lo = '", range_lo, "' must be numeric"))

    if (! is.numeric (range_hi))
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", range_hi = '", range_hi, "' must be numeric"))

    if (range_lo > range_hi)
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", range_lo = '", range_lo, "' must be <= ",
                      "range_hi = '", range_hi, "'"))

        #----------------------------------------------------------------------
        #  Check that the range bounds are specified correctly and that the
        #  var_value to be returned from the function falls within the bounds.
        #
        #  The upper and lower bounds can be either exclusive or inclusive
        #  bounds. This is specified by a 2 character string with the first
        #  character for the lower bound and the second character for the
        #  upper bound.  For each bound, the specifier is an "i" if the bound
        #  is inclusive and an "e" if it's exclusive, e.g., if a value must
        #  be >= to the lower bound and strictly less than the upper bound,
        #  the bounds_type would be "ie".
        #----------------------------------------------------------------------

    if (! is.character (bounds_types))
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", bounds_types = '", range_hi, "' must be a string"))

    if (nchar (bounds_types) != 2)
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", bounds_types = '", range_hi, "' must be a 2 character string"))

    lower_bound_type = substring (bounds_types, 1, 1)
    if (lower_bound_type == "i")
        {
        if (var_value < range_lo)
            {
            stop (paste0 (err_string_lead, " '", var_value,
                          "' used for input variable ", var_name,
                          ", must be >= ", "range_lo = '", range_lo, "'"))
            }
        } else if (lower_bound_type == "e")
        {
        if (var_value <= range_lo)
            {
            stop (paste0 (err_string_lead, " '", var_value,
                          "' used for input variable ", var_name,
                          ", must be > ", "range_lo = '", range_lo, "'"))
            }
        } else  #  bounds_type NOT "i" or "e"
        {
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", lower_bound_type = '", lower_bound_type, "' must be 'i' or 'e'"))
        }

    upper_bound_type = substring (bounds_types, 2, 2)
    if (upper_bound_type == "i")
        {
        if (var_value > range_hi)
            {
            stop (paste0 (err_string_lead, " '", var_value,
                          "' used for input variable ", var_name,
                          ", must be <= ", "range_hi = '", range_hi, "'"))
            }
        } else if (upper_bound_type == "e")
        {
        if (var_value >= range_hi)
            {
            stop (paste0 (err_string_lead, " '", var_value,
                          "' used for input variable ", var_name,
                          ", must be < ", "range_hi = '", range_hi, "'"))
            }
        } else  #  bounds_type NOT "i" or "e"
        {
        stop (paste0 (err_string_lead, " '", var_value,
                      "' used for input variable ", var_name,
                      ", upper_bound_type = '", upper_bound_type, "' must be 'i' or 'e'"))
        }

    return (var_value)
    }

#===============================================================================




