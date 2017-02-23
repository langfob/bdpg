#===============================================================================

                        #  biodivprobgen_utilities.R

#===============================================================================

#  2015 04 27 - BTL
#       Created by extracting functions from generateSetCoverProblem.R.

#===============================================================================

    #  2015 04 08 - BTL
    #  I just got bitten very badly by the incredibly annoying behavior of R's
    #  sample() function, so here is a replacement function that I need to
    #  use everywhere now.
    #  When I called sample with a vector that sometimes had length n=1,
    #  it sampled from 1:n instead of returning the single value.
    #  This majorly screwed all kinds of things in a very subtle, very hard
    #  to find way.

safe_sample = function (x,...) { if (length (x) == 1) x else sample (x,...) }

#===============================================================================

    #  This function is used two different ways.
    #  It's called when the program quits because there are too many species
    #  or it's called when the program runs successfully.
    #  It's declared here because you don't know which path the program
    #  will take.
    #  It should go in a file of misc utilities, but it might be the only
    #  thing in that file at the moment.

write_results_to_files = function (results_df, parameters,
                                   cur_result_row)    #  Added 2016 03 28 - BTL.
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

    results_df$run_ID [cur_result_row] = 0
    write.csv (results_df, file = parameters$summary_without_run_id_filename, row.names = FALSE)

    results_df$run_ID [cur_result_row] = parameters$run_id
    write.csv (results_df, file = parameters$summary_filename, row.names = FALSE)
    }

#===============================================================================

get_PU_costs = function (num_PUs) { return (rep (1, num_PUs)) }

#===============================================================================

    #  Note that you can only uniquely decode an edge list from an occurrence
    #  matrix if the species only occurs on 2 patches, i.e., the underlying
    #  assumption in the Xu problem generator.
    #  However, the only reason I'm building this routine now is to run it
    #  on the Xu benchmark problems to see if those allow any duplicate
    #  edges, i.e., more than one species occurring on the same pair of
    #  patches.

see_if_there_are_any_duplicate_links = function (occ_matrix, num_spp,
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

    return (edge_list)
    }

#===============================================================================

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

    #  When error is added to the input data, it sometimes results in
    #  planning units that appear to have no species on them.
    #       NOTE:  There could also be correct problems that have some
    #               patches with no species on them.
    #  When the pu_spp_pair_indices are written out in marxan's input
    #  format, there is no record of the empty planning units in those
    #  pairs since each pair is a planning unit ID followed by the ID
    #  of a species on that planning unit.
    #  Consequently, marxan's output solutions will have fewer planning
    #  units than the problem generator generated and you will get size
    #  warnings (that should be errors) when comparing them to things
    #  like nodes$dependent_set_member.
    #  For example, here is the error that showed this was happening:
    #
    #       Error in marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member :
    #       (converted from warning) longer object length is not a multiple of shorter object length
    #
    #  To fix this, you need to add entries for each of the missing PUs.

#add_missing_PUs_to_marxan_solutions
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
#' @param PU_spp_pair_indices 2 column data frame of PU IDs vs species IDs
#' @param cor_or_app_label Character string for labelling things as correct or apparent
#' @param all_correct_node_IDs Cector of integer node IDs, one for every node in the "correct" problem
#' @param plot_output_dir Full path string to location for storing plotting output files
#' @param spp_col_name Character string giving column name for spp IDs in PU_spp_pair_indices
#' @param PU_col_name Character string giving column name for PU IDs in PU_spp_pair_indices
#' @param presences_col_name Character string giving column name for species counts in returned final_link_counts_for_each_node
#' @export
#' @return species count for each node, i.e., 2 column data frame of PU_IDs vs. number of species on corresponding PU
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
#' @details
#' Writes to a file whose name contains:
#' \itemize{
#'  \item{the UUID of the problem}
#'  \item{whether it's a basic problem or a wrapped problem}
#'  \item{whether it's a correct or an apparent problem}
#' }
#'For example:  "saved_bdprob.f470f75b-116b-4ff9-9db0-4e9448bcb2ef.BASIC.COR.rds"
#'
#'Note that the BASIC/WRAPPED and COR/APP arguments to save_bdprob() don't have
#'to be capitalized or even use the suggested strings.  They can be anything
#'that you want to have built into those spots in the file name.  Capitalizing
#'and using those strings has just proven easy to spot or search for in
#'directory listings so they seem like a good convention to use.
#'
#' @param bdprob_type character string indicating problem type, either "BASIC" or "WRAPPED" (could eventually also be "combined")
#' @param app_vs_cor character string indicating whether it's a correct or apparent problem, either "COR" or "APP"
#' @param uuid character string indicating the UUID of the problem being saved
#' @param base_outdir character sting giving the full path to the directory where the problem will be saved
#' @param bdprob a Xu_bd_problem to write to disk (subclasses of Xu_bd_problem allowed)
#'
#' @return character string giving the full path (including file name) to file where problem is saved on disk
#' @export
#'
#' @examples
#' \dontrun{
#' Xu_bdprob_cor@full_saved_bdprob_path =
#'     save_bdprob ("BASIC", "COR", Xu_bdprob_cor@UUID, Xu_bdprob_cor@base_outdir,
#'                  Xu_bdprob_cor)
#' Xu_bdprob_app@full_saved_bdprob_path =
#'     save_bdprob ("BASIC", "APP", Xu_bdprob_app@UUID, Xu_bdprob_app@base_outdir,
#'                  Xu_bdprob_app)
#'
#' Xu_bdprob_cor@full_saved_bdprob_path =
#'     save_bdprob ("WRAPPED", "COR", Xu_bdprob_cor@UUID, Xu_bdprob_cor@base_outdir,
#'                  Xu_bdprob_cor)
#' Xu_bdprob_app@full_saved_bdprob_path =
#'     save_bdprob ("WRAPPED", "APP", Xu_bdprob_app@UUID, Xu_bdprob_app@base_outdir,
#'                  Xu_bdprob_app)
#'
#' #  To reload the problem as a Xu_bd_problem, call readRDS () with the
#' #  full path to the saved file
#' reloaded_Xu_prob =
#'     readRDS ("saved_bdprob.f470f75b-116b-4ff9-9db0-4e9448bcb2ef.BASIC.COR.rds")
#' }

save_bdprob <- function (bdprob_type,    #  i.e., "BASIC" or "WRAPPED"
                         app_vs_cor,     #  i.e., "COR" or "APP"
                         uuid,
                         base_outdir,
                         bdprob
                         )
    {
    saved_bdprob_filename = paste0 ("saved_bdprob.",
                                    uuid, ".",
                                    bdprob_type, ".",
                                    app_vs_cor, ".rds")

    full_saved_bdprob_path = file.path (base_outdir, saved_bdprob_filename)

    bdprob = compute_and_set_obj_checksum (bdprob, base_outdir)

    saveRDS (bdprob, full_saved_bdprob_path)
#    reloaded_bdprob = readRDS (full_saved_bdprob_path)    #  testing only

    cat ("\n\n>>>>> bdprob saved to: \n    '", full_saved_bdprob_path, "'",
         "\nTo reload problem, use readRDS (full_saved_bdprob_path)\n\n", sep='')

    return (full_saved_bdprob_path)
    }

#===============================================================================

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

    #  Imitate unix touch function to create an empty file.
    #  I couldn't find an existing function for this, but did find a
    #  stack overflow question that suggested using write.table() on an
    #  empty table, so that's what I've done.

touch <- function (file_path_to_touch)
    {
    file_path_to_touch = normalizePath (file_path_to_touch, mustWork=FALSE)
#    cat ("\nfile_path_to_touch = '", file_path_to_touch, "'\n")
    write.table (data.frame(),
                 file = file_path_to_touch,
                 col.names=FALSE)
    }

#===============================================================================

#' Compute a checksum for the object
#'
#' The point of this function is to give a quick way to compare two complex
#' objects to see whether they are really the same object excluding their UUID
#' and checksum fields.  This is useful when running lots of experiments over
#' and over during development where the same random seed may end up
#' accidentally being re-used and generating identical experiments.  When lots
#' experiments are archived, it may be necessary to quickly cross-compare many
#' archived experiments against each other and having a stored checksum will
#' make it easier to do this and therfore, easier to spot accidental
#' duplicates.
#'
#' This function works by replacing the UUID and checksum fields of the local
#' copy of the object with empty strings and then writing the object copy to a
#' temporary file.  Then, a checksum is computed for that temporary file and
#' file.  The modified original object is not returned from the function.
#'
#' @param obj object whose checksum is to be computed
#' @param base_outdir directory where temporary file for use in checksum
#'     computation will be written and then deleted
#'
#' @return character string checksum of the object
#' @export

compute_obj_checksum <- function (obj, base_outdir=".")
    {
        #------------------------------------------------------------------
        #  Clear the UUID and checksum fields if they exist,
        #  since we don't want them to cause the checksum to be different
        #  when two objects are identical other than those two fields.
        #------------------------------------------------------------------

    if ("UUID" %in% slotNames (obj))
        obj@UUID <- ""
    if ("checksum" %in% slotNames (obj))
        obj@checksum <- ""

        #--------------------------------------------------------------
        #  Write the object to a temporary file that checksum can be
        #  computed over (checksum only works on files, not objects).
        #--------------------------------------------------------------

    saved_obj_filename = paste0 ("tmp_for_checksum__ok_to_delete_if_left_after_run.rds")
    full_saved_obj_path = file.path (normalizePath (base_outdir),
                                     saved_obj_filename)
    saveRDS (obj, full_saved_obj_path)

        #-----------------------------------------------------
        #  Now ready to compute the checksum
        #  and get rid of the temporary file created for it.
        #-----------------------------------------------------

    checksum = tools::md5sum (full_saved_obj_path)
    if (file.exists (full_saved_obj_path)) file.remove (full_saved_obj_path)

    return (checksum)
    }

#' Convenience function for computing the object's checksum and setting that slot
#'
#' This function computes the checksum and sets the checksum slot in the object,
#' then returns the modified object.  It just packages those up for convenience.
#'
#' @param obj object whose checksum is to be computed
#' @param base_outdir directory where temporary file for use in checksum
#'     computation will be written and then deleted
#'
#' @return  the same object that was passed in except that its checksum is
#'     now set
#' @export

compute_and_set_obj_checksum <- function (obj, base_outdir=".")
    {
    obj@checksum <- compute_obj_checksum (obj, base_outdir)

    return (obj)
    }

#===============================================================================




