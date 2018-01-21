#===============================================================================

                        #  std_param_defns.R

#===============================================================================

#' Standardized parameter definitions for reuse across all functions
#'
#' This function should never be called.  It is intended strictly as a
#' place to assemble all @@param values that are to be inherited by other
#' functions in their Roxygen documentation.
#'
#' In general, when you're documenting a function's parameters using Roxygen and
#' the same parameter is defined for multiple routines, you're supposed to use
#' \code{@@inheritParams} and point to one other function as the single source
#' of that parameter's definition.  That doesn't work well for me because often,
#' the function that you want to inherit from:
#'      \enumerate{
#'       \item{has \emph{some} of the variables you want to inherit, and/or}
#'       \item{is \emph{missing} others, and/or}
#'       \item{has \emph{irrelevant} variables that you want to ignore, or}
#'       \item{has some \emph{combination} of the above and you have to inherit
#'             from multiple functions to get some subset of params from
#'             all of them and it becomes difficult to guess which param
#'             definition is drawn from which other function.}
#'       }
#'
#' There is no designation of what is \emph{the} canonical base function
#' for all copies of a particular \code{@@param} that gets inherited.
#' Consequently, if you don't realize that something depends on a function
#' used in an \code{@@inheritParams} command somewhere in your code
#' and you delete or rename the function, or delete or rename an inherited
#' argument, you can break downstream documentation.
#'
#' It would be better to have a designated data dictionary where all of these
#' standardized things can be assembled and all \code{@@inheritParams} can
#' point.  Since that doesn't exist, I'm making this dummy function that I will
#' use by convention as the one central place to copy parameter declarations
#' from when I want them.  However, the only way I can see to fool Roxygen into
#' doing this is by creating a dummy function that's never called and exists
#' solely to be the source of inherited parameter definitions.
#'
#' There are some potential problems with this:
#'      \itemize{
#'       \item{It may have performance problems when Roxygen documentation is
#'             built since the list of parameters here will be long.  I have
#'             no idea whether that will be a problem or not.}
#'       \item{It doesn't solve the problem in point 3 above, i.e., it will
#'             have \emph{tons} of irrelevant variables for nearly every
#'             function that inherits from it.  Again, I don't know if there
#'             are any consequences of that.  Roxygen does properly ignore the
#'             irrelevant variables I think, but it still makes for a lot of
#'             mental noise for the person doing the documenting when they're
#'             looking at all these other functions and trying to sort out what
#'             is relevant and what isn't.  Hopefully, doing it as one giant
#'             alphabetical list in just one place makes it a little easier...}
#'       \item{I \emph{think} that you can override a parameter definition
#'             locally if you want a different local definition but say that
#'             you want to inherit params (e.g., to satisfy other variables in
#'             your parameter list), but I'm not absolutely certain of that at
#'             the moment.  If that doesn't work, it may be that you just have
#'             to rename the variable locally to avoid the conflict.}
#'             }
#'
#-------------------------------------------------------------------------------

#' @section Conventions:
#' The main convention here will be to try to keep the parameters in
#' alphabetical order to make them easier to find and not duplicate.
#'
#-------------------------------------------------------------------------------

#' @section How to use:
#' \describe{
#'           \item{To inherit an \code{@@param} from here:}{In the Roxygen documentation
#'                 for any function where you want to use a parameter definition from
#'                 this list, you just include a statement saying:
#'
#'                 \code{#' @@inheritParams std_param_defns}
#'                }
#'           \item{To add a parameter \code{x} to the list of params in the list here:}{
#'                 \itemize{
#'                          \item{Insert the name \code{x} in alphabetical order
#'                                in the parameter list for the dummy function
#'                                \code{std_param_defns}.
#'                                }
#'                          \item{Insert "\code{@@param x blah blah blah}" in
#'                                alphabetical order in the list of \code{@@param}s in
#'                                the documentation for the dummy function
#'                                \code{std_param_defns}.
#'                                }
#'                          }
#'                }
#'          }
#'
#'
#-------------------------------------------------------------------------------

#' @param all_correct_node_IDs numeric vector containing all planning unit IDs
#'     that appear in the correct problem, not to be confused with the set of
#'     planning IDs that appear in the correct \emph{solution}
#' @param allow_imperfect_wrap boolean indicating whether the wrapping
#'     distribution needs to fully contain the base distribution; TRUE implies
#'     it must fully contain the base distribution and FALSE implies it need not
#'     fully contain the base distribution
#' @param APP_bd_prob an apparent Xu_bd_problem
#' @param base_outdir_for_checksum directory where temporary file for use in checksum
#'     computation will be written and then deleted
#' @param bdpg_error_name string containing name of error code to look up in
#'     list of error codes
#' @param bpm bipartite matrix; integer matrix with one row for each species
#'     and one column for each planning unit.  Each matrix entry specifies
#'     whether that species occupies that planning unit; 1 indicates the
#'     species does occupy the planning unit and 0 indicates it does not.
#'     Same as occ_matrix.
#' @param compute_network_metrics_for_this_prob boolean indicating whether to
#'     compute network metrics for the current problem; TRUE indicates to
#'     compute one or more network metrics specified elsewhere and FALSE
#'     indicate not compute any network metrics for the current problem.
#' @param COR_bd_prob a correct Xu_bd_problem
#' @param cor_or_app_label character string for labelling as Correct or Apparent
#' @param correct_solution_vector_is_known boolean flag indicating whether
#'     a correct optimal solution vector is known for the problem (as opposed
#'     for example, to only knowing the correct cost)
#' @param dependent_node_IDs integer vector of IDs of planning units contained
#'     in a constructed correct solution to a Xu problem
#' @param duplicate_links_allowed boolean indicating whether more than one link
#'     is allowed between the same two planning units, i.e., whether more than
#'     one species can occupy exactly the same pair of planning units; TRUE
#'     implies more than one allowed, FALSE implies only one allowed
#' @param edge_list two column integer matrix of node IDs with one row for
#'     each edge and columns for the 2 ends of the edge
#' @param exp_root_dir character string path to root directory of an
#'     experiment, usually parameters$fullOutputDir_NO_slash
#' @param final_link_counts_for_each_node integer vector of counts of number
#'     of links for each node, i.e., number of species for each planning unit
#' @param final_node_counts_for_each_link integer vector of counts of number
#'     of nodes for each link, i.e., number of planning units for each species
#'     (same as final_rank_abundance_dist)
#' @param final_rank_abundance_dist integer vector of counts of number
#'     of nodes for each link, i.e., number of planning units for each species
#'     (same as final_node_counts_for_each_link)
#' @param given_correct_solution_cost boolean indicating whether the correct
#'     optimal solution cost is given for a reserve selection problem
#' @param integerize function to use in converting floats to ints
#' @param integerize_string string containing name of the function to use to
#'     convert floats to integers
#' @param marxan_elapsed_time numeric value containing elapsed time for running
#'     marxan (in seconds)
#' @param marxan_solution data frame with one row for each planning unit.
#'    Has 2 columns, one for the planning unit IDs and the other
#'    for the count or indicator of presence/absence.
#'    The second column usually contains 0 or 1 to indicate presence
#'    or absence of that PU in the marxan solution.
#'    However, in the case of marxan's summed solution, the second
#'    column contains the number of iterations (restarts) where that
#'    planning unit appeared in marxan's solution.
#' @param max_allowed_num_spp integer maximum number of species allowed in the
#'     problem (to keep from generating problems that are too large when trying
#'     to keep the run-time down)
#' @param max_possible_tot_num_links integer maximum possible number of links
#'     in the Xu problem given all the other parameter settings
#' @param n__num_groups integer number of groups in the Xu problem, one of the
#'     4 base input parameters for specifying the Xu problem
#' @param nodes data frame containing integer node_ID, integer group_ID, and boolean dependent_set_member
#' @param num_nodes_per_group integer number of planning units per group in the
#'     Xu problem
#' @param num_PUs integer number of planning units
#' @param num_rounds_of_linking_between_groups integer number of rounds of
#'     linking to be done between groups in constructing the Xu problems
#' @param num_spp integer number of species in the problem
#' @param obj an object such as a Xu_bd_problem or an RSrun
#' @param obj_with_UUID_and_checksum object whose checksum is to be computed
#' @param occ_matrix occupancy matrix, integer matrix with one row for each
#'     species and one column for each planning unit.  Each matrix entry
#'     specifies whether that species occupies that planning unit; 1 indicates
#'     the species does occupy the planning unit and 0 indicates it does not.
#'     Same as bpm.
#' @param parameters parameters list for the run, usually derived from project.yaml
#'     and can have a varying number and set of elements depending on the run
#' @param plot_output_dir character string giving pat to directory where plot
#'     should be written
#' @param presences_col_name character string giving column name for species
#'     counts (presences) in data frames
#' @param PU_col_name character string giving planning unit column name in data frames
#' @param PU_costs numeric vector of planning unit costs
#' @param PU_set_to_test integer vector of planning unit IDs where each ID
#'     specifies a planning unit to include in the set to be tested
#' @param PU_spp_pair_indices 2 column data frame of PU IDs vs species IDs,
#'     where each row identifies a the ID of a given species that
#'     occurs on the given planning unit
#' @param read_Xu_problem_from_Xu_file boolean TRUE if problem is to be
#'     read from a benchmark problem description file created by Xu;
#'     FALSE if problem is to be created from scratch.
#' @param rsprob a reserve selection problem object, e.g., a Xu_bd_problem
#' @param rsrun an RSrun (reserve selection run) object, e.g., a marxan run
#' @param rs_method_name character string for reserve selection method, e.g.,
#'     "marxan_sa"
#' @param spp_col_name character string giving species column name in data frames
#' @param spp_rep_fracs numeric vector of fractions of species
#'     representation targets achieved by a given set of planning units
#' @param spp_rep_targets numeric vector with a target abundance for each
#'     species in the problem
#' @param spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU integer matrix of
#'     abundance of each species in each planning unit, with a row for each
#'     species ID and a column for each planning unit ID
#' @param src_rds_file_dir string containing full path of directory where
#'     source rds file is or was found
#' @param target_num_links_between_2_groups_per_round integer target number of
#'     links between any two groups per round of linking in constructing the
#'     Xu problem
#' @param top_dir character string giving the path to the top directory of the
#'     tree where the problem is written out, usually something like the tzar
#'     output directory of the experiment followed by some object-specific
#'     information and the UUID of the object, e.g.,
#'     ~/tzarout/RSprob-COR-Base.299140ae-3be2-4331-bb72-b24570902587
#' @param use_marxan_time_as_limit boolean indicating whether reserve selector
#'     should use marxan's elapsed run time as the time limit for its own run
#' @param wrap_lognormal_dist_around_Xu boolean indicating whether to wrap a
#'     lognormal distribution around a base Xu problem; TRUE implies wrapping
#'     should be done; FALSE implies not
#' @param Xu_bench_infile_name string containing the name of the Xu benchmark file to
#'     read a problem from if reading from a Xu benchmark file

#-------------------------------------------------------------------------------

std_param_defns <-
    function (
            all_correct_node_IDs,
            allow_imperfect_wrap,
            APP_bd_prob,
            base_outdir_for_checksum,
            bdpg_error_name,
            bpm,
            compute_network_metrics_for_this_prob,
            COR_bd_prob,
            cor_or_app_label,
            correct_solution_vector_is_known,
            dependent_node_IDs,
            duplicate_links_allowed,
            edge_list,
            exp_root_dir,
            final_link_counts_for_each_node,
            final_node_counts_for_each_link,
            final_rank_abundance_dist,
            given_correct_solution_cost,
            integerize,
            integerize_string,
            marxan_elapsed_time,
            marxan_solution,
            max_allowed_num_spp,
            max_possible_tot_num_links,
            n__num_groups,
            nodes,
            num_nodes_per_group,
            num_PUs,
            num_rounds_of_linking_between_groups,
            num_spp,
            obj,
            obj_with_UUID_and_checksum,
            occ_matrix,
            parameters,
            plot_output_dir,
            presences_col_name,
            PU_col_name,
            PU_costs,
            PU_set_to_test,
            PU_spp_pair_indices,
            read_Xu_problem_from_Xu_file,
            rsprob,
            rsrun,
            rs_method_name,
            spp_col_name,
            spp_rep_fracs,
            spp_rep_targets,
            spp_rows_by_PU_cols_matrix_of_spp_cts_per_PU,
            src_rds_file_dir,
            target_num_links_between_2_groups_per_round,
            top_dir,
            use_marxan_time_as_limit,
            wrap_lognormal_dist_around_Xu,
            Xu_bench_infile_name
             )
    {
    stop (paste0 ("\n\nstd_param_defns() is not meant to be called.  ",
                  "\n    Exist solely to provide standardized parameter ",
                  "definitions to inherit @param statements from.\n\n"))

    return ("Does nothing")
    }

#===============================================================================

