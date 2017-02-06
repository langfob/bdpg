#===============================================================================

create_base_dir_structure <- function (base_outdir)
    {
        #  Create list of directory names.
    derived_bdpg_dir_names = list()

        #  Create PLOT OUTPUT directory.
    derived_bdpg_dir_names$plot_output_dir = file.path (base_outdir, "plots")
    dir.create (derived_bdpg_dir_names$plot_output_dir,
                showWarnings = TRUE, recursive = TRUE)

        #  Create NETWORK OUTPUT directory.
    derived_bdpg_dir_names$network_output_dir = file.path (base_outdir,
                                                           "networks")
    dir.create (derived_bdpg_dir_names$network_output_dir,
                showWarnings = TRUE, recursive = TRUE)

        #  Create RES_SEL directory.
    derived_bdpg_dir_names$res_sel_dir = file.path (base_outdir, "res_sel")
    dir.create (derived_bdpg_dir_names$res_sel_dir,
                showWarnings = TRUE, recursive = TRUE)

    return (derived_bdpg_dir_names)
    }

#===============================================================================

create_one_res_sel_dir_structure <- function (res_sel_dir_name = "marxan",
                                              base_outdir = ".")
    {
    res_sel_dir_names = vector ("list", 2)
    names (res_sel_dir_names) <- c("input_dir", "output_dir")

        #  Clean up base_outdir into usable path if necessary,
        #  e.g., change "." or "~somebody/x" into fully qualified path name.
    base_outdir = normalizePath (base_outdir)

        #  Create reserve selector INPUT directory.
    res_sel_dir_names$input_dir = file.path (base_outdir, "input")
    dir.create (res_sel_dir_names$input_dir,
                showWarnings = TRUE, recursive = TRUE)

        #  Create reserve selector OUTPUT directory.
    res_sel_dir_names$output_dir = file.path (base_outdir, "output")
    dir.create (res_sel_dir_names$output_dir,
                showWarnings = TRUE, recursive = TRUE)

    return (res_sel_dir_names)
    }

#===============================================================================

create_multiple_res_sel_dir_structures <- function (res_sel_dir_names,
                                                    base_outdir)
    {
    res_sel_dir_structures <- lapply (res_sel_dir_names,
                                      create_one_res_sel_dir_structure,
                                      base_outdir)

    names (res_sel_dir_structures) <- res_sel_dir_names

    return (res_sel_dir_structures)
    }

#===============================================================================

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param max_allowed_num_spp DESCRIPTION.
#' @param parameters DESCRIPTION.
#' @param bdpg_error_codes DESCRIPTION.
#' @param integerize DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @export

create_Xu_problem_from_scratch <- function (max_allowed_num_spp,
                                            parameters,
                                            bdpg_error_codes,
                                            integerize)
    {
        #----------------------------------------------------------
        #  The 4 basic Xu parameters describe problem attributes
        #  related to the theory in the original Xu paper, but
        #  they don't directly specify some of the specifics
        #  necessary to create and populate the problem data
        #  structures, e.g., various node and link counts.
        #  Derive those specific parameters from the base problem
        #  descriptors now.
        #----------------------------------------------------------

    Xu_parameters =
      derive_Xu_control_parameters (parameters,
                                    bdpg_error_codes,
                                    integerize)

    derived_Xu_params    = Xu_parameters@derived_params
    base_Xu_params       = Xu_parameters@base_params
    bdpg_extended_params = Xu_parameters@bdpg_extended_params

    #-------------------------------------------------------------------------------

      #-----------------------------------------------------------
      #  Now that specific problem attributes have been derived,
      #  create and load nodes and edges data structures.
      #-----------------------------------------------------------

    nodes = create_nodes_data_structure (derived_Xu_params@tot_num_nodes,
                                         derived_Xu_params@num_nodes_per_group,
                                         base_Xu_params@n__num_groups,
                                         bdpg_extended_params@num_independent_nodes_per_group
                                        )
    edge_list =
      create_Xu_graph (derived_Xu_params@num_nodes_per_group,
                       base_Xu_params@n__num_groups,
                       nodes,
                       derived_Xu_params@max_possible_tot_num_links,
                       derived_Xu_params@target_num_links_between_2_groups_per_round,
                       derived_Xu_params@num_rounds_of_linking_between_groups,
                       duplicate_links_allowed=FALSE,
                       bdpg_error_codes
                       )

    dependent_node_IDs = get_dependent_node_IDs (nodes)
    num_PUs = get_num_nodes (nodes)
    PU_costs = get_PU_costs (num_PUs)

    #-------------------------------------------------------------------------------

        #-------------------------------------------------------------
        #  Combine the information in the nodes and edge_list data
        #  structures into a single data structure that has one line
        #  for each species on each planning unit (where species
        #  are edges and PUs are nodes).
        #-------------------------------------------------------------

    PU_spp_pair_info = create_PU_spp_pair_indices (edge_list,
                                                    nodes,
                                                    dependent_node_IDs,
                                                    PU_costs,
                                                    num_PUs)

    #-------------------------------------------------------------------------------

    PU_spp_pair_info@Xu_parameters = Xu_parameters
    PU_spp_pair_info@correct_solution_vector_is_known = TRUE
    PU_spp_pair_info@dependent_node_IDs = dependent_node_IDs
    PU_spp_pair_info@nodes = nodes

    PU_spp_pair_info@prob_generator_params_known = TRUE
    PU_spp_pair_info@PU_costs = PU_costs

    #-------------------------------------------------------------------------------

    if (PU_spp_pair_info@num_spp > max_allowed_num_spp)
        {
        cat ("\n\nQuitting:  PU_spp_pair_info@num_spp (",
             PU_spp_pair_info@num_spp, ") > maximum allowed (",
             max_allowed_num_spp, ").\n\n")  #parameters$max_allowed_num_spp, ").\n\n")

            #------------------------------------------------------------------
            #  Even though it's an illegal number of species and you're
            #  quitting, echo the attributes of the problem that was created.
            #------------------------------------------------------------------

        cur_result_row = 1
        write_abbreviated_results_to_files (
                cur_result_row,
                parameters,
                PU_spp_pair_info@num_PUs,
                PU_spp_pair_info@num_spp,
                base_Xu_params$n__num_groups,
                base_Xu_params$alpha__,
                base_Xu_params$p__prop_of_links_between_groups,
                base_Xu_params$r__density,
                derived_Xu_params$num_nodes_per_group,
                derived_Xu_params$tot_num_nodes,
                derived_Xu_params$num_independent_set_nodes,
                derived_Xu_params$num_dependent_set_nodes,
                derived_Xu_params$num_rounds_of_linking_between_groups,
                derived_Xu_params$target_num_links_between_2_groups_per_round,
                derived_Xu_params$num_links_within_one_group,
                derived_Xu_params$tot_num_links_inside_groups,
                derived_Xu_params$max_possible_num_links_between_groups,
                derived_Xu_params$max_possible_tot_num_links
                )

        }  #  end if - num_spp > max_allowed_num_spp

    return (PU_spp_pair_info)
    }

#===============================================================================

    #-----------------------------------------------------------------
    #  Read a Xu problem from files of ones already created by Xu or
    #  create one from scratch.
    #-----------------------------------------------------------------

#' Generate a single biodiversity problem
#'
#' Read a Xu problem from files of ones already created by Xu or
#' create one from scratch.
#'
#' @param parameters named list of all parameters, generally from project.yaml file
#' @param read_Xu_problem_from_Xu_file boolean TRUE if problem is to be
#'     read from a file created by Xu; FALSE if problem is to be created from
#'     scratch.
#' @param infile_name DESCRIPTION.
#' @param given_correct_solution_cost DESCRIPTION.
#' @param max_allowed_num_spp DESCRIPTION.
#' @param bdpg_error_codes DESCRIPTION.
#' @param integerize DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @export

gen_single_bdprob_COR = function (parameters,
                              read_Xu_problem_from_Xu_file,
                              infile_name,
                              given_correct_solution_cost,
                              max_allowed_num_spp,
                              bdpg_error_codes,
                              integerize)
    {
    if (read_Xu_problem_from_Xu_file)
        {
        PU_spp_pair_info =
            read_Xu_problem_from_Xu_file (infile_name,
                                          given_correct_solution_cost)

        } else  #  Create Xu problem from scratch
        {
        PU_spp_pair_info =
            create_Xu_problem_from_scratch (max_allowed_num_spp,
                                            parameters,
                                            bdpg_error_codes,
                                            integerize)
        }  #  end - create Xu problem from scratch

    #===============================================================================

        #------------------------------------------------------------
        #  Save data known so far for the newly created Xu problem.
        #------------------------------------------------------------

    Xu_bdprob_cor = new ("Xu_bd_problem")

        #---------------------------------------------------------------
        #  Assign a unique identifier to this newly generated problem.
        #  These IDs are useful when combining or adding error to
        #  problems so that you can identify exactly which problems
        #  were combined or used as a base when provenance might get
        #  confusing.
        #---------------------------------------------------------------

    Xu_bdprob_cor@UUID = uuid::UUIDgenerate()


    Xu_bdprob_cor@prob_is_ok                       = FALSE
    Xu_bdprob_cor@read_Xu_problem_from_Xu_file     = read_Xu_problem_from_Xu_file
    Xu_bdprob_cor@prob_generator_params_known      = PU_spp_pair_info@prob_generator_params_known
    Xu_bdprob_cor@correct_solution_vector_is_known = PU_spp_pair_info@correct_solution_vector_is_known

    Xu_bdprob_cor@PU_spp_pair_indices       = PU_spp_pair_info@PU_spp_pair_indices
    Xu_bdprob_cor@all_PU_IDs                = 1:PU_spp_pair_info@num_PUs
    Xu_bdprob_cor@all_spp_IDs               = 1:PU_spp_pair_info@num_spp

    Xu_bdprob_cor@PU_col_name               = PU_spp_pair_info@PU_col_name
    Xu_bdprob_cor@spp_col_name              = PU_spp_pair_info@spp_col_name
    Xu_bdprob_cor@num_PUs                   = PU_spp_pair_info@num_PUs
    Xu_bdprob_cor@num_spp                   = PU_spp_pair_info@num_spp
    Xu_bdprob_cor@cor_optimum_cost          = PU_spp_pair_info@correct_solution_cost
    Xu_bdprob_cor@PU_costs                  = PU_spp_pair_info@PU_costs
    Xu_bdprob_cor@Xu_parameters             = PU_spp_pair_info@Xu_parameters
    Xu_bdprob_cor@nodes                     = PU_spp_pair_info@nodes

        #-----------------------------------------------------------
        #  Convert PU/spp data structure into other formats needed
        #  downstream.
        #-----------------------------------------------------------

    bpm =
        create_adj_matrix_with_spp_rows_vs_PU_cols (Xu_bdprob_cor@num_spp,
                                                    Xu_bdprob_cor@num_PUs,
                                                    Xu_bdprob_cor@PU_spp_pair_indices,
                                            Xu_bdprob_cor@PU_costs,
                                                    Xu_bdprob_cor@spp_col_name,
                                                    Xu_bdprob_cor@PU_col_name,
                                                    PU_spp_pair_info@dependent_node_IDs,
                                                    PU_spp_pair_info@correct_solution_vector_is_known,
                                                    bdpg_error_codes)

    Xu_bdprob_cor@bpm = bpm

        #-------------------------------------------------------------
        #  Quit if there are any duplicate edges/spp in the problem.
        #-------------------------------------------------------------

    see_if_there_are_any_duplicate_links (bpm, Xu_bdprob_cor@num_spp, bdpg_error_codes)

        #-----------------------------------------------------------
        #  No duplicates found.
        #  Create the basic set of directories for problem output.
        #-----------------------------------------------------------

    Xu_bdprob_cor@base_outdir =
        file.path (normalizePath (parameters$fullOutputDirWithSlash), "cor")
    dir.create (Xu_bdprob_cor@base_outdir, showWarnings = TRUE,
                recursive = TRUE)

    Xu_bdprob_cor@derived_bdpg_dir_names =
        create_base_dir_structure (Xu_bdprob_cor@base_outdir)

        #-----------------------------------------------------------------
        #  Compute and save the distribution and network metrics for the
        #  problem.
        #-----------------------------------------------------------------

        #  Summarize and plot graph and distribution structure information.
    Xu_bdprob_cor@final_link_counts_for_each_node =
        summarize_and_plot_graph_and_distribution_structure_information (
                  Xu_bdprob_cor@PU_spp_pair_indices,
                  "cor",
                  Xu_bdprob_cor@all_PU_IDs,    #####!!!!!#####all_correct_node_IDs,
                  Xu_bdprob_cor@derived_bdpg_dir_names$plot_output_dir,
                  Xu_bdprob_cor@spp_col_name,
                  Xu_bdprob_cor@PU_col_name,
                  Xu_bdprob_cor@presences_col_name
                  )

        #  Compute network metrics.
    if (parameters$compute_network_metrics)
        {
        Xu_bdprob_cor@bipartite_metrics_from_bipartite_package =
          compute_network_measures_using_bipartite_package (bpm)

        Xu_bdprob_cor@bipartite_metrics_from_igraph_package_df =
          compute_igraph_related_network_measures (
                                    Xu_bdprob_cor@PU_spp_pair_indices,
                                    Xu_bdprob_cor@derived_bdpg_dir_names$network_output_dir,
                                    Xu_bdprob_cor@PU_col_name,
                                    Xu_bdprob_cor@spp_col_name
                                                    )
        }

        #--------------------------------------------------------------
        #  Everything seems to have worked.
        #  Save the bdprob to disk as a test for how I might archive
        #  and retrieve problems in general.
        #  This particular bit of code may disappear later on, once I
        #  decide how to archive.
        #--------------------------------------------------------------

    Xu_bdprob_cor@prob_is_ok = TRUE

    Xu_bdprob_cor@full_saved_bdprob_path =
        save_bdprob ("basic", "COR", Xu_bdprob_cor@UUID,
                     Xu_bdprob_cor@base_outdir, Xu_bdprob_cor)


#     saved_bdprob_filename = paste0 ("saved_basic_bdprob.",
#                                     Xu_bdprob_cor@UUID,
#                                     ".COR.rds")
#     Xu_bdprob_cor@full_saved_bdprob_path =
#         file.path (Xu_bdprob_cor@base_outdir, saved_bdprob_filename)
#
#     saveRDS (Xu_bdprob_cor, Xu_bdprob_cor@full_saved_bdprob_path)
# #    reloaded_bdprob = readRDS (saved_bdprob_filename)    #  testing only
#
#     cat ("\n\n>>>>> Xu_bdprob_cor saved to: \n    '",
#          Xu_bdprob_cor@full_saved_bdprob_path, "'",
#          "\nTo reload problem, use readRDS ()\n\n", sep='')

    return (Xu_bdprob_cor)
    }

#===============================================================================

