#===============================================================================

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param max_allowed_num_spp DESCRIPTION.
#' @param parameters DESCRIPTION.
#' @param bdpg_error_codes DESCRIPTION.
#' @param integerize DESCRIPTION.
#' @param DEBUG_LEVEL DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @export

create_Xu_problem_from_scratch <- function (max_allowed_num_spp,
                                            parameters,
                                            bdpg_error_codes,
                                            integerize,
                                            DEBUG_LEVEL)
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
                       DEBUG_LEVEL
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
        cat ("\n\nQuitting:  num_spp (", num_spp, ") > maximum allowed (",
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
#' @param read_Xu_problem_from_Xu_file boolean TRUE if problem is to be
#'     read from a file created by Xu; FALSE if problem is to be created from
#'     scratch.
#' @param infile_name DESCRIPTION.
#' @param given_correct_solution_cost DESCRIPTION.
#' @param max_allowed_num_spp DESCRIPTION.
#' @param bdpg_error_codes DESCRIPTION.
#' @param integerize DESCRIPTION.
#' @param DEBUG_LEVEL DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @export
#' @examples
#' # ADD_EXAMPLES_HERE
gen_single_bdprob = function (parameters,
                              read_Xu_problem_from_Xu_file,
                              infile_name,
                              given_correct_solution_cost,
                              max_allowed_num_spp,
                              bdpg_error_codes,
                              integerize,
                              DEBUG_LEVEL)
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
                                            integerize,
                                            DEBUG_LEVEL)
        }  #  end - create Xu problem from scratch

    #===============================================================================

        #------------------------------------------------------------
        #  Save data known so far for the newly created Xu problem.
        #------------------------------------------------------------

    Xu_bdprob = new ("Xu_bd_problem")

    Xu_bdprob@prob_is_ok                       = FALSE
    Xu_bdprob@read_Xu_problem_from_Xu_file     = read_Xu_problem_from_Xu_file
    Xu_bdprob@prob_generator_params_known      = PU_spp_pair_info@prob_generator_params_known
    Xu_bdprob@correct_solution_vector_is_known = PU_spp_pair_info@correct_solution_vector_is_known

    Xu_bdprob@PU_spp_pair_indices       = PU_spp_pair_info@PU_spp_pair_indices
    Xu_bdprob@all_PU_IDs                = 1:PU_spp_pair_info@num_PUs
    Xu_bdprob@all_spp_IDs               = 1:PU_spp_pair_info@num_spp

    Xu_bdprob@PU_col_name               = PU_spp_pair_info@PU_col_name
    Xu_bdprob@spp_col_name              = PU_spp_pair_info@spp_col_name
    Xu_bdprob@num_PUs                   = PU_spp_pair_info@num_PUs
    Xu_bdprob@num_spp                   = PU_spp_pair_info@num_spp
    Xu_bdprob@cor_optimum_cost          = PU_spp_pair_info@correct_solution_cost
    Xu_bdprob@PU_costs                  = PU_spp_pair_info@PU_costs
    Xu_bdprob@Xu_parameters             = PU_spp_pair_info@Xu_parameters
    Xu_bdprob@nodes                     = PU_spp_pair_info@nodes

        #-----------------------------------------------------------
        #  Convert PU/spp data structure into other formats needed
        #  downstream.
        #-----------------------------------------------------------

    bpm =
        create_adj_matrix_with_spp_rows_vs_PU_cols (Xu_bdprob@num_spp,
                                                    Xu_bdprob@num_PUs,
                                                    Xu_bdprob@PU_spp_pair_indices,
                                            Xu_bdprob@PU_costs,
                                                    Xu_bdprob@spp_col_name,
                                                    Xu_bdprob@PU_col_name,
                                                    PU_spp_pair_info@dependent_node_IDs,
                                                    PU_spp_pair_info@correct_solution_vector_is_known,
                                                    bdpg_error_codes,
                                                    DEBUG_LEVEL)

        #-------------------------------------------------------------
        #  Quit if there are any duplicate edges/spp in the problem.
        #-------------------------------------------------------------

    see_if_there_are_any_duplicate_links (bpm, Xu_bdprob@num_spp, bdpg_error_codes)

        #---------------------------------------------------------------
        #  No duplicates found.
        #  Assign a unique identifier to this newly generated problem.
        #  These IDs are useful when combining or adding error to
        #  problems so that you can identify exactly which problems
        #  were combined or used as a base when provenance might get
        #  confusing.
        #---------------------------------------------------------------

    Xu_bdprob@UUID = uuid::UUIDgenerate()

    Xu_bdprob@prob_is_ok = TRUE
    Xu_bdprob@bpm = bpm

    return (Xu_bdprob)
    }

#===============================================================================


