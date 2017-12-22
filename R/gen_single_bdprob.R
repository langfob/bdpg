#===============================================================================

#' Generate a single biodiversity problem from scratch of from a file
#'
#===============================================================================

#' Generate a single correct Xu biodiversity problem
#'
#' @param base_prob_name_stem a character string
#' @param cor_dir_name_stem a character string
#' @inheritParams std_param_defns
#'
#' @return Returns a correct Xu biodiversity problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_COR <- function (parameters,
                                   bdpg_error_codes,
                                   integerize,
                                   base_prob_name_stem = "base_prob",
                                   cor_dir_name_stem = "cor"
                                   )
    {
    exp_root_dir = file.path (normalizePath (parameters$full_output_dir_with_slash))
    Xu_bench_infile_name = parameters$Xu_bench_infile_name
    if (is.null (Xu_bench_infile_name)) Xu_bench_infile_name = ""

    COR_Xu_bdprob =
        gen_single_bdprob_COR_from_scratch_or_Xu_bench_file (
            exp_root_dir,
            value_or_FALSE_if_null (parameters$compute_network_metrics_COR),
            parameters,
            value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file),
            Xu_bench_infile_name,
            value_or_FALSE_if_null (parameters$given_correct_solution_cost),
            parameters$max_allowed_num_spp,
            bdpg_error_codes,
            integerize,
            base_prob_name_stem = "base_prob",
            cor_dir_name_stem = "cor")

    return (COR_Xu_bdprob)
    }

#===============================================================================

#' Generate a Xu problem from scratch
#'
#' Generate a Xu biodiversity problem based on 4 input control parameters
#' rather than reading it from a file.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{base_Xu_params}{
#' \preformatted{
#' base_Xu_params : Formal class 'Xu_base_params' [package "bdpg"] with 4 slots
#' }}
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
#' \subsection{bdpg_extended_params}{
#' \preformatted{
#' bdpg_extended_params : Formal class 'Xu_bdpg_extended_params' [package "bdpg"] with 18 slots
#' }}
#' \subsection{dependent_node_IDs}{
#' \preformatted{
#' dependent_node_IDs :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{derived_Xu_params}{
#' \preformatted{
#' derived_Xu_params : Formal class 'Xu_derived_params' [package "bdpg"] with 13 slots
#' }}
#' \subsection{edge_list}{
#' \preformatted{
#' edge_list :  int [1:814, 1:2] 1 3 5 7 9 11 13 15 17 19 ...
#' }}
#' \subsection{integerize}{
#' \preformatted{
#' integerize : function (x, digits = 0)
#' }}
#' \subsection{max_allowed_num_spp}{
#' \preformatted{
#' max_allowed_num_spp :  num 2000
#' }}
#' \subsection{nodes}{
#' \preformatted{
#' nodes : 'data.frame':	122 obs. of  3 variables:
#'  $ node_ID             : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ group_ID            : num  1 1 2 2 3 3 4 4 5 5 ...
#'  $ dependent_set_member: logi  FALSE TRUE FALSE TRUE FALSE TRUE ...
#' }}
#' \subsection{num_PUs}{
#' \preformatted{
#' num_PUs :  int 122
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{PU_costs}{
#' \preformatted{
#' PU_costs :  num [1:122] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{PU_spp_pair_info}{
#' \preformatted{
#' PU_spp_pair_info : Formal class 'PU_spp_pair_info_class' [package "bdpg"] with 12 slots
#' }}
#' \subsection{Xu_parameters}{
#' \preformatted{
#' Xu_parameters : Formal class 'Xu_params' [package "bdpg"] with 3 slots
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns a PU_spp_pair_info_class object

#-------------------------------------------------------------------------------

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

#docaids::doc_vars_in_this_func_once ()
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
#' **NOTE that if other kinds of problems are created, this routine will have
#' to be replaced or cloned into something appropriate for the new problem
#' type.**
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{base_prob_name_stem}{
#' \preformatted{
#' base_prob_name_stem :  chr "base_prob"
#' }}
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
#' \subsection{bpm}{
#' \preformatted{
#' bpm :  num [1:814, 1:122] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{cor_dir_name_stem}{
#' \preformatted{
#' cor_dir_name_stem :  chr "cor"
#' }}
#' \subsection{given_correct_solution_cost}{
#' \preformatted{
#' given_correct_solution_cost :  num 420
#' }}
#' \subsection{Xu_bench_infile_name}{
#' \preformatted{
#' Xu_bench_infile_name :  chr ""
#' }}
#' \subsection{integerize}{
#' \preformatted{
#' integerize : function (x, digits = 0)
#' }}
#' \subsection{max_allowed_num_spp}{
#' \preformatted{
#' max_allowed_num_spp :  num 2000
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{PU_spp_pair_info}{
#' \preformatted{
#' PU_spp_pair_info : Formal class 'PU_spp_pair_info_class' [package "bdpg"] with 12 slots
#' }}
#' \subsection{read_Xu_problem_from_Xu_file}{
#' \preformatted{
#' read_Xu_problem_from_Xu_file :  logi FALSE
#' }}
#' \subsection{exp_root_dir}{
#' \preformatted{
#' exp_root_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{Xu_bdprob_cor}{
#' \preformatted{
#' Xu_bdprob_cor : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{Xu_prob_gen_info}{
#' \preformatted{
#' Xu_prob_gen_info : Formal class 'Xu_prob_gen_info_class' [package "bdpg"] with 3 slots
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns a new Xu_bd_problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_COR_from_scratch_or_Xu_bench_file <-
    function (exp_root_dir,
                compute_network_metrics_for_this_prob,
                parameters,
            read_Xu_problem_from_Xu_file,
            Xu_bench_infile_name,
                given_correct_solution_cost,
                max_allowed_num_spp,
                bdpg_error_codes,
                integerize,
            base_prob_name_stem = "base_prob",
            cor_dir_name_stem = "cor")
    {
    # forced_seed =
    #     get_forced_seed_value_if_necessary (is_rsrun = FALSE,
    #                                         is_rsprob = TRUE,
    #                                         parameters,
    #                                         cor_or_app = "COR",
    #                                         basic_or_wrapped_or_comb_str = "BASE")
    #
    # new_seed =
    #     set_new_or_forced_rand_seed_if_necessary (value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
    #                                               "Start of gen_single_bdprob_COR_from_scratch_or_Xu_bench_file(),COR,BASE",
    #                                               forced_seed)

    new_seed_list =
        set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
                                                  is_rsprob = TRUE,
                                                  parameters,
                                                  cor_or_app_str = "COR",
                                                  basic_or_wrapped_or_comb_str = "BASE",
                                                  location_string = "Start of gen_single_bdprob_COR_from_scratch_or_Xu_bench_file(),COR,BASE")

        #-------------------------------------------------------------------
        #  Determine whether to create the problem from scratch or read it
        #  from a Xu benchmark file, then create the problem.
        #  Load the information about the generation of the problem into
        #  an object to store with the full problem object.
        #-------------------------------------------------------------------

    if (read_Xu_problem_from_Xu_file)
        {
        PU_spp_pair_info =
            read_Xu_problem_from_Xu_file (Xu_bench_infile_name,
                                          given_correct_solution_cost)

        } else  #  Create Xu problem from scratch
        {
        PU_spp_pair_info =
            create_Xu_problem_from_scratch (max_allowed_num_spp,
                                            parameters,
                                            bdpg_error_codes,
                                            integerize)
        }  #  end - create Xu problem from scratch

        #---------------------------------------------------------
        #  Save information about how the problem was generated.
        #---------------------------------------------------------

    Xu_prob_gen_info = new ("Xu_prob_gen_info_class")

    Xu_prob_gen_info@read_Xu_problem_from_Xu_file = read_Xu_problem_from_Xu_file
    Xu_prob_gen_info@Xu_parameters                = PU_spp_pair_info@Xu_parameters
    Xu_prob_gen_info@infile_name                  = Xu_bench_infile_name

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
    Xu_bdprob_cor@rand_seed                        = new_seed_list$seed_value
    Xu_bdprob_cor@R_internal_seed_array            = new_seed_list$R_internal_seed_array

        #---------------------------------------------------------------
        #  Build file and dir naming prefix.
        #  This needs to be done before any directories are created
        #  for this problem.
        #---------------------------------------------------------------

    Xu_bdprob_cor@obj_type_str                     = "RSprob"
    Xu_bdprob_cor@basic_or_wrapped_or_comb_str     = "Base"
    Xu_bdprob_cor@cor_or_app_str                   = "COR"    #  "cor" or "app" - used in building file and dir names

    Xu_bdprob_cor@file_name_prefix =
                            paste (Xu_bdprob_cor@obj_type_str,
                                   Xu_bdprob_cor@cor_or_app_str,
                                   Xu_bdprob_cor@basic_or_wrapped_or_comb_str,
                                   sep='-')

        #------------------------------------------------------------------
        #  Save whatever information is known about the problem generator
        #  that produced this problem.
        #------------------------------------------------------------------

    Xu_bdprob_cor@prob_type = "Xu_prob_gen_info_class"
    Xu_bdprob_cor@prob_gen_info = Xu_prob_gen_info

    Xu_bdprob_cor@prob_generator_params_known      = PU_spp_pair_info@prob_generator_params_known
    Xu_bdprob_cor@correct_solution_vector_is_known = PU_spp_pair_info@correct_solution_vector_is_known

#    Xu_bdprob_cor@PU_spp_pair_indices       = PU_spp_pair_info@PU_spp_pair_indices
    Xu_bdprob_cor@cor_PU_spp_pair_indices       = PU_spp_pair_info@PU_spp_pair_indices

    Xu_bdprob_cor@all_PU_IDs                = 1:PU_spp_pair_info@num_PUs
    Xu_bdprob_cor@all_spp_IDs               = 1:PU_spp_pair_info@num_spp

    Xu_bdprob_cor@num_PUs                   = PU_spp_pair_info@num_PUs
    Xu_bdprob_cor@num_spp                   = PU_spp_pair_info@num_spp
    Xu_bdprob_cor@correct_solution_cost          = PU_spp_pair_info@correct_solution_cost
    Xu_bdprob_cor@PU_costs                  = PU_spp_pair_info@PU_costs

    Xu_bdprob_cor@nodes                     = PU_spp_pair_info@nodes

        #-----------------------------------------------------------
        #  Convert PU/spp data structure into other formats needed
        #  downstream.
        #-----------------------------------------------------------

    bpm =
        create_adj_matrix_with_spp_rows_vs_PU_cols (Xu_bdprob_cor@num_spp,
                                                    Xu_bdprob_cor@num_PUs,

#                                                    Xu_bdprob_cor@PU_spp_pair_indices,
                                                    Xu_bdprob_cor@cor_PU_spp_pair_indices,

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

        #  Create directory names.
        #  Creating as lists now to hold the place of storing these
        #  names somewhere rather than hard-coding them.
        #  Will figure out where to put them later.

#####
#  NOTE:  Have to be careful not to include anything unique here that will
#         mess up checksum comparisons, e.g., uuid.
#####


        #  Create directories for this problem.

    create_RSprob_dir_and_subdirs (exp_root_dir,  #  parameters$fullOutputDir_NO_slash,  #  usually parameters$fullOutputDir_NO_slash
                                   Xu_bdprob_cor)

        #-----------------------------------------------------------------
        #  Compute and save the distribution and network metrics for the
        #  problem.
        #-----------------------------------------------------------------

        #  Summarize and plot graph and distribution structure information.
    Xu_bdprob_cor@final_link_counts_for_each_node =
        summarize_and_plot_graph_and_distribution_structure_information (

#                  Xu_bdprob_cor@PU_spp_pair_indices,
                  Xu_bdprob_cor@cor_PU_spp_pair_indices,

                  "COR",
                  Xu_bdprob_cor@all_PU_IDs,    #####!!!!!#####all_correct_node_IDs,

#                  Xu_bdprob_cor@derived_bdpg_dir_names$plot_output_dir,
                  get_RSprob_path_plots (Xu_bdprob_cor, exp_root_dir),

                  Xu_bdprob_cor@spp_col_name,
                  Xu_bdprob_cor@PU_col_name,
                  Xu_bdprob_cor@presences_col_name
                  )
cat ("\n>>> gen_single_bdprob_COR_from_scratch_or_Xu_bench_file(), compute_network_metrics_for_this_prob = '",
     compute_network_metrics_for_this_prob, "'\n")
        #  Compute network metrics.
    Xu_bdprob_cor <- init_object_graph_data (Xu_bdprob_cor,
                                             exp_root_dir,
                                             parameters$compute_network_metrics,
                                #parameters$compute_network_metrics_COR,
                                compute_network_metrics_for_this_prob,
                                             parameters$use_igraph_metrics,
                                             parameters$use_bipartite_metrics,
                                             parameters$bipartite_metrics_to_use)

#     Xu_bdprob_cor@compute_network_metrics = parameters$compute_network_metrics_COR
#     if (parameters$compute_network_metrics_COR)
#         {
#         Xu_bdprob_cor@bipartite_metrics_from_bipartite_package =
#           compute_network_measures_using_bipartite_package (bpm)
#
#         Xu_bdprob_cor@bipartite_metrics_from_igraph_package_df =
#           compute_igraph_related_network_measures (
#                                     Xu_bdprob_cor@PU_spp_pair_indices,
#
# #                                    Xu_bdprob_cor@derived_bdpg_dir_names$network_output_dir,
#                                     get_RSprob_path_networks (Xu_bdprob_cor, exp_root_dir),
#
#                                     Xu_bdprob_cor@PU_col_name,
#                                     Xu_bdprob_cor@spp_col_name
#                                                     )
#         }

        #------------------------------------------------------------
        #  Everything seems to have worked.
        #  Save the bdprob to disk as a first cut at how to archive
        #  and retrieve problems in general.
        #  This particular bit of code may disappear later on, once
        #  it's clearer how to archive.
        #------------------------------------------------------------

    Xu_bdprob_cor@prob_is_ok = TRUE

    Xu_bdprob_cor <- save_rsprob (Xu_bdprob_cor, exp_root_dir)

#    save_rsprob_results_data_for_Xu_NOT_read_from_bench_file (Xu_bdprob_cor,
    save_rsprob_results_data (Xu_bdprob_cor, exp_root_dir, parameters)

#docaids::doc_vars_in_this_func_once ()
    return (Xu_bdprob_cor)
    }

#===============================================================================

