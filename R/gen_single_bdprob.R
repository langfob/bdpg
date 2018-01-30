#===============================================================================

#                            gen_single_bdprob.R

#===============================================================================

#' Generate a single correct biodiversity problem from scratch or from a file
#'
#' @param base_prob_name_stem a character string
#' @param cor_dir_name_stem a character string
#' @inheritParams std_param_defns
#'
#' @return Returns a correct Xu biodiversity problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_COR <- function (parameters,
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
            parameters,
            value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file),
            Xu_bench_infile_name,
            value_or_FALSE_if_null (parameters$given_correct_solution_cost),
            parameters$max_allowed_num_spp,
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
#' @param max_allowed_num_spp integer

#' @inheritParams std_param_defns
#'
#' @return Returns a PU_spp_pair_info_class object

#' @family interfaces to creation of Xu problems

#' @export

#-------------------------------------------------------------------------------

create_Xu_problem_from_scratch <- function (max_allowed_num_spp,
                                            parameters,
                                            integerize)
    {
    duplicate_links_allowed =
        value_or_FALSE_if_null (parameters$duplicate_links_allowed)

    dont_derive_prob_params_from_4_Xu_metaparams =
        value_or_FALSE_if_null (parameters$dont_derive_prob_params_from_4_Xu_metaparams)

    if (dont_derive_prob_params_from_4_Xu_metaparams)
        {
        PU_spp_pair_info =
            create_Xu_problem_from_scratch_not_using_4_Xu_metaparams (max_allowed_num_spp,
                                                                      duplicate_links_allowed,
                                                                      parameters,
                                                                      integerize)
        } else
        {
        PU_spp_pair_info =
            create_Xu_problem_from_scratch_given_4_Xu_metaparams (max_allowed_num_spp,
                                                                  duplicate_links_allowed,
                                                                  parameters,
                                                                  integerize)
        }

    return (PU_spp_pair_info)
    }

#===============================================================================

#' Generate a Xu problem from scratch
#'
#' Generate a Xu biodiversity problem based on 4 input control parameters
#' rather than reading it from a file.
#'
#' @param max_allowed_num_spp integer
#' @param duplicate_links_allowed boolean (defaults to FALSE)

#' @inheritParams std_param_defns
#'
#' @return Returns a PU_spp_pair_info_class object

#' @family interfaces to creation of Xu problems

#' @export

#-------------------------------------------------------------------------------

create_Xu_problem_from_scratch_given_4_Xu_metaparams <-
                                                function (max_allowed_num_spp,
                                                          duplicate_links_allowed,
                                                          parameters,
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
      derive_Xu_control_parameters (parameters, integerize)

    #-------------------------------------------------------------------------------

    derived_Xu_params    = Xu_parameters@derived_params
    base_Xu_params       = Xu_parameters@base_params
    bdpg_extended_params = Xu_parameters@bdpg_extended_params

    #-------------------------------------------------------------------------------

    PU_spp_pair_info =
        create_Xu_problem_from_scratch_given_params (

                 derived_Xu_params@tot_num_nodes,
                 derived_Xu_params@num_nodes_per_group,
                    base_Xu_params@n__num_groups,
              bdpg_extended_params@num_independent_nodes_per_group,
                 derived_Xu_params@max_possible_tot_num_links,
                 derived_Xu_params@target_num_links_between_2_groups_per_round,
                 derived_Xu_params@num_rounds_of_linking_between_groups,
                                   duplicate_links_allowed,
                                                    max_allowed_num_spp,
                                                    parameters,
                                                    integerize)

    PU_spp_pair_info@Xu_parameters = Xu_parameters

    return (PU_spp_pair_info)
    }

#===============================================================================

#' create_Xu_problem_from_scratch_not_using_4_Xu_metaparams
#'
#' Generate a Xu biodiversity problem NOT based on 4 input control parameters
#' rather than reading it from a file.
#'
#' @param max_allowed_num_spp integer
#' @param duplicate_links_allowed boolean (defaults to FALSE)

#' @inheritParams std_param_defns
#'
#' @return Returns a PU_spp_pair_info_class object

#' @family interfaces to creation of Xu problems

#' @export

#-------------------------------------------------------------------------------

create_Xu_problem_from_scratch_not_using_4_Xu_metaparams <- function (max_allowed_num_spp,
                                                                      duplicate_links_allowed,
                                                                      parameters,
                                                                      integerize)
    {
    tot_num_nodes                               = parameters$tot_num_nodes
    num_nodes_per_group                         = parameters$num_nodes_per_group
    n__num_groups                               = parameters$n__num_groups
    num_independent_nodes_per_group             = parameters$num_independent_nodes_per_group
    max_possible_tot_num_links                  = parameters$max_possible_tot_num_links
    target_num_links_between_2_groups_per_round = parameters$target_num_links_between_2_groups_per_round
    num_rounds_of_linking_between_groups        = parameters$num_rounds_of_linking_between_groups

    PU_spp_pair_info =
        create_Xu_problem_from_scratch_given_params (
            tot_num_nodes,
            num_nodes_per_group,
            n__num_groups,
            num_independent_nodes_per_group,
            max_possible_tot_num_links,
            target_num_links_between_2_groups_per_round,
            num_rounds_of_linking_between_groups,

            duplicate_links_allowed,
            max_allowed_num_spp,
            parameters,
            integerize)

    return (PU_spp_pair_info)
    }

#===============================================================================

#' Generate a Xu problem from scratch given parameters
#'
#' Generate a Xu biodiversity problem based on a set of specific parameters
#' (not necessarily derived from derive_Xu_control_parameters()) passed in
#' rather than reading the problem from a file.  The purpose of this function
#' is to do the main work of building the problem once parameters have been
#' chosen.  It allows dependency injection for testing and it allows for
#' experiments that want to bypass the original Xu indirect creation pathway
#' that depends on the 4 values n, alpha, p, and r.
#'
#' The 4 original Xu parameters are not necessary for deriving a problem and
#' its correct answer.  It is the values derived from them that are necessary
#' for building the problem (e.g., number of dependent nodes, etc.).  The code
#' in this function is where the actual building of the problem occurs.
#'
#' In some cases, it may be more desirable in experimental setup to manipulate
#' the derived values directly, e.g., to make sure that a problem ends up
#' having a specific number of planning units or species, etc.
#' The 4 original parameters are most useful in theoretically motivating and
#' predicting where to look for problems with a given level of difficulty.
#' Once we can see the different regions of problem structure implied by
#' combinations of those parameters, then the values derived from them may be
#' easier to manipulate in search  algorithms and explanations, etc.

#' @param tot_num_nodes integer
#' @param num_nodes_per_group integer
#' @param n__num_groups integer
#' @param num_independent_nodes_per_group integer
#' @param max_possible_tot_num_links integer
#' @param target_num_links_between_2_groups_per_round integer
#' @param num_rounds_of_linking_between_groups integer
#' @param duplicate_links_allowed boolean (defaults to FALSE)
#'
#' @inheritParams std_param_defns
#'
#' @return Returns a PU_spp_pair_info_class object
#' @family interfaces to creation of Xu problems

#' @export

#-------------------------------------------------------------------------------

create_Xu_problem_from_scratch_given_params <-
    function (
              tot_num_nodes,
              num_nodes_per_group,
              n__num_groups,
              num_independent_nodes_per_group,
              max_possible_tot_num_links,
              target_num_links_between_2_groups_per_round,
              num_rounds_of_linking_between_groups,
              duplicate_links_allowed,
                                            max_allowed_num_spp,
                                            parameters,
                                            integerize)
    {
      #-----------------------------------------------------------
      #  Now that specific problem attributes have been derived,
      #  create and load nodes and edges data structures.
      #-----------------------------------------------------------

    nodes = create_nodes_data_structure (tot_num_nodes,
                                         num_nodes_per_group,
                                         n__num_groups,
                                         num_independent_nodes_per_group
                                        )
    edge_list =
      create_Xu_graph (num_nodes_per_group,
                       n__num_groups,
                       nodes,
                       max_possible_tot_num_links,
                       target_num_links_between_2_groups_per_round,
                       num_rounds_of_linking_between_groups,
                       duplicate_links_allowed)

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

    PU_spp_pair_info@correct_solution_vector_is_known = TRUE
    PU_spp_pair_info@dependent_node_IDs = dependent_node_IDs
    PU_spp_pair_info@nodes = nodes

        #----------------------------------------------------------------------
        #  2018 01 04 - BTL
        #  This may need to be modified if downstream uses of it (e.g.,
        #  in writing results files) assume that this means ALL of the
        #  parameters are known.  Changes now allow you to skip
        #  the Xu generation and parameter derivation and pass the derived
        #  values in directly and this means that things like alpha may
        #  no longer be known.
        #  Might need to split this boolean variable into 2 or 3 variables
        #  to indicate exactly which things ARE known and unknown.
        #  May also just want to make sure that there are usable default
        #  values for the unknown things when downstream needs to know them.
        #  If the only downstream use is for printing things out, then just
        #  having them set to NA may be fine - though this could also cause
        #  some issues if trying to fit a predictive model to the generator
        #  parameters and some of them are NA.
        #----------------------------------------------------------------------

    PU_spp_pair_info@prob_generator_params_known = TRUE

    PU_spp_pair_info@PU_costs = PU_costs

    return (PU_spp_pair_info)
    }

#===============================================================================

#' Generate a Xu problem from scratch but insure it's less than a given size
#'
#' Generate a Xu biodiversity problem based on 4 input control parameters
#' rather than reading it from a file.
#'
#' @param default_num_prob_size_retries_allowed integer number of retries of
#'     problem creation to try to find one no larger than a given number of
#'     species
#' @inheritParams std_param_defns
#'
#' @return Returns a PU_spp_pair_info_class object or quits if number of
#'     allowed retries is exceeded

#-------------------------------------------------------------------------------

create_allowable_size_Xu_problem_from_scratch <- function (
        max_allowed_num_spp,
        parameters,
        integerize,
        default_num_prob_size_retries_allowed = 20)
    {
        #-----------------------------------------------------------------------
        #  If the parameters list didn't specify a number of retries to allow,
        #  then use a default value.
        #-----------------------------------------------------------------------

    num_prob_size_retries_allowed = vn (parameters$num_prob_size_retries_allowed,
                                        def_on_empty = TRUE,
                                        def = default_num_prob_size_retries_allowed,
                                        range_lo = 0, bounds_types = "ii")

        #-----------------------------------------------------------------------
        #  Create a problem and check its size.
        #  If too big, try again unless you've now reached the allowed number
        #  of retries.
        #  If not too big, then return the problem's information.
        #-----------------------------------------------------------------------

    if (is.null (max_allowed_num_spp)) max_allowed_num_spp = Inf

    keep_trying = TRUE
    try_num = 1
    while (keep_trying)
        {
        PU_spp_pair_info =
            create_Xu_problem_from_scratch (max_allowed_num_spp,
                                            parameters,
                                            integerize)

        if (PU_spp_pair_info@num_spp > max_allowed_num_spp)
            {
            cat ("\n\nAt prob size try '", try_num,
                 "': PU_spp_pair_info@num_spp (",
                 PU_spp_pair_info@num_spp, ") > maximum allowed (",
                 max_allowed_num_spp, ").\n\n")  #parameters$max_allowed_num_spp, ").\n\n")

            try_num = try_num + 1
            if (try_num > num_prob_size_retries_allowed)
                keep_trying = FALSE

            }  else  #  Got a legal sized problem so quit looking for one
            {
            keep_trying = FALSE
            }
        }  #  end - while (keep_trying)

    return (PU_spp_pair_info)
    }

#===============================================================================

#' Generate a single biodiversity problem
#'
#' Read a Xu problem from files of ones already created by Xu or
#' create one from scratch.
#'
#' **NOTE that if other kinds of problems are created, this routine will have
#' to be replaced or cloned into something appropriate for the new problem
#' type.**
#'
#' @inheritParams std_param_defns

#' @param base_prob_name_stem character string
#' @param cor_dir_name_stem character string
#'
#' @return Returns a new Xu_bd_problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_COR_from_scratch_or_Xu_bench_file <-
    function (exp_root_dir,
                parameters,
            read_Xu_problem_from_Xu_file,
            Xu_bench_infile_name,
                given_correct_solution_cost,
                max_allowed_num_spp,
                integerize,
            base_prob_name_stem = "base_prob",
            cor_dir_name_stem = "cor")
    {
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
            create_allowable_size_Xu_problem_from_scratch (max_allowed_num_spp,
                                                           parameters,
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

                                                    Xu_bdprob_cor@cor_PU_spp_pair_indices,

                                            Xu_bdprob_cor@PU_costs,
                                                    Xu_bdprob_cor@spp_col_name,
                                                    Xu_bdprob_cor@PU_col_name,
                                                    PU_spp_pair_info@dependent_node_IDs,
                                                    PU_spp_pair_info@correct_solution_vector_is_known)

    Xu_bdprob_cor@bpm = bpm

        #-------------------------------------------------------------
        #  Quit if there are any duplicate edges/spp in the problem.
        #-------------------------------------------------------------

    see_if_there_are_any_duplicate_links (bpm, Xu_bdprob_cor@num_spp)

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
        #  Compute network metrics.
    Xu_bdprob_cor = init_object_graph_data (Xu_bdprob_cor,
                                             exp_root_dir,

                                             parameters$compute_network_metrics,

                                parameters$compute_network_metrics_base_COR,

                                             parameters$use_igraph_metrics,
                                             parameters$use_bipartite_metrics,
                                             parameters$bipartite_metrics_to_use)

        #------------------------------------------------------------
        #  Everything seems to have worked.
        #  Save the bdprob to disk as a first cut at how to archive
        #  and retrieve problems in general.
        #  This particular bit of code may disappear later on, once
        #  it's clearer how to archive.
        #------------------------------------------------------------

    Xu_bdprob_cor@prob_is_ok = TRUE

    Xu_bdprob_cor <- save_rsprob (Xu_bdprob_cor, exp_root_dir)

    save_rsprob_results_data (Xu_bdprob_cor, exp_root_dir, parameters$run_ID)

    return (Xu_bdprob_cor)
    }

#===============================================================================

