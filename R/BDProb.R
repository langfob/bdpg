#===============================================================================
#
#                               BDProb.R
#
#  Class definitions for biodiversity problems.
#
#===============================================================================
#
#  History:
#
#  2016 06 09 - BTL
#  Starting to try to redefine many of the data structures as classes
#  to get a little standardization and some validity checks.
#
#===============================================================================

#  Fairly brief, helpful site about OOP in S4:
#      http://thomas-cokelaer.info/blog/2013/01/r-language-object-oriented-programming/

#===============================================================================

    #  Xu_base_params class.

setClass ("Xu_base_params",
          representation (
                            alpha__                         = "numeric",
                            n__num_groups                   = "numeric",
                            p__prop_of_links_between_groups = "numeric",
                            r__density                      = "numeric"
                            )
            )

#===============================================================================

    #  Xu_bdpg_extended_params class.

setClass ("Xu_bdpg_extended_params",
          representation (
                            alpha___lower_bound                                        = "numeric",
                            alpha___upper_bound                                        = "numeric",
                            derive_alpha_from_n__num_groups_and_opt_frac_0.5           = "logical",
                            use_unif_rand_alpha__                                      = "logical",

                            n__num_groups                                              = "numeric",
                            n__num_groups_lower_bound                                  = "numeric",
                            n__num_groups_upper_bound                                  = "numeric",
                            use_unif_rand_n__num_groups                                = "logical",

                            num_independent_nodes_per_group                            = "numeric",

                            use_unif_rand_p__prop_of_links_between_groups              = "logical",
                            p__prop_of_links_between_groups_lower_bound                = "numeric",
                            p__prop_of_links_between_groups_upper_bound                = "numeric",
                            base_for_target_num_links_between_2_groups_per_round       = "character",  #  Correct type?
                            at_least_1_for_target_num_links_between_2_groups_per_round = "logical",  #  Not used?  See comment in gscp_5...R.

                            use_unif_rand_r__density                                   = "logical",
                            r__density_lower_bound                                     = "numeric",
                            r__density_upper_bound                                     = "numeric",

                            integerize                                                 = "function"
                            ),
          prototype (
                          num_independent_nodes_per_group = 1,
                          integerize                      = round
                    )

            )

#===============================================================================

    #  Xu_derived_params class.

setClass ("Xu_derived_params",
          representation (
                            num_nodes_per_group                         = "numeric",
                            num_rounds_of_linking_between_groups        = "numeric",
                            target_num_links_between_2_groups_per_round = "numeric",
                            num_links_within_one_group                  = "numeric",
                            tot_num_links_inside_groups                 = "numeric",
                            max_possible_num_links_between_groups       = "numeric",
                            max_possible_tot_num_links                  = "numeric",
                            max_possible_tot_num_node_link_pairs        = "numeric",

                            num_independent_nodes_per_group             = "numeric",
                            num_independent_set_nodes                   = "numeric",
                            tot_num_nodes                               = "numeric",
                            num_dependent_set_nodes                     = "numeric",
                            opt_solution_as_frac_of_tot_num_nodes       = "numeric"  #,
#                            base_for_target_num_links_between_2_groups_per_round       = "numeric",
#                            at_least_1_for_target_num_links_between_2_groups_per_round = "numeric"
                            )
            )

#===============================================================================

    #  Xu_params class.

setClass ("Xu_params",
          representation (
                          base_params = "Xu_base_params",
                          bdpg_extended_params = "Xu_bdpg_extended_params",
                          derived_params = "Xu_derived_params"
                            )
            )

#===============================================================================

    #  PU_spp_pair_info class.

setClass ("PU_spp_pair_info_class",
          representation (
                            PU_spp_pair_indices              = "data.frame",
                            PU_col_name                      = "character",
                            spp_col_name                     = "character",
                            num_PUs                          = "numeric",
                            num_spp                          = "numeric",
                            correct_solution_cost            = "numeric",
                            Xu_parameters                    = "Xu_params",
                            correct_solution_vector_is_known = "logical",
                            dependent_node_IDs               = "vector",
                            nodes                            = "data.frame",
                            PU_costs                         = "vector",
                            prob_generator_params_known      = "logical"
                            )
        )

#===============================================================================

    #  Xu_bd_problem class.

setClass ("Xu_bd_problem",
          representation (
                          UUID                             = "character",

                                #  Known for all Xu problems, by definition.
                          cor_optimum_cost                 = "numeric",
                          PU_costs                         = "vector",   #  of numeric

                          PU_col_name                      = "character",
                          spp_col_name                     = "character",
                          presences_col_name               = "character",

                          correct_solution_vector_is_known = "logical",
                          prob_generator_params_known      = "logical",
                          prob_is_ok                       = "logical",

                          read_Xu_problem_from_Xu_file     = "logical",
                          infile_name                      = "character",

                          Xu_parameters                    = "Xu_params",

                          num_spp                          = "numeric",
                          num_PUs                          = "numeric",
                          PU_spp_pair_indices              = "data.frame",
                          all_PU_IDs                       = "numeric",
                          all_spp_IDs                      = "numeric",

                          nodes                            = "data.frame",
                dependent_node_IDs               = "numeric",
                          final_link_counts_for_each_node  = "data.frame",

                          bpm                              = "matrix",

                          bipartite_metrics_from_bipartite_package = "matrix",
                          bipartite_metrics_from_igraph_package_df = "data.frame"
                          ),
          prototype (
                          PU_col_name                      = "PU_ID",
                          spp_col_name                     = "spp_ID",
                                #  2017 02 06 - Adding this because it doesn't seem to be set anywhere.
                                #  Looks like it might have been hard-coded at one point and then mistakenly
                                #  replaced with a variable in an argument list without having ever set its
                                #  value anywhere.
                                #  I'm just guessing here and adding a variable to the class, based on
                                #  a comment in the arg list for do_graph_and_marxan_analysis(), which says:
                                #      presences_col_name, #  hard-coded as "freq"
                          presences_col_name               = "freq",

                          bipartite_metrics_from_bipartite_package = NULL,
                          bipartite_metrics_from_igraph_package_df = NULL
                    )
         )

# setMethod ("show", "Xu_bd_problem",
#            function (object)
#                {
#                cat ("Xu_bd_problem = ",
#                     " \n ")
#            }
#           )

#===============================================================================

    #  Xu_wrapped_bd_problem class.

setClass ("Xu_wrapped_bd_problem",
          representation (
                            UUID_of_base_problem_that_is_wrapped = "character"  #  UUID string
                            ),
          contains = "Xu_bd_problem"
        )

#===============================================================================

    #  Xu_bd_problem_app class.

setClass ("Xu_bd_problem_app",
          representation (
                            UUID_of_base_problem_that_has_err_added = "character",  #  UUID string
                            ret_vals_from_add_errors = "list"
                            ),
          contains = "Xu_bd_problem"
        )

#===============================================================================




