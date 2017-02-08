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

# Seems like there is a lot of possible overlap between species distribution
# modelling and reserve selection data structures, e.g., all of the data about
# planning units.  However, sdm doesn't generally call them planning units and
# they're more often at the pixel level, so the representation is more likely
# to be a map than a list or a data frame.
#
# So, it probably doesn't make sense to share data structures via inheritance,
# but I'll just keep this commit around to save the information.


    #  Spp_Dist_problem class.        2017 02 29 - BTL - JUST TRYING THIS OUT FOR A MOMENT TO SEE IF THERE IS OVERLAP BETWEEN SD AND RES SEL DATA STRUCTURES TO KEEP IN MIND.

setClass ("Spp_Dist_problem",
          representation (
                          UUID                             = "character",                  #  both

                                #  Known for all Xu problems, by definition.
cor_optimum_cost                 = "numeric",                  #  bd_prob
                          PU_costs                         = "vector",   #  of numeric   #  both

                          PU_col_name                      = "character",                  #  both
            spp_col_name                     = "character",                  #  both MAYBE, e.g., if using joint distributions to predict
                          presences_col_name               = "character",                  #  both

                          correct_solution_vector_is_known = "logical",                  #  both
                          prob_generator_params_known      = "logical",                  #  both

                          prob_is_ok                       = "logical",                  #  both

read_Xu_problem_from_Xu_file     = "logical",                  #  XU only
infile_name                      = "character",                  #  Xu only

Xu_parameters                    = "Xu_params",                  #  XU only

num_spp                          = "numeric",                  #  bd_prob
                          num_PUs                          = "numeric",                  #  both
            PU_spp_pair_indices              = "data.frame",                  #  both? - but may be different forms
                          all_PU_IDs                       = "numeric",                  #  both
            all_spp_IDs                      = "numeric",                  #  both MAYBE, e.g., if using joint distributions to predict

nodes                            = "data.frame",                  #  not sure - XU only or is this just the list of planning units?
dependent_node_IDs               = "numeric",                  #  XU only?  or, this is named wrong and should be "solution set if known"
final_link_counts_for_each_node  = "data.frame",                  #  XU only

            bpm                              = "matrix",                  #  both MAYBE, e.g., if using joint distributions to predict

            starting_dir                     = "character",                  #  both?
            base_outdir                      = "character",                  #  both?
            derived_bdpg_dir_names           = "list",                  #  both?
            full_saved_bdprob_path           = "character",                  #  both?

bipartite_metrics_from_bipartite_package = "matrix",                  #  bd_prob
bipartite_metrics_from_igraph_package_df = "data.frame"                  #  bd_prob
                          ),
          prototype (
                          PU_col_name                      = "PU_ID",
                          spp_col_name                     = "spp_ID",
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

    #  Xu_bd_problem class.

setClass ("Xu_bd_problem",
          representation (
                          UUID                             = "character",                  #  bd_prob

                                #  Known for all Xu problems, by definition.
                          cor_optimum_cost                 = "numeric",                  #  bd_prob (only known sometimes) - should this not be COR?, i.e., apparents may have what they perceive to be the opt cost
                          PU_costs                         = "vector",   #  of numeric   #  bd_prob

                          PU_col_name                      = "character",                  #  bd_prob
                          spp_col_name                     = "character",                  #  bd_prob
                          presences_col_name               = "character",                  #  bd_prob

                          correct_solution_vector_is_known = "logical",                  #  bd_prob
                          prob_generator_params_known      = "logical",                  #  bd_prob

                          prob_is_ok                       = "logical",                  #  bd_prob

read_Xu_problem_from_Xu_file     = "logical",                  #  XU only
infile_name                      = "character",                  #  Xu only

Xu_parameters                    = "Xu_params",                  #  XU only

                          num_spp                          = "numeric",                  #  bd_prob
                          num_PUs                          = "numeric",                  #  bd_prob
PU_spp_pair_indices              = "data.frame",                  #  bd_prob - but may be different forms
                          all_PU_IDs                       = "numeric",                  #  bd_prob
                          all_spp_IDs                      = "numeric",                  #  bd_prob

nodes                            = "data.frame",                  #  not sure - XU only or is this just the list of planning units?
dependent_node_IDs               = "numeric",                  #  XU only?  or, this is named wrong and should be "solution set if known"
final_link_counts_for_each_node  = "data.frame",                  #  XU only

                          bpm                              = "matrix",                  #  bd_prob - but could be integer or float, depending on problem

                          starting_dir                     = "character",                  #  bd_prob
                          base_outdir                      = "character",                  #  bd_prob
                          derived_bdpg_dir_names           = "list",                  #  bd_prob
                          full_saved_bdprob_path           = "character",                  #  bd_prob

                          bipartite_metrics_from_bipartite_package = "matrix",                  #  bd_prob
                          bipartite_metrics_from_igraph_package_df = "data.frame"                  #  bd_prob
                          ),
          prototype (
                          PU_col_name                      = "PU_ID",
                          spp_col_name                     = "spp_ID",
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

    #  Xu_bd_problem_APP class.

            #***  IS THERE A PROBLEM HERE WITH HAVING AN APP VERSION OF WRAPPED OR COMBINED XU PROBLEM OR EVEN OF NON-XU PROBLEM?
            #     Do we need an app version of every problem class?
            #     Or, is APP an attribute of any problem and in COR, it's empty?
            #         For example, there could be a function:
            #             is.COR=function(prob) {is.null(prob@APP)}

setClass ("Xu_bd_problem_APP",
          representation (
                            UUID_of_base_problem_that_has_err_added = "character",  #  UUID string
                            ret_vals_from_add_errors = "list"
                            ),
          contains = "Xu_bd_problem"
        )

#===============================================================================




