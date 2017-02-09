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

            #  SHOULD JUST BE A LIST, NOT BE A CLASS, SINCE IT'S ONLY USED TO BUNDLE A
            #  BUNCH OF RETURN VALUES?  (is that true?)

setClass ("PU_spp_pair_info_class",
          representation (
                            PU_spp_pair_indices              = "data.frame",
                            PU_col_name                      = "character",
                            spp_col_name                     = "character",
                            num_PUs                          = "numeric",
                            num_spp                          = "numeric",
                            correct_solution_cost            = "numeric",
                            Xu_parameters                    = "Xu_params",    #  Xu only
                            correct_solution_vector_is_known = "logical",
                            dependent_node_IDs               = "vector",    #  Xu only?  or at least rename?
                            nodes                            = "data.frame",    #  Xu only?  or at least rename?
                            PU_costs                         = "vector",
                            prob_generator_params_known      = "logical"    #  Doesn't belong in this structure?
                            )
        )

#===============================================================================

    #  Xu_prob_gen_info_class class definition.

setClass ("Xu_prob_gen_info_class",
          representation (
                            read_Xu_problem_from_Xu_file     = "logical",
                            infile_name                      = "character",
                            Xu_parameters                    = "Xu_params"
                            )
          # ,  #  Dropping prototype for now since it was causing crashes.
          #    #  May already have fixed the problem though with testing
          #    #  of infile_name for NULL and replacing that with "".
          #    #  May want to try reinstating this.  BTL - 2017 02 09.
          # #-----------------------------------------------------------------
          #
          # prototype (
          #               read_Xu_problem_from_Xu_file     = FALSE,
          #               infile_name                      = NA_character_,
          #               Xu_parameters                    = NULL    #  Don't know what else to put here.  It won't accept NA.
          #           )
        )

#===============================================================================

    #  Xu_bd_problem class.
                            #  WHAT ELSE NEEDS TO BE INCLUDED TO BE MORE GENERAL,
                            #  E.G., TO DESCRIBE ILP PROBLEMS AND PROBLEMS WITH
                            #  BOUNDARY LENGTHS, ETC.?

                            #  NEED TO WALK THROUGH THE WHOLE PROCESS AS IF
                            #  USING ILP AND MORE COMPLEX MARXAN (E.G., WITH
                            #  BOUNDARY LENGTHS, SPP WEIGHTS, ETC.) AND
                            #  SIMPLE RICHNESS AND ZONATION TO SEE IF THEY
                            #  WILL WORK OR FORCE CHANGES.

                            #  SHOULD THERE JUST BE A PLANNING UNIT DATA CLASS
                            #  THAT CAN BE SUBCLASSED TO HOLD ALL THE VARIANTS
                            #  LIKE BOUNDARY LENGTHS, ETC WHEN NECESSARY?

                            #  SAME IDEA FOR RESERVE SELECTORS?

setClass ("Xu_bd_problem",
          representation (
                                #-----------------------
                                #  General information
                                #-----------------------

                            UUID                             = "character",                  #  bd_prob
                            prob_is_ok                       = "logical",                    #  bd_prob


    #---------------------------------
    #  Problem generator information
    #---------------------------------

prob_generator_params_known      = "logical",                   #  bd_prob [COR and APP values? i.e., ERROR MODEL params too? Or, just change this cor_prob_gen_params_known?]

prob_type                        = "character",                 #  Add this to allow having a pointer to problem-specific information in prob_gen_info?
prob_gen_info                    = "ANY",                       #  Add this to point to Xu-specific info or something else if different problem source or generator?
                                                                #  Can't find exactly what to put as the value for a slot whose type you don't want to specify, but on page:
                                                                #      https://stat.ethz.ch/R-manual/R-patched/library/methods/html/setClass.html
                                                                #  in the slots section, it mentions "ANY" in a backhanded sort of way, so I'm going to try it here...
                #     #-----------
                #     #  Xu only        MOVING INTO NEW prob_gen_info element...
                #     #-----------
                # read_Xu_problem_from_Xu_file     = "logical",                    #  XU only
                # infile_name                      = "character",                  #  Xu only
                # Xu_parameters                    = "Xu_params",                  #  XU only

    #---------------------
    #  Apparent problem
    #---------------------

#APP_info,      #  Add this to point to all apparent information if this is an apparent problem?

                                #-----------------------------
                                #  Column naming information
                                #-----------------------------

                            PU_col_name                      = "character",                  #  bd_prob
                            spp_col_name                     = "character",                  #  bd_prob
                            presences_col_name               = "character",                  #  bd_prob

                                #------------------------
                                #  Solution information
                                #------------------------
                                                                            #  possible problem:  "cost" may need to be "EF_value", since cost is going to be something other than just sum of pu costs when using boundary lengths in EF, etc.  Maybe not though, since there's always the financial cost, which may or may not be the same as the EF.  In that case, you might want to have separate entries for EF_cost and Financial_cost.
                            cor_optimum_cost                 = "numeric",     #  bd_prob (only known sometimes) - should this not be COR?, i.e., apparents may have what they perceive to be the opt cost
#correct_solution_cost_is_known = "logical",                  #  bd_prob   #  Add this?  Would it ever be used or is it enough to just have NA for dependent_node_IDs/cor_solution_PU_IDs?

                            correct_solution_vector_is_known = "logical",                  #  bd_prob
                            dependent_node_IDs               = "numeric",                  #  XU only?  OR, IS THIS JUST NAMED POORLY AND SHOULD BE "cor_solution_PU_IDs"
                                                                                           #  integer vector (sorted?) of IDs of nodes in the dependent set
                                                                                           #  CAN BE DERIVED, SO MAKE IT A FUNCTION?
                                #-----------------------
                                #  Species information
                                #-----------------------

                            num_spp                          = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?
                            all_spp_IDs                      = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?
                                                                                           #  SHOULD EITHER BE A TABLE WITH MORE IN IT OR THAT TABLE SHOULD ALSO EXIST SEPARATELY.
                                                                                           #  TABLE SHOULD CONTAIN ONE ROW PER SPECIES AND THE FOLLOWING COLUMNS:
                                                                                           #        SPP_ID, SPP_NAME, TOT_SPP_OCCUPANCY, SPP_TARGET, SPP_WEIGHT, SPP_BASE_PROB_UUID, SPP_ID_IN_BASE_PROB, (spp_name_in_base_prob-could be looked up)
                                                                                           #  TOT_SPP_OCCUPANCY means SUM OF PUs or abundances, etc. - to use in rank abundance plots.
                                                                                           #  The TOT_SPP_OCCUPANCY values would be different for COR and APP.

                                #-----------------------------
                                #  Planning unit information
                                #-----------------------------

                            num_PUs                          = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?
                            all_PU_IDs                       = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?

                            PU_costs                         = "vector",   #  of numeric   #  bd_prob [COR and APP values]

                                        #  ???  Xu only, or just in need of renaming  ???
                            nodes                            = "data.frame",                  #  not sure - XU only or is this just the list of planning units?
                                                                                              #  Should be renamed and rebuilt as a general table of PU info, with one row for each PU:
                                                                                              #      PU_ID, PU_NAME, NUM_SPP_ON_PU, IN_SOL_SET, PU_AREA, PU_BOUNDARY_LENGTH, PU_WEIGHT, PU_BASE_PROB_UUID, PU_ID_IN_BASE_PROB, (pu_name_in_base_prob-could be looked up), PU_IN_EXTRA_SET, PU_IN_IND_SET, PU_GROUP_ID
                                                                                              #  CURRENTLY, COLS ARE: node_ID, group_ID, dependent_set_member
                                                                                              #  MIGHT ALSO WANT TO INCLUDE A COLUMN INDICATING WHETHER NODE IS A)SOLUTION B)INDSET C)EXTRA
                                                                                              #  HOWEVER, THAT'S NOT A GENERALIZED PROBLEM CHARACTERISTIC; IT'S PART OF BOTH XU AND PROBLEM COMBINING/WRAPPING.
                                                                                              #  STILL, ARE WRAPPING AND COMBINING only ALLOWABLE WHEN YOU KNOW THE XU SETUP?
                                                                                              #  OR, CAN YOU DO COMBINING AND WRAPPING WHENEVER YOU KNOW A BASE PROBLEM AND ITS SOLUTION SET
                                                                                              #  BY JUST NOT STEPPING ON ANY BASE PROBLEM PU OTHER THAN THE SOLUTION SET.

                            final_link_counts_for_each_node  = "data.frame",                  #  not XU only?  CREATED LONG AFTER XU GENERATION?
                                                                                              #  SHOULD BE RENAMED TO final_spp_cts_for_each_PU ?
                                                                                              #  SHOULD BE A COLUMN IN THE PU INFO TABLE INSTEAD OF A SEPARATE DATA FRAME?
                                                                                              #  cols are: PU_ID, freq
                                                                                              #  data frame with one column of unique PU IDs (both empty and non-empty) and a second column containing the number of species occurring on the corresponding PU
                                #-------------------------
                                #  Occupancy information
                                #-------------------------

                            PU_spp_pair_indices              = "data.frame",            #  bd_prob - but may be different forms [COR and APP values]  {DOES THIS ALSO (NEED TO) REFLECT ABUNDANCE/DENSITY/PROBABILITY/LIKELIHOOD(A LA MAXENT)?}
                                                                                        #  THIS SHOULD INCLUDE "ABUNDANCE/DENSITY/PROBABILITY/LIKELIHOOD(A LA MAXENT)" AS ANOTHER COLUMN
                                                                                        #  Table has one row for each PU/SPP pair where the spp occurs on the PU (where "occurs" means non-zero value for ABUND... above)
                            bpm                              = "matrix",                  #  bd_prob  [COR and APP values]

                                #-------------------------
                                #  Directory information
                                #-------------------------

                            starting_dir                     = "character",                  #  bd_prob [COR and APP values]
                            base_outdir                      = "character",                  #  bd_prob [COR and APP values]
                            derived_bdpg_dir_names           = "list",                  #  bd_prob [COR and APP values]
                            full_saved_bdprob_path           = "character",                  #  bd_prob [COR and APP values]

                                #---------------------------------------
                                #  Problem attributes/measures/metrics
                                #---------------------------------------

                                        #  Post-generation measures
                            bipartite_metrics_from_bipartite_package = "matrix",                  #  bd_prob [COR and APP values]
                            bipartite_metrics_from_igraph_package_df = "data.frame"                  #  bd_prob [COR and APP values]

                     ),

          #-----------------------------------------------------------------

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




