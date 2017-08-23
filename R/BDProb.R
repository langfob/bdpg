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
                     ),

          #-----------------------------------------------------------------

          prototype (
                            alpha__                         = as.numeric (NA),
                            n__num_groups                   = as.numeric (NA),
                            p__prop_of_links_between_groups = as.numeric (NA),
                            r__density                      = as.numeric (NA)
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
                            alpha___lower_bound                                        = as.numeric (NA),
                            alpha___upper_bound                                        = as.numeric (NA),
                            derive_alpha_from_n__num_groups_and_opt_frac_0.5           = NA,
                            use_unif_rand_alpha__                                      = NA,

                            n__num_groups                                              = as.numeric (NA),
                            n__num_groups_lower_bound                                  = as.numeric (NA),
                            n__num_groups_upper_bound                                  = as.numeric (NA),
                            use_unif_rand_n__num_groups                                = NA,

                        num_independent_nodes_per_group                            = 1,

                            use_unif_rand_p__prop_of_links_between_groups              = NA,
                            p__prop_of_links_between_groups_lower_bound                = as.numeric (NA),
                            p__prop_of_links_between_groups_upper_bound                = as.numeric (NA),
                            base_for_target_num_links_between_2_groups_per_round       = as.character (NA),
                            at_least_1_for_target_num_links_between_2_groups_per_round = NA,

                            use_unif_rand_r__density                                   = NA,
                            r__density_lower_bound                                     = as.numeric (NA),
                            r__density_upper_bound                                     = as.numeric (NA),

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
                     ),

          #-----------------------------------------------------------------

          prototype (
                            num_nodes_per_group                         = as.numeric (NA),
                            num_rounds_of_linking_between_groups        = as.numeric (NA),
                            target_num_links_between_2_groups_per_round = as.numeric (NA),
                            num_links_within_one_group                  = as.numeric (NA),
                            tot_num_links_inside_groups                 = as.numeric (NA),
                            max_possible_num_links_between_groups       = as.numeric (NA),
                            max_possible_tot_num_links                  = as.numeric (NA),
                            max_possible_tot_num_node_link_pairs        = as.numeric (NA),

                        num_independent_nodes_per_group             = as.numeric (NA),
                            num_independent_set_nodes                   = as.numeric (NA),
                            tot_num_nodes                               = as.numeric (NA),
                            num_dependent_set_nodes                     = as.numeric (NA),
                            opt_solution_as_frac_of_tot_num_nodes       = as.numeric (NA)  #,
#                            base_for_target_num_links_between_2_groups_per_round       = as.numeric (NA),
#                            at_least_1_for_target_num_links_between_2_groups_per_round = as.numeric (NA)
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

    #  Need to make these constant names like Xu_bd_problem does
    #  AND make sure they have the same values in both classes.
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
                     ),

          #-----------------------------------------------------------------

          prototype (
#                            PU_spp_pair_indices              = "data.frame",

    #  Need to make these constant names like Xu_bd_problem does
    #  AND make sure they have the same values in both classes.
        PU_col_name                      = "PU_ID",
        spp_col_name                     = "spp_ID",

                            num_PUs                          = as.numeric (NA),
                            num_spp                          = as.numeric (NA),
                            correct_solution_cost            = as.numeric (NA),
#                            Xu_parameters                    = "Xu_params",    #  Xu only
                            correct_solution_vector_is_known = NA,
#                            dependent_node_IDs               = "vector",    #  Xu only?  or at least rename?
#                            nodes                            = "data.frame",    #  Xu only?  or at least rename?
#                            PU_costs                         = "vector",
                            prob_generator_params_known      = NA
                    )
        )

#===============================================================================

    #  Xu_prob_gen_info_class class definition.

setClass ("Xu_prob_gen_info_class",
          representation (
                            read_Xu_problem_from_Xu_file     = "logical",
                            infile_name                      = "character",
                            Xu_parameters                    = "Xu_params"
                     ),

          #-----------------------------------------------------------------

          prototype (
                            read_Xu_problem_from_Xu_file     = NA,
                            infile_name                      = as.character (NA)
#                            ,
#                            Xu_parameters                    = "Xu_params"
                    )
        )

#===============================================================================

    #  APP_prob_info_class class definition.

setClass ("APP_prob_info_class",
          representation (
                            UUID_of_base_problem_that_has_err_added = "character",  #  UUID string

                            original_FP_const_rate = "numeric",  #  ret_vals_from_apply_errors$original_FP_const_rate
                            original_FN_const_rate = "numeric",  #  ret_vals_from_apply_errors$original_FN_const_rate
                            match_error_counts     = "logical",  #  ret_vals_from_apply_errors$match_error_counts
                            FP_const_rate          = "numeric",  #  ret_vals_from_apply_errors$FP_const_rate
                            FN_const_rate          = "numeric",  #  ret_vals_from_apply_errors$FN_const_rate

                            realized_FP_rate       = "numeric",  #  ret_vals_from_apply_errors$realized_FP_rate
                            realized_FN_rate       = "numeric",  #  ret_vals_from_apply_errors$realized_FN_rate

                            app_num_spp = "numeric",  #  ret_vals_from_apply_errors$app_num_spp
                            app_num_PUs = "numeric",  #  ret_vals_from_apply_errors$app_num_PUs

                            app_PU_spp_pair_indices = "data.frame"  #  ret_vals_from_apply_errors$app_PU_spp_pair_indices
                     ),

          #-----------------------------------------------------------------

          prototype (
                            UUID_of_base_problem_that_has_err_added = as.character (NA),  #  UUID string

                            original_FP_const_rate = 0,    #as.numeric (NA),  #  ret_vals_from_apply_errors$original_FP_const_rate
                            original_FN_const_rate = 0,    #as.numeric (NA),  #  ret_vals_from_apply_errors$original_FN_const_rate
                            match_error_counts     = NA,               #  ret_vals_from_apply_errors$match_error_counts
                            FP_const_rate          = 0,    #as.numeric (NA),  #  ret_vals_from_apply_errors$FP_const_rate
                            FN_const_rate          = 0,    #as.numeric (NA),  #  ret_vals_from_apply_errors$FN_const_rate

                            realized_FP_rate       = 0,    #as.numeric (NA),  #  ret_vals_from_apply_errors$realized_FP_rate
                            realized_FN_rate       = 0,    #as.numeric (NA),  #  ret_vals_from_apply_errors$realized_FN_rate

                            app_num_spp = as.numeric (NA),  #  ret_vals_from_apply_errors$app_num_spp
                            app_num_PUs = as.numeric (NA)    #,  #  ret_vals_from_apply_errors$app_num_PUs

#                            app_PU_spp_pair_indices = "data.frame"  #  ret_vals_from_apply_errors$app_PU_spp_pair_indices
                    )
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

                            #  2017 08 21 - BTL
                            #  Since I don't know what form all of these will
                            #  take and there are issues with backward
                            #  compatibility if the class definintion changes,
                            #  I think that I'll have to handle these kinds of
                            #  changes in the future by deriving a new class,
                            #  e.g., by inheriting from this one - though
                            #  I'm not sure what that will do to wrapped
                            #  problems, which already inherit from this one.
                            #  Might need to use multiple inheritance?

setClass ("Xu_bd_problem",
          representation (
                                #-----------------------
                                #  General information
                                #-----------------------

                            UUID                             = "character",                  #  bd_prob
                            checksum                         = "character",

                            rand_seed                        = "numeric",    #  Random seed set at the start of building this object.

                            obj_type_str                     = "character",    #  e.g., "RSprob" - used in building name for file and dir names
                            cor_or_app_str                   = "character",    #  "COR" or "APP" - used in building file and dir names
                            basic_or_wrapped_or_comb_str     = "character",    #  "Base" or "Wrap" or "Comb" - used in building file and dir names
                            file_name_prefix                 = "character",    #  string to combine with uuid to build file and dir names

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
                                #--------------------------------
                                #  Apparent problem information
                                #--------------------------------

                            APP_prob_info                     = "ANY",

                                #-------------------------------------------------------
                                #  Constant naming information for df cols and dirs
                                #  These are the same in all of these problem objects,
                                #  but since this is research code that changes a lot,
                                #  it seems good to have these names saved somewhere
                                #  as self-documentation for the object in case the
                                #  names change in some different version of the code.
                                #  This way, we will always know for a given object,
                                #  what names it assumed when it was built (even
                                #  though this wastes some space by duplicating
                                #  identical values in many objects).
                                #-------------------------------------------------------

                            PU_col_name                      = "character",                  #  bd_prob
                            spp_col_name                     = "character",                  #  bd_prob
                            presences_col_name               = "character",                  #  bd_prob

                            plot_output_dir                  = "character",
                            network_output_dir               = "character",

                            bipartite_metrics_file_name_stem = "character",
                            igraph_metrics_file_name_stem    = "character",

                                #------------------------
                                #  Solution information
                                #------------------------
                                                                            #  possible problem:  "cost" may need to be "EF_value", since cost is going to be something other than just sum of pu costs when using boundary lengths in EF, etc.  Maybe not though, since there's always the financial cost, which may or may not be the same as the EF.  In that case, you might want to have separate entries for EF_cost and Financial_cost.
                            correct_solution_cost                 = "numeric",     #  bd_prob (only known sometimes) - should this not be COR?, i.e., apparents may have what they perceive to be the opt cost
#correct_solution_cost_is_known = "logical",                  #  bd_prob   #  Add this?  Would it ever be used or is it enough to just have NA for dependent_node_IDs/correct_solution_PU_IDs?

                            correct_solution_vector_is_known = "logical",                  #  bd_prob
                            dependent_node_IDs               = "numeric",                  #  XU only?  OR, IS THIS JUST NAMED POORLY AND SHOULD BE "correct_solution_PU_IDs"
                                                                                           #  integer vector (sorted?) of IDs of nodes in the dependent set
                                                                                           #  CAN BE DERIVED, SO MAKE IT A FUNCTION?
                                #-----------------------
                                #  Species information
                                #-----------------------

                            num_spp                          = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?
                            all_spp_IDs                      = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?  #  Shouldn't this be vector?
                                                                                           #  SHOULD EITHER BE A TABLE WITH MORE IN IT OR THAT TABLE SHOULD ALSO EXIST SEPARATELY.
                                                                                           #  TABLE SHOULD CONTAIN ONE ROW PER SPECIES AND THE FOLLOWING COLUMNS:
                                                                                           #        SPP_ID, SPP_NAME, TOT_SPP_OCCUPANCY, SPP_TARGET, SPP_WEIGHT, SPP_BASE_PROB_UUID, SPP_ID_IN_BASE_PROB, (spp_name_in_base_prob-could be looked up)
                                                                                           #  TOT_SPP_OCCUPANCY means SUM OF PUs or abundances, etc. - to use in rank abundance plots.
                                                                                           #  The TOT_SPP_OCCUPANCY values would be different for COR and APP.

                                #-----------------------------
                                #  Planning unit information
                                #-----------------------------

                            num_PUs                          = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?
                            all_PU_IDs                       = "numeric",                  #  bd_prob  #  CAN BE DERIVED, SO MAKE IT A FUNCTION?  #  Shouldn't this be vector?

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

                            RSprob_dir_names = "list",

                                #---------------------------------------
                                #  Problem attributes/measures/metrics
                                #---------------------------------------

                                        #  Post-generation measures
                            compute_network_metrics                  = "logical",    #  Toggles whether to ignore all other network metric flags or not.

                            compute_network_metrics_COR_APP_WRAP     = "logical",
                            use_igraph_metrics                       = "logical",
                            use_bipartite_metrics                    = "logical",
                            bipartite_metrics_to_use                 = "character",

                            bipartite_metrics_from_bipartite_package = "data.frame",                 #  bd_prob [COR and APP values]
                            bipartite_metrics_from_igraph_package_df = "data.frame"                  #  bd_prob [COR and APP values]

                     ),

          #-----------------------------------------------------------------

          prototype (
                            UUID                             = as.character (NA),
                            checksum                         = as.character (NA),

                            obj_type_str                     = as.character (NA),
                            cor_or_app_str                   = as.character (NA),
                            basic_or_wrapped_or_comb_str     = as.character (NA),
                            file_name_prefix                 = as.character (NA),

                            prob_is_ok                       = NA,
                            prob_generator_params_known      = NA,
                            prob_type                        = as.character (NA),

                          PU_col_name                      = "PU_ID",
                          spp_col_name                     = "spp_ID",
                          presences_col_name               = "freq",

                          plot_output_dir                  = "plots",
                          network_output_dir               = "networks",

                          bipartite_metrics_file_name_stem  = "bipartite_metrics_from_bipartite_pkg_df",
                          igraph_metrics_file_name_stem     = "bipartite_metrics_from_igraph_pkg_df",

                            correct_solution_cost            = as.numeric (NA),
                            correct_solution_vector_is_known = NA,
                            dependent_node_IDs               = as.numeric (NA),

                            num_spp                          = as.numeric (NA),
                            all_spp_IDs                      = as.numeric (NA),

                            num_PUs                          = as.numeric (NA),
                            all_PU_IDs                       = as.numeric (NA),

                            compute_network_metrics                  = NA,
                            compute_network_metrics_COR_APP_WRAP     = NA,
                            use_igraph_metrics                       = NA,
                            use_bipartite_metrics                    = NA,
                            bipartite_metrics_to_use                 = as.character (NA)

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

    #  RS_run class.

setClass ("RSrun",
          representation (
                            UUID        = "character",
                            checksum    = "character",

                            rand_seed        = "numeric",    #  Random seed set at the start of building this object.

                                #  UUID of the problem the reserve selector
                                #  is running on.

                            run_on_prob_UUID = "character",

                            obj_type_str     = "character",    #  e.g., "RSrun_" - used in building name for file and dir names
                            cor_or_app_str   = "character",    #  "COR" or "APP" - used in building file and dir names
                            basic_or_wrapped_or_comb_str = "character",    #  "Base" or "Wrap" or "Comb" - used in building file and dir names
                            rs_method_name   = "character",    #  e.g., "Marxan_sa" - used in building file and dir names
                            file_name_prefix = "character",    #  string to combine with uuid to build file and dir names

                            input_dir_name  = "character",
                            output_dir_name = "character",
                            plot_dir_name   = "character",

                            targets         = "vector"
                     ),

          #-----------------------------------------------------------------

          prototype (
                            UUID        = as.character (NA),
                            checksum    = as.character (NA),

                            run_on_prob_UUID = as.character (NA),

                            obj_type_str     = as.character (NA),
                            cor_or_app_str   = as.character (NA),
                            basic_or_wrapped_or_comb_str = as.character (NA),
                            rs_method_name   = as.character (NA),
                            file_name_prefix = as.character (NA),

                    input_dir_name  = "input",
                    output_dir_name = "output",
                    plot_dir_name   = "plots"

                    )

         )

#===============================================================================

#'Class to hold global constants to avoid having to constantly pass them around
#'
#'There are a bunch of constant strings used for things like directory and
#'column names that are a pain in the ass to pass all up and down the call tree
#'or to duplicate in every copy of all class objects, so I'm going to do the
#'evil thing and make a global variable to store these global constants.  I
#'won't create the variable inside the bdpg package though.  That will need to
#'be done in the calling application.  See example below.
#'
#'@section Assumptions:
#'
#' \enumerate{
#'  \item{The values in this structure will only be set inside the prototype
#'      section of this class declaration, never in the user's code.}
#'  \item{There will be just one instance of this class in an application.}
#'  \item{The class definition is stable and should not change.  If it does
#'      need to change at some point, the version number at the end of the
#'      class name should be incremented, e.g., from Global_Constants_v01 to
#'      Global_Constants_v02.  This should make it clearer in the application
#'      what versions of the constants are being used.}
#'  }
#'
#'@section Why make a global?:
#'
#'When I don't do it this way and I pass these variables all over, they
#'make the code harder to read because they're fairly meaningless variables
#'handed all over the place for little utility. The only reasons for having
#'these values as variables at all (as opposed to hard-coding the strings), is
#'to make sure they're consistent in all occurrences of various structures and
#'to make it easy to change their values everywhere at once on the rare
#'occasions when I want to do that.

#' @slot rsprob_PU_col_name character string column name for planning unit IDs
#'     in RSprob-related data frames.
#' @slot rsprob_spp_col_name character string column name for species IDs in
#'     RSprob-related data frames.
#' @slot rsprob_presences_col_name character string column name for presence
#'     counts in RSprob-related data.
#' @slot mss_frac_of_all_spp_meeting_their_tgt_col_name character string
#'     column name for the fractions of all species that meet their targets
#'     in marxan_solution_costs data frames.
#' @slot mss_frac_of_total_landscape_cost_tgt_col_name character string column
#'     name for the fraction of the total landscape cost that each solution's
#'     absolute cost reperesents in marxan_solutions_costs data frames.
#' @slot mss_solution_num_col_name character string column name for the IDs of
#'     each solution vector in marxan_solutions_costs data frames.
#' @slot rsprob_plot_output_dir character string directory name for plot
#'     output files in RSprob-related code.
#' @slot rsprob_network_output_dir character string directory name for network
#'     output files in RSprob-related code
#' @slot rsrun_input_dir_name character string directory name for input files
#'     in RSrun-related code
#' @slot rsrun_output_dir_name character string directory name for run
#'     output in RSrun-related code (e.g., marxan output files)
#' @slot rsrun_plot_dir_name character string directory name for plot
#'     output files in RSrun-related code
#'
#' @seealso \code{\link{create_global_constants}}
#' @export
#'
#' @examples \dontrun{
#' #  Note that this is now done indirectly by calling
#' #  create_global_constants(), which executes the following command:
#' # assign("GC", new ("Global_Constants_v01"), envir = .GlobalEnv)
#' }
#'
setClass ("Global_Constants_v01",
          representation (
                                #  RSprob column names
                            rsprob_PU_col_name        = "character",
                            rsprob_spp_col_name       = "character",
                            rsprob_presences_col_name = "character",

                                #  Marxan solution scores column names
                            mss_frac_of_all_spp_meeting_their_tgt_col_name = "character",
                            mss_frac_of_total_landscape_cost_col_name      = "character",
                            mss_solution_num_col_name                      = "character",

                                #  RSprob directory names
                            rsprob_plot_output_dir    = "character",
                            rsprob_network_output_dir = "character",

                                #  RSrun directory names
                            rsrun_input_dir_name      = "character",
                            rsrun_output_dir_name     = "character",
                            rsrun_plot_dir_name       = "character"

                     ),

          #-----------------------------------------------------------------

          prototype (
                                #  Column names
                            rsprob_PU_col_name        = "PU_ID",
                            rsprob_spp_col_name       = "spp_ID",
                            rsprob_presences_col_name = "freq",

                                #  Marxan solution scores column names
                            mss_frac_of_all_spp_meeting_their_tgt_col_name = "frac_of_all_spp_meeting_their_tgt_col_name",
                            mss_frac_of_total_landscape_cost_col_name      = "frac_of_total_landscape_cost",
                            mss_solution_num_col_name                      = "solution_num",

                                #  Directory names
                            rsprob_plot_output_dir    = "plots",
                            rsprob_network_output_dir = "networks",

                            rsrun_input_dir_name      = "input",
                            rsrun_output_dir_name     = "output",
                            rsrun_plot_dir_name       = "plots"

                    )

         )

#===============================================================================


