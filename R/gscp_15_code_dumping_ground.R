#                    gscp_15_code_dumping_ground.R

#  2017 02 20 - BTL
#  This was excess code that I had cut out of gscp_15 but wasn't ready to
#  completely ditch yet because there were many questions in the code.
#  I'm moving it out into its own file until I'm ready to delete it all.
#  It's just a big distraction when it's at the end of gscp_15.

#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#  THIS LAST SECTION IS A TEMPORARY DUMPING GROUND FOR CODE THAT'S BEEN
#  COMMENTED OUT BUT HAS SOME UTILITY IN BUILDING UP SOME DOCUMENTATION FOR
#  VARIOUS BITS OF DATA.  SEE MORE DETAILED EXPLANATION BELOW.
#-------------------------------------------------------------------------------
{
#  BTL - 2017 02 17
#  Need to document what some of the various marxan and bdpg vectors and lists
#  and data frame columns mean, since some of the names only made sense in
#  the context where they were created and now they're being used for other
#  things.  So here, I'm going to just collect a pile of code that's been
#  commented out but had some useful information in it before it was removed.
#  Later, you can get rid of all this, but not until some more documentation
#  is scavenged from it.  Not sure what all I need in that way though, so I
#  don't want to waste time on it right now.
#  solutions_df is no longer even referenced anywhere in bdpg.
#  Should I just get rid of it?
#  At the moment, it's slightly useful as a documentation of what some of the
#  different vectors mean (i.e., the data frame column names are more
#  meaningful than some of the vectors assigned to them.
#  Actually, that only applies to two of them:
#      - marxan_best_solution = marxan_best_df_sorted$SOLUTION, #  assumes already sorted by PU_ID
#      - marxan_votes         = marxan_ssoln_df$number
#  Would probably be better to just have a documentation section somewhere that
#  explains the different marxan output columns.

#   solutions_df = data.frame (puid = marxan_best_df_sorted$PUID,
#                              optimal_solution = correct_solution_vector,
#                              marxan_best_solution = marxan_best_df_sorted$SOLUTION, #  assumes already sorted by PU_ID
#                              marxan_votes = marxan_ssoln_df$number,
#                              cor_signed_diff = cor_signed_difference,
#                              cor_abs_val_diff = cor_abs_val_signed_difference,
#           #  2015 06 19 - BTL
#           #  Until now (3:04 pm), this said:
#           #       cor_num_spp_on_patch = final_link_counts_for_each_node$freq
#           #  That means that it was using the apparent, not the correct,
#           #  final link counts for each node.
#           #  I have now changed this to say cor_... instead.
#           #  Not sure if this was a source of unknown bugs before or will be
#           #  a source now.  Need to look more closely at this to see the
#           #  consequences where solutions_df$cor_num_spp_on_patch is used
#           #  downstream of here.  However, since this is the only place it
#           #  appears in this file and this file (gscp_15) is at the end of
#           #  the whole program, maybe it doesn't matter at all except being
#           #  echoed in some output file.
#           #  2017 02 11 - BTL
#           #  cor_final_counts_for_each_node seems to be the wrong name all over.
#           #  Trying cor_link_counts_for_each_node now.
#           #  All of this definitely needs some checking into...
# #                             cor_num_spp_on_patch = cor_final_counts_for_each_node$freq
#                              cor_num_spp_on_patch = cor_link_counts_for_each_node$freq
#                              )

#===============================================================================

#  cut from start of else clause of "if (read_Xu_problem_from_Xu_file)"

# cat ("\n\nJust before things requiring major fix in gscp_15:\n")
# #browser()
# # biodivprobgen_utilities.R:235:    #  like nodes$dependent_set_member.
# # biodivprobgen_utilities.R:238:    #       Error in marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member :
# # gen_bdprob.R:738:largest_PU_ID = max (Xu_nodes$node_ID)
# # generateSetCoverProblem.R:242:#all_correct_node_IDs = cor_nodes$node_ID
# # generateSetCoverProblem.R:243:all_correct_node_IDs = 1:max(cor_nodes$node_ID)
# # gscp_15_create_master_output_structure.R:131:2016 07 16 - nodes$dependent_set_member ONLY HAS THE NUMBER OF PLANNING UNITS
# # gscp_15_create_master_output_structure.R:159:      correct_solution_vector = nodes$dependent_set_member
# # gscp_15_create_master_output_structure.R:162:      cor_signed_difference = marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
# #
# #
# # 2016 07 16 - nodes$dependent_set_member ONLY HAS THE NUMBER OF PLANNING UNITS
# #           THAT WERE IN THE ORIGINAL XU PROBLEM, NOT THE WRAPPED PROBLEM.
# #           MEANWHILE, THE MARXAN SOLUTION DOES HAVE THE WRAPPED PROBLEM PUs SO
# #           THE LENGTHS DO NOT MATCH.  NEED TO MAKE SURE THAT EVERYTHING IN THE
# #           WRAPPED PROBLEM HAS THE CORRECT DIMENSIONS AND VALUES.
# #             This is part of a larger problem of making sure that the problem
# #             returned by wrapping is correctly sized in every way to allow
# #             subsequent operations to act on it exactly as they would act on
# #             a base Xu problem.
# #
# #             One test of that is to make sure that all of
# #             the dimensions of the object elements include all planning units
# #             of the wrapped problem.  This may also be complicated by the
# #             application of error to generate an apparent problem.  That means
# #             you will also need to verify the problem dimensions and values
# #             again, after you have generated the apparent version.
# #
# #           ANOTHER PROBLEM HERE IS THAT THE XU SOLUTION IS NOT NECESSARILY
# #           THE ONLY CORRECT SOLUTION.  THIS MATCHING OF NODES TO A SOLUTION CAN
# #           BE WRONG IF MARXAN HAS FOUND A DIFFERENT CORRECT SOLUTION.
# #           NEED TO AT LEAST CHECK WHETHER
# #             A) MARXAN SOLUTION IS THE CORRECT SIZE (I.E., COST)
# #             AND
# #             B) IF IT IS THE CORRECT SIZE, THEN YOU ALSO NEED TO CHECK THAT
# #                IT REALLY DOES COVER THE SET, I.E., IT IS A CORRECT SOLUTION.

#===============================================================================

# #  2017 02 17 - BTL
# #  I don't think that this "Xu read from file" works anymore.
# #  When you go down and look at the loading of the final results dataframe,
# #  none of these variables are referenced.  In fact, none of them are ever
# #  referenced from here on, except for the first 3 and they're only used
# #  for calculations related to when not reading from a benchmark file.
# #  The results data frame is built using the Xu parameters structures that
# #  are attached to the problem itself, not from the variables below.
# #  So, either the building of the data frame needs to change or else the
# #  creation of the problem object from a benchmark file needs to do the
# #  empty assignment stuff below as part of its initialization.
# #  However, either way leaves the first 3 entries in the if section wrong.
# #  They look like they were only used in comparisons of marxan solution
# #  vectors with Xu solution vectors and that stuff never gets to the final
# #  results because the Xu solution vector might not be the only correct one
# #  and I think that all of the code and output that referenced that stuff
# #  has been removed.
# #  So, I'm going to remove this whole clause but leave it in the dead code
# #  section at the bottom of this file until I've got it all worked out.
# #
# #  Regardless of that, one important thing is to check to see how building
# #  the results data frame behaves when given a Xu problem that's read from
# #  a benchmark file.  For that matter, how does it behave when given ANY
# #  problem that lacks the Xu parameters that are currently being written
# #  to the data frame?
#
#       #---------------------------------------------------------------------
#
#     if (read_Xu_problem_from_Xu_file)
#         {
#         correct_solution_vector = rep (NA, num_PUs)
#         cor_signed_difference = rep (NA, num_PUs)
#         cor_abs_val_signed_difference = rep (NA, num_PUs)
#
#         #  Xu options
#         n__num_groups = NA
#         alpha__ = NA
#         p__prop_of_links_between_groups = NA
#         r__density = NA
#
#         #  Derived Xu options
#         num_nodes_per_group = NA
#         tot_num_nodes = num_PUs
#         num_independent_set_nodes = tot_num_nodes - correct_solution_cost
#         num_dependent_set_nodes = correct_solution_cost
#         num_rounds_of_linking_between_groups = NA
#         target_num_links_between_2_groups_per_round = NA
#         num_links_within_one_group = NA
#         tot_num_links_inside_groups = NA
#         max_possible_num_links_between_groups = NA
#         max_possible_tot_num_links = NA
#
#         opt_solution_as_frac_of_tot_num_nodes = correct_solution_cost / tot_num_nodes
#
#         } else  #  generated the problem

#===============================================================================

#-----------------------------------------------------
#  2017 02 17 12:17 am - BTL
#  Variables that RStudio had flagged as never used:
#-----------------------------------------------------

#unreferenced#bdpg_extended_params = Xu_parameters@bdpg_extended_params
#unreferenced#        cor_signed_difference = marxan_best_df_sorted$SOLUTION - nodes$dependent_set_member
#unreferenced#        cor_abs_val_signed_difference = abs (cor_signed_difference)
#unreferenced#  app_solution_spp_rep_fracs              = app_results_list$spp_rep_fracs
#unreferenced#  cor_spp_rep_fracs              = cor_results_list$spp_rep_fracs

#-------------------------------------------------------------------------
#  2017 02 17 12:17 am - BTL
#  Variables that are commented out throughout the code and just getting
#  in the way at the moment but I might want to restore later on:
#-------------------------------------------------------------------------

  # app_solution_NUM_spp_covered            = app_results_list$num_spp_covered
  # app_solution_FRAC_spp_covered           = app_results_list$frac_spp_covered
  # app_spp_rep_shortfall                   = app_results_list$spp_rep_shortfall

  #         #  These should be the same as those computed via Marxan.
  #     app_solution_spp_rep_fracs =
  #         compute_rep_fraction (app_bpm, marxan_best_solution_PU_IDs, DEBUG_LEVEL, spp_rep_targets)
  #     app_solution_unmet_spp_rep_frac_indices = which (app_solution_spp_rep_fracs < 1)
  #     app_solution_NUM_spp_covered = num_spp - length (app_solution_unmet_spp_rep_frac_indices)
  #
  #     app_solution_FRAC_spp_covered = app_solution_NUM_spp_covered / num_spp
  #     app_spp_rep_shortfall = 1 - app_solution_FRAC_spp_covered

            #     cat ("\napp_solution_NUM_spp_covered =", app_solution_NUM_spp_covered)
            #     cat ("\napp_solution_FRAC_spp_covered =", app_solution_FRAC_spp_covered)
            #     cat ("\napp_spp_rep_shortfall =", app_spp_rep_shortfall)

#----------------------------------
#----------------------------------

  # cor_NUM_spp_covered            = cor_results_list$num_spp_covered
  # cor_FRAC_spp_covered           = cor_results_list$frac_spp_covered
  # cor_spp_rep_shortfall                   = cor_results_list$spp_rep_shortfall

  #     cor_spp_rep_fracs =
  #         compute_rep_fraction (cor_bpm, marxan_best_solution_PU_IDs, DEBUG_LEVEL, spp_rep_targets)
  #     cor_unmet_spp_rep_frac_indices = which (cor_spp_rep_fracs < 1)
  #     cor_NUM_spp_covered = num_spp - length (cor_unmet_spp_rep_frac_indices)
  #
  #     cor_FRAC_spp_covered = cor_NUM_spp_covered / num_spp
  #     cor_spp_rep_shortfall = 1 - cor_FRAC_spp_covered



  #     cat ("\ncor_NUM_spp_covered =", cor_NUM_spp_covered)
  #     cat ("\ncor_FRAC_spp_covered =", cor_FRAC_spp_covered)
  #     cat ("\ncor_spp_rep_shortfall =", cor_spp_rep_shortfall)

#===============================================================================

      #------------------------------------------------------------------------
      #  2017 02 17 - BTL
      #  I HAVE NO IDEA WHAT THIS COMMENT IS ABOUT OR WHETHER IT EVEN APPLIES
      #  ANYMORE.
      #  FOR EXAMPLE, THE LAST BIT ABOUT FP AND FN RATES IS DEFINITELY OUT
      #  OF DATE NOW, SINCE THOSE _ARE_ INCLUDED IN THE OUTPUT.
      #------------------------------------------------------------------------

  #  Supporting data not in binding
  #   species vs planning units (database?) to allow computation of performance
  #   measures related to which species are covered in solutions
  #   (e.g., SELECT species WHERE planning unit ID == curPlanningUnitID)
  #   (e.g., SELECT planning unit ID WHERE species == curSpeciesID))
  #       - planning unit IDs
  #       - set of species on planning unit

  #     ALSO STILL NEED TO ADD THE FP AND FN RATES AND OTHER ERROR MODEL
  #     ATTRIBUTES TO THE OUTPUT DATA FRAME AND CSV FILE.

#===============================================================================

#------------------------------------------------------------------------------
#  BTL - 2017 02 17
#  I'm not sure whether "apparent" in all of this means apparent in
#  the sense of the added error in the problem itself or in the sense
#  of something being marxan's result instead of the correct result.
#
#  Is this stuff being done for all of marxan's solutions in its output or
#  is it only being done for marxan's choice of best solution?
#  Would be interesting to see it for all of marxan's solutions and in
#  particular, it would be interesting to see how well the ranking across
#  solutions compares between correct and apparent, i.e., do apparent rank
#  solutions hold when you look at what they're really getting in the correct
#  solutions?
#
#  Would it be useful to do a rank correlation between correct and apparent
#  or among the top 10 or 25 percent of solutions or whatever set is most
#  likely to be what people are paying attention to.
#------------------------------------------------------------------------------


#===============================================================================

            # cat ("\n\n----------------------------------")
            # cat ("\nAPP_ VALUES AS COMPUTED BY MARXAN:")
            # cat ("\n----------------------------------")
            # cat ("\napp_solution_NUM_spp_covered__fromMarxan  =", app_solution_NUM_spp_covered__fromMarxan)
            # cat ("\napp_solution_FRAC_spp_covered__fromMarxan =", app_solution_FRAC_spp_covered__fromMarxan)
            # cat ("\napp_spp_rep_shortfall__fromMarxan         =", app_spp_rep_shortfall__fromMarxan)

# #unreferenced#    app_solution_unmet_spp_rep_frac_indices = app_results_list$indices_of_spp_with_unmet_rep_frac
#
#             cat ("\n\n-----------------------------------------")
#             cat ("\nAPP_ VALUES AS COMPUTED BY BIODIVPROBGEN:")
#             cat ("\n-----------------------------------------")
#             cat ("\nlength (app_solution_unmet_spp_rep_frac_indices) = ",
#                 length (app_solution_unmet_spp_rep_frac_indices))
#
# #unreferenced#    cor_unmet_spp_rep_frac_indices = cor_results_list$indices_of_spp_with_unmet_rep_frac
#
#             cat ("\n\n-----------------------------------------")
#             cat ("\nCOR_ VALUES AS COMPUTED BY BIODIVPROBGEN:")
#             cat ("\n-----------------------------------------")
#             cat ("\nlength (cor_unmet_spp_rep_frac_indices) = ",
#                 length (cor_unmet_spp_rep_frac_indices))

#===============================================================================

    #-------------------------------------------------------------------------
    #  Create full results_df.
    #
    #  I.e., just allocate it and initialize it before going on to populate
    #  it with the results.
    #
    #  Would it make more sense to be building up partial data frames
    #  elsewhere and then here, just cbind the partial frames together?
    #  That seems like it could be written as a more flexible routine that
    #  builds results frames to suit particular questions rather than
    #  building a mammoth frame that holds every possible thing and has to
    #  choose dummy values to put into places that don't apply to some data.
    #-------------------------------------------------------------------------

}

#===============================================================================

#===============================================================================

#===============================================================================

#===============================================================================

#===============================================================================

