#===============================================================================
#
#                       external_startup_code.R
#
#===============================================================================
#                       Load function definitions.
#===============================================================================
#  This will eventually be replaced with a library() call for the bdpg library.
#  Any files sourced in this section can only contain function definitions.
#  Any freestanding code has to be sourced or included elsewhere so that I can
#  tell what still needs doing to be able to finish conversion for a library.
#===============================================================================

#####library (plyr)    #  For count() and arrange()
#####library (marxan)

#####library (methods)    #  bipartite needs this if run before igraph under RScript
#####cat ("\n\nAbout to load bipartite library.")
#####library (bipartite)

#####library (assertthat)    #  For unit testing.
#####library (stringr)   #  For str_replace_all.

#####library (igraph)

#####library (marxan)    #  Need to rename this library (i.e., my marxan library) to allow use of UQ library that has same name.

#===============================================================================

source (paste0 (sourceCodeLocationWithSlash, "gscp_16_clean_up_run.R"))
source (paste0 (sourceCodeLocationWithSlash, "timepoints.R"))

source (paste0 (sourceCodeLocationWithSlash, "BDProb.R"))

source (paste0 (sourceCodeLocationWithSlash, "biodivprobgen_utilities.R"))
source (paste0 (sourceCodeLocationWithSlash, "biodivprobgen_initialization.R"))
source (paste0 (sourceCodeLocationWithSlash, "gen_bdprob.R"))
source (paste0 (sourceCodeLocationWithSlash, "add_error_to_spp_occupancy_data.R"))
source (paste0 (sourceCodeLocationWithSlash, "testing_bdpg_functions.R"))
source (paste0 (sourceCodeLocationWithSlash, "load_and_parse_Xu_set_cover_problem_file.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_5_derive_control_parameters.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_6_create_data_structures.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_8_link_nodes_within_groups.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_9_link_nodes_between_groups.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_9a_create_Xu_graph.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_10a_clean_up_completed_graph_structures.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_10b_compute_solution_rep_levels_and_costs.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_10c_build_adj_and_cooccurrence_matrices.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_11_summarize_and_plot_graph_structure_information.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_11aa_write_abbreviated_results_to_files.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_11a_network_measures_using_bipartite_package.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_11b_network_measures_using_igraph_package.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_12_write_network_to_marxan_files.R"))
source (paste0 (sourceCodeLocationWithSlash, "compute_solution_scores.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_13_write_marxan_control_file_and_run_marxan.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_14_read_marxan_output_files.R"))
source (paste0 (sourceCodeLocationWithSlash, "gscp_15_create_master_output_structure.R"))

source (paste0 (sourceCodeLocationWithSlash, "do_graph_and_marxan_analysis.R"))
source (paste0 (sourceCodeLocationWithSlash, "gen_lognormal_overlay.R"))

#===============================================================================
    #  Code in this section is not just function definitions.
    #  It contains code that will have to either be incorporated in the
    #  mainline function or further abstracted into functions or
    #  into testing conventions.
#===============================================================================

    #--------------------------------------------------------------------
    #  Set up to record timepoints during the run.
    #
    #  NOTE:  This has to come AFTER tzar emulation or other method of
    #         creation of parameters variable since it uses parameters.
    #--------------------------------------------------------------------

        #  This variable could be specified somewhere else as well, e.g.,
        #  in the tzar parameters file.
timepoints_df_default_length = 20

run_ID = parameters$run_id
runset_name = parameters$runset_name

timepoints_df =
    data.frame (timepoint_num = 1:timepoints_df_default_length,
                timepoint_name = rep (NA, timepoints_df_default_length),
                prev_chunk_elapsed_user = rep (NA, timepoints_df_default_length),
                tot_elapsed_user = rep (NA, timepoints_df_default_length),
                prev_chunk_elapsed_system = rep (NA, timepoints_df_default_length),
                tot_elapsed_system = rep (NA, timepoints_df_default_length),
                cur_time_user = rep (NA, timepoints_df_default_length),
                cur_time_system = rep (NA, timepoints_df_default_length),
                cur_time_wall_clock = rep (NA, timepoints_df_default_length),
                run_ID = run_ID,
                runset_name = runset_name
                )

    #  First time, intialization.
    #  NOTE that these two values are updated all over the place using the
    #  global assignment operator "<<-" in the call to timepoint().
    #  The timepoints dataframe is updated by returning it from the
    #  timepoint() call.
    #  *** However, does that still work when it's called inside of a function?
    #  If it's not returned from the function, then I don't think it will get
    #  updated.
cur_timepoint_num = 0
prev_time = start_time = proc.time()

    #  Each time...

timepoints_df = timepoint (timepoints_df, "start", "Run start...")

#===============================================================================

    #  Set random seed to help reproducibility.
    #  Has to be done after startup code that loads parameters structure.
set.seed (parameters$seed)

#===============================================================================

source (paste0 (sourceCodeLocationWithSlash, "testing_bdpg.R"))

#===============================================================================
