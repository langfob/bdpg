#===============================================================================
#                       external_startup_code.R
#===============================================================================
#
#  This file contains all of the startup code that will not be part of 
#  running things as a package.  In particular, it includes:
#
#   - some global variables for debugging
#   - code for controlling printing of timepoints
#   - code related to using tzar and tzar emulation
#
#===============================================================================
#          Global variables not to be included in package.
#===============================================================================

cat ("\n\nSTARTING generateSetCoverProblem.R at ", date(), "\n\n")

#===============================================================================

    #  debugging level: 0 means don't output debugging write statements.
    #  Having this as an integer instead of binary so that I can have 
    #  multiple levels of detail if I want to.
DEBUG_LEVEL = 0

    #  Turn all R warnings into errors.
options (warn=2)

#===============================================================================
                    #  START TZAR EMULATION CODE
#===============================================================================

    #  This just does the local build of the parameters list of not using tzar.
    #  I suspect that parameters will just be an argument to the mainline 
    #  function in the end, and this will go away or be external.
    #  gscp_3_get_parameters.R only contains a dummy example of how to create 
    #  a parameters list yourself instead of using the tzar-yaml stuff.
    #  So, it doesn't really even need to exist other than where the emulation 
    #  calls it when no parameters are presented (in get_parameters() below).
    #  That means that all of this Tzar Emulation section can be externalized 
    #  from the mainline and just pass the parameters list into the mainline 
    #  as an argument.
    #  I don't think that even the tzar cleanup code needs to be included 
    #  if tzar is not being used.  Even though it's referenced in the general 
    #  cleanup code, I think that lazy evaluation would mean that it never 
    #  gets called so no error would occur.
source (paste0 (sourceCodeLocationWithSlash, "gscp_3_get_parameters.R"))

    #  Will be a "suggests" rather than a "requires" for bdpg library...
source (paste0 (sourceCodeLocationWithSlash, "emulateRunningUnderTzar.R"))

#-------------------------------------------------------------------------------

    #  Need to set emulation flag every time you swap between emulating 
    #  and not emulating.  
    #  This is the only variable you should need to set for that.
    #  Make the change in the file called emulatingTzarFlag.R so that 
    #  every file that needs to know the value of this flag is using 
    #  the synchronized to the same value.

    #  NOTE: The mainline Does need to know the value of this flag 
    #  throughout, because it tests it when it's doing cleanup after 
    #  a failure or at the end of a successful run.
    #  So, it will at least need to be an input argument to the mainline 
    #  function, even if no tzar or tzar emulation is done.

source (paste0 (sourceCodeLocationWithSlash, "emulatingTzarFlag.R"))
#emulatingTzar = TRUE

    #-----------

    #  When using RStudio, the console output buffer is currently limited and 
    #  you can lose informative console output from bdprobdiff in a big run.
    #  To capture that output, tee the output to a scratch sink file.
    #
    #  NOTE:  For some reason, this sink causes a warning message after 
    #  the code comes back from a marxan run:
    #      unused connection
    #  I have no idea why this happens, especially because it doesn't always 
    #  happen.  Since I have warnings set to generate errors rather than 
    #  warnings, it stops the program by calling browser().  If I just hit Q, 
    #  then the program continues from there without any problems.  
    #  At the moment, trapping all the output is more important than having 
    #  this annoying little hitch, so I'm leaving all this in.  
    #  At production time, I'll need to either remove it or fix it.  
    #  I should add an issue for this in the github issue tracking.  

echoConsoleToTempFile = TRUE
if (emulatingTzar & echoConsoleToTempFile)
    {
        #  Open a file to echo console to.
    tempConsoleOutFile <- file("consoleSinkOutput.temp.txt", open="wt")

    	#  Redirect console output to the file.
    sink (tempConsoleOutFile, split=TRUE)
    }

    #-----------

    #  This flag is only set here to make clear what is being handed to 
    #  the get_parameters() call's first argument.  You could just put 
    #  a TRUE or FALSE value there since this variable is not used anywhere 
    #  else in the mainline.
running_tzar_or_tzar_emulator = TRUE

parameters = get_parameters (running_tzar_or_tzar_emulator, emulatingTzar)

#===============================================================================
                    #  END TZAR EMULATION CODE
#===============================================================================
#===============================================================================
#                       Load function definitions.
#===============================================================================
#  This will eventually be replaced with a library() call for the bdpg library.
#  Any files sourced in this section can only contain function definitions.  
#  Any freestanding code has to be sourced or included elsewhere so that I can 
#  tell what still needs doing to be able to finish conversion for a library.
#===============================================================================

library (plyr)    #  For count() and arrange()
library (marxan)

library (methods)    #  bipartite needs this if run before igraph under RScript
cat ("\n\nAbout to load bipartite library.")
library (bipartite)

library (assertthat)    #  For unit testing.
library (stringr)   #  For str_replace_all.

library (igraph)

library (marxan)    #  Need to rename this library (i.e., my marxan library) to allow use of UQ library that has same name.

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
