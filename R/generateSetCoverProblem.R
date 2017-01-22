#===============================================================================

                        #  generateSetCoverProblem.R

#===============================================================================

#  routines could be explicit tests of the assumptions (and conclusions?) of  
#  the 4 cases in the proof.

#===============================================================================

#  Procedure to run a single test case EMULATING tzar:

#   - Duplicate single_test.yaml into project.yaml.

#   - Set the tzar emulation flags wherever necessary.
#       In emulatingTzarFlag.R, set:
#           emulatingTzar = TRUE
#       In this file (generateSetCoverProblem.R), set:
#           running_tzar_or_tzar_emulator = TRUE
#       However, for nearly everything that _I_ am doing, that flag will never 
#       change since I'm nearly always using tzar.  It would be more of an 
#       issue for someone else who is not using tzar.

#           IS THIS STEP RIGHT?  DOES IT MATTER?  NOT SURE...
#   - Make sure current working directory is:    
#       /Users/bill/D/rdv-framework/projects/rdvPackages/biodivprobgen/R
#     e,g, 
#       setwd ("/Users/bill/D/rdv-framework/projects/rdvPackages/biodivprobgen/R")

#   - Source this file (generateSetCoverProblem.R)
#       Under RStudio, that just means having this file open in the 
#       edit window and hitting the Source button.
#       
#   - Just to give a ballpark estimate of how long to expect the test to run, 
#     as the code and yaml file stand right now (2015 01 26 4:40 pm) took about 
#     1 minute 45 seconds to run on my MacBook Pro with much of the memory 
#     and disk unavailable and a backup copying process going on 
#     in the background. 

#  NOTE that the choice of random seed in the yaml file is important 
#  because the example creates a test problem based on drawing the control 
#  parameters from a random distribution.  When the seed was 111, the 
#  test crashed with the message below.  When I changed it to 701, it 
#  ran to completion.
#       Failing:  max_possible_tot_num_links ( 3291 ) > maximum allowed ( 2000 ).
#       Save workspace image to ~/D/rdv-framework/projects/rdvPackages/biodivprobgen/.RData? [y/n/c]: 
#  However, the fail was just what it was supposed to do when those 
#  parameters came up, so the yaml file could be changed to use 111 
#  instead of 701 if you want to induce a crash to test that error 
#  trapping.
    
#-------------------------------------------------------------------------------

#  Procedure to run a single test case USING tzar:

#   - Same as above, except:

#   - Set the emulatingTzar flag to FALSE, i.e., 
#       In emulatingTzarFlag.R, set:
#           emulatingTzar = FALSE

#   - In a terminal window, Make sure current working directory is not where 
#     the source code is, but rather, in a terminal window, cd to where the 
#     tzar jar is:    
#       /Users/bill/D/rdv-framework

#   - Instead of sourcing this R file, run the following command in the 
#     terminal window:
#           To put results in the tzar default area
#       java -jar tzar.jar execlocalruns /Users/bill/D/rdv-framework/projects/rdvPackages/biodivprobgen/R/
#   OR
#           To use a specially named tzar area for the test
#       java -jar tzar.jar execlocalruns /Users/bill/D/rdv-framework/projects/rdvPackages/biodivprobgen/R/ --runset=single_test_unifRand_p_r_n_a_seed_701_in_phase_transition_area
#       
#   - Just to give a ballpark estimate of how long to expect the test to run, 
#     as the code and yaml file stand right now (2015 01 26 4:40 pm) took about 
#     1 minute 15 or 20 seconds to run on my MacBook Pro with much of the  
#     memory and disk unavailable but no backup copying process going on 
#     in the background. 

#===============================================================================

#  History:

#  v1 - 
#  v2 - 
#  v3 - add degree distribution calculations
#  v4 - cleaning up code layout formatting
#  v5 - replacing node_link_pairs with link_node_pairs to match marxan puvspr

#  2014 12 10 - BTL
#       Starting to replace the data frames and methods for operating on them 
#       so that it's mostly done with sqlite.  It should make the code clearer 
#       and make it easy to keep the whole structure in files in the tzar 
#       output directory for each run.  This, in turn, will make it much 
#       easier to go back and do post-hoc operations on the graph and output 
#       data after the run.  Currently, as the experiments evolve, I keep 
#       finding that I want to do things that I had not foreseen and I need 
#       access to the data without re-running the whole set of experiments.
#
#       Before making the big changes, I've removed the huge block of notes 
#       and code that I had at the end of the file that were related to 
#       marxan and things I need to do.  I cut all of that out and pasted it 
#       into another R file called:
#           /Users/bill/D/rdv-framework/projects/rdvPackages/biodivprobgen/R/
#               oldCommentedMarxanNotesCodeRemovedFrom_generateSetCoverProblem_2014_12_10.R

#  2014 12 11 - BTL
#       - Replacing all references to "group" with "group", since I'm going to 
#           add the ability to have more than one independent node per group.   
#           These groups will no longer be groups if there is more than one 
#           independent node because by definition, no independent nodes can 
#           be linked.  There will still be a group inside the group, i.e., the 
#           dependent nodes, and the independent nodes will still link to those 
#           dependent nodes.  They just won't link to each other.  
#           Renaming also means that I need to change the names of a couple of 
#           variables in the yaml file who have "clique" in their name.
#
#       - Converted choice of integerize() function from hard-coded value to 
#           switch statement based on option given in yaml file.

#  2014 12 25 - BTL
#       - Split long original file into 17 source files and sourced them 
#         since the file had gotten unmanageable to work on and understand.

#  2014 12 28 - BTL
#       - Changed all source() calls to use the full path name since the 
#         source files weren't being found.  I think that tzar had the 
#         current directory set to biodivprobgen but the source is all in 
#         biodivprobgen/R.
#       - Revised all of the tzar emulation code since it wasn't working.  
#         Much of this was due to River having introduced the "metadata/" 
#         subdirectory into the tzar output directory structure and putting 
#         the parameters.R file in there instead of in the output directory 
#         itself.

#  2014 12 29 - BTL
#       - Moved both tzar control flag settings up into this file so that 
#         they are easier to find and control in one place.  I'm still 
#         leaving the actual setting of emulatingTzar in the file called 
#         emulatingTzarFlag.R for the reasons explained below, but I'm 
#         sourcing that file from here instead of in gscp_2_tzar_emulation.R 
#         because I couldn't remember where it was happening when I wanted 
#         to change it between runs.

#  2015 04 27 - BTL
#       - Extracted initialization code into biodivprobgen_initialization.R.
#       - Extracted utility functions into biodivprobgen_utilities.R.

#  2016 03 28 - BTL
#       - Converted most of the code in this project into functions instead 
#         of source() calls.  This is to help testing and conversion to being 
#         a package.

#===============================================================================
#       Run startup code that will not be part of the final bdpg package.
#       It creates a few global debugging variables and some code for 
#       printing debugging timepoints.
#       It also sets up a few things for tzar and tzar emulation.
#===============================================================================

    #  Specify the directory where code is to be sourced from.
    #  Need to do this in a better way so that it is appropriate for  
    #  anybody's setup.
    #  However, sourcing should go away altogether once I get all this 
    #  turned into a library, so this should stay now as a global variable 
    #  and should disappear once this is a package.

if (!exists ("sourceCodeLocationWithSlash"))
    sourceCodeLocationWithSlash = 
        "/Users/bill/D/Projects/ProblemDifficulty/src/bdprobdiff/R/"
    
source (paste0 (sourceCodeLocationWithSlash, "external_startup_code.R"))

#===============================================================================
#===============================================================================
#       ***  FROM HERE ON IS WHAT WOULD BE IN A BDPG PACKAGE       ***
#       ***  EXCEPT FOR SOME LIBRARY DECLARATIONS THAT ARE IN THE  ***
#       ***  EXTERNAL STARTUP CODE ABOVE.                          ***
#===============================================================================
#===============================================================================

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param parameters DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
gen_set_cover_problem = function (parameters)
{
#===============================================================================
#       Initialize.
#===============================================================================

    #  Initialize variables that have to be derived from values 
    #  set in the parameters structure (which is generally built 
    #  from project.yaml).

derived_bdpg_parameters = initialize_and_derive_parameters (parameters)   #  BUG?  UNKNOWN FOR XU FROM FILE?
bdpg_error_codes = derived_bdpg_parameters$bdpg_error_codes
    
#===============================================================================
#       Generate a problem, i.e, create the Xu graph nodes and edge_list.
#===============================================================================

EF_num <<- 0    #  2016 06 12 - BTL - Only used for debugging in searching for a lognormal...  Can remove if that search gets axed.

bdprob = gen_bdprob (parameters, bdpg_error_codes, 
                     derived_bdpg_parameters$integerize)

if (!bdprob@prob_is_ok)
    {
    clean_up (timepoints_df, cur_timepoint_num, parameters, emulatingTzar, 
              "\n\n>>>>>  gen_bdprob() failed.  <<<<<\n\n")
    stop ()
    }

#===============================================================================
#                   Save the values for the "correct" problem.
#===============================================================================

    #  The problem structures built so far represent the correct values.
    #  Adding error to the problem structure will create an apparent 
    #  problem structure that is probably different from the correct 
    #  structure.
    #  When we compute scores at the end of all this, we need to compute 
    #  them with respect to the correct problem rather than the apparent.
    #  So, before we add error, we need to save the values defining the 
    #  correct structure.

cor_PU_spp_pair_indices = bdprob@PU_spp_pair_indices
cor_bpm                 = bdprob@bpm
cor_num_PUs             = bdprob@num_PUs
cor_num_spp             = bdprob@num_spp
cor_nodes               = bdprob@nodes
cor_optimum_cost        = bdprob@cor_optimum_cost  #  BUG?  HOW IS THIS LOADED FOR XU FROM FILE?
cor_PU_costs            = bdprob@PU_costs
PU_col_name             = bdprob@PU_col_name
spp_col_name            = bdprob@spp_col_name

cor_PU_IDs              = bdprob@all_PU_IDs
cor_spp_IDs             = bdprob@all_spp_IDs

cat ("\n\nAbout to set all_correct_node_IDs.\n")
#browser()
    #  Some nodes may be unusued in cor_nodes, particularly if building a 
    #  compound problem, e.g., if wrapping a distribution around a Xu problem.  
    #  Need to add them into this list since it will be used to index array 
    #  entries, you can't have any missing indices.
#all_correct_node_IDs = cor_nodes$node_ID
all_correct_node_IDs = 1:max(cor_nodes$node_ID)    

presences_col_name = "freq"

#===============================================================================

cor_or_app_subdir_name = "cor"

        #  NOTE:  2016 06 12 - Need to add writing of flags resulting from 
        #                       reading Xu file, e.g., prob_generator_params_known.
        #                       This is because learning alg downstream needs to 
        #                       know things like whether the generator's params 
        #                       are even known, so that it doesn't try to learn 
        #                       something from missing data.
  
do_graph_and_marxan_analysis (cor_or_app_subdir_name, 
                              
                                            #  input parameters
                                          parameters, 
                                          emulatingTzar, 
                                          DEBUG_LEVEL, 
                                          #derived_bdpg_parameters,   #  BUG?  UNKNOWN FOR XU FROM FILE?
                                          derived_bdpg_parameters$current_os, 
                                          
                                            #  parameters from gen prob.
#                                          bdprob$derived_Xu_params, 
                                          bdprob@Xu_parameters, 
                                          bdprob@read_Xu_problem_from_Xu_file, 
                                          
#            PU_spp_pair_names,  #NO
                                                         
                                            #  From bdprob structure, i.e., results of gen prob routine                           
                                          cor_num_spp, 
                                          cor_num_PUs, 
                                          cor_PU_spp_pair_indices, 
    cor_PU_IDs, #####!!!!!#####
    cor_spp_IDs,  #####!!!!!#####
                                          cor_bpm, 

                            cor_PU_costs, 
                                          cor_optimum_cost, 
                                          cor_nodes, 
                                          spp_col_name, 
                                          PU_col_name, 
                                          
                                            #  Immediately after bdprob struct vars above.
                                          presences_col_name, #  hard-coded as "freq"
#####!!!!!#####                                          all_correct_node_IDs, 

                                            #  Results of adding error.
                                            cor_num_spp,
                                            cor_num_PUs,
                                          cor_PU_spp_pair_indices, 
                                          cor_bpm, 
                                          
                                            #  input parameters for error model.
                                          add_error=FALSE, 
                                          match_error_counts=FALSE, 
                                          FP_const_rate=0, 
                                          FN_const_rate=0, 
                                          original_FP_const_rate=0, 
                                          original_FN_const_rate=0
                                          )

#===============================================================================
#                   Add error to the species occupancy data.
#===============================================================================

add_error = FALSE   
if (! is.null (parameters$add_error_to_spp_occupancy_data))
    add_error = parameters$add_error_to_spp_occupancy_data

if (add_error)
    {
    ret_vals_from_add_errors = 
        add_error_to_spp_occupancy_data (parameters, cor_bpm, 
                                         cor_num_PU_spp_pairs, 
                                         cor_num_PUs, cor_num_spp, 
                                         bdpg_error_codes)

        #  Save the chosen error parameters to output later with results.
    original_FP_const_rate = ret_vals_from_add_errors$original_FP_const_rate
    original_FN_const_rate = ret_vals_from_add_errors$original_FN_const_rate
    match_error_counts     = ret_vals_from_add_errors$match_error_counts
    FP_const_rate          = ret_vals_from_add_errors$FP_const_rate
    FN_const_rate          = ret_vals_from_add_errors$FN_const_rate
    app_num_spp            = ret_vals_from_add_errors$app_num_spp
    app_num_PUs            = ret_vals_from_add_errors$app_num_PUs

        #  Set the values for the apparent problem structure.        
    app_PU_spp_pair_indices      = ret_vals_from_add_errors$app_PU_spp_pair_indices
    app_bpm                      = ret_vals_from_add_errors$app_spp_occupancy_data  
    
#=================================

        #  Create subdirectory name for this apparent problem.
        #  In the future, we may allow more than 1 app per cor, so 
        #  I'll add an app count to the end of the subdirectory name and 
        #  nest it under a more general "app" directory that corresponds to 
        #  the "cor" directory.

    cur_app_num = 1
    cor_or_app_subdir_name = paste0 ("app", .Platform$file.sep, "app.", cur_app_num)
    
    do_graph_and_marxan_analysis (cor_or_app_subdir_name, 
                                  
                                                #  input parameters
                                              parameters, 
                                              emulatingTzar, 
                                              DEBUG_LEVEL, 
                                              #derived_bdpg_parameters,   #  BUG?  UNKNOWN FOR XU FROM FILE?
                                              derived_bdpg_parameters$current_os, 
                                              
                                                #  parameters from gen prob.
#                                              bdprob$derived_Xu_params, 
                                              bdprob@Xu_parameters, 
                                              bdprob@read_Xu_problem_from_Xu_file, 
                                              
    #            PU_spp_pair_names,  #NO
                                                             
                                                #  From bdprob structure, i.e., results of gen prob routine                           
                                              cor_num_spp, 
                                              cor_num_PUs, 
                                              cor_PU_spp_pair_indices, 
    cor_PU_IDs, #####!!!!!#####
    cor_spp_IDs,  #####!!!!!#####
                                              cor_bpm, 

                            cor_PU_costs, 
                                              cor_optimum_cost, 
                                              cor_nodes, 
                                              spp_col_name, 
                                              PU_col_name, 
                                              
                                                #  Immediately after bdprob struct vars above.
                                              presences_col_name, #  hard-coded as "freq"
#####!!!!!#####                                              all_correct_node_IDs, 
    
                                                #  Results of adding error.
                                                app_num_spp,
                                                app_num_PUs,
                                              app_PU_spp_pair_indices, 
                                              app_bpm, 
                                              
                                                #  input parameters for error model.
                                              add_error, 
                                              match_error_counts, 
                                              FP_const_rate, 
                                              FN_const_rate, 
                                              original_FP_const_rate, 
                                              original_FN_const_rate
                                              )
    } #####else    #  Don't add error.
#     {         #  Since no error is being added, correct and apparent are the same.
#         
#     app_PU_spp_pair_indices      = cor_PU_spp_pair_indices
#     app_bpm                      = cor_bpm        
#     }


#===============================================================================

clean_up (timepoints_df, cur_timepoint_num, parameters, emulatingTzar, 
          "\n\n>>>>>  Ran to completion.  <<<<<\n\n")

#===============================================================================

}

gen_set_cover_problem (parameters)

    #  If you were echoing console output to a temp file, 
    #  stop echoing and close the temp file.
    #  That echoing is currently initiated by a sink() call in 
    #  external_startup_code.R (Dec 14, 2016).

if (echoConsoleToTempFile)
    {
    sink ()
    close (tempConsoleOutFile)
    }

#===============================================================================


