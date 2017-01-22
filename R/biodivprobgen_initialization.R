#===============================================================================
#                       biodivprobgen_initalization.R
#===============================================================================

#  History:

#  2016 03 28 - BTL - (re)created
#   A file with this same name existed before and then was deleted after 
#   all of its contents were rolled into various functions during refactoring.
#   However, I now have an initialization function that didn't exist before 
#   so I'm creating a new file reusing the old name.

#===============================================================================

initialize_and_derive_parameters = function (parameters)
    {
        #  Structure to hold values that used to be global variables 
        #  before everything was turned into functions.  
        #  These are values that are derived once the run has started, 
        #  so they can't be set in the project.yaml file.
    derived_bdpg_parameters = list()
    
        #----------------------------------------------------------------
        #  Values to return from the program when quitting on a serious
        #  error.
        #----------------------------------------------------------------
    
    bdpg_error_codes = list()
    bdpg_error_codes$ERROR_STATUS_num_inside_or_within_group_links_less_than_one = 1001
    bdpg_error_codes$ERROR_STATUS_optimal_solution_is_not_optimal = 1002
    bdpg_error_codes$ERROR_STATUS_num_nodes_per_group_must_be_at_least_2 = 1003
    bdpg_error_codes$ERROR_STATUS_duplicate_spp_in_Xu_input_file = 1004
    bdpg_error_codes$ERROR_STATUS_unknown_spp_occ_FP_error_type = 1005
    bdpg_error_codes$ERROR_STATUS_unknown_spp_occ_FN_error_type = 1006
    
    derived_bdpg_parameters$bdpg_error_codes = bdpg_error_codes
    
    #---------------------------------------------------------------    
        #  Determine the OS so you can assign the correct name for 
        #  the marxan executable, etc.
        #   - for linux this returns linux-gnu
        #   - for mac this currently returns os = 'darwin13.4.0'
        #   - for windows this returns mingw32
    
    derived_bdpg_parameters$current_os <- sessionInfo()$R.version$os
    cat ("\n\nos = '", derived_bdpg_parameters$current_os, "'\n", sep='')

    #---------------------------------------------------------------    

    cat ("\n\n", 
         "run_id = '", parameters$run_id, "'\n", 
         "runset_name = '", parameters$runset_name, "'\n", 
         "runset_description = \n", parameters$runset_description, "\n", 
         "\n", sep='')
    
    #---------------------------------------------------------------    

    derived_bdpg_parameters$integerize = switch (parameters$integerize_string, 
                                                 round=round, 
                                                 ceiling=ceiling, 
                                                 floor=floor, 
                                                 round)    #  default to round()
    
    #---------------------------------------------------------------    

    return (derived_bdpg_parameters)
    }

#===============================================================================

