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

    #  BTL - 2017 01 23
    #  DEPRECATED
    #  This function is going to be replaced by the set of new functions
    #  defined below.  I'm leaving this function definition here for now
    #  to make it easier to figure out how to make the transition to using
    #  the new functions throughout the existing calling code.
    #  This function can be deleted after all downstream uses of the data
    #  it generates have been provided by replacing this call with calls
    #  to the new functions.

#' Title
#'
#' @param parameters list of parameters (normally read from project.yaml)
#'
#' @return derived_bdpg_parameters
#' @export

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
#===============================================================================

#' Builds the error codes returned by errors in bdpg package
#'
#' @return list of integer error codes
#' @export
#' @examples
#' get_bdpg_error_codes ()

get_bdpg_error_codes <- function ()
    {
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

    return (bdpg_error_codes)
    }

#===============================================================================

#' Look up a specific bdpg error code given its name
#'
#' @param bdpg_error_name string containing name of error code to look up in list of error codes
#'
#' @return integer error code for the given error name
#' @export
#'
#' @examples
#' get_bdpg_error_code ("ERROR_STATUS_unknown_spp_occ_FP_error_type")
#'
get_bdpg_error_code <- function (bdpg_error_name)
    {
    bdpg_error_codes = get_bdpg_error_codes ()

    return (bdpg_error_codes [[bdpg_error_name]])
    }

#===============================================================================

#' Get name and version number of current operating system as a string
#'
#' This function is only here as a convenience because I can never remember
#' where the current operating system is stored.
#'
#' @return string containing name and version of current operating system
#' @export
#'
#' @examples
#' get_current_os ()
#'
get_current_os <- function ()
    {
    sessionInfo()$R.version$os
    }

#===============================================================================

#' Get function to use when converting numeric values to integers
#'
#' Many computations return floating point numbers that need to be passed
#' to other routines as integers.  The method used to convert them to integers
#' may affect the outcome of the downstream operation, so this function
#' looks up which function to use based on the user's specification in the
#' parameter inputs.
#'
#' @param integerize_string string containing name of the function to use to
#'     convert floats to integers
#'
#' @return function to use to convert floats to integers
#' @export
#'
#' @examples
#' get_integerize_function ("ceiling")
#'
get_integerize_function <- function (integerize_string)
    {
#    switch (parameters$integerize_string,
    switch (integerize_string,
            round=round,
            ceiling=ceiling,
            floor=floor,
            round)    #  default to round()
    }

#===============================================================================

