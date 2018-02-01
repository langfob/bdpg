#===============================================================================
#                       biodivprobgen_initalization.R
#===============================================================================

#-------------------------------------------------------------------------------

#' Get name and version number of current operating system as a string
#'
#' This function is only here as a convenience because I can never remember
#' where the current operating system is stored.
#'
#-------------------------------------------------------------------------------

#' @return Returns string containing name and version of current operating system
#' @export
#'
#' @examples
#' get_current_os ()

#-------------------------------------------------------------------------------

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
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns function to use to convert floats to integers
#' @export
#'
#' @examples
#' get_integerize_function ("ceiling")
#'
#-------------------------------------------------------------------------------

get_integerize_function <- function (integerize_string = "round")
    {
    if (is.null (integerize_string)) integerize_string = "round"

    switch (integerize_string,
            round   = round,
            ceiling = ceiling,
            floor   = floor,
            round)    #  default to round()
    }

#===============================================================================

#' Initialize for biodiversity problem generation
#'
#' Central point for doing a few odds and ends of initializing.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns parameters list
#' @export

#-------------------------------------------------------------------------------

init_for_bdpg <- function (parameters)
    {
        #  Set random seed to help reproducibility.
        #  Has to be done after startup code that loads parameters structure.
        #  If bdpg_run_init_seed is not in the parameters list,
        #  parameters$bdpg_run_init_seed will be NULL.

    always_set_new_or_forced_rand_seed ("Start of init_for_bdpg()",
                                        parameters$bdpg_run_init_rand_seed)


    cat ("\n\n================================================================================")
    cat ("\n================================================================================\n\n")

        #-----------------------------------------------------------------------
        #  For historical reasons, tzar returns the output directory path with
        #  a slash on the end.  This is often inconvenient when passing the
        #  directory to file.open(), so make a version of the variable that
        #  has the slash stripped off.
        #  The reason that the slash can be a problem is that if you call
        #  file.open() like this, it will double up the slash in the
        #  output since file.open() only strips a trailing slash from the
        #  LAST ENTRY in its list of args.  For example, assume that
        #  parameters$full_output_dir_with_slash was "tzarout/", then :
        #       file.open (parameters$full_output_dir_with_slash, "abc/")
        #  would return "tzarout//abc", rather than the desired "tzarout/abc".
        #-----------------------------------------------------------------------

    parameters$fullOutputDir_NO_slash <-
        normalizePath (strip_trailing_slash (parameters$full_output_dir_with_slash),
                       mustWork=FALSE)

    return (parameters)
    }

#===============================================================================

