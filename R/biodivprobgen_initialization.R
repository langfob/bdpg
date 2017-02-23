#===============================================================================
#                       biodivprobgen_initalization.R
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

########################################################################################
#  TEMPORARY:  Echo information about data structures of all currently active variables.
cat("\n\nvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
print (sys.call())
v=ls();
sapply (v,function(x){cat ("\n", x, "\n", sep='');str(get(x),vec.len=1,max.level=1)});
cat("\n\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n")
########################################################################################

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

#' Title
#'
#' @return
#' @export

init_for_bdpg <- function (parameters)
    {
        #  Set random seed to help reproducibility.
        #  Has to be done after startup code that loads parameters structure.
    set.seed (parameters$seed)

        #  Initialize error codes.
    bdpg_error_codes        = get_bdpg_error_codes ()

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
        #  parameters$fullOutputDirWithSlash was "tzarout/", then :
        #       file.open (parameters$fullOutputDirWithSlash, "abc/")
        #  would return "tzarout//abc", rather than the desired "tzarout/abc".
        #-----------------------------------------------------------------------

    parameters$fullOutputDir_NO_slash <-
        normalizePath (strip_trailing_slash (parameters$fullOutputDirWithSlash),
                       mustWork=FALSE)

    return (list (parameters = parameters,
                  bdpg_error_codes = bdpg_error_codes))
    }

#===============================================================================

#' Helper function to create object to hold global constants
#'
#'  When I try to create an instance of the Global_Constants_v01 class outside
#'  of the bdpg package (e.g., in bdpgxupaper), I can't get R to recognize
#'  the class, even though the bdpg package is imported.  I've tried a bunch
#'  of different ways to trick R into recognizing it, but none of them have
#'  worked except wrapping the creation inside a bdpg function and exporting
#'  that function.  This is the wrapper function that I created to do that.
#'
#'@section Attempts That Failed:
#'
#'  Just to document what I tried, here are the different bits of code that I
#'  stuck in bdpgxupaper's mainline function and the errors they caused, some
#'  at run time and others at build time.
#'
#'\enumerate{
#'  \item{\strong{Direct use of assign \emph{without} bdpg package qualification}
#'
#'  Inserting the following command in the main function:
#'      \preformatted{
#'      assign("GC", new ("Global_Constants_v01"), envir = .GlobalEnv)}
#'      caused the following runtime error message:
#'      \preformatted{
#'      Error in getClass(Class, where = topenv(parent.frame())) :
#'        “Global_Constants_v01” is not a defined class
#'      Called from: getClass(Class, where = topenv(parent.frame()))
#'      }}
#'  \item{\strong{Direct use of assign \emph{with} bdpg package qualification}
#'
#'  Inserting the following command in the main function:
#'      \preformatted{
#'      assign("GC", new ("bdpg::Global_Constants_v01"), envir = .GlobalEnv)}
#'      caused the following runtime error message:
#'      \preformatted{
#'      Error in getClass(Class, where = topenv(parent.frame())) :
#'        “bdpg::Global_Constants_v01” is not a defined class
#'      Called from: getClass(Class, where = topenv(parent.frame()))
#'      }}
#'  \item{\strong{Leaving bdpg qualifier but adding @import to main function documentation}
#'
#'  Leaving the "\code{new ("bdpg::Global_Constants_v01")...}" and inserting
#'  the following roxygen command in the main function's documentation:
#'      \preformatted{
#'       @import bdpg::Global_Constants_v01
#'       xu_paper_main (parameters)
#'       ...}
#'      caused the following BUILD error message:
#'      \preformatted{
#'      ==> devtools::document(roclets=c('rd', 'collate', 'namespace'))
#'
#'      Updating bdpgxupaper documentation
#'      Loading bdpgxupaper
#'      Writing NAMESPACE
#'      Warning messages:
#'      1: In loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
#'        there is no package called ‘#'’
#'      2: In loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
#'        there is no package called ‘@export’
#'      3: In loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
#'        there is no package called ‘bdpg::Global_Constants_v01’
#'      Documentation completed
#'
#'      ==> R CMD INSTALL --preclean --no-multiarch --with-keep.source bdpgxupaper
#'
#'      * installing to library ‘/Users/bill/D/R_libraries’
#'      * installing *source* package ‘bdpgxupaper’ ...
#'      ** R
#'      ** preparing package for lazy loading
#'      Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
#'        there is no package called ‘bdpg::Global_Constants_v01’
#'      ERROR: lazy loading failed for package ‘bdpgxupaper’
#'      * removing ‘/Users/bill/D/R_libraries/bdpgxupaper’
#'      * restoring previous ‘/Users/bill/D/R_libraries/bdpgxupaper’
#'
#'      Exited with status 1.
#'       }}
#'
#'  \item{\strong{Leaving bdpg qualifier but adding @import outside main function documentation}
#'
#'  Leaving the "\code{new ("bdpg::Global_Constants_v01")...}" and inserting
#'  the following roxygen command at the very start of the main function's file
#'   but not part of the function's documentation:
#'      \preformatted{
#'       @import bdpg::Global_Constants_v01
#'       ...}
#'      caused the following BUILD error message:
#'      \preformatted{
#'      ==> devtools::document(roclets=c('rd', 'collate', 'namespace'))
#'
#'      Updating bdpgxupaper documentation
#'      Loading bdpgxupaper
#'      Writing xu_paper_main.Rd
#'      Writing NAMESPACE
#'      Warning message:
#'      In loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
#'        there is no package called ‘bdpg::Global_Constants_v01’
#'      Documentation completed
#'
#'      ==> R CMD INSTALL --preclean --no-multiarch --with-keep.source bdpgxupaper
#'
#'      * installing to library ‘/Users/bill/D/R_libraries’
#'      * installing *source* package ‘bdpgxupaper’ ...
#'      ** R
#'      ** preparing package for lazy loading
#'      Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) :
#'        there is no package called ‘bdpg::Global_Constants_v01’
#'      ERROR: lazy loading failed for package ‘bdpgxupaper’
#'      * removing ‘/Users/bill/D/R_libraries/bdpgxupaper’
#'      * restoring previous ‘/Users/bill/D/R_libraries/bdpgxupaper’
#'
#'      Exited with status 1.
#'       }}
#'
#'  \item{\strong{Leaving bdpg qualifier but importing the class explicitly in DESCRIPTION}
#'
#'  Leaving the "\code{new ("bdpg::Global_Constants_v01")...}" and inserting
#'  \code{bdpg::Global_Constants_v01} in the DESCRIPTION file with the other
#'  imports:
#'      \preformatted{
#'      Imports:
#'          bdpg,
#'          bdpg::Global_Constants_v01
#'      Suggests:
#'      }
#'      caused the following BUILD error message:
#'      \preformatted{
#'      ==> devtools::document(roclets=c('rd', 'collate', 'namespace'))
#'
#'      Updating bdpgxupaper documentation
#'      Loading bdpgxupaper
#'      Invalid DESCRIPTION:
#'      Malformed Depends or Suggests or Imports or Enhances field.
#'      Offending entries:
#'        bdpg::Global_Constants_v01
#'      Entries must be names of packages optionally followed by '<=' or '>=',
#'      white space, and a valid version number in parentheses.
#'
#'      See section 'The DESCRIPTION file' in the 'Writing R Extensions'
#'      manual.
#'
#'      Error in (function (dep_name, dep_ver = NA, dep_compare = NA)  :
#'        Dependency package bdpg::Global_Constants_v01 not available.
#'      Calls: suppressPackageStartupMessages ... <Anonymous> -> load_all -> load_imports -> mapply -> <Anonymous>
#'      Execution halted
#'
#'      Exited with status 1.
#'       }}
#'       }
#' @export
#' @seealso \code{"\linkS4class{Global_Constants_v01}"}

create_global_constants <- function ()
    {
#    x <- new ("Global_Constants_v01")
#    assign("GC", x, envir = .GlobalEnv)
    assign("GC", new ("Global_Constants_v01"), envir = .GlobalEnv)
    }

#===============================================================================

