#===============================================================================

                        #  std_param_defns.R

#===============================================================================

#' Standardized parameter definitions for reuse across all functions
#'
#' This function should \emph{never} be called.  It is intended strictly as a
#' place to assemble all @@param values that are to be inherited by other
#' functions in their Roxygen documentation.
#'
#' In general, when you're documenting a function's parameters using Roxygen and
#' the same parameter is defined for multiple routines, you're supposed to use
#' \code{@@inheritParams} and point to one other function as the single source
#' of that parameter's definition.  That doesn't work well for me because often,
#' the function that you want to inherit from:
#'      \enumerate{
#'       \item{has \emph{some} of the variables you want to inherit, and/or}
#'       \item{is \emph{missing} others, and/or}
#'       \item{has \emph{irrelevant} variables that you want to ignore, or}
#'       \item{has some \emph{combination} of the above and you have to inherit
#'             from multiple functions to get some subset of params from
#'             all of them and it becomes difficult to guess which param
#'             definition is drawn from which other function.}
#'       }
#'
#' There is no designation of what is \emph{the} canonical base function
#' for all copies of a particular \code{@@param} that gets inherited.
#' Consequently, if you don't realize that something depends on a function
#' used in an \code{@@inheritParams} command somewhere in your code
#' and you delete or rename the function, or delete or rename an inherited
#' argument, you can break downstream documentation.
#'
#' It would be better to have a designated data dictionary where all of these
#' standardized things can be assembled and all \code{@@inheritParams} can
#' point.  Since that doesn't exist, I'm making this dummy function that I will
#' use by convention as the one central place to copy parameter declarations
#' from when I want them.  However, the only way I can see to fool Roxygen into
#' doing this is by creating a dummy function that's never called and exists
#' solely to be the source of inherited parameter definitions.
#'
#' There are some potential problems with this:
#'      \itemize{
#'       \item{It may have performance problems when Roxygen documentation is
#'             built since the list of parameters here will be long.  I have
#'             no idea whether that will be a problem or not.}
#'       \item{It doesn't solve the problem in point 3 above, i.e., it will
#'             have \emph{tons} of irrelevant variables for nearly every
#'             function that inherits from it.  Again, I don't know if there
#'             are any consequences of that.  Roxygen does properly ignore the
#'             irrelevant variables I think, but it still makes for a lot of
#'             mental noise for the person doing the documenting when they're
#'             looking at all these other functions and trying to sort out what
#'             is relevant and what isn't.  Hopefully, doing it as one giant
#'             alphabetical list in just one place makes it a little easier...}
#'       \item{I \emph{think} that you can override a parameter definition
#'             locally if you want a different local definition but say that
#'             you want to inherit params (e.g., to satisfy other variables in
#'             your parameter list), but I'm not absolutely certain of that at
#'             the moment.  If that doesn't work, it may be that you just have
#'             to rename the variable locally to avoid the conflict.}
#'             }
#'
#' @section Conventions:
#' The main convention here will be to try to keep the parameters in
#' alphabetical order to make them easier to find and not duplicate.
#'
#' @section How to use:
#' \describe{
#'           \item{To inherit an \code{@@param} from here:}{In the Roxygen documentation
#'                 for any function where you want to use a parameter definition from
#'                 this list, you just include a statement saying:
#'
#'                 \code{#' @@inheritParams std_param_defns}
#'                }
#'           \item{To add a parameter \code{x} to the list of params in the list here:}{
#'                 \itemize{
#'                          \item{Insert the name \code{x} in alphabetical order
#'                                in the parameter list for the dummy function
#'                                \code{std_param_defns}.
#'                                }
#'                          \item{Insert "\code{@@param x blah blah blah}" in
#'                                alphabetical order in the list of \code{@@param}s in
#'                                the documentation for the dummy function
#'                                \code{std_param_defns}.
#'                                }
#'                          }
#'                }
#'          }
#'
#'
#' @param integerize_string string containing name of the function to use to
#'     convert floats to integers
#' @param parameters parameters list for the run, usually derived from project.yaml
#'     and can have a varying number and set of elements depending on the run

std_param_defns <-
    function (
            integerize_string,
            parameters
             )
    {
    stop (paste0 ("\n\nstd_param_defns() is not meant to be called.  ",
                  "\n    Exist solely to provide standardized parameter ",
                  "definitions to inherit @param statements from.\n\n"))

    return ("Does nothing")
    }

#===============================================================================

