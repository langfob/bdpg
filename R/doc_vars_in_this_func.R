#===============================================================================
#
#                       doc_vars_in_this_func.R
#
#===============================================================================

#  Library references:
#      - Uses gtools::odd()
#      - Uses sourcetools::tokenize_string()

#  Part of the mainline was derived by reading:
#  From:  http://www.numbertheory.nl/2013/03/24/parsing-complex-text-files-using-regular-expressions-and-vectorization/

#===============================================================================

#' Document variables and their types that are visibile inside current function
#'
#' Call the str() function for all variables visible inside the current
#' function.  Before doing that, write out a flag line to indicate the start
#' of a section of output for a particular function and write the call made
#' to that function, i.e., function name followed by its argument list.
#' After writing out the structures of all the variables write out a
#' closing line to flag the end of str() output for the current function.
#'
#' @param sys.call_ht negative integer to pass to sys.call() to specify how
#'     high to look on the \code{\link{sys.call}} stack.
#' @param sys.frame_ht negative integer to pass to sys.frame() to specify how
#'     high to look on the \code{\link{sys.frame}} stack.
#'
#' Generally shouldn't need to set either of the arguments since the value
#' either defaults to the correct value or is set correctly inside
#' \code{\link{doc_vars_in_this_func_once}}.
#'
#' @seealso \code{\link{doc_vars_in_this_func_once}}
#' @return Returns nothing
#' @export

doc_vars_in_this_func <- function (sys.call_ht = -1,
                                   sys.frame_ht = -1)
    {
    cat("\n\n>>>>>>>>>>>>>>>>>>>>>>>>  START doc_vars_in_this_func  >>>>>>>>>>>>>>>>>>>>>>>>\n");
    print (sys.call (sys.call_ht))
    print(ls.str(envir = sys.frame (sys.frame_ht))) ## [1] "aa" "t2"
    cat("<<<<<<<<<<<<<<<<<<<<<<<<  END doc_vars_in_this_func  <<<<<<<<<<<<<<<<<<<<<<<<<<\n")
    }

#===============================================================================

#' Increment a global counter specific to the current function
#'
#' This function creates a global counter for a function if that counter
#' doesn't exist yet.  If it already exists, then it increments that counter.
#' This is primarily used to help \code{\link{doc_vars_in_this_func_once}}
#' determine whether to call \code{\link{doc_vars_in_this_func}}.  This is
#' because you usually don't want to call the documenter more than once,
#' no matter how often the function it's documenting is called.
#'
#' This function creates what is hoped to be a unique name for the counter
#' based on the name of the function, i.e., it prepends a copy of the function's
#' name with a strange string.  That prepend string is
#' "TEMPCTR___RM_THIS_AT_END__".  If the function being documented was called
#' func1, then the resulting counter's name would be
#' "TEMPCTR___RM_THIS_AT_END__func1".
#'
#' @section Possible (though unlikely) problems:
#' This method of creating names for counters is not foolproof, but it's good
#' enough for the quick hacking of documentation aids that this code is
#' intended to support.  There are at least two ways that it might screw up,
#' but each has a fix.
#'
#' \describe{
#'   \item{Name matches a name in user's code}{If the user's code contained
#'       a name matching the very strange generated name, the ctr_name_prefix
#'       can be set in the argument list.}
#'   \item{Generic function}{There may be different versions of the same
#'       generic function in use at the same time and they will all end up
#'       being counted as the same function since the generated name will
#'       be the same.  In this case, one solution is to pass in a different
#'       unique ctr_name_prefix in the call in each of the different uses of the
#'       generic. Depending on how long it takes for the program to run,
#'       it may be simpler to just re-run the whole program with the call
#'       cut and pasted into each different form of the generic each time
#'       the program is re-run; not pretty, but perhaps the least trouble in
#'       the end. }
#' }
#'
#' @inheritParams doc_vars_in_this_func
#' @param ctr_name_prefix character string to prepend to function name when
#'     building a counter name that is supposed to be unique
#'
#' @return integer value of global counter for the current function
#' @example
#' @export

bump_global_ctr_for_cur_func <- function (sys.call_ht=-7,
                                          ctr_name_prefix =
                                              "TEMPCTR___RM_THIS_AT_END__")
    {
    v7 <- capture.output (sys.call (sys.call_ht))
    syscall_tokens <- tokenize_string (v7)
    func_name <- syscall_tokens [1, "value"]
    ctr_name <- paste0 (ctr_name_prefix, func_name)

    ctr <- 0
    if (exists (ctr_name))
        ctr <- get (ctr_name)
    ctr <- ctr + 1

    assign (ctr_name, ctr, envir = .GlobalEnv)

    return (ctr)
    }

#===============================================================================

#' Document function's variables a limited number of times
#'
#' This is a wrapper function for \code{\link{doc_vars_in_this_func}}.
#' It makes sure that \code{doc_vars_in_this_func} is only run once
#' in a given invocation of a program, no matter how many times the function
#' containing the \code{doc_vars_in_this_func} call is called.  This is
#' useful in avoiding getting a bunch of duplicate outputs for the same
#' function when it is called more than once in a program.
#'
#'
#' @param max_ct integer maximum number of times to write the documentation
#'     for a function, no matter how many times it's called
#' @inheritParams bump_global_ctr_for_cur_func
#'
#' @seealso \code{\link{doc_vars_in_this_func}} for the main action of this
#'     routine
#' @seealso \code{\link{doc_vars_in_this_func_once}} for explanation about
#'     ctr_name_prefix, which you will probably never touch
#' @return Returns nothing
#' @export

doc_vars_in_this_func_once <- function (max_ct = 1,
                                        ctr_name_prefix =
                                              "TEMPCTR___RM_THIS_AT_END__")
    {
    ctr_for_cur_func <- bump_global_ctr_for_cur_func (sys.call_ht=-7,
                                                      ctr_name_prefix)

    if (ctr_for_cur_func <= max_ct)
        doc_vars_in_this_func (sys.call_ht=-2, sys.frame_ht=-2)
    }

#===============================================================================
                                #  TESTS...
#===============================================================================

test_bump_global_ctr_for_cur_func <- function (num_times=5)
    {
    for (kkk in 1:num_times)
        ctr_value <- bump_global_ctr_for_cur_func ()

    return (ctr_value)
    }

#-------------------------------------------------------------------------------

xxx <- function (y)
    {
    cat ("\n\nIn xxx(), y = '", y, sep='')
    bump_global_ctr_for_cur_func ()
    cat ("\nLeaving xxx() now.\n-----------------------\n")
    }

#-------------------------------------------------------------------------------

zzz <- function (a_in_z=3, run_once=TRUE)
    {
    if (run_once)
        {
        doc_vars_in_this_func_once ()
        } else
        {
        doc_vars_in_this_func()
        }
    }

kkk <- function (num_times=2, run_once=TRUE)
    {
    for (iii in 1:num_times)
        zzz (a_in_z=3, run_once)

    if (run_once)
        {
        doc_vars_in_this_func_once ()
        } else
        {
        doc_vars_in_this_func()
        }
    }

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

if (FALSE)
{
cat ("\n\n================  run ONCE  ================\n\n")
kkk (3, TRUE)
cat ("\n\n================  run 3 times  ================\n\n")
kkk (3, FALSE)

temp_vars_to_delete <- ls (pattern="TEMPCTR___RM_THIS_AT_END__*")
cat ("\n\ntemp_vars_to_delete = \n")
print (temp_vars_to_delete)
yes_or_no <- utils::menu(c("Yes", "No"), title="Remove all of these variables?")
if (yes_or_no == 1)
    rm (list=temp_vars_to_delete)
}

#===============================================================================

