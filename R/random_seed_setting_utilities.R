#===============================================================================

                        #  random_seed_setting_utilities.R

#  Tools for managing the seed in random number generation to make it easier
#  to reproduce all or part of a run.

#===============================================================================

#' Get forced seed value if necessary
#'
#' Determines whether a seed value is given or needs to be set or not
#' generated at all based on input options in the parameters list.
#'
#' 2017 12 06 - BTL
#'  NOTE:  basic_or_wrapped_or_comb_str = "COMB" does not exist at the moment,
#'  but it's mentioned in BDProb.R, so it's covered here for
#'  future compatibility.

#-------------------------------------------------------------------------------

#' @param is_rsrun boolean indicating that the seed creation test is being
#' done just before the creation of an rs RUN object
#' @param is_rsprob boolean indicating that the seed creation test is being
#' done just before the creation of an rs PROBLEM object
#' @param cor_or_app_str character string containing "COR" or "APP"
#' @param basic_or_wrapped_or_comb_str character string containing "BASE" or
#' "WRAP" or "COMB"
#'
#' @inheritParams std_param_defns
#'
#' @return Returns an integer seed value or NA
#' @export

#-------------------------------------------------------------------------------

get_forced_seed_value_if_necessary <- function (is_rsrun,
                                                is_rsprob,
                                                parameters,
                                                cor_or_app_str,
                                                basic_or_wrapped_or_comb_str)
    {
        #  Make sure that one and only one type of seed is chosen to examine.
    if (xor (is_rsrun, is_rsprob))
        {
        stop (paste0 ("\n\nERROR in get_forced_seed_value():",
                      "\n    one and only one of these arguments must be TRUE:",
                      "\n    is_rsrun = '", is_rsrun, "'",
                      "\n    is_rsprob = '", is_rsprob, "'",
                      "\n\n"))
        }

        #  Simplify comparisons by making sure string values are upper case.
    cor_or_app_str = toupper (cor_or_app_str)
    basic_or_wrapped_or_comb_str = toupper (basic_or_wrapped_or_comb_str)

    #---------------------------------------------------------------------------
    #                       Handle seeds for rs RUNS.
    #---------------------------------------------------------------------------

    if (is_rsrun)
    {                               #-----------------------
    if (cor_or_app_str == "APP")    #  An apparent problem
        {                           #-----------------------
        forced_seed = switch (basic_or_wrapped_or_comb_str,
                              "BASE" = parameters$app_base_rsrun_rand_seed,
                              "WRAP" = parameters$app_wrap_rsrun_rand_seed,
                              "COMB" = parameters$app_comb_rsrun_rand_seed,
                              NA)
        # if (!is.null (forced_seed) & is.na (forced_seed))
        #     bad_basic_or_wrapped_or_comb_str = TRUE

        #------------------------------
                                               #---------------------
        } else if (cor_or_app_str == "COR")    #  A correct problem
        {                                      #---------------------
        forced_seed = switch (basic_or_wrapped_or_comb_str,
                              "BASE" = parameters$cor_base_rsrun_rand_seed,
                              "WRAP" = parameters$cor_wrap_rsrun_rand_seed,
                              "COMB" = parameters$cor_comb_rsrun_rand_seed,
                              NA)

        #------------------------------
                  #---------------------------------------------
        } else    #  Error:  cor_or_app_str not "APP" or "COR"
        {         #---------------------------------------------
        stop (paste0 ("\n\nERROR in get_forced_seed_value():",
                      "\n    Bad string match for cor_or_app_str arg = '",
                      cor_or_app_str, "'",
                      "\n    Must be BASE or WRAP or COMB.\n\n"))
        }

    #---------------------------------------------------------------------------
    #                   Handle seeds for rs PROBLEMS.
    #---------------------------------------------------------------------------

    } else if (is_rsprob)
    {                               #-----------------------
    if (cor_or_app_str == "APP")    #  An APPARENT problem
        {                           #-----------------------
        forced_seed = switch (basic_or_wrapped_or_comb_str,
                              "BASE" = parameters$app_base_rsprob_rand_seed,
                              "WRAP" = parameters$app_wrap_rsprob_rand_seed,
                              "COMB" = parameters$app_comb_rsprob_rand_seed,
                              NA)

        #------------------------------
                                               #---------------------
        } else if (cor_or_app_str == "COR")    #  A CORRECT problem
        {                                      #---------------------
        forced_seed = switch (basic_or_wrapped_or_comb_str,
                              "BASE" = parameters$cor_base_rsprob_rand_seed,
                              "WRAP" = parameters$cor_wrap_rsprob_rand_seed,
                              "COMB" = parameters$cor_comb_rsprob_rand_seed,
                              NA)

        #------------------------------
                  #---------------------------------------------
        } else    #  ERROR:  cor_or_app_str not "APP" or "COR"
        {         #---------------------------------------------
        stop (paste0 ("\n\nERROR in get_forced_seed_value():",
                      "\n    Bad string match for cor_or_app_str arg = '",
                      cor_or_app_str, "'",
                      "\n    Must be BASE or WRAP or COMB.\n\n"))
        }
    }

    #---------------------------------------------------------------------------
    #                   Quit if bad problem type strings.
    #---------------------------------------------------------------------------
                                   #  2017 12 07 - BTL
    if (!is.null (forced_seed))    #  Had to separate these 2 tests to keep R
        {                          #  from generating warnings, since I have
        if (is.na (forced_seed))   #  warnings set to generate errors.
            {
            stop (paste0 ("\n\nERROR in get_forced_seed_value():",
                          "\n    Bad string match for basic_or_wrapped_or_comb_str arg = '",
                          basic_or_wrapped_or_comb_str, "'",
                          "\n    Must be BASE or WRAP or COMB.\n\n"))
            }
        }

    #---------------------------------------------------------------------------
    #               Return seed if given, NULL otherwise.
    #---------------------------------------------------------------------------

    return (forced_seed)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Generate a new random seed based on the current time
#'
#' This function is probably similar to the seed that set.seed() uses when
#' you don't give it an argument.  This function allows you to create a
#' similar seed but know what its value is.  Using set.seed() without an
#' argument doesn't allow you to easily regenerate the same thing without
#' saving the large array form of seed that R uses internally.
#'
#-------------------------------------------------------------------------------

#' @return Returns an integer seed value
#' @export

#-------------------------------------------------------------------------------

gen_new_seed_from_cur_time <- function ()
    {
    systime_num = as.numeric (Sys.time())
    new_seed = as.integer ((systime_num - floor (systime_num)) * 2e9)
    cat ("\nnew rand_seed = ", new_seed, "\n", sep='')

    return (new_seed)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Always set a seed, either by generating a new one or reusing a given one
#'
#' Generate a new seed based on the current time if the forced_seed argument
#' is NULL.  If that argument is not NULL, then use it as a seed.  Regardless
#' of which way the seed is derived, call set.seed() with it as the argument.
#'
#-------------------------------------------------------------------------------

#' @param location_string string to indicate where this function was called,
#' e.g., at the start of the creation of an rsproblem
#' @param forced_seed integer to use as seed in set.seed() call or
#' NULL to indicate that no seed has been provided
#'
#' @inheritParams std_param_defns
#'
#' @return Returns a 2 element named list with element new_seed containing
#' the new integer seed value or NA and the element R_internal_seed_array
#' containing the array value .Random.seed at the end of this function (i.e.,
#' the internal state of R's random number generator)
#'
#' @export

#-------------------------------------------------------------------------------

always_set_new_or_forced_rand_seed <- function (location_string,
                                                forced_seed = NULL)
    {
    new_seed =
        if (is.null (forced_seed)) gen_new_seed_from_cur_time() else forced_seed
    set.seed (new_seed)

        #-----------------------------------------------------------
        #  Save the current state of R's internal seed array
        #  so that the state of the random number generater can be
        #  reproduced even when a new seed value is not set.
        #-----------------------------------------------------------

    R_internal_seed_array = .Random.seed

    cat ("\n\nRAND_SEED - always_set_new_or_forced_rand_seed: ", location_string,
         "\n    forced_seed = '", forced_seed,
         "\n    new_seed = '", "'\n", sep='')

    return (list (seed_value = new_seed,
                  R_internal_seed_array = R_internal_seed_array))
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Helper function for setting a seed when necessary
#'
#-------------------------------------------------------------------------------

#' @param set_rand_seed_at_creation_of_all_new_major_objects boolean indicating
#' whether a new seed should be set every time an rsrun or rsproblem object
#' is created
#' @param location_string string to indicate where this function was called,
#' e.g., at the start of the creation of an rsproblem
#' @param forced_seed integer to use as seed in set.seed() call or
#' NULL to indicate that no seed has been provided
#'
#' @return Returns a 2 element named list with element new_seed containing
#' the new integer seed value or NA and the element R_internal_seed_array
#' containing the array value .Random.seed at the end of this function (i.e.,
#' the internal state of R's random number generator)

#-------------------------------------------------------------------------------

set_seed_if_necessary_helper <- function (set_rand_seed_at_creation_of_all_new_major_objects,
                                          location_string,
                                          forced_seed = NULL)
    {
        #  If the caller provides a non-NULL value for forced_seed, then
        #  they want to reset the seed to that given value.
        #  If they provide a NULL forced seed, then it can be interpreted
        #  in more than one way.
        #  If the set_rand_seed_at_creation_of_all_new_major_objects flag is
        #  TRUE, then
    new_seed = as.numeric (NA)    #  default value
    if (! is.null (forced_seed))
        {
        new_seed = forced_seed

        } else if (set_rand_seed_at_creation_of_all_new_major_objects)
        {
        new_seed = gen_new_seed_from_cur_time ()
        }

        #-----------------------------------------------------------------------
        #  If NA was returned as the new seed, that flags that there is no
        #  need to set the seed.  Otherwise, set the seed to the returned value.
        #-----------------------------------------------------------------------

    if (! is.na (new_seed))  set.seed (new_seed)

        #-----------------------------------------------------------
        #  Save the current state of R's internal seed array
        #  so that the state of the random number generater can be
        #  reproduced even when a new seed value is not set.
        #-----------------------------------------------------------

    R_internal_seed_array = .Random.seed

        #----------------------------------------------------------------
        #  Write some information to the log so that someone can find
        #  the seed value that was set in a particular instance and use
        #  it as a forced seed later if they want to reproduce a run or
        #  an object.
        #----------------------------------------------------------------

    cat ("\n\nRAND_SEED - set_seed_if_necessary_helper: ",
         "\n    set_rand_seed_at_creation_of_all_new_major_objects = '",
         set_rand_seed_at_creation_of_all_new_major_objects, "'",
         "\n    location_string = '",
         location_string, "'",
         "\n    forced_seed = '",
         forced_seed, "'",
         "\n    new_seed = '",
         new_seed, "'\n", sep='')

    return (list (seed_value = new_seed,
                  R_internal_seed_array = R_internal_seed_array))
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Set a new or forced random seed if caller specifies that
#'
#' The seed for the random number generator needs to be set at times to enable
#' reproducibility of objects and runs.  This function controls when that is
#' done and what value is used to set the seed.  It allows you to reset the
#' seed at various checkpoints during a bdpg run.  If you don't reset the seed
#' at a given checkpoint, then it will continue using the sequence of values
#' from the last time the seed was set until it reaches another checkpoint
#' where you do choose to reset the seed, though you don't ever have to reset
#' it if you don't want to.
#'
#-------------------------------------------------------------------------------

#'@section Rules for seed creation and setting:
#'
#'\subsection{seed names}{
#' There are nine different locations in the code where a seed can be set.
#' These locations are specified through the use of variable names in the
#' parameters list (usually derived from project.yaml).  The nine associated
#' names are:
#'
#'\itemize{
#'    \item{bdpg_run_init_rand_seed}
#'    \item{cor_base_rsprob_rand_seed}
#'    \item{cor_base_rsrun_rand_seed}
#'    \item{app_base_rsprob_rand_seed}
#'    \item{app_base_rsrun_rand_seed}
#'    \item{cor_wrap_rsprob_rand_seed}
#'    \item{cor_wrap_rsrun_rand_seed}
#'    \item{app_wrap_rsprob_rand_seed}
#'    \item{app_wrap_rsrun_rand_seed}
#'}
#'
#'The first name corresponds to the seed for the bdpg run as a whole.
#'The remaining names correspond to the seeds for correct and apparent versions
#'of basic and wrapped problems and reserve selector runs over each of those
#'problems.}
#'
#' \subsection{forced seed}{
#' If a particular named seed is specified in the parameters list (e.g., in
#' project.yaml), then the seed corresponding to that named seed will be
#' set to the value provided with the name.  In other words, providing a seed
#' overrides all other rules given below.  For example,
#' "app_wrap_rsprob_rand_seed: 555" in project.yaml will force the seed for the
#' creation of the apparent version of the wrapped problem to be set to 555.
#'}
#' \subsection{bdpg initialization seed}{
#' A run of bdpg will always set a seed at the start of the run.
#'
#'
#'\itemize{
#'    \item{If the
#' bdpg_run_init_rand_seed variable exists in the parameters list and has an
#' integer value, then set.seed() will be called with that value at
#' the start of the entire bdpg run.}
#'    \item{If the variable exists but
#' has a value that is not a legal argument for set.seed(), the program will
#' probably crash.}
#'    \item{If the bdgpinitseed variable is not in the list or has a
#' NULL value, then a seed will be generated based on the current time and
#' set.seed() will be called using that value.}
#'}
#'}
#'
#' \subsection{set_rand_seed_at_creation_of_all_new_major_objects}{
#' All other seeds besides the bdpg initialization seed are controlled by a few
#' other things.
#'
#'\itemize{
#'    \item{As stated earlier, any named seed that has a value assigned to
#' it in the parameters list will have the seed set to that value just before
#' the creation of the associated object.}
#'    \item{If the seed name does not
#' appear in the parameters list or has a NULL value, then the seed will not be set
#' at the start of the creation of that object UNLESS the parameters list
#' contains a variable called set_rand_seed_at_creation_of_all_new_major_objects
#' and it is set to TRUE.}
#'    \item{If set_rand_seed_at_creation_of_all_new_major_objects exists and
#'    is set to TRUE and the named seed does not
#' appear in the parameters list or has a NULL value, then the seed WILL be set
#' at the start of the creation of that object.  In that case, it will be set
#' to a seed derived from the current time.}
#'}
#'}
#'
#' \subsection{Recovering seed values for reproducibility}{
#' Every time a seed is set, its value is written to the console with a
#' label indicating where it was set (e.g., at bdpg initialization).
#'\itemize{
#'    \item{At all times other than the bdpg initialization, the value is
#'    saved in the object whose creation it immediately precedes.}
#'    \item{If the seed is NOT set at object creation, NA is saved in the
#'    object as the seed value.}
#'}
#' So, if you want to regenerate a particular object or run, the easiest way
#' to do it is to find the seed value(s) inside the console output and
#' redo the entire bdpg run with the seed value(s) set in project.yaml
#' to those found in the previous bdpg run's output.  Note that this is only
#' designed to be done with single runs.  See Caveats below.
#'}
#'
#-------------------------------------------------------------------------------

#'@section Caveats:
#' While the intent of this routine is to give lots of flexibility in setting
#' seeds for reproducibility, it still doesn't solve the whole problem and it
#' does have a few things to be careful about when trying to reproduce results.
#'
#' \subsection{Recovering seed values for reproducibility}{
#' If you do a tzar run with multiple repetitions, this function will
#'    make the same seed assignments in every repetition.  It might be possible
#'    to extend this routine in the future by allowing arrays of seed values,
#'    but at the moment, it's doing what's needed for most development cases.
#' }
#'
#' \subsection{Beware of cascading seed effects}{
#' Even if you set the named seed for an object you may expect it to produce
#' the same object as a different run that used the same seed.  However, if the
#' object interacts with another object generated earlier in the same run but
#' its seed is not reset, you can get different results for the second object
#' in the second run.  For example:
#'
#' \itemize{
#'    \item{Suppose that you generate a correct base object and then
#' a wrapped object in the same run and you have
#' set_rand_seed_at_creation_of_all_new_major_objects set to TRUE so that
#' you can see the seed that is set for each object as it is created.}
#'    \item{Suppose that you run bdpg and note that the seed generated for
#' the bdpg initalization in the output was 123 and the seed set for the correct
#' problem was 456 and the seed set for the wrap problem was 789.
#'     \item{Then, suppose that you want to test the re-creation of the wrap
#'     problem and redo the run with "cor_wrap_rsprob_rand_seed: 789" but
#'     none of the other seeds specified.}
#'    \item{The problem is that the generation of the correct base problem
#'    that your wrap is depending on will be generated using a different seed
#'    than it was the first time and will therefore be a different problem.
#'    When the wrap goes to build around the base problem it will result in a
#'    different wrap problem even though it's using the same seed as on the
#'    previous run.}
#'}
#'}
#'}

#-------------------------------------------------------------------------------

#' @param is_rsrun boolean indicating that the seed creation test is being
#' done just before the creation of an rs RUN object
#' @param is_rsprob boolean indicating that the seed creation test is being
#' done just before the creation of an rs PROBLEM object
#' @param cor_or_app_str string indicating correct or apparent (i.e.,
#' "COR" or "APP")
#' @param basic_or_wrapped_or_comb_str string indicating basic or wrapped (i.e.,
#' "BASE" or "WRAP")
#' @param location_string string to indicate where this function was called,
#' e.g., at the start of the creation of an rsproblem
#'
#' @inheritParams std_param_defns
#'
#' @return Returns a 2 element named list with element new_seed containing
#' the new integer seed value or NA and the element R_internal_seed_array
#' containing the array value .Random.seed at the end of this function (i.e.,
#' the internal state of R's random number generator)
#'
#' @export

#-------------------------------------------------------------------------------

set_new_or_forced_rand_seed_if_necessary <- function (is_rsrun,
                                                      is_rsprob,
                                                      parameters,
                                                      cor_or_app_str,
                                                      basic_or_wrapped_or_comb_str,
                                                      location_string)
    {
    forced_seed =
        get_forced_seed_value_if_necessary (is_rsrun,
                                            is_rsprob,
                                            parameters,
                                            cor_or_app_str,
                                            basic_or_wrapped_or_comb_str)

    new_seed_list =
        set_seed_if_necessary_helper (value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
                                                  location_string,
                                                  forced_seed)

    return (new_seed_list)
    }

#===============================================================================

