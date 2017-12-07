#===============================================================================

                        #  random_seed_setting_utilities.R

#  Tools for managing the seed in random number generation to make it easier
#  to reproduce all or part of a run.

#===============================================================================

#-------------------------------------------------------------------------------

#  2017 12 06 - BTL
#  NOTE:  basic_or_wrapped_or_comb_str = "COMB" does not exist at the moment,
#  but it's mentioned in BDProb.R, so it's covered here for
#  future compatibility.

#-------------------------------------------------------------------------------

get_forced_seed_value_if_necessary <- function (is_rsrun,
                                                is_rsprob,
                                                parameters,
                                                cor_or_app_str,
                                                basic_or_wrapped_or_comb_str)
    {
        #  Make sure that one and only one type of seed is chosen to examine.
    if ((is_rsrun + is_rsprob) != 1)
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

gen_new_seed_from_cur_time <- function ()
    {
    systime_num = as.numeric (Sys.time())
    new_seed = as.integer ((systime_num - floor (systime_num)) * 2e9)
    cat ("\nnew rand_seed = ", new_seed, "\n", sep='')

    return (new_seed)
    }

#===============================================================================

always_set_new_or_forced_rand_seed <- function (location_string,
                                                forced_seed = NULL)
    {
    cat ("\n\nalways_set_new_or_forced_rand_seed: ", location_string,
         "\n    forced_seed = '", forced_seed, "'\n", sep='')

    new_seed =
        if (is.null (forced_seed)) gen_new_seed_from_cur_time() else forced_seed
    set.seed (new_seed)

    return (new_seed)
    }

#===============================================================================

#' Set a new or forced random seed if caller specifies that
#'
#'
#'
#' @param set_rand_seed_at_creation_of_all_new_major_objects boolean indicating
#' whether a new seed should be set every time an rsrun or rsproblem is created
#' @param location_string string to indicate where this function was called,
#' e.g., at the start of the creation of an rsproblem
#' @param forced_seed integer to use as seed in set.seed() call or
#' NULL to indicate that no seed has been provided
#'
#' @return Returns a value that is probably an integer but is definitely not null

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

        #----------------------------------------------------------------
        #  Write some information to the log so that someone can find
        #  the seed value that was set in a particular instance and use
        #  it as a forced seed later if they want to reproduce a run or
        #  an object.
        #----------------------------------------------------------------

    cat ("\n\nset_seed_if_necessary_helper: ",
         "\n    set_rand_seed_at_creation_of_all_new_major_objects = '",
         set_rand_seed_at_creation_of_all_new_major_objects, "'",
         "\n    location_string = '",
         location_string, "'",
         "\n    forced_seed = '",
         forced_seed, "'",
         "\n    new_seed = '",
         new_seed, "'\n", sep='')

    return (new_seed)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#  Test code for random number seed setting.

#  2017 12 07 - BTL
#  This code should eventually be in the test code directory, but the
#  automatic code testing stuff doesn't work in this package currently,
#  so I'll leave it here until it does.

#-------------------------------------------------------------------------------

test_set_seed_if_necessary_helper <- function ()
    {
    cat ("\ntest_set_seed_if_necessary_helper:\n    ")

    x1 = set_seed_if_necessary_helper (TRUE, "abc")
    x2 = set_seed_if_necessary_helper (TRUE, "abc", 123)
    x3 = set_seed_if_necessary_helper (FALSE, "abc")
    x4 = set_seed_if_necessary_helper (FALSE, "abc", 123)

    if (is.numeric (x1) & (x1 != 123)) cat (".") else cat("F")
    if (x2 == 123) cat (".") else cat("F")
    if (is.na (x3)) cat (".") else cat("F")
    if (x4 == 123) cat (".") else cat("F")
    }

if (FALSE)
    test_set_seed_if_necessary_helper()

#===============================================================================

# gen_single_bdprob.R
# new_seed = set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
#                      is_rsprob = TRUE,
#                      parameters,
#                      cor_or_app_str = "COR",
#                      basic_or_wrapped_or_comb_str = "BASE",
#                      location_string = "Start of gen_single_bdprob_COR_from_scratch_or_Xu_bench_file(),COR,BASE")

# gen_single_bdprob_APP.R
# new_seed = set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
#                      is_rsprob = TRUE,
#                      parameters,
#                      cor_or_app_str = "APP",
#                      basic_or_wrapped_or_comb_str = Xu_bdprob_COR@basic_or_wrapped_or_comb_str,
#                      location_string = paste0 ("Start of create_and_init_APP_bdprob(),APP,",
#                                                Xu_bdprob_COR@basic_or_wrapped_or_comb_str))

# gen_wrapped_bdprob.R
# new_seed = set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
#                      is_rsprob = TRUE,
#                      parameters,
#                      cor_or_app_str = "COR",
#                      basic_or_wrapped_or_comb_str = "WRAP",
#                      location_string = "Start of wrap_abundance_dist_around_Xu_problem(),COR,WRAP")

# do_marxan_analysis_and_output.R
# new_seed = set_new_or_forced_rand_seed_if_necessary (is_rsrun = TRUE,
#                      is_rsprob = FALSE,
#                      parameters,
#                      cor_or_app_str,
#                      basic_or_wrapped_or_comb_str,
#                      location_string = paste0 ("Start of create_RSrun(),",
#                                                cor_or_app_str, ",",
#                                                basic_or_wrapped_or_comb_str))

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

    new_seed =
        set_seed_if_necessary_helper (value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
                                                  location_string,
                                                  forced_seed)

    return (new_seed)
    }

#===============================================================================

