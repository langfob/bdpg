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

get_full_run_random_seed <- function (parameters)
    {
        #  Default to value of bdpg_run_init_rand_seed parameter.
        #  If it has not been set in the yaml file, then it will be NULL.
        #  If the full_run_rand_seed_from_yaml_array parameter has been been set to indicate
        #  the random seed is to be taken from a yaml array, then
        #  look up the seed in that array and replace the default value
        #  for full_run_rand_seed that was just set .


    get_full_run_rand_seed_from_yaml_array =
                    vb (parameters$get_full_run_rand_seed_from_yaml_array,
                        def_on_empty = TRUE, def = FALSE)

    if (get_full_run_rand_seed_from_yaml_array)
        {
        rand_seed_yaml_array = parameters$rand_seed_yaml_array
        max_rand_seed_idx = length (rand_seed_yaml_array)

        #cur_input_prob_idx = vn (parameters$cur_input_prob_idx, range_lo=1)
        cur_rand_seed_idx = vn (parameters$cur_rand_seed_idx,
                                range_lo=1, range_hi=max_rand_seed_idx)

        full_run_rand_seed = rand_seed_yaml_array [cur_rand_seed_idx]

        } else  #  Not loading seed from yaml array
        {
        full_run_rand_seed = parameters$bdpg_run_init_rand_seed
        }

    return (full_run_rand_seed)
    }

#===============================================================================

#' @export

test_get_full_run_random_seed <- function ()
    {
    test_num = 0

        #  Should succeed and return NULL as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return NULL", sep='')
    parameters = list ()
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')

        #  Should succeed and return NULL as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return NULL", sep='')
    parameters = list (bdpg_run_init_rand_seed = NULL)
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')

        #  Should succeed and return 99 as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return 99", sep='')
    parameters = list (bdpg_run_init_rand_seed = 99)
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')

        #  Should succeed and return NULL as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return NULL", sep='')
    parameters = list (get_full_run_rand_seed_from_yaml_array = FALSE)
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')

        #  Should succeed and return 88 as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return 88", sep='')
    parameters = list (get_full_run_rand_seed_from_yaml_array = FALSE,
                       bdpg_run_init_rand_seed = 88)
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')

        #  Should succeed and return 11111 as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return 11111", sep='')
    parameters = list (get_full_run_rand_seed_from_yaml_array = TRUE,
                       rand_seed_yaml_array = c(11111, 22222, 33333),
                       cur_rand_seed_idx = 1)
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')

        #  Should succeed and return 22222 as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return 22222", sep='')
    parameters = list (get_full_run_rand_seed_from_yaml_array = TRUE,
                       rand_seed_yaml_array = c(11111, 22222, 33333),
                       cur_rand_seed_idx = 2)
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')

        #  Should succeed and return 33333 as the seed.
        test_num = test_num + 1
        cat ("\nTest ", test_num, ": should succeed and return 33333", sep='')
    parameters = list (get_full_run_rand_seed_from_yaml_array = TRUE,
                       rand_seed_yaml_array = c(11111, 22222, 33333),
                       cur_rand_seed_idx = 3)
    full_run_random_seed = get_full_run_random_seed (parameters)
        cat ("\n    full_run_random_seed = ", full_run_random_seed, sep='')
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

    full_run_rand_seed = get_full_run_random_seed (parameters)

    cat ("\n\nfull_run_rand_seed = '", full_run_rand_seed, "'", sep='')

    always_set_new_or_forced_rand_seed ("Start of init_for_bdpg()",
                                        full_run_rand_seed)
    #                                     parameters$bdpg_run_init_rand_seed)

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

