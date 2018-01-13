#===============================================================================

                        #  test_biodivprobgen_utilities.R

#===============================================================================

library (bdpg)

context ("biodivprobgen_utilities")

#===============================================================================

                #-----------------------------------------------------
                #  Test list_as_data_frame_with_nulls_replaced_by_NA
                #-----------------------------------------------------

a_list_that_should_fail_as_data_frame = list (a = 123,
                                              b = "blah",
                                              d = NULL,            #  bad
                                              e = numeric(0),      #  bad
                                              f = logical(0),      #  bad
                                              g = character(0),    #  bad
                                              h = "no problem")

desired_list = list (a = 123,
                     b = "blah",
                     d = NA,
                     e = NA,
                     f = NA,
                     g = NA,
                     h = "no problem")

desired_data_frame = data.frame (a = 123,
                                 b = "blah",
                                 d = NA,
                                 e = NA,
                                 f = NA,
                                 g = NA,
                                 h = "no problem")

test_that("list_as_data_frame_with_nulls_replaced_by_NA: list that should fail as data frame", {

        #  Should give:
        #      Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  :
        #      arguments imply differing number of rows: 1, 0
    expect_error (as.data.frame (a_list_that_should_fail_as_data_frame))

        #  These should succeed:
    expect_equal (desired_list,
                  list_with_0_length_vals_replaced_by_NA (
                                        a_list_that_should_fail_as_data_frame))

    expect_equal (desired_data_frame,
                  list_as_data_frame_with_0_length_vals_replaced_by_NA (
                                        a_list_that_should_fail_as_data_frame))
})

#===============================================================================

                #------------------
                #  Test stop_bdpg
                #------------------

#-------------------------------------------------------------------------------
#  2018 01 13 - BTL
#  THIS FUNCTION IS NOT EXECUTED HERE AND SHOULD NEVER BE EXECUTED
#  BECAUSE IT WILL CRASH ON THE FIRST stop_bdpg() CALL.
#  IT'S ONLY HERE TO RECORD THE TESTS DONE BY HAND BY SELECTING AND RUNNING
#  EACH LITTLE BLOCK OF CODE SEPARATELY IN THE R CONSOLE.
#  EACH LITTLE TEST PRINTS A MESSAGE THAT SAYS WHETHER THE RESULT OF THE
#  TEST SHOULD CALL THE BROWSER OR NOT.
#  HOWEVER, IN ALL CASES WHERE THE BROWSER SHOULD BE CALLED, A REPLACEMENT
#  MESSAGE WILL APPEAR AND INDICATE THIS IS WHERE THE BROWSER WOULD HAVE
#  BEEN CALLED.
#-------------------------------------------------------------------------------

test_stop_bdpg <- function()
    {
            #  16 Combinations:

            #  exists ("browse_on_crash") == TRUE/FALSE
            #  browse_on_crash = TRUE/FALSE
            #  bdpg.emulating_tzar in options list == TRUE/FALSE
            #  getOption ("bdpg.emulating_tzar", default=FALSE) == TRUE/FALSE

    #  0000
    rm (browse_on_crash)
    options (bdpg.emulating_tzar = NULL)
    stop_bdpg (msg="Testing 0000 - should NOT browse", browser_on=FALSE)

    #  0001  not possible since bdpg.emulating_tzar not set

    #  0010
    rm (browse_on_crash)
    options (bdpg.emulating_tzar = FALSE)
    stop_bdpg (msg="Testing 0010 - should NOT browse", browser_on=FALSE)

    #  0011
    rm (browse_on_crash)
    options (bdpg.emulating_tzar = TRUE)
    stop_bdpg (msg="Testing 0011 - SHOULD browse", browser_on=FALSE)

        #  Anything with 1 in second bit is not possible
    #  0100  not possible since browse_on_crash not set
    #  0101  not possible since browse_on_crash not set
    #  0110  not possible since browse_on_crash not set
    #  0111  not possible since browse_on_crash not set

        #  Anything with 10 as first 2 bits should not browse because
        #  FALSE browse_on_crash overrides all others
    #  1000
    browse_on_crash = FALSE
    options (bdpg.emulating_tzar = NULL)
    stop_bdpg (msg="Testing 1000 - should NOT browse", browser_on=FALSE)

    #  1001  not possible since bdpg.emulating_tzar not set

    #  1010
    browse_on_crash = FALSE
    options (bdpg.emulating_tzar = FALSE)
    stop_bdpg (msg="Testing 1010 - should NOT browse", browser_on=FALSE)

    #  1011
    browse_on_crash = FALSE
    options (bdpg.emulating_tzar = TRUE)
    stop_bdpg (msg="Testing 1011 - should NOT browse", browser_on=FALSE)

        #  Anything with 11 as first 2 bits SHOULD browse because
        #  FALSE browse_on_crash overrides all others

    #  1100
    browse_on_crash = TRUE
    options (bdpg.emulating_tzar = NULL)
    stop_bdpg (msg="Testing 1111 - SHOULD browse", browser_on=FALSE)

    #  1101  not possible since bdpg.emulating_tzar not set

    #  1110  not possible since bdpg.emulating_tzar not set
    browse_on_crash = TRUE
    options (bdpg.emulating_tzar = FALSE)
    stop_bdpg (msg="Testing 1111 - SHOULD browse", browser_on=FALSE)

    #  1111
    browse_on_crash = TRUE
    options (bdpg.emulating_tzar = TRUE)
    stop_bdpg (msg="Testing 1111 - SHOULD browse", browser_on=FALSE)
    }

#===============================================================================

