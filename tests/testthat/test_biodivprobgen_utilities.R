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

