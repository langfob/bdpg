#===============================================================================

                        #  test_biodivprobgen_utilities.R

#===============================================================================

library (bdpg)

context ("apply_error_to_spp_occupancy_data")

#===============================================================================

test_no_spp_eradicated_by_setting_FNs <- function ()
    {
    set.seed (3456)

    num_spp = 5
    num_PUs = 5

    bpm = matrix (c(1,0,0,0,0,
                    1,1,0,0,0,
                    1,1,1,0,0,
                    1,1,1,1,0,
                    1,1,1,1,1), nrow = num_spp, ncol = num_PUs, byrow = TRUE)
    #print (bpm)
    old_cts_for_spp = rowSums (bpm)

    bpm [1,1]= -1
    bpm [2,1:2] = -1
    bpm [4,1:4]=-1
    bpm [5,3:5]=-1
    #print (bpm)

    bpm = make_sure_no_spp_eradicated_by_setting_FNs (bpm, old_cts_for_spp)

    correct_new_bpm = matrix (c(1,0,0,0,0,
                                0,1,0,0,0,
                                1,1,1,0,0,
                                0,0,0,1,0,
                                1,1,0,0,0), nrow = num_spp, ncol = num_PUs, byrow = TRUE)

    #print (bpm)
    #print (correct_new_bpm)

    return (identical (bpm, correct_new_bpm))
    }

#===============================================================================

                #-----------------------------------------------------
                #  Test list_as_data_frame_with_nulls_replaced_by_NA
                #-----------------------------------------------------

test_that("adding error to spp with FN does not eradicate any spp", {

    expect_true (test_no_spp_eradicated_by_setting_FNs ())
})

#===============================================================================

