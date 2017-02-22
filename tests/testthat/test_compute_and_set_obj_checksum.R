#===============================================================================

                    #  test_compute_and_set_obj_checksum.R

#===============================================================================

test_compute_and_set_obj_checksum <- function ()
    {
    run_on_prob_UUID_1 <- uuid::UUIDgenerate()
    run_on_prob_UUID_2 <- uuid::UUIDgenerate()

    x_uuid_1 <- uuid::UUIDgenerate()
    x_uuid_2 <- uuid::UUIDgenerate()
    x_uuid_3 <- uuid::UUIDgenerate()

        #-----------------------------------------------------------------------
        #  Create an object x and then create 2 variants of it, x_same and
        #  x_different.
        #  x_same has a different UUID than x but is the same in all other ways.
        #  x_different has a different UUID than x and has a different value
        #  for the run_on_prob_UUID field.
        #
        #  We want to be able to tell that x and x_different are different and
        #  we want to be able to tell that x and x_same are identical if you
        #  ignore their UUID and checksum fields.
        #-----------------------------------------------------------------------

    x                            <- new ("RSrun")
    x@UUID                       <- x_uuid_1
    x@run_on_prob_UUID           <- run_on_prob_UUID_1

    x_same                       <- new ("RSrun")
    x_same@UUID                  <- x_uuid_2
    x_same@run_on_prob_UUID      <- run_on_prob_UUID_1

    x_different                  <- new ("RSrun")
    x_different@UUID             <- x_uuid_3
    x_different@run_on_prob_UUID <- run_on_prob_UUID_2

        #-------------------------------------------------------------------
        #  Make sure that all of the objects are different from each other
        #  when you include the UUID and checksum fields in the test.
        #  These 3 tests should all return FALSE, since they all have
        #  different UUIDs.
        #-------------------------------------------------------------------

    cat ("\n\nidentical (x, x_different)    = ", identical (x, x_different))
    cat ("\nidentical (x_different, x_same) = ", identical (x_different, x_same))
    cat ("\nidentical (x, x_same)           = ", identical (x, x_same))
    cat ("\n\n")

        #---------------------------------------------------------------
        #  The first 2 of these should return FALSE.
        #  The last one should return TRUE since even though they have
        #  different UUIDs, the rest of their structure is the same.
        #---------------------------------------------------------------

    x           <- bdpg::compute_and_set_obj_checksum (x)
    x_different <- bdpg::compute_and_set_obj_checksum (x_different)
    x_same      <- bdpg::compute_and_set_obj_checksum (x_same)

    cat ("\n\n(x@checksum == x_different@checksum)    = ",  x@checksum == x_different@checksum)
    cat ("\n(x_different@checksum == x_same@checksum) = ",  x_different@checksum == x_same@checksum)
    cat ("\n(x@checksum == x_same@checksum)           = ",  x@checksum == x_same@checksum)
    cat ("\n\n")
    }

#===============================================================================

