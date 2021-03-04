#===============================================================================

                    #  test_random_seed_setting_utilities.R

#===============================================================================

library (bdpg)

context ("random_seed_setting_utilities")

#-------------------------------------------------------------------------------

cat ("\ntest_set_seed_if_necessary_helper:\n    ")

x1_list = set_seed_if_necessary_helper (TRUE, "location: testing set seed TRUE with no forced seed")
x1 = x1_list$seed_value
test_that("set seed TRUE with no forced seed", {
    expect_type (x1, "integer")
})

x2_list = set_seed_if_necessary_helper (TRUE, "location: testing set seed TRUE with forced seed = 123", 123)
x2 = x2_list$seed_value
test_that("set seed TRUE with forced seed = 123", {
    expect_equal (x2, 123)
})

x3_list = set_seed_if_necessary_helper (FALSE, "location: testing set seed FALSE with no forced seed")
x3 = x3_list$seed_value
test_that("set seed FALSE with no forced seed", {
    expect_true (is.na (x3))
})

x4_list = set_seed_if_necessary_helper (FALSE, "location: testing set seed FALSE with forced seed = 123", 123)
x4 = x4_list$seed_value
test_that("set seed FALSE with forced seed = 123", {
    expect_equal (x4, 123)
})

#===============================================================================

