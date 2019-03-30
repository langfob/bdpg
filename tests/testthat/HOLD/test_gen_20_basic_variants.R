#===============================================================================

                        #  test_gen_20_basic_variants.R

#===============================================================================

library (bdpg)

context ("gen_20_variants")

#===============================================================================

#  There are 10 possible combinations of error combinations for the
#  gen_20_variants code.  This should generate and test the labels for
#  each of those.

#-------------------------------------------------------------------------------


test_that ("All compound error name possibilities correctly assigned", {

#  0000
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.0
spp_occ_FN_const_rate = 0
match_error_counts    = FALSE

expect_equal ("00_No_compound_error_name_assigned",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  0001
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.0
spp_occ_FN_const_rate = 0
match_error_counts    = TRUE

expect_equal ("00_No_compound_error_name_assigned",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  0010
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.0
spp_occ_FN_const_rate = 0.05
match_error_counts    = FALSE

expect_equal ("03-FN_only_NO_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  0011
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.0
spp_occ_FN_const_rate = 0.05
match_error_counts    = TRUE

expect_equal ("03-FN_only_NO_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  0100
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0
match_error_counts    = FALSE

expect_equal ("02-FP_only_NO_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  0101
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0
match_error_counts    = TRUE

expect_equal ("02-FP_only_NO_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  0110
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0.05
match_error_counts    = FALSE

expect_equal ("05-FP_and_FN_not_matched_NO_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  0111
gen_cost_errors       = FALSE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0.05
match_error_counts    = TRUE

expect_equal ("04-FP_and_FN_matched_NO_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1000
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0
spp_occ_FN_const_rate = 0.0
match_error_counts    = FALSE

expect_equal ("10-Cost_err_only",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1001
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0
spp_occ_FN_const_rate = 0.0
match_error_counts    = TRUE

expect_equal ("10-Cost_err_only",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1010
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0
spp_occ_FN_const_rate = 0.05
match_error_counts    = FALSE

expect_equal ("07-FN_only_WITH_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1011
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0
spp_occ_FN_const_rate = 0.05
match_error_counts    = TRUE

expect_equal ("07-FN_only_WITH_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1100
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0
match_error_counts    = FALSE

expect_equal ("06-FP_only_WITH_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1101
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0
match_error_counts    = TRUE

expect_equal ("06-FP_only_WITH_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1110
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0.05
match_error_counts    = FALSE

expect_equal ("09-FP_and_FN_not_matched_WITH_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))

#  1111
gen_cost_errors       = TRUE
spp_occ_FP_const_rate = 0.05
spp_occ_FN_const_rate = 0.05
match_error_counts    = TRUE

expect_equal ("08-FP_and_FN_matched_WITH_cost_err",
              get_compound_err_name (gen_cost_errors,
                                     spp_occ_FP_const_rate,
                                     spp_occ_FN_const_rate,
                                     match_error_counts))
})

#===============================================================================

