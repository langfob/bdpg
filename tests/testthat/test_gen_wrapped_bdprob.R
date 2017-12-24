#===============================================================================

                        #  test_gen_wrapped_bdprob.R

#===============================================================================

library (bdpg)

context ("gen_wrapped_bdprob")

#===============================================================================

                    #-------------------------------
                    #  Test create_eligible_PU_set
                    #-------------------------------

    Xu_dep_set = c(1,2)
    extra_PUs = c(3,4)
    dep_set_PUs_eligible = TRUE

    eligible_PUs = create_eligible_PU_set (Xu_dep_set, extra_PUs, dep_set_PUs_eligible)

test_that("create_eligible_PU_set: eligible_PUs include dependent and extra PUs", {
    expect_equal (eligible_PUs, c(1,2,3,4))
})

    #----------

    Xu_dep_set = c(1,2)
    extra_PUs = c(3,4)
    dep_set_PUs_eligible = FALSE

    eligible_PUs = create_eligible_PU_set (Xu_dep_set, extra_PUs, dep_set_PUs_eligible)

test_that("create_eligible_PU_set: eligible_PUs only include extra PUs", {
    expect_equal (eligible_PUs, c(3,4))
})

#===============================================================================

                    #------------------------
                    #  Test trim_abundances
                    #------------------------

        #  Test endpoints equal to bounds of randomly ordered vector.
    rounded_abundances = c (8,9,5,3,0,10,7,1,6,2,4)
    min_abund = 0
    max_abund = 10

    trimmed_abundances = trim_abundances (rounded_abundances,
                                          min_abund=min_abund,
                                          max_abund=max_abund)

test_that("trim_abundances: endpoints equal to bounds of randomly ordered vector", {
    expect_equal (trimmed_abundances, rounded_abundances)
})

    #----------

        #  Test some elements < min and > max bounds of randomly ordered vector.
    rounded_abundances = c (8,9,5,3,0,10,7,1,6,2,4)
    min_abund = 5
    max_abund = 6

    trimmed_abundances = trim_abundances (rounded_abundances,
                                          min_abund=min_abund,
                                          max_abund=max_abund)

test_that("trim_abundances: some elements < min and > max bounds of randomly ordered vector", {
    expect_equal (trimmed_abundances, c(5,6))
})

    #----------

        #  Test elements < min bound only.
    rounded_abundances = 0:10
    min_abund = 5
    max_abund = 10

    trimmed_abundances = trim_abundances (rounded_abundances,
                                          min_abund=min_abund,
                                          max_abund=max_abund)

test_that("trim_abundances: elements < min bound only", {
    expect_equal (trimmed_abundances, 5:10)
})

    #----------

        #  Test elements > max bound only.
    rounded_abundances = 0:10
    min_abund = 0
    max_abund = 8

    trimmed_abundances = trim_abundances (rounded_abundances,
                                          min_abund=min_abund,
                                          max_abund=max_abund)

test_that("trim_abundances: elements > max bound only", {
    expect_equal (trimmed_abundances, 0:8)
})

    #----------

        #  Test all elements < min OR > max bounds of vector.
    rounded_abundances = c(0,1,2,3,9,10)
    min_abund = 4
    max_abund = 8

    trimmed_abundances = trim_abundances (rounded_abundances,
                                          min_abund=min_abund,
                                          max_abund=max_abund)

test_that("trim_abundances: all elements < min OR > max bounds of vector", {
    expect_length (trimmed_abundances, 0)
})

    #----------

        #  Test elements < min bound only with max bound default argument.
    rounded_abundances = 0:10
    min_abund = 3

    trimmed_abundances = trim_abundances (rounded_abundances,
                                          min_abund=min_abund)

test_that("trim_abundances: elements < min bound only with max bound default argument", {
    expect_equal (trimmed_abundances, 3:10)
})

    #----------

        #  Test elements < min bound only with min and max bound default args.
    rounded_abundances = 0:10

    trimmed_abundances = trim_abundances (rounded_abundances)

test_that("trim_abundances: elements < min bound only with min and max bound default args", {
    expect_equal (trimmed_abundances, 2:10)
})

#===============================================================================

            #------------------------------------------
            #  Test gen_raw_histogram_of_wrapped_dist
            #------------------------------------------

    #------------------
    #  Simple example
    #------------------

        #  Xu_PU_spp_table PU_spp table for original Xu problem being wrapped
        #  around
    num_spp = 5
    num_PU = 10
    Xu_PU_spp_table = data.frame (PU_ID=1:num_PU, spp_ID=rep(1:num_spp,each=2))
# Xu_PU_spp_table
#    PU_ID spp_ID
# 1      1      1
# 2      2      1
# 3      3      2
# 4      4      2
# 5      5      3
# 6      6      3
# 7      7      4
# 8      8      4
# 9      9      5
# 10    10      5

        #  trimmed_rounded_abund_per_spp vector of abundances of all species in
        #  the full wrapped distribution, i.e., including the original Xu problem
        #  abundances
# trimmed_rounded_abund_per_spp =
#   [1] 3 2 3 3 2 5 2 2 2 5 2 3 2 6 2

    trimmed_rounded_abund_per_spp = c(3,2,3,3,2,
                                      5,2,2,2,5,
                                      2,3,2,6,2)

    desired_result = data.frame (x = c(2,3,5,6),
                                 freq.x = c(8, 4, 2, 1),
                                 freq.y = c(5, NA, NA, NA))
#   x freq.x freq.y
# 1 2     8     5
# 2 3     4     NA
# 4 5     2     NA
# 5 6     1     NA

    wrapped_extra_spp_abund_merge =
        gen_raw_histogram_of_wrapped_dist (Xu_PU_spp_table,
                                           trimmed_rounded_abund_per_spp,
                                           spp_col_name = "spp_ID")

test_that("gen_raw_histogram_of_wrapped_dist: simple example", {
    expect_equal (wrapped_extra_spp_abund_merge, desired_result)
})

    #------------------------
    #  More complex example
    #------------------------

        #  Xu_PU_spp_table PU_spp table for original Xu problem being wrapped
        #  around
    num_spp = 79    # 5
    num_PU = 158    # 10
    Xu_PU_spp_table = data.frame (PU_ID=1:num_PU, spp_ID=rep(1:num_spp,each=2))
# Xu_PU_spp_table
#    PU_ID spp_ID
# 1      1      1
# 2      2      1
# 3      3      2
# 4      4      2
# 5      5      3
# 6      6      3
# 7      7      4
# 8      8      4
# 9      9      5
# 10    10      5
# ...
# 155   155     78
# 156   156     78
# 157   157     79
# 158   158     79

        #  trimmed_rounded_abund_per_spp vector of abundances of all species in
        #  the full wrapped distribution, i.e., including the original Xu problem
        #  abundances
# trimmed_rounded_abund_per_spp =
#   [1] 5 2 2 2 2 3 2 3 3 2 2 3 2 6 2 4 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 3 2 4 2 3 3
#  [38] 2 3 2 2 2 3 4 2 3 3 2 2 2 2 3 2 2 2 2 3 2 4 2 3 3 2 2 3 4 2 2 2 2 2 3 2 3
#  [75] 3 3 2 3 3 2 2 3 3 2 3 2 2 3 3 2 2 2 2 3 2 6 2 3 2 4 2 2 3 2 2 3 2 2 3 2 3
# [112] 4 4 2 2 3 3 2 2 4 2 2 3 3 2 2 3 3 2 3 3 2 2 2 5 2 3 2 2 2 4 2 3 3 2 2
    trimmed_rounded_abund_per_spp = c(
        5,2,2,2,2,3,2,3,3,2,2,3,2,6,2,4,2,2,2,2,2,2,2,2,2,2,2,3,3,2,2,3,2,4,2,3,3,
        2,3,2,2,2,3,4,2,3,3,2,2,2,2,3,2,2,2,2,3,2,4,2,3,3,2,2,3,4,2,2,2,2,2,3,2,3,
        3,3,2,3,3,2,2,3,3,2,3,2,2,3,3,2,2,2,2,3,2,6,2,3,2,4,2,2,3,2,2,3,2,2,3,2,3,
        4,4,2,2,3,3,2,2,4,2,2,3,3,2,2,3,3,2,3,3,2,2,2,5,2,3,2,2,2,4,2,3,3,2,2)

    desired_result = data.frame (x = 2:6,
                                 freq.x = c(86, 46, 10, 2, 2),
                                 freq.y = c(79, NA, NA, NA, NA))
#   x freq.x freq.y
# 1 2     86     79
# 2 3     46     NA
# 3 4     10     NA
# 4 5      2     NA
# 5 6      2     NA

    wrapped_extra_spp_abund_merge =
        gen_raw_histogram_of_wrapped_dist (Xu_PU_spp_table,
                                           trimmed_rounded_abund_per_spp,
                                           spp_col_name = "spp_ID")

test_that("gen_raw_histogram_of_wrapped_dist: more complex example", {
    expect_equal (wrapped_extra_spp_abund_merge, desired_result)
})

#===============================================================================

            #---------------------------------------------------
            #  Test compute_final_wrapped_extra_spp_abund_hist
            #---------------------------------------------------

    #------------------------------------------------------------------
    #  Test that it fails if the wrapping distribution doesn't
    #  completely contain the wrapped distribution, e.g.,
    #  if there are more species on 2 patches in the base Xu problem
    #  (freq.y) than in the wrapping lognormal distribution (freq.x).
    #------------------------------------------------------------------

    wrapped_extra_spp_abund_merge = data.frame (x = 2:5,
                                               freq.x = c(10, 46, 10, 2),
                                               freq.y = c(20, NA, NA, NA))
#   x freq.x freq.y
# 1 2     10     20  <<<<<-----  20 spp on 2 patches in Xu problem but only 10 spp on 2 patches in wrapping distribution
# 2 3     46     NA
# 3 4     10     NA
# 4 5      2     NA

        #  This call should fail, so need to catch the error.
        #  Could this be done with a call to expect_error() instead?
    error_in_compute =
        tryCatch ({ compute_final_wrapped_extra_spp_abund_hist (wrapped_extra_spp_abund_merge,
                                                                allow_imperfect_wrap = FALSE)
                    FALSE
                  },
                  error = function (err) { TRUE }
                 )


test_that("compute_final_wrapped_extra_spp_abund_hist: wrap fails when base set not a proper subset", {
    expect_equal (error_in_compute, TRUE)    #  Replace with expect_error()?
})

    #-----------------------
    #  Test normal example
    #-----------------------

    wrapped_extra_spp_abund_merge = data.frame (x = 2:6,
                                                freq.x = c(86, 46, 10, 2, 2),
                                                freq.y = c(79, NA, NA, NA, NA))
    desired_result = data.frame (abund = 2:6,
                                 freq = c(7, 46, 10, 2, 2))

    final_wrapped_extra_spp_abund_merge =
        compute_final_wrapped_extra_spp_abund_hist (wrapped_extra_spp_abund_merge,
                                                    allow_imperfect_wrap = FALSE)

test_that("compute_final_wrapped_extra_spp_abund_hist: proper wrap should succeed", {
    expect_equal (final_wrapped_extra_spp_abund_merge, desired_result)
})

#===============================================================================

            #---------------------------------
            #  Test check_for_imperfect_wrap
            #---------------------------------

    #-------------------------------------------------------------------------
    #  Perfect wrap, i.e., wrapping distribution fully encloses base problem.
    #  Should succeed regardless of setting of allow_imperfect_wrap flag.
    #-------------------------------------------------------------------------

# final_wrapped_extra_spp_abund_hist =
#   abund freq
# 1     2    7     <<<<<-----  perfect:  wrap fully encloses base problem
# 2     3   46
# 3     4   10
# 4     5    2
# 5     6    2

    in_final_wrapped_extra_spp_abund_hist =
        data.frame (abund = 2:6, freq = c(7, 46, 10, 2, 2))

    out_final_wrapped_extra_spp_abund_hist =
        check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                  allow_imperfect_wrap = TRUE)

test_that("check_for_imperfect_wrap: proper wrap should succeed when imperfect wrap allowed", {
    expect_equal (out_final_wrapped_extra_spp_abund_hist,
                  in_final_wrapped_extra_spp_abund_hist)
})

    out_final_wrapped_extra_spp_abund_hist =
        check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                  allow_imperfect_wrap = FALSE)

test_that("check_for_imperfect_wrap: proper wrap should succeed when imperfect wrap NOT allowed", {
    expect_equal (out_final_wrapped_extra_spp_abund_hist,
                  in_final_wrapped_extra_spp_abund_hist)
})

    #-------------------------------------------------------------------------
    #  Imperfect wrap, i.e., wrapping distribution does NOT fully enclose
    #  base problem.
    #-------------------------------------------------------------------------
    #  Should succeed if imperfect wrap allowed and set any negative
    #  abundances (i.e., imperfections in the wrap) to 0.
    #
    #  It should fail if imperfect wrap not allowed.
    #-------------------------------------------------------------------------

# final_wrapped_extra_spp_abund_hist =
#   abund freq
# 1     2   -20     <<<<<-----  imperfect: does not fully enclose base problem
# 2     3   46
# 3     4   10
# 4     5    2
# 5     6    2

    in_final_wrapped_extra_spp_abund_hist =  #  hist with negative freq value(s)
        data.frame (abund = 2:6, freq = c(-20, 46, 10, -2, 2))

    corrected_final_wrapped_extra_spp_abund_hist = # hist with negs replaced with 0s
        data.frame (abund = 2:6, freq = c(0, 46, 10, 0, 2))

    out_final_wrapped_extra_spp_abund_hist =
        check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                  allow_imperfect_wrap = TRUE)

test_that("check_for_imperfect_wrap: imperfect wrap should succeed & correct when imperfect wrap allowed", {
    expect_equal (out_final_wrapped_extra_spp_abund_hist,
                  corrected_final_wrapped_extra_spp_abund_hist)
})

    #--------------------

test_that("check_for_imperfect_wrap: imperfect wrap should fail when imperfect wrap NOT allowed", {
    expect_error (check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                            allow_imperfect_wrap = FALSE))
})

#===============================================================================

            #----------------------------------------------------
            #  Test build_vec_of_extra_spp_and_their_abundances
            #----------------------------------------------------

    in_final_wrapped_extra_spp_abund_hist =
        data.frame (abund = 2:6, freq = c(3, 1, 2, 4, 2))

    desired_result = c(2,2,2,    #  3 spp on 2 patches
                       3,        #  1 spp on 3 patches
                       4,4,      #  2 spp on 4 patches
                       5,5,5,5,  #  4 spp on 5 patches
                       6,6)      #  2 spp on 6 patches

extra_spp_abund =
    build_vec_of_extra_spp_and_their_abundances (in_final_wrapped_extra_spp_abund_hist)

test_that("build_vec_of_extra_spp_and_their_abundances: all non-zero abundance frequencies in wrapped_extra_spp_abund_hist", {
    expect_equal (extra_spp_abund, desired_result)
})

    #--------------------

    in_final_wrapped_extra_spp_abund_hist =
        data.frame (abund = 2:6, freq = c(0, 4, 1, 2, 5))

    desired_result = c(             #  0 spp on 2 patches
                       3,3,3,3,     #  4 spp on 3 patches
                       4,           #  1 spp on 4 patches
                       5,5,         #  2 spp on 5 patches
                       6,6,6,6,6)   #  5 spp on 6 patches

    extra_spp_abund =
        build_vec_of_extra_spp_and_their_abundances (in_final_wrapped_extra_spp_abund_hist)

test_that("build_vec_of_extra_spp_and_their_abundances: 1 zero abundance frequencies in wrapped_extra_spp_abund_hist", {
    expect_equal (extra_spp_abund, desired_result)
})

#===============================================================================

                    #------------------------------------
                    #  Test clean_up_wrapped_abund_dist
                    #------------------------------------

    #------------------------------------------------------------------
    #  Test that it fails if the wrapping distribution doesn't
    #  completely contain the wrapped distribution, e.g.,
    #  if there are more species on 2 patches in the base Xu problem
    #  (freq.y) than in the wrapping lognormal distribution (freq.x).
    #------------------------------------------------------------------

    wrapped_extra_spp_abund_merge = data.frame (x = 2:5,
                                               freq.x = c(10, 46, 10, 2),
                                               freq.y = c(20, NA, NA, NA))

test_that("clean_up_wrapped_abund_dist: imperfect wrap should fail when imperfect wrap NOT allowed", {
    expect_error (clean_up_wrapped_abund_dist (wrapped_extra_spp_abund_merge,
                                               allow_imperfect_wrap = FALSE))
})

    #-----------------------
    #  Test normal example
    #-----------------------

    wrapped_extra_spp_abund_merge = data.frame (x = 2:5,
                                               freq.x = c(8, 1, 2, 4),
                                               freq.y = c(5, NA, NA, NA))

    desired_result = c(2,2,2,    #  3 spp on 2 patches
                       3,        #  1 spp on 3 patches
                       4,4,      #  2 spp on 4 patches
                       5,5,5,5)  #  4 spp on 5 patches

test_that("clean_up_wrapped_abund_dist: proper wrap should succeed regardless of value of allow_imperfect_wrap flag", {
    expect_equal (clean_up_wrapped_abund_dist (wrapped_extra_spp_abund_merge,
                                               allow_imperfect_wrap = TRUE),
                  desired_result)

    expect_equal (clean_up_wrapped_abund_dist (wrapped_extra_spp_abund_merge,
                                               allow_imperfect_wrap = FALSE),
                  desired_result)
})

#===============================================================================

        #--------------------------------------------------------------
        #  Test remove_base_spp_abundances_from_wrapping_distribution
        #--------------------------------------------------------------

    #------------------
    #  Simple example
    #------------------

        #  Xu_PU_spp_table PU_spp table for original Xu problem being wrapped
        #  around
    num_spp = 5
    num_PU = 10
    Xu_PU_spp_table = data.frame (PU_ID=1:num_PU, spp_ID=rep(1:num_spp,each=2))
# Xu_PU_spp_table
#    PU_ID spp_ID
# 1      1      1
# 2      2      1
# 3      3      2
# 4      4      2
# 5      5      3
# 6      6      3
# 7      7      4
# 8      8      4
# 9      9      5
# 10    10      5

        #  trimmed_rounded_abund_per_spp vector of abundances of all species in
        #  the full wrapped distribution, i.e., including the original Xu problem
        #  abundances

    trimmed_rounded_abund_per_spp = c(3,2,3,3,2,
                                      5,2,2,2,5,
                                      2,3,2,6,2)

#   x freq.x freq.y
# 1 2     8     5
# 2 3     4     NA
# 4 5     2     NA
# 5 6     1     NA

    desired_result = c(2,2,2,    #  3 spp on 2 patches
                       3,3,3,3,  #  4 spp on 3 patches
                                 #  0 spp on 4 patches
                       5,5,      #  2 spp on 5 patches
                       6)        #  1 spp on 6 patches


test_that("remove_base_spp_abundances_from_wrapping_distribution: simple example", {
    expect_equal (remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                                         trimmed_rounded_abund_per_spp,
                                                                         spp_col_name = "spp_ID",
                                                                         allow_imperfect_wrap = TRUE),
                  desired_result)

    expect_equal (remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                                         trimmed_rounded_abund_per_spp,
                                                                         spp_col_name = "spp_ID",
                                                                         allow_imperfect_wrap = FALSE),
                  desired_result)

})

    #---------------------------------------------------------
    #  Simple example that should fail due to imperfect wrap
    #  if allow_imperfect_wrap is FALSE but succeed if TRUE
    #---------------------------------------------------------

    #  Use same Xu_PU_spp_table as above

        #  trimmed_rounded_abund_per_spp vector of abundances of all species in
        #  the full wrapped distribution, i.e., including the original Xu problem
        #  abundances

    trimmed_rounded_abund_per_spp = c(3,2,3,3,2,
                                      5,2,2,5,
                                      3,6)

#   x freq.x freq.y
# 1 2     4     5     <<<<<-----  Fewer spp in the wrapping distr than in base
# 2 3     4     NA
# 4 5     2     NA
# 5 6     1     NA

    desired_result = c(          #  0 spp on 2 patches
                       3,3,3,3,  #  4 spp on 3 patches
                                 #  0 spp on 4 patches
                       5,5,      #  2 spp on 5 patches
                       6)        #  1 spp on 6 patches


test_that("remove_base_spp_abundances_from_wrapping_distribution: simple example", {
    expect_equal (remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                                         trimmed_rounded_abund_per_spp,
                                                                         spp_col_name = "spp_ID",
                                                                         allow_imperfect_wrap = TRUE),
                  desired_result)

    expect_error (remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                                         trimmed_rounded_abund_per_spp,
                                                                         spp_col_name = "spp_ID",
                                                                         allow_imperfect_wrap = FALSE))
})

#===============================================================================

                #-----------------------------------------
                #  Test create_wrapping_spp_PU_spp_table
                #-----------------------------------------

    #---------------------------------------------------------------------------
    #  NOTE:  In the tests that use validate_wrap(), it's not necessary
    #         to rerun the generation of the PU_spp_table... when you
    #         switch the dep_set_PUs_eligible flag between TRUE and FALSE.
    #         The table gets generated the same either way; that flag only
    #         pertains to whether you're VALIDATING whether the generated table
    #         SHOULD contained dep_set PUs other than the first PU for each spp.
    #         The generation control itself is done through the choice of PU_IDs
    #         that are included in the eligible_set vector and whether they
    #         overlap the dep_set vector PU_IDs.
    #---------------------------------------------------------------------------

    extra_abund = c(2,2,2,    #  3 spp on 2 patches
                    3,        #  1 spp on 3 patches
                    4,        #  1 spp on 4 patches
                    5)        #  1 spp on 5 patches

    dep_set_PUs_eligible = FALSE
    dep_set = 1:3

    eligible_set = 4:8

    PU_spp_table_w_dep_set_NOT_eligible =
        create_wrapping_PU_spp_table (extra_abund,
                                          dep_set,
                                          eligible_set,
                                          use_testing_only_rand_seed = TRUE,
                                          testing_only_rand_seed = 17)

#    PU_ID spp_ID    <<<<<----- dep_set NOT eligible ----->>>>>
# 1      1      1
# 2      8      1
# 3      2      2
# 4      7      2
# 5      2      3
# 6      6      3
# 7      1      4
# 8      4      4
# 9      7      4
# 10     1      5
# 11     6      5
# 12     4      5
# 13     8      5
# 14     3      6
# 15     8      6
# 16     7      6
# 17     5      6
# 18     4      6

    desired_PU_spp_table = data.frame (PU_ID = c(1,8,2,7,2,6,1,4,7,1,6,4,8,3,8,7,5,4),
                                       spp_ID = c(1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6))

test_that("create_wrapping_spp_PU_spp_table: dep PUs NOT eligible, test seed 17", {
    expect_equal (desired_PU_spp_table, PU_spp_table_w_dep_set_NOT_eligible)
    expect_true (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = FALSE,
                                PU_spp_table_w_dep_set_NOT_eligible,
                                dep_set))
})
test_that("create_wrapping_spp_PU_spp_table: dep PUs ELIGIBLE, test seed 17", {
    expect_true (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = TRUE,
                                PU_spp_table_w_dep_set_NOT_eligible,
                                dep_set))
})

#---------------------------------------------------

    extra_abund = c(2,2,2,    #  3 spp on 2 patches
                    3,        #  1 spp on 3 patches
                    4,        #  1 spp on 4 patches
                    5)        #  1 spp on 5 patches

    dep_set_PUs_eligible = TRUE
    dep_set = 1:3
    eligible_set = 1:8    #  all patches eligible

    PU_spp_table_w_dep_set_ELIGIBLE =
        create_wrapping_spp_PU_spp_table (extra_abund,
                                          dep_set,
                                          eligible_set,
                                          use_testing_only_rand_seed = TRUE,
                                          testing_only_rand_seed = 17)


# > PU_spp_table
#    PU_ID spp_ID    <<<<<----- PU_ID changes now that dep_set IS eligible ----->>>>>
# 1      1      1
# 2      8      1
# 3      2      2
# 4      7      2
# 5      2      3
# 6      5      3    <<<<<----- 5 instead of 6 ----->>>>>
# 7      1      4
# 8      3      4    <<<<<----- 3 instead of 4 ----->>>>>
# 9      6      4    <<<<<----- 6 instead of 7 ----->>>>>
# 10     1      5
# 11     5      5    <<<<<----- 5 instead of 6 ----->>>>>
# 12     2      5    <<<<<----- 2 instead of 4 ----->>>>>
# 13     6      5    <<<<<----- 6 instead of 8 ----->>>>>
# 14     3      6
# 15     8      6
# 16     7      6
# 17     5      6
# 18     2      6    <<<<<----- 2 instead of 4 ----->>>>>

    desired_PU_spp_table = data.frame (PU_ID = c(1,8,2,7,2,5,1,3,6,1,5,2,6,3,8,7,5,2),
                                       spp_ID = c(1,1,2,2,3,3,4,4,4,5,5,5,5,6,6,6,6,6))

    #  Test that a correct outcome generated with dep_set PUs eligible passes.

test_that("create_wrapping_spp_PU_spp_table: on dep PUs and they should be ELIGIBLE, test seed 17", {
    expect_equal (desired_PU_spp_table, PU_spp_table_w_dep_set_ELIGIBLE)
    expect_true (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = TRUE,
                                PU_spp_table_w_dep_set_ELIGIBLE,
                                dep_set))
})

    #  Test for validation errors.

        #  Rule 1:  At least one occurrence of each species ID must be on a PU in the dep set
PU_spp_table_rule_1_violation = PU_spp_table_w_dep_set_ELIGIBLE

PU_spp_table_rule_1_violation [3, "PU_ID"] = 4    #  i.e., change dep_set PU 2 to non-dep_set PU 4
PU_spp_table_rule_1_violation [5, "PU_ID"] = 6    #  i.e., change dep_set PU 2 to non-dep_set PU 6

test_that("create_wrapping_spp_PU_spp_table: on dep PUs but they should NOT be eligible, test seed 17, violates wrapping Rule 1", {
    expect_error (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = TRUE,
                                PU_spp_table_rule_1_violation,
                                dep_set))
})

        #  Rule 2:  If dep set not eligible, then one and only one occurrence of the species can be in dep set
test_that("create_wrapping_spp_PU_spp_table: on dep PUs but they should NOT be eligible, test seed 17, violates wrapping Rule 2", {
    expect_error (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = FALSE,
                                PU_spp_table_w_dep_set_ELIGIBLE,
                                dep_set))
})

        #  Rule 3:  No PU can occur more than once within a species
PU_spp_table_rule_3_violation = PU_spp_table_w_dep_set_ELIGIBLE

PU_spp_table_rule_3_violation [15, "PU_ID"] = 5    #  i.e., change spp 6 non-dep_set PU 8 to non-dep_set PU 5, which is same as spp 6 value in row 17
PU_spp_table_rule_3_violation [9,  "PU_ID"] = 1    #  i.e., change spp 4 dep_set PU 6 to dep_set PU 1, which is same as spp 4 value in row 7
PU_spp_table_rule_3_violation [4,  "PU_ID"] = 2    #  i.e., change spp 2 non-dep_set PU 7 to dep_set PU 2, which is same as spp 2 value in row 3
PU_spp_table_rule_3_violation [10, "PU_ID"] = 6    #  i.e., change spp 5 dep_set PU 1 to non-dep_set PU 6, which is same as spp 2 value in row 13

test_that("create_wrapping_spp_PU_spp_table: on dep PUs but they should NOT be eligible, test seed 17, violates wrapping Rule 3", {
    expect_error (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = TRUE,
                                PU_spp_table_rule_3_violation,
                                dep_set))
})

        #  Rule 4:  All species must occur in result table
PU_spp_table_rule_4_violation = PU_spp_table_w_dep_set_ELIGIBLE [-(10:13),]    #  i.e., remove species 5 from the PU_spp_table

test_that("create_wrapping_spp_PU_spp_table: on dep PUs but they should NOT be eligible, test seed 17, violates wrapping Rule 4", {
    expect_error (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = TRUE,
                                PU_spp_table_rule_4_violation,
                                dep_set))
})


        #  Rule 5:  All species must occur the number of times specified in their abundance
PU_spp_table_rule_5_violation = PU_spp_table_w_dep_set_ELIGIBLE [-c(2,6,16,17),]    #  i.e., remove 1 occurrence from spp 1 & 3, and 2 occurrences from spp 6

test_that("create_wrapping_spp_PU_spp_table: on dep PUs but they should NOT be eligible, test seed 17, violates wrapping Rule 5", {
    expect_error (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = TRUE,
                                PU_spp_table_rule_5_violation,
                                dep_set))
})


        #  Rule 6:  Total number of lines in the result table must equal total number of occurrences
PU_spp_table_rule_6_violation = PU_spp_table_w_dep_set_ELIGIBLE [-18,]    #  i.e., remove 1 occurrence from spp 1 & 3, and 2 occurrences from spp 6

test_that("create_wrapping_spp_PU_spp_table: on dep PUs but they should NOT be eligible, test seed 17, violates wrapping Rule 6", {
    expect_error (validate_wrap (extra_abund,
                                                    dep_set_PUs_eligible = TRUE,
                                PU_spp_table_rule_6_violation,
                                dep_set))
})


#===============================================================================

                #--------------------------------------------
                #  Test wrap_abundances_around_eligible_set
                #--------------------------------------------
    #------------------
    #  Simple example
    #------------------

        #  Xu_PU_spp_table PU_spp table for original Xu problem being wrapped
        #  around
    num_base_spp = 5
    num_base_PU  = 10
    Xu_nodes     = 1:10
    Xu_dep_set   = c(2,4,6,8,10)

    Xu_PU_spp_table = data.frame (PU_ID  = 1:num_base_PU,
                                  spp_ID = rep (1:num_base_spp, each=2))

# Xu_PU_spp_table
#    PU_ID spp_ID
# 1      1      1
# 2      2      1    dep
# 3      3      2
# 4      4      2    dep
# 5      5      3
# 6      6      3    dep
# 7      7      4
# 8      8      4    dep
# 9      9      5
# 10    10      5    dep


        #  rounded_abundances vector of abundances of all species in
        #  the full wrapped distribution, i.e., including the original Xu problem
        #  abundances
# rounded_abundances =
#   [1] 3 2 3 3   1   2 5 2 2 2 5 2 3 2   1   6 2   1 1 1

    rounded_abundances = c(3,2,3,3,
                           1,
                           2,5,2,2,2,5,2,3,2,
                           1,
                           6,2,
                           1,1,1)

# Should remove all 5 of the 1s to give:
# trimmed_rounded_abund_per_spp =
#   [1] 3 2 3 3 2 5 2 2 2 5 2 3 2 6 2

#      wrap   base                wrap
#   x freq.x freq.y           extra_spp_abund
# 1 2     8     5                   3  extra spp on 2 patches
# 2 3     4     NA    ----->>>>>    4  extra spp on 3 patches
# 4 5     2     NA                  2  extra spp on 5 patches
# 5 6     1     NA                  1  extra spp on 6 patches

    dep_set_PUs_eligible = FALSE    #  <<<<<-----
    allow_imperfect_wrap = FALSE
    tot_num_PUs_in_landscape = 15

    # set in wrap_abundance_dist_around_Xu_problem(), i.e., calling routine

    largest_PU_ID = max (Xu_nodes)
    extra_PUs     = (largest_PU_ID + 1) : tot_num_PUs_in_landscape

    eligible_PUs =
        create_eligible_PU_set (Xu_dep_set           = Xu_dep_set,
                                extra_PUs            = extra_PUs,
                                dep_set_PUs_eligible = dep_set_PUs_eligible)

    wrapped_PU_spp_indices =
        wrap_abundances_around_eligible_set (
            dep_set                         = Xu_dep_set,
            eligible_set                    = eligible_PUs,
            rounded_abund_per_spp           = rounded_abundances,
            num_base_spp                    = num_base_spp,
            Xu_PU_spp_table                 = Xu_PU_spp_table,
            allow_imperfect_wrap            = allow_imperfect_wrap,
            min_allowed_abundance           = 2,
            PU_col_name                     = "PU_ID",
            spp_col_name                    = "spp_ID",
            use_testing_only_rand_seed      = TRUE,
            testing_only_rand_seed          = 17)


desired_result =
    data.frame (PU_ID  = c(1,2,3,4,5,6,7,8,9,10,2,15,6,14,6,13,4,11,14,2,13,11,
                           10,15,14,10,14,12,8,13,14,12,15,10,15,11,12,13,10,12,
                           15,11,14,13),
                spp_ID = c(1,1,2,2,3,3,4,4,5,5,6,6,7,7,8,8,9,9,9,10,10,10,11,
                           11,11,12,12,12,13,13,13,13,13,14,14,14,14,14,15,15,
                           15,15,15,15))

test_that("wrap_abundances_around_eligible_set: simple example", {
    expect_equal (desired_result, wrapped_PU_spp_indices)
})

#===============================================================================

                #-------------------------------------------------
                #  Test options_are_legal_for_single_bdprob_WRAP
                #-------------------------------------------------

    #  Fail if Xu_bench is TRUE   OR   if lognormal is FALSE

parameters = list()    #  lognormal FALSE => fail
test_that("options_are_legal_for_single_bdprob_WRAP: empty list", {
    expect_error (options_are_legal_for_single_bdprob_WRAP (parameters))
})

parameters = list (read_Xu_problem_from_Xu_bench_file = FALSE)    #  lognormal FALSE => fail
test_that("options_are_legal_for_single_bdprob_WRAP: bench FALSE, log empty", {
    expect_error (options_are_legal_for_single_bdprob_WRAP (parameters))
})

parameters = list (read_Xu_problem_from_Xu_bench_file = FALSE,    #  lognormal FALSE => fail
                   wrap_lognormal_dist_around_Xu      = FALSE)
test_that("options_are_legal_for_single_bdprob_WRAP: bench FALSE, log FALSE", {
    expect_error (options_are_legal_for_single_bdprob_WRAP (parameters))
})

parameters = list (read_Xu_problem_from_Xu_bench_file = TRUE,     #  lognormal FALSE => fail
                   wrap_lognormal_dist_around_Xu      = FALSE)
test_that("options_are_legal_for_single_bdprob_WRAP: bench TRUE, log FALSE", {
    expect_error (options_are_legal_for_single_bdprob_WRAP (parameters))
})

parameters = list (read_Xu_problem_from_Xu_bench_file = TRUE,     #  Xu_bench TRUE => fail
                   wrap_lognormal_dist_around_Xu      = TRUE)
test_that("options_are_legal_for_single_bdprob_WRAP: bench TRUE, log TRUE", {
    expect_error (options_are_legal_for_single_bdprob_WRAP (parameters))
})

parameters = list (read_Xu_problem_from_Xu_bench_file = FALSE,    #  SUCCEED
                   wrap_lognormal_dist_around_Xu      = TRUE)
test_that("options_are_legal_for_single_bdprob_WRAP: bench FALSE, log TRUE", {
    expect_true (options_are_legal_for_single_bdprob_WRAP (parameters))
})

parameters = list (wrap_lognormal_dist_around_Xu      = TRUE)    #  SUCCEED
test_that("options_are_legal_for_single_bdprob_WRAP: bench empty, log TRUE", {
    expect_true (options_are_legal_for_single_bdprob_WRAP (parameters))
})

#===============================================================================

                #--------------------------------------------
                #  Test wrap_abundance_dist_around_Xu_problem
                #--------------------------------------------
if(FALSE)
wrap_abundance_dist_around_Xu_problem (starting_dir,
                                                  compute_network_metrics_for_this_prob,
                                                  rounded_abundances,
                                                  Xu_bdprob,
                                                  dep_set_PUs_eligible,
                                                  tot_num_PUs_in_landscape,
                            seed_value_for_search_list,
                                        allow_imperfect_wrap,

                                                  bdpg_error_codes,
                                                  search_outfile_name_base,
                                                  search_outfile_name,
                                            wrap_prob_name_stem = "wrap_prob",
                                            cor_dir_name_stem = "cor"
                                                  )

#===============================================================================

                #--------------------------------------------
                #  Test gen_wrapped_bdprob_COR
                #--------------------------------------------
if(FALSE)
            gen_wrapped_bdprob_COR (starting_dir,
                                    parameters,
                                    base_bdprob,
                                    bdpg_error_codes)

#===============================================================================

    #----------------------------------------------------------------------
    #  Copies of parameters list generated by tzar from a particular run.
    #  These are here to use as the basis for building a parameters list
    #  for tests in building objects.
    #----------------------------------------------------------------------

if(FALSE)
{
parameters__full_including_marxan__original = list (
        marxan_pu_file_name = "pu.dat",
        too_many_possible_links_error_flag_file_name = "/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/too_many_possible_links_error_flag_file.txt",
        desired_max_abundance_frac = 0.7,
        marxan_spf_rule = "POWER_OF_10",
        allow_imperfect_wrap = TRUE,
        spp_occ_FN_const_rate = 0.1,
        dep_set_PUs_eligible = FALSE,
        max_search_iterations = 500,
        use_igraph_metrics = TRUE,
        full_output_dir_with_slash = "/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/",
        use_bipartite_metrics = TRUE,
        gen_multi_bdproblem = TRUE,
        solution_frac_of_landscape = 0.3,
        single_action_using_tzar_reps = FALSE,
        marxan_num_iterations = '1000000',
        p__prop_of_links_between_groups_lower_bound = 0.01,
        runset_abbrev = "bdpgSingleRunForTestAndDebug",
        max_allowed_possible_tot_num_links = 1500,
        read_Xu_problem_from_Xu_bench_file = FALSE,
        plot_rounded_abundances = TRUE,
        r__density_upper_bound = 5.0,
        marxan_puvspr_file_name = "puvspr.dat",
        p__prop_of_links_between_groups_upper_bound = 0.99,
        apply_error_to_spp_occupancy_data = TRUE,
        seed = 1,
        use_unif_rand_r__density = TRUE,
        wrap_lognormal_dist_around_Xu = TRUE,
        n__num_groups_upper_bound = 80,
        compute_network_metrics_APP = TRUE,
        spp_occ_FP_error_type = "CONSTANT",
        marxan_spec_file_name = "spec.dat",
        bdpg_run_init_rand_seed = 14329,
        marxan_input_parameters_file_name = "/Users/bill/tzar/modelcode/marxanLibrary_1360018653/input.dat",
        spp_occ_FP_const_rate = 0.0,
        integerize_string = "round",
        compute_network_metrics = FALSE,
        derive_alpha_from_n__num_groups_and_opt_frac_0.5 = TRUE,
        r__density_lower_bound = 0.01,
        desired_Xu_spp_frac_of_all_spp = 0.5,
        given_correct_solution_cost = 420,
        compute_network_metrics_wrapped_COR = TRUE,
        marxan_num_reps = 100,
        n__num_groups_lower_bound = 61,
        use_unif_rand_p__prop_of_links_between_groups = TRUE,
        marxan_use_default_input_parameters = FALSE,
        max_allowed_num_spp = 2000,
        run_id = 39,
        bipartite_metrics_to_use = "quick_test",
        marxan_runmode = 1,
        runset_description = "|, Small set of runs of a Xu hard scenario with error generation both on and off plus running simulated annealing and richness heuristic.",
        gen_4_basic_variants = TRUE,
        spp_occ_FN_error_type = "CONSTANT",
        marxan_heurtype = -1,
        set_rand_seed_at_creation_of_all_new_major_objects = FALSE,
        marxan_dir = "/Users/bill/tzar/modelcode/marxanLibrary_1360018653/",
        compute_network_metrics_COR = TRUE,
        use_unif_rand_n__num_groups = TRUE,
        Xu_bench_infile_name = "/Users/bill/D/Projects/ProblemDifficulty/data/Xu - problem difficulty datasets/frb30-15-msc with MSC 420/frb30-15-1.msc",
        add_one_to_lognormal_abundances = FALSE,
        match_error_counts = FALSE,
        compute_network_metrics_wrapped_APP = TRUE,
        runset_name = "default_runset"
        )

parameters__full_including_marxan__grouped = list (

            #  general - run
        run_id = 39,
        runset_name = "default_runset",

        full_output_dir_with_slash = "/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/",

        runset_abbrev = "bdpgSingleRunForTestAndDebug",
        runset_description = "|, Small set of runs of a Xu hard scenario with error generation both on and off plus running simulated annealing and richness heuristic.",

        seed = 1,
        bdpg_run_init_rand_seed = 14329,
        set_rand_seed_at_creation_of_all_new_major_objects = FALSE,

        integerize_string = "round",

        gen_4_basic_variants = TRUE,
        single_action_using_tzar_reps = FALSE,


            #  cor

                #  Xu from scratch
        max_allowed_num_spp = 2000,
        max_allowed_possible_tot_num_links = 1500,
        too_many_possible_links_error_flag_file_name = "/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/too_many_possible_links_error_flag_file.txt",

        desired_Xu_spp_frac_of_all_spp = 0.5,
        derive_alpha_from_n__num_groups_and_opt_frac_0.5 = TRUE,

        use_unif_rand_n__num_groups = TRUE,
        n__num_groups_lower_bound = 61,
        n__num_groups_upper_bound = 80,

        use_unif_rand_p__prop_of_links_between_groups = TRUE,
        p__prop_of_links_between_groups_lower_bound = 0.01,
        p__prop_of_links_between_groups_upper_bound = 0.99,

        use_unif_rand_r__density = TRUE,
        r__density_lower_bound = 0.01,
        r__density_upper_bound = 5.0,

                #  Xu benchmark
        read_Xu_problem_from_Xu_bench_file = FALSE,
        given_correct_solution_cost = 420,
        Xu_bench_infile_name = "/Users/bill/D/Projects/ProblemDifficulty/data/Xu - problem difficulty datasets/frb30-15-msc with MSC 420/frb30-15-1.msc",


            #  network
        compute_network_metrics = FALSE,

        use_igraph_metrics = TRUE,
        use_bipartite_metrics = TRUE,
        compute_network_metrics_wrapped_COR = TRUE,
        compute_network_metrics_COR = TRUE,
        compute_network_metrics_wrapped_APP = TRUE,
        bipartite_metrics_to_use = "quick_test",
        compute_network_metrics_APP = TRUE,


            #  marxan
        marxan_num_iterations = '1000000',
        marxan_pu_file_name = "pu.dat",
        marxan_spf_rule = "POWER_OF_10",
        marxan_puvspr_file_name = "puvspr.dat",
        marxan_spec_file_name = "spec.dat",
        marxan_runmode = 1,
        marxan_input_parameters_file_name = "/Users/bill/tzar/modelcode/marxanLibrary_1360018653/input.dat",
        marxan_num_reps = 100,
        marxan_use_default_input_parameters = FALSE,
        marxan_heurtype = -1,
        marxan_dir = "/Users/bill/tzar/modelcode/marxanLibrary_1360018653/",


            #  wrap
        gen_multi_bdproblem = TRUE,

        solution_frac_of_landscape = 0.3,
        allow_imperfect_wrap = TRUE,
        dep_set_PUs_eligible = FALSE,
        max_search_iterations = 500,
        wrap_lognormal_dist_around_Xu = TRUE,
        add_one_to_lognormal_abundances = FALSE,
        plot_rounded_abundances = TRUE,
        desired_max_abundance_frac = 0.7,


            #  app
        apply_error_to_spp_occupancy_data = TRUE,

        spp_occ_FN_const_rate = 0.1,
        spp_occ_FP_error_type = "CONSTANT",
        spp_occ_FP_const_rate = 0.0,
        spp_occ_FN_error_type = "CONSTANT",
        match_error_counts = FALSE,

dummylistend=NULL
        )

}

#===============================================================================

                #-------------------------------
                #  Test gen_single_bdprob_WRAP
                #-------------------------------

test_gen_single_COR <- function (parameters, bdpg_error_codes)
    {
    base_COR_bd_prob = gen_single_bdprob_COR (parameters,
                                              bdpg_error_codes,
                                              integerize          = round,
                                              base_prob_name_stem = "base_prob",
                                              cor_dir_name_stem   = "cor"
                                              )

    cat ("\n\n-----  base_COR_bd_prob@UUID = '", base_COR_bd_prob@UUID,
         "', checksum = '", base_COR_bd_prob@checksum, "'  -----\n\n")

    cat ("\n\nfull_output_dir_with_slash = ",
         "\n    '", full_output_dir_with_slash, "'\n\n")

    return (base_COR_bd_prob)
    }

parameters = list (

            #  general - run
        run_id = 39,
        runset_name = "default_runset",

        full_output_dir_with_slash = "/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/",

        runset_abbrev = "test_runset",
        runset_description = "test_runset long description",

        # seed = 1,
        bdpg_run_init_rand_seed = 14329,
        set_rand_seed_at_creation_of_all_new_major_objects = FALSE,

        integerize_string = "round",

        # gen_4_basic_variants = TRUE,
        # single_action_using_tzar_reps = FALSE,

            #  cor - Xu from scratch

        read_Xu_problem_from_Xu_bench_file = FALSE,

        max_allowed_num_spp = 2000,
        max_allowed_possible_tot_num_links = 1500,
        too_many_possible_links_error_flag_file_name = "/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/too_many_possible_links_error_flag_file.txt",

        desired_Xu_spp_frac_of_all_spp = 0.5,
        derive_alpha_from_n__num_groups_and_opt_frac_0.5 = TRUE,

        use_unif_rand_n__num_groups = TRUE,
        n__num_groups_lower_bound = 61,
        n__num_groups_upper_bound = 80,

        use_unif_rand_p__prop_of_links_between_groups = TRUE,
        p__prop_of_links_between_groups_lower_bound = 0.01,
        p__prop_of_links_between_groups_upper_bound = 0.99,

        use_unif_rand_r__density = TRUE,
        r__density_lower_bound = 0.01,
        r__density_upper_bound = 5.0,


            #  network
        compute_network_metrics = FALSE,

            #  wrap
        # gen_multi_bdproblem = TRUE,
        #
        solution_frac_of_landscape = 0.3,
        # allow_imperfect_wrap = FALSE,
        # dep_set_PUs_eligible = FALSE,
        max_search_iterations = 500,
        wrap_lognormal_dist_around_Xu = TRUE,
        # add_one_to_lognormal_abundances = FALSE,
        # plot_rounded_abundances = TRUE,
        desired_max_abundance_frac = 0.7,


dummylistend=NULL
        )

    #  Create test output directory.
full_output_dir = tempdir()
last_char = substr (full_output_dir, nchar(full_output_dir), nchar(full_output_dir))
full_output_dir_with_slash =
    if (last_char == "/") full_output_dir else paste0 (full_output_dir, "/")

parameters$full_output_dir_with_slash                   = full_output_dir_with_slash    #"/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/"
parameters$too_many_possible_links_error_flag_file_name =
    paste0 (full_output_dir_with_slash, "too_many_possible_links_error_flag_file.txt")  #"/Users/bill/tzar/outputdata/bdpgxupaper_4_variants_no_network_metrics/default_runset/39_marxan_simulated_annealing.inprogress/too_many_possible_links_error_flag_file.txt"

    #  Initialize for a bdpg run.
params_and_error_codes = init_for_bdpg (parameters)

bdpg_error_codes = params_and_error_codes$bdpg_error_codes
parameters       = params_and_error_codes$parameters

    #  Generate a correct problem from scratch.
bdprob_to_wrap = test_gen_single_COR (parameters,
                                      bdpg_error_codes)

    #  Generate a wrapped problem around the correct problem just generated.
wrapped_problem = gen_single_bdprob_WRAP (bdprob_to_wrap,
                                          parameters,
                                          bdpg_error_codes)

test_that("gen_single_bdprob_WRAP: COR and WRAP problem generation succeed", {
    expect_true (! is.null (bdprob_to_wrap))
    expect_true (! is.null (wrapped_problem))
})

#===============================================================================


