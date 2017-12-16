library (bdpg)

context ("gen_wrapped_bdprob")

#-------------------------------------------------------------------------------

                    #-------------------------------
                    #  Test create_eligible_PU_set
                    #-------------------------------

    Xu_dep_set = c(1,2)
    extra_PUs = c(3,4)
    dep_set_PUs_eligible = TRUE

    eligible_PUs = create_eligible_PU_set (Xu_dep_set, extra_PUs, dep_set_PUs_eligible)

test_that("eligible_PUs include dependent and extra PUs", {
    expect_equal (eligible_PUs, c(1,2,3,4))
})

    #----------

    Xu_dep_set = c(1,2)
    extra_PUs = c(3,4)
    dep_set_PUs_eligible = FALSE

    eligible_PUs = create_eligible_PU_set (Xu_dep_set, extra_PUs, dep_set_PUs_eligible)

test_that("eligible_PUs only include extra PUs", {
    expect_equal (eligible_PUs, c(3,4))
})

#-------------------------------------------------------------------------------

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

test_that("endpoints equal to bounds of randomly ordered vector", {
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

test_that("some elements < min and > max bounds of randomly ordered vector", {
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

test_that("elements < min bound only", {
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

test_that("elements > max bound only", {
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

test_that("all elements < min OR > max bounds of vector", {
    expect_length (trimmed_abundances, 0)
})

    #----------

        #  Test elements < min bound only with max bound default argument.
    rounded_abundances = 0:10
    min_abund = 3

    trimmed_abundances = trim_abundances (rounded_abundances,
                                          min_abund=min_abund)

test_that("elements < min bound only with max bound default argument", {
    expect_equal (trimmed_abundances, 3:10)
})

    #----------

        #  Test elements < min bound only with min and max bound default args.
    rounded_abundances = 0:10

    trimmed_abundances = trim_abundances (rounded_abundances)

test_that("elements < min bound only with min and max bound default args", {
    expect_equal (trimmed_abundances, 2:10)
})

#-------------------------------------------------------------------------------

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

    spp_col_name = "spp_ID"

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
                                           spp_col_name)

test_that("simple example for gen_raw_histogram_of_wrapped_dist()", {
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

    spp_col_name = "spp_ID"

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
                                           spp_col_name)

test_that("more complex example for gen_raw_histogram_of_wrapped_dist()", {
    expect_equal (wrapped_extra_spp_abund_merge, desired_result)
})

#-------------------------------------------------------------------------------

            #---------------------------------------------------
            #  Test compute_final_wrapped_extra_spp_abund_hist
            #---------------------------------------------------

    #------------------------------------------------------------------
    #  Test that it fails if the wrapping distribution doesn't
    #  completely contain the wrapped distribution, e.g.,
    #  if there are more species on 2 patches in the base Xu problem
    #  (freq.y) than in the wrapping lognormal distribution (freq.x).
    #------------------------------------------------------------------

    allow_imperfect_wrap = FALSE
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
                                                                allow_imperfect_wrap)
                    FALSE
                  },
                  error = function (err) { TRUE }
                 )


test_that("wrap fails when base set not a proper subset", {
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
        compute_final_wrapped_extra_spp_abund_hist (wrapped_extra_spp_abund_merge)

test_that("proper wrap should succeed", {
    expect_equal (final_wrapped_extra_spp_abund_merge, desired_result)
})

#-------------------------------------------------------------------------------

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

    allow_imperfect_wrap = TRUE
    out_final_wrapped_extra_spp_abund_hist =
        check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                  allow_imperfect_wrap)

test_that("proper wrap should succeed when imperfect wrap allowed", {
    expect_equal (out_final_wrapped_extra_spp_abund_hist,
                  in_final_wrapped_extra_spp_abund_hist)
})

    allow_imperfect_wrap = FALSE
    out_final_wrapped_extra_spp_abund_hist =
        check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                  allow_imperfect_wrap)

test_that("proper wrap should succeed when imperfect wrap NOT allowed", {
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

    allow_imperfect_wrap = TRUE
    out_final_wrapped_extra_spp_abund_hist =
        check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                  allow_imperfect_wrap)

test_that("imperfect wrap should succeed & correct when imperfect wrap allowed", {
    expect_equal (out_final_wrapped_extra_spp_abund_hist,
                  corrected_final_wrapped_extra_spp_abund_hist)
})

    #--------------------

    allow_imperfect_wrap = FALSE

test_that("imperfect wrap should fail when imperfect wrap NOT allowed", {
    expect_error (check_for_imperfect_wrap (in_final_wrapped_extra_spp_abund_hist,
                                            allow_imperfect_wrap))
})

#-------------------------------------------------------------------------------

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

test_that("all non-zero abundance frequencies in wrapped_extra_spp_abund_hist", {
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

test_that("all non-zero abundance frequencies in wrapped_extra_spp_abund_hist", {
    expect_equal (extra_spp_abund, desired_result)
})

#-------------------------------------------------------------------------------

                    #------------------------------------
                    #  Test clean_up_wrapped_abund_dist
                    #------------------------------------
if(FALSE)
extra_spp_abund = clean_up_wrapped_abund_dist (wrapped_extra_spp_abund_merge,
                                               allow_imperfect_wrap)

#-------------------------------------------------------------------------------

        #--------------------------------------------------------------
        #  Test remove_base_spp_abundances_from_wrapping_distribution
        #--------------------------------------------------------------
if(FALSE)
extra_abund =
    remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                           trimmed_rounded_abund_per_spp,
                                                           spp_col_name,
                                                           allow_imperfect_wrap)

#-------------------------------------------------------------------------------

                #--------------------------------------------
                #  Test wrap_abundances_around_eligible_set
                #--------------------------------------------
if(FALSE)
wrapped_PU_spp_indices = wrap_abundances_around_eligible_set (dep_set,
                                                   eligible_set,
                                                   rounded_abund_per_spp,
                                                   num_base_spp,
                                                   Xu_PU_spp_table,
                                                   min_allowed_abundance = 2,
                                                   allow_imperfect_wrap,
                                                   PU_col_name = "PU_ID",
                                                   spp_col_name = "spp_ID"
                                                   )

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------


