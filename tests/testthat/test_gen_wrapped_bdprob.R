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

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

                #-----------------------------------------
                #  Test create_wrapping_spp_PU_spp_table
                #-----------------------------------------

compute_PU_spp_table_attributes_by_spp <- function (PU_IDs_for_one_spp_as_df,
                                                    dep_set)
    {
    num_pu_this_spp        = length (PU_IDs_for_one_spp_as_df)
    num_unique_pu_this_spp = length (unique (PU_IDs_for_one_spp_as_df))
    num_pu_in_dep_set      = sum (PU_IDs_for_one_spp_as_df %in% dep_set)

    return (data.frame (num_pu_this_spp, num_unique_pu_this_spp, num_pu_in_dep_set))
    }

validate_wrap <- function (num_extra_spp,
                           dep_set_PUs_eligible,
                           PU_spp_table,
                           dep_set)
    {
        #  Apply compute_PU_spp_table_attributes_by_spp() function to PU_spp_table by spp_ID group
        #  Form of this call is taken from:
        #  http://www.milanor.net/blog/dplyr-do-tips-for-using-and-programming/
    values_by_spp =
        PU_spp_table %>%
        dplyr::group_by (spp_ID) %>%
        dplyr::do (compute_PU_spp_table_attributes_by_spp (PU_IDs_for_one_spp_as_df = .$PU_ID,
                                                           dep_set))

        #  Rules that PU_spp_table for wrapped spp must satisfy to be valid:

    num_test_rules = 6
    ok = rep (FALSE, num_test_rules)

        #  At least one occurrence of each species ID must be on a PU in the dep set
    ok [1] = all (values_by_spp$num_pu_in_dep_set > 0)
    if (! ok[1])
        {
        message ("\nWrapping rule 1 violation:")
        cat ("\nAt least one occurrence of each species ID must be on a PU in the dep set",
             "\nnum_extra_spp = ", num_extra_spp,
             "\nsum (values_by_spp$num_pu_in_dep_set > 0) = ",
             sum (values_by_spp$num_pu_in_dep_set > 0),
             "\nwhich (values_by_spp$num_pu_in_dep_set <= 0) = '",
             which (values_by_spp$num_pu_in_dep_set <= 0), "'")
        print (values_by_spp$num_pu_in_dep_set)
        }

        #  If dep set not eligible, then one and only one occurrence of the species can be in dep set
    ok [2] = TRUE
    if (! dep_set_PUs_eligible)
        ok [2] = all (values_by_spp$num_pu_in_dep_set == 1)
    if (! ok[2])
        {
        message ("\nWrapping rule 2 violation:")
        cat ("\nIf dep set not eligible, then one and only one occurrence of the species can be in dep set",
             "\nnum_extra_spp = ", num_extra_spp,
             "\nsum (values_by_spp$num_pu_in_dep_set == 1) = ",
             sum (values_by_spp$num_pu_in_dep_set == 1),
             "\nwhich (values_by_spp$num_pu_in_dep_set != 1) = '",
             which (values_by_spp$num_pu_in_dep_set != 1), "'")
        print (values_by_spp$num_pu_in_dep_set)
        }

        #  No PU can occur more than once within a species
    ok [3] = all (values_by_spp$num_pu_this_spp == values_by_spp$num_unique_pu_this_spp)
    if (! ok[3])
        {
        cts_not_same = which (values_by_spp$num_pu_this_spp !=
                                  values_by_spp$num_unique_pu_this_spp)
        message ("\nWrapping rule 3 violation:")
        cat ("\nNo PU can occur more than once within a species",
             "\nduplicates at index(s): '", cts_not_same, "'")
        cat ("\nvalues_by_spp$num_pu_this_spp = ")
        print (values_by_spp$num_pu_this_spp)
        cat ("\nvalues_by_spp$num_unique_pu_this_spp = ")
        print (values_by_spp$num_unique_pu_this_spp)
        }

        #  All species must occur in result table
    ok [4] = (num_extra_spp == length (values_by_spp$spp_ID))
    if (! ok[4])
        {
        message ("\nWrapping rule 4 violation:")
        cat ("\nAll species must occur in result table",
             "\nnum_extra_spp = ", num_extra_spp,
             "\nlength (values_by_spp$spp_ID) = ", length (values_by_spp$spp_ID))
        }

        #  All species must occur the number of times specified in their abundance
            #  Had to use isTRUE() here because all.equal doesn't seem to return
            #  a boolean.  For some reason, it returns a string saying
            #  something like "Mean relative difference: ..." when they
            #  don't match.  When they do match, it does return TRUE though...
    ok [5] = isTRUE (all.equal (extra_abund, values_by_spp$num_unique_pu_this_spp))
    if (! ok[5])
        {
        message ("\nWrapping rule 5 violation:")
        indices_of_mismatches = which (extra_abund != values_by_spp$num_unique_pu_this_spp)
        cat ("\nAll species must occur the number of times specified in their abundance",
             "\nindices_of_mismatches = '", indices_of_mismatches, "'")
        cat ("\nextra_abund = ")
        print (extra_abund)
        cat ("\nvalues_by_spp$num_unique_pu_this_spp = ")
        print (values_by_spp$num_unique_pu_this_spp)
        }

        #  Total number of lines in the result table must equal total number of occurrences
    ok [6] = (sum (extra_abund) == nrow (PU_spp_table))
    if (! ok[6])
        {
        message ("\nWrapping rule 6 violation:")
        cat ("\nTotal number of lines in the result table must equal total number of occurrences",
             "\ntotal num spp = sum (extra_abund) = ", sum (extra_abund),
             "\ntotal num lines in result table = ", nrow (PU_spp_table))
        cat ("\nextra_abund = ")
        print (extra_abund)
        cat ("\nPU_spp_table = ")
        print (PU_spp_table)
        }

    all_ok = all (ok)
    if (! all_ok)
        {
        cat ("\nRule(s) ", which (!ok),
                      " violated in wrapped species PU_spp_table.\n")
        stop()
        }

    return (all_ok)
    }

#---------------------------------------------------

    extra_abund = c(2,2,2,    #  3 spp on 2 patches
                    3,        #  1 spp on 3 patches
                    4,        #  1 spp on 4 patches
                    5)        #  1 spp on 5 patches

    dep_set_PUs_eligible = FALSE
    dep_set = 1:3

    eligible_set = 4:8

    PU_spp_table = create_wrapping_spp_PU_spp_table (extra_abund,
                                                     dep_set,
                                                     eligible_set,
                                                     use_testing_only_rand_seed = TRUE,
                                                     testing_only_rand_seed = 17)

#    PU_ID spp_ID
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

    wrap_ok = validate_wrap (num_extra_spp = length (extra_abund),
                             dep_set_PUs_eligible,
                             PU_spp_table,
                             dep_set)

test_that("create_wrapping_spp_PU_spp_table: dep PUs NOT eligible, test seed 17", {
    expect_equal (desired_PU_spp_table, PU_spp_table)
    expect_true (validate_wrap (num_extra_spp = length (extra_abund),
                                dep_set_PUs_eligible = FALSE,
                                PU_spp_table,
                                dep_set))
})
test_that("create_wrapping_spp_PU_spp_table: dep PUs ELIGIBLE, test seed 17", {
    expect_true (validate_wrap (num_extra_spp = length (extra_abund),
                                dep_set_PUs_eligible = TRUE,
                                PU_spp_table,
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

PU_spp_table = create_wrapping_spp_PU_spp_table (extra_abund,
                                                 dep_set,
                                                 eligible_set,
                                                 use_testing_only_rand_seed = TRUE,
                                                 testing_only_rand_seed = 17)

# > PU_spp_table
#    PU_ID spp_ID    <<<<<----- PU_ID CHANGES NOW THAT DEP_SET IS ELIGIBLE ----->>>>>
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

    wrap_ok = validate_wrap (num_extra_spp = length (extra_abund),
                             dep_set_PUs_eligible,
                             PU_spp_table,
                             dep_set)

test_that("create_wrapping_spp_PU_spp_table: dep PUs NOT eligible, test seed 17", {
    expect_equal (desired_PU_spp_table, PU_spp_table)
    expect_error (validate_wrap (num_extra_spp = length (extra_abund),
                                dep_set_PUs_eligible = FALSE,
                                PU_spp_table,
                                dep_set))
})
test_that("create_wrapping_spp_PU_spp_table: dep PUs ELIGIBLE, test seed 17", {
    expect_true (validate_wrap (num_extra_spp = length (extra_abund),
                                dep_set_PUs_eligible = TRUE,
                                PU_spp_table,
                                dep_set))
})

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
                                                               allow_imperfect_wrap,
                                                               min_allowed_abundance = 2,
                                                               PU_col_name = "PU_ID",
                                                               spp_col_name = "spp_ID"
                                                               )

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

                #--------------------------------------------
                #  Test gen_wrapped_bdprob_COR
                #--------------------------------------------
if(FALSE)
gen_wrapped_bdprob_COR (starting_dir,
                                    compute_network_metrics_for_this_prob,
                                    parameters,
                                    base_bdprob,
                                    bdpg_error_codes)

#-------------------------------------------------------------------------------

                #--------------------------------------------
                #  Test options_are_legal_for_single_bdprob_WRAP
                #--------------------------------------------
if(FALSE)
options_are_legal_for_single_bdprob_WRAP (bdprob_to_wrap,
                                                      parameters,
                                                      bdpg_error_codes)

#-------------------------------------------------------------------------------

                #--------------------------------------------
                #  Test gen_single_bdprob_WRAP
                #--------------------------------------------
if(FALSE)
gen_single_bdprob_WRAP (bdprob_to_wrap,
                                    parameters,
                                    bdpg_error_codes)

#-------------------------------------------------------------------------------


