#===============================================================================

                        #  test_validation_functions.R

#===============================================================================

library (bdpg)

context ("validation_functions")

#===============================================================================

test_that("vb: input value is a constant", {

        #  TRUE always returns TRUE even if default is bad
    expect_true (vb (TRUE))
    expect_true (vb (TRUE, if_bad_use_def=TRUE, def=FALSE))
    expect_true (vb (TRUE, if_bad_use_def=TRUE, def=NA))

        #  FALSE always returns FALSE even if default is bad
    expect_false (vb (FALSE))
    expect_false (vb (FALSE, if_bad_use_def=TRUE, def=FALSE))
    expect_false (vb (FALSE, if_bad_use_def=TRUE, def="aString"))

        #  NULL returns default when default is good
    expect_true  (vb (NULL, if_bad_use_def=TRUE, def=TRUE))
    expect_false (vb (NULL, if_bad_use_def=TRUE, def=FALSE))

        #  bad value with no default correction always throws error
    expect_error (vb (NULL),
                  "Value '' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NA),
                  "Value 'NA' used for input variable NA must be boolean",
                  fixed=TRUE)
    expect_error (vb (3),
                  "Value '3' used for input variable 3 must be boolean",
                  fixed=TRUE)
    expect_error (vb ("aString"),
                  "Value 'aString' used for input variable \"aString\" must be boolean",
                  fixed=TRUE)

        #  bad value with bad default correction throws error
    expect_error (vb (NULL, if_bad_use_def=TRUE, def=NULL),
                  "Default value '' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, if_bad_use_def=TRUE, def=NA),
                  "Default value 'NA' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, if_bad_use_def=TRUE, def="aString"),
                  "Default value 'aString' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, if_bad_use_def=TRUE, def=27),
                  "Default value '27' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, if_bad_use_def=TRUE, def=0),
                  "Default value '0' used for input variable NULL must be boolean",
                  fixed=TRUE)
})

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

test_that("valid_boolean: input value not a constant", {
    myVar = TRUE
    expect_true (vb (myVar))
    expect_true (vb (myVar, if_bad_use_def=TRUE, def=FALSE))
    expect_true (vb (myVar, if_bad_use_def=TRUE, def=NA))

    myVar = FALSE
    expect_false (vb (myVar))
    expect_false (vb (myVar, if_bad_use_def=TRUE, def=FALSE))
    expect_false (vb (myVar, if_bad_use_def=TRUE, def="aString"))


    myVar = NULL
    expect_error (vb (myVar),
                  "Value '' used for input variable myVar must be boolean",
                  fixed=TRUE)
    myVar = NA
    expect_error (vb (myVar),
                  "Value 'NA' used for input variable myVar must be boolean",
                  fixed=TRUE)

    myList = list (nullElement_1 = NULL, naElement_2 = NA)
    expect_error (vb (myList$nullElement_1),
                  "Value '' used for input variable myList$nullElement_1 must be boolean",
                  fixed=TRUE)
    expect_error (vb (myList$naElement_2),
                  "Value 'NA' used for input variable myList$naElement_2 must be boolean",
                  fixed=TRUE)
    expect_error (vb (myList[[1]]),
                  "Value '' used for input variable myList[[1]] must be boolean",
                  fixed=TRUE)
    expect_error (vb (myList[[2]]),
                  "Value 'NA' used for input variable myList[[2]] must be boolean",
                  fixed=TRUE)
    idx1 = 1
    expect_error (vb (myList[[idx1]]),
                  "Value '' used for input variable myList[[idx1]] must be boolean",
                  fixed=TRUE)
    idx2 = 2
    expect_error (vb (myList[[idx2]]),
                  "Value 'NA' used for input variable myList[[idx2]] must be boolean",
                  fixed=TRUE)

    vec = c(100,200,300)
    expect_error (vb (vec[3]),
                  "Value '300' used for input variable vec[3] must be boolean",
                  fixed=TRUE)
    expect_error (vb (vec[idx1]),
                  "Value '100' used for input variable vec[idx1] must be boolean",
                  fixed=TRUE)


    myVar = NA
    expect_error (vb (myVar),
                  "Value 'NA' used for input variable myVar must be boolean",
                  fixed=TRUE)

    myVar = 3
    expect_error (vb (myVar),
                  "Value '3' used for input variable myVar must be boolean",
                  fixed=TRUE)

    myVar = "aString"
    expect_error (vb (myVar),
                  "Value 'aString' used for input variable myVar must be boolean",
                  fixed=TRUE)

})

#===============================================================================

test_that("valid_boolean_with_default: with and without NULL arg and/or explicit default", {
    expect_true (valid_boolean_with_default (TRUE))
    expect_false (valid_boolean_with_default (FALSE))

    expect_true (valid_boolean_with_default (TRUE, default=FALSE))
    expect_false (valid_boolean_with_default (FALSE, default=FALSE))

    expect_true (valid_boolean_with_default (TRUE, default=NA))
    expect_false (valid_boolean_with_default (FALSE, default="aString"))

    expect_false (valid_boolean_with_default (NULL))
    expect_false (valid_boolean_with_default (NULL, default=FALSE))
    expect_true (valid_boolean_with_default (NULL, default=TRUE))

    expect_error (valid_boolean_with_default (NULL, default=NA),
                  "Value 'NA' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean_with_default (NULL, default="aString"),
                  "Value 'aString' for variable (no variable name given) must be boolean",
                  fixed=TRUE)

    expect_false (valid_boolean_with_default (NULL, "myVar"))
    expect_false (valid_boolean_with_default (NULL, "myVar", default=FALSE))
    expect_true (valid_boolean_with_default (NULL, "myVar", default=TRUE))

    expect_false (valid_boolean_with_default (NULL, default=FALSE))
    expect_true (valid_boolean_with_default (NULL, default=TRUE))


    expect_error (valid_boolean_with_default (NA),
                  "Value 'NA' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean_with_default (3),
                  "Value '3' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean_with_default ("aString"),
                  "Value 'aString' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
})

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

test_that("valid_boolean: const value but no variable name given", {
    expect_true (valid_boolean (TRUE))
    expect_false (valid_boolean (FALSE))

    expect_error (valid_boolean (NULL),
                  "Value '' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean (NA),
                  "Value 'NA' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean (3),
                  "Value '3' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean ("aString"),
                  "Value 'aString' for variable (no variable name given) must be boolean",
                  fixed=TRUE)
})

#-------------------------------------------------------------------------------

test_that("valid_boolean: const value but variable name supplied", {
    expect_true (valid_boolean (TRUE, "myVar"))
    expect_false (valid_boolean (FALSE, "myVar"))

    expect_error (valid_boolean (NULL, "myVar"),
                  "Value '' for variable myVar must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean (NA, "myVar"),
                  "Value 'NA' for variable myVar must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean (3, "myVar"),
                  "Value '3' for variable myVar must be boolean",
                  fixed=TRUE)
    expect_error (valid_boolean ("aString", "myVar"),
                  "Value 'aString' for variable myVar must be boolean",
                  fixed=TRUE)
})

#-------------------------------------------------------------------------------

test_that("valid_boolean: value passed in variable but no variable name given", {
    myVar = TRUE
    expect_true (valid_boolean (myVar))

    myVar = FALSE
    expect_false (valid_boolean (myVar))

    myVar = NULL
    expect_error (valid_boolean (myVar),
                  "Value '' for variable (no variable name given) must be boolean",
                  fixed=TRUE)

    myVar = NA
    expect_error (valid_boolean (myVar),
                  "Value 'NA' for variable (no variable name given) must be boolean",
                  fixed=TRUE)

    myVar = 3
    expect_error (valid_boolean (myVar),
                  "Value '3' for variable (no variable name given) must be boolean",
                  fixed=TRUE)

    myVar = "aString"
    expect_error (valid_boolean (myVar),
                  "Value 'aString' for variable (no variable name given) must be boolean",
                  fixed=TRUE)

})

#-------------------------------------------------------------------------------

test_that("valid_boolean: value passed in variable and variable name supplied", {
    myVar = TRUE
    expect_true (valid_boolean (myVar, "myVar"))

    myVar = FALSE
    expect_false (valid_boolean (myVar, "myVar"))

    myVar = NULL
    expect_error (valid_boolean (myVar, "myVar"),
                  "Value '' for variable myVar must be boolean",
                  fixed=TRUE)

    myVar = NA
    expect_error (valid_boolean (myVar, "myVar"),
                  "Value 'NA' for variable myVar must be boolean",
                  fixed=TRUE)

    myVar = 3
    expect_error (valid_boolean (myVar, "myVar"),
                  "Value '3' for variable myVar must be boolean",
                  fixed=TRUE)
})

#===============================================================================

test_that("valid_numeric_in_range_with_default: with and without NULL or NA arg and using implicit default", {
    expect_equal (-1, valid_numeric_in_range_with_default (-1))
    expect_equal (-1, valid_numeric_in_range_with_default (-1, range_lo=-5, range_hi=1))

    expect_equal (0, valid_numeric_in_range_with_default (NULL))
    expect_equal (0, valid_numeric_in_range_with_default (NA))
    expect_equal (0, valid_numeric_in_range_with_default (NULL, range_lo = -5, range_hi=1))
    expect_equal (0, valid_numeric_in_range_with_default (NA, range_lo = -5, range_hi=1))

    expect_error (valid_numeric_in_range_with_default (NULL, range_lo = -5, range_hi= -1),
                  "Validating (no variable name given), var_value = '0' must be <= range_hi = '-1'",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range_with_default (NA, range_lo = -5, range_hi= -1),
                   "Validating (no variable name given), var_value = '0' must be <= range_hi = '-1'",
                  fixed=TRUE)

    #-----  Now, passing the variable name  -----#

    expect_equal (-1, valid_numeric_in_range_with_default (-1, "myVar"))
    expect_equal (-1, valid_numeric_in_range_with_default (-1, "myVar", range_lo=-5, range_hi=1))

    expect_equal (0, valid_numeric_in_range_with_default (NULL, "myVar"))
    expect_equal (0, valid_numeric_in_range_with_default (NA, "myVar"))
    expect_equal (0, valid_numeric_in_range_with_default (NULL, "myVar", range_lo = -5, range_hi=1))
    expect_equal (0, valid_numeric_in_range_with_default (NA, "myVar", range_lo = -5, range_hi=1))

    expect_error (valid_numeric_in_range_with_default (NULL, "myVar", range_lo = -5, range_hi= -1),
                  "Validating myVar, var_value = '0' must be <= range_hi = '-1'",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range_with_default (NA, "myVar", range_lo = -5, range_hi= -1),
                   "Validating myVar, var_value = '0' must be <= range_hi = '-1'",
                  fixed=TRUE)
})

#-------------------------------------------------------------------------------

test_that("valid_numeric_in_range_with_default: with and without NULL or NA arg and using explicit default", {
    expect_equal (-1, valid_numeric_in_range_with_default (-1, default=999))
    expect_equal (-1, valid_numeric_in_range_with_default (-1, range_lo=-5, range_hi=1, default=999))

    expect_equal (999, valid_numeric_in_range_with_default (NULL, default=999))
    expect_equal (999, valid_numeric_in_range_with_default (NA, default=999))
    expect_equal (999, valid_numeric_in_range_with_default (NULL, range_lo = -5, range_hi=1000, default=999))
    expect_equal (999, valid_numeric_in_range_with_default (NA, range_lo = -5, range_hi=1000, default=999))

    expect_error (valid_numeric_in_range_with_default (NULL, range_lo = -5, range_hi= -1, default=999),
                  "Validating (no variable name given), var_value = '999' must be <= range_hi = '-1'",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range_with_default (NA, range_lo = -5, range_hi= -1, default=999),
                   "Validating (no variable name given), var_value = '999' must be <= range_hi = '-1'",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range_with_default (NULL, range_lo = -5, range_hi= -1, default=NULL),
                  "Value '' for variable (no variable name given) must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range_with_default (NA, range_lo = -5, range_hi= -1, default=NA),
                   "Value 'NA' for variable (no variable name given) must be numeric",
                  fixed=TRUE)

    #-----  Now, passing the variable name  -----#

    expect_equal (-1, valid_numeric_in_range_with_default (-1, "myVar", default=999))
    expect_equal (-1, valid_numeric_in_range_with_default (-1, "myVar", range_lo=-5, range_hi=1, default=999))

    expect_equal (999, valid_numeric_in_range_with_default (NULL, "myVar", default=999))
    expect_equal (999, valid_numeric_in_range_with_default (NA, "myVar", default=999))
    expect_equal (999, valid_numeric_in_range_with_default (NULL, "myVar", range_lo = -5, range_hi=1000, default=999))
    expect_equal (999, valid_numeric_in_range_with_default (NA, "myVar", range_lo = -5, range_hi=1000, default=999))

    expect_error (valid_numeric_in_range_with_default (NULL, "myVar", range_lo = -5, range_hi= -1, default=999),
                  "Validating myVar, var_value = '999' must be <= range_hi = '-1'",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range_with_default (NA, "myVar", range_lo = -5, range_hi= -1, default=999),
                   "Validating myVar, var_value = '999' must be <= range_hi = '-1'",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range_with_default (NULL, "myVar", range_lo = -5, range_hi= -1, default=NULL),
                  "Value '' for variable myVar must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range_with_default (NA, "myVar", range_lo = -5, range_hi= -1, default=NA),
                   "Value 'NA' for variable myVar must be numeric",
                  fixed=TRUE)
})

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

test_that("valid_numeric_in_range: const value but no variable name given", {
    expect_equal (-1, valid_numeric_in_range (-1))
    expect_equal (-1, valid_numeric_in_range (-1, range_lo=-5, range_hi=1))

    expect_error (valid_numeric_in_range (NULL),
                  "Value '' for variable (no variable name given) must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (NA),
                  "Value 'NA' for variable (no variable name given) must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range ("aString"),
                  "Value 'aString' for variable (no variable name given) must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, range_lo=NULL),
                  "Validating (no variable name given), range_lo = '' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (777, range_hi=NULL),
                  "Validating (no variable name given), range_hi = '' must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, range_lo=NA),
                  "Validating (no variable name given), range_lo = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (777, range_hi=NA),
                  "Validating (no variable name given), range_hi = 'NA' must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, range_lo=55),
                  "Validating (no variable name given), var_value = '-1' must be >= range_lo = '55'",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (777, range_hi=-2.8),
                  "Validating (no variable name given), var_value = '777' must be <= range_hi = '-2.8'",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, range_lo=NULL, range_hi=100),
                  "Validating (no variable name given), range_lo = '' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, range_lo=-5, range_hi=NULL),
                  "Validating (no variable name given), range_hi = '' must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, range_lo=NA, range_hi=100),
                  "Validating (no variable name given), range_lo = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, range_lo=-5, range_hi=NA),
                  "Validating (no variable name given), range_hi = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, range_lo=NA, range_hi=NULL),
                  "Validating (no variable name given), range_lo = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, range_lo="bottom", range_hi=100),
                  "Validating (no variable name given), range_lo = 'bottom' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, range_lo=0, range_hi="aString"),
                  "Validating (no variable name given), range_hi = 'aString' must be numeric",
                  fixed=TRUE)

})

#-------------------------------------------------------------------------------

test_that("valid_numeric_in_range: const value but variable name supplied", {
    expect_equal (-1, valid_numeric_in_range (-1, "myVar"))
    expect_equal (-1, valid_numeric_in_range (-1, "myVar", range_lo=-5, range_hi=1))

    expect_error (valid_numeric_in_range (NULL, "myVar"),
                  "Value '' for variable myVar must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (NA, "myVar"),
                  "Value 'NA' for variable myVar must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range ("aString", "myVar"),
                  "Value 'aString' for variable myVar must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=NULL),
                  "Validating myVar, range_lo = '' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (777, "myVar", range_hi=NULL),
                  "Validating myVar, range_hi = '' must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=NA),
                  "Validating myVar, range_lo = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (777, "myVar", range_hi=NA),
                  "Validating myVar, range_hi = 'NA' must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=55),
                  "Validating myVar, var_value = '-1' must be >= range_lo = '55'",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (777, "myVar", range_hi=-2.8),
                  "Validating myVar, var_value = '777' must be <= range_hi = '-2.8'",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=NULL, range_hi=100),
                  "Validating myVar, range_lo = '' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=-5, range_hi=NULL),
                  "Validating myVar, range_hi = '' must be numeric",
                  fixed=TRUE)

    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=NA, range_hi=100),
                  "Validating myVar, range_lo = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=-5, range_hi=NA),
                  "Validating myVar, range_hi = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=NA, range_hi=NULL),
                  "Validating myVar, range_lo = 'NA' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, "myVar", range_lo="bottom", range_hi=100),
                  "Validating myVar, range_lo = 'bottom' must be numeric",
                  fixed=TRUE)
    expect_error (valid_numeric_in_range (-1, "myVar", range_lo=0, range_hi="aString"),
                  "Validating myVar, range_hi = 'aString' must be numeric",
                  fixed=TRUE)

})

#===============================================================================

