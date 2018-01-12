#===============================================================================

                            #  test_vn.R

#===============================================================================

library (bdpg)

context ("validation_functions-vn")

#===============================================================================

#  There are 2^3 possible combinations of valid boolean options for vn (ignoring
#  the contents of var_value, range, and bounds_types), so there are 8 cases
#  for each different kind of var_value.  Each is broken out in a separate
#  test_that clause below.

#===============================================================================

test_that("vn: var_value is numeric and in range w/good bounds types - vn should ignore all flags", {

myVar = 0.5

#  000
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE))


#  001
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE))


#  010
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE))


#  011
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))


#  100
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = 666,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE))


#  101
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = 666,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE))


#  110
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = 666,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE))


#  111
expect_equal (myVar, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = 666,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is numeric BUT <= exclusive range_lo w/good bounds types - vn should ignore all flags", {

myVar = -100

#  000
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)


#  001
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)


#  010
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)


#  011
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)


#  100
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)


#  101
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)


#  110
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)


#  111
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '-100' used for input variable myVar, must be >= range_lo = '0'",
    fixed=TRUE)

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is numeric and == inclusive range_lo w/good bounds types - vn should ignore all flags", {

myVar = 10

#  000
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE))


#  001
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE))


#  010
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE))


#  011
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))


#  100
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE))


#  101
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE))


#  110
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE))


#  111
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is numeric BUT == exclusive range_lo w/good bounds types - vn should ignore all flags", {

myVar = 10

#  000
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)


#  001
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)


#  010
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)


#  011
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)


#  100
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)


#  101
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)


#  110
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)


#  111
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ei",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '10' used for input variable myVar, must be > range_lo = '10'",
    fixed=TRUE)

})

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

test_that("vn: var_value is numeric BUT >= exclusive range_hi w/good bounds types - vn should ignore all flags", {

myVar = 50

#  000
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)


#  001
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)


#  010
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)


#  011
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)


#  100
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)


#  101
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)


#  110
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)


#  111
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '50' used for input variable myVar, must be <= range_hi = '1'",
    fixed=TRUE)

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is numeric and == inclusive range_hi w/good bounds types - vn should ignore all flags", {

myVar = 20

#  000
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE))


#  001
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE))


#  010
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE))


#  011
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))


#  100
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE))


#  101
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE))


#  110
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE))


#  111
expect_equal (myVar, vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ii",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is numeric BUT == exclusive range_hi w/good bounds types - vn should ignore all flags", {

myVar = 20

#  000
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)


#  001
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)


#  010
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)


#  011
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = FALSE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)


#  100
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)


#  101
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)


#  110
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)


#  111
expect_error (vn (myVar, range_lo = 10, range_hi = 20, bounds_types = "ee",
    def_on_empty = TRUE,
    def = -57,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '20' used for input variable myVar, must be < range_hi = '20'",
    fixed=TRUE)

})

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

test_that("vn: var_value is NULL", {

myVar = NULL

#  000
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  001
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  010
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating '' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  011
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating '' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  100
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating '' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  101
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating '' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  110
expect_equal (0.3, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE))


#  111
expect_equal (0.3, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is NA", {

myVar = NA

#  000
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating 'NA' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  001
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Validating 'NA' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  010
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating 'NA' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  011
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = FALSE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Validating 'NA' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  100
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE),
    "Validating 'NA' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  101
expect_equal (0.3, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE))


#  110
expect_error (vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Validating 'NA' used for input variable myVar, must be numeric",
    fixed=TRUE)


#  111
expect_equal (0.3, vn (myVar, range_lo = 0, range_hi = 1, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE))

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is NULL with default BUT default is bad", {

myVar = NULL

#  101
expect_error (vn (myVar, range_lo = 0.5, range_hi = 10, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 10,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE),
    "Default value '10' used for input variable myVar, must be < range_hi = '10'",
    fixed=TRUE)


#  111
expect_error (vn (myVar, range_lo = 0.5, range_hi = 10, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.5,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Default value '0.5' used for input variable myVar, must be > range_lo = '0.5'",
    fixed=TRUE)

})

#-------------------------------------------------------------------------------

test_that("vn: var_value is NA with default BUT default is bad", {

myVar = NA

#  101
expect_error (vn (myVar, range_lo = 0.5, range_hi = 10, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 0.3,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE),
    "Default value '0.3' used for input variable myVar, must be > range_lo = '0.5'",
    fixed=TRUE)


#  111
expect_error (vn (myVar, range_lo = 0.5, range_hi = 10, bounds_types = "ee",
    def_on_empty = TRUE,
    def = 300,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE),
    "Default value '300' used for input variable myVar, must be < range_hi = '10'",
    fixed=TRUE)

})

#===============================================================================

