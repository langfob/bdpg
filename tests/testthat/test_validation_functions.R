#===============================================================================

                        #  test_validation_functions.R

#===============================================================================

library (bdpg)

context ("validation_functions")

#===============================================================================

#  There are 2^5 possible combinations of valid options for vb (ignoring
#  the contents of var_value itself), so there are 32 cases for each different
#  kind of var_value.  Each is broken out in a separate test_that clause
#  below.

#-------------------------------------------------------------------------------

test_that("vb: var_value is TRUE", {

myVar = TRUE

#  00000
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  00001
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00010
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  00011
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  00100
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  00101
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00110
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  00111
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01000
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  01001
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01010
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  01011
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01100
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  01101
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01110
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  01111
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#----------------------------------------

#  10000
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  10001
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10010
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  10011
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  10100
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  10101
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10110
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  10111
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11000
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  11001
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11010
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  11011
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11100
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  11101
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11110
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  11111
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))
})

#-------------------------------------------------------------------------------

test_that("vb: var_value is FALSE", {

myVar = FALSE

#  00000
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  00001
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00010
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  00011
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  00100
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  00101
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00110
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  00111
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01000
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  01001
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01010
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  01011
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01100
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  01101
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01110
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  01111
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#----------------------------------------

#  10000
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  10001
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10010
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  10011
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  10100
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  10101
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10110
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  10111
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11000
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  11001
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11010
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  11011
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11100
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  11101
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11110
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  11111
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))
})

#-------------------------------------------------------------------------------

test_that("vb: var_value is NULL", {

myVar = NULL

#  00000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00001
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00011
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00101
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00111
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01001
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01011
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01101
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01111
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#----------------------------------------

#  10000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10001
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10011
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10100
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  10101
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10110
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  10111
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11001
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11011
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value '' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11100
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE))

#  11101
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11110
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  11111
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))
})

#-------------------------------------------------------------------------------

test_that("vb: var_value is NA", {

myVar = NA

#  00000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00001
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00011
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00101
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00111
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01001
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01011
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01101
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01111
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#----------------------------------------

#  10000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10001
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10010
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  10011
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  10100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10101
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10110
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  10111
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11001
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11010
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  11011
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11101
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'NA' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11110
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE))

#  11111
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))
})

#-------------------------------------------------------------------------------

test_that("vb: var_value is numeric other than 0 or 1", {

myVar = -17

#  00000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00001
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00011
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  00100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00101
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00111
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01001
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01011
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01101
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01111
expect_true (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#----------------------------------------

#  10000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10001
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10011
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  10100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10101
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10110
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10111
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11001
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11011
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11101
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11110
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '-17' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11111
expect_true (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))
})

#-------------------------------------------------------------------------------

test_that("vb: var_value is numeric 0", {

myVar = 0

#  00000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00001
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00011
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  00100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00101
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  00110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00111
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01001
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01011
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  01100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01101
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  01110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01111
expect_false (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#----------------------------------------

#  10000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10001
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10011
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  10100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10101
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  10110
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10111
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11001
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11011
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))

#  11100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11101
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE))

#  11110
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Numeric value '0' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11111
expect_false (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE))
})

#-------------------------------------------------------------------------------

test_that("vb: var_value is string", {

myVar = "someString"

#  00000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00001
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00011
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00101
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  00111
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01000
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01001
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01010
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01011
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01100
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01101
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01110
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  01111
expect_error (vb (myVar,
    def_on_empty = FALSE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#----------------------------------------

#  10000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10001
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10011
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10101
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10110
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  10111
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = FALSE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11000
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11001
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11010
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11011
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = FALSE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11100
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11101
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = FALSE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11110
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = FALSE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)

#  11111
expect_error (vb (myVar,
    def_on_empty = TRUE,
    def = TRUE,
    treat_NULL_as_empty = TRUE,
    treat_NA_as_empty = TRUE,
    allow_num = TRUE),
    "Value 'someString' used for input variable myVar must be boolean",
    fixed=TRUE)
})

#-------------------------------------------------------------------------------

test_that("vb: input value is a constant", {

        #  TRUE always returns TRUE even if default is bad
    expect_true (vb (TRUE))
    expect_true (vb (TRUE, def_on_empty=TRUE, def=FALSE))
    expect_true (vb (TRUE, def_on_empty=TRUE, def=NA))

        #  FALSE always returns FALSE even if default is bad
    expect_false (vb (FALSE))
    expect_false (vb (FALSE, def_on_empty=TRUE, def=FALSE))
    expect_false (vb (FALSE, def_on_empty=TRUE, def="aString"))

        #  NULL returns default when default is good
    expect_true  (vb (NULL, def_on_empty=TRUE, def=TRUE))
    expect_false (vb (NULL, def_on_empty=TRUE, def=FALSE))

        #  bad value with "def_on_empty=TRUE" but no default, always returns FALSE
    expect_false (vb (NULL, def_on_empty=TRUE))

        #  bad value with no default correction always throws error
    expect_error (vb (NULL),
                  "Value '' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NA),
                  "Value 'NA' used for input variable NA must be boolean",
                  fixed=TRUE)
    expect_error (vb (3),
                  "Numeric value '3' used for input variable 3 must be boolean",
                  fixed=TRUE)
    expect_error (vb ("aString"),
                  "Value 'aString' used for input variable \"aString\" must be boolean",
                  fixed=TRUE)

    expect_error (vb (NA, def_on_empty=TRUE),
                  "Value 'NA' used for input variable NA must be boolean",
                  fixed=TRUE)
    expect_error (vb (3, def_on_empty=TRUE),
                  "Numeric value '3' used for input variable 3 must be boolean",
                  fixed=TRUE)
    expect_error (vb ("aString", def_on_empty=TRUE),
                  "Value 'aString' used for input variable \"aString\" must be boolean",
                  fixed=TRUE)



        #  bad value with bad default correction throws error
    expect_error (vb (NULL, def_on_empty=TRUE, def=NULL),
                  "Default value '' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, def_on_empty=TRUE, def=NA),
                  "Default value 'NA' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, def_on_empty=TRUE, def="aString"),
                  "Default value 'aString' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, def_on_empty=TRUE, def=27),
                  "Default value '27' used for input variable NULL must be boolean",
                  fixed=TRUE)
    expect_error (vb (NULL, def_on_empty=TRUE, def=0),
                  "Default value '0' used for input variable NULL must be boolean",
                  fixed=TRUE)
})

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

test_that("vb: input value not a constant", {

        #  TRUE always returns TRUE even if default is bad
    myVar = TRUE
    expect_true (vb (myVar))
    expect_true (vb (myVar, def_on_empty=TRUE, def=FALSE))
    expect_true (vb (myVar, def_on_empty=TRUE, def=NA))

        #  FALSE always returns FALSE even if default is bad
    myVar = FALSE
    expect_false (vb (myVar))
    expect_false (vb (myVar, def_on_empty=TRUE, def=FALSE))
    expect_false (vb (myVar, def_on_empty=TRUE, def="aString"))

        #  bad value with "def_on_empty=TRUE" but no default, always returns FALSE
    myVar = NULL
    expect_false (vb (myVar, def_on_empty=TRUE))


    myVar = NA
    expect_error (vb (myVar, def_on_empty=TRUE),
                  "Value 'NA' used for input variable myVar must be boolean",
                  fixed=TRUE)
    myVar = 3
    expect_error (vb (myVar, def_on_empty=TRUE),
                  "Numeric value '3' used for input variable myVar must be boolean",
                  fixed=TRUE)
    myVar = "aString"
    expect_error (vb (myVar, def_on_empty=TRUE),
                  "Value 'aString' used for input variable myVar must be boolean",
                  fixed=TRUE)

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
                  "Numeric value '300' used for input variable vec[3] must be boolean",
                  fixed=TRUE)
    expect_error (vb (vec[idx1]),
                  "Numeric value '100' used for input variable vec[idx1] must be boolean",
                  fixed=TRUE)


    myVar = NA
    expect_error (vb (myVar),
                  "Value 'NA' used for input variable myVar must be boolean",
                  fixed=TRUE)

    myVar = 3
    expect_error (vb (myVar),
                  "Numeric value '3' used for input variable myVar must be boolean",
                  fixed=TRUE)

    myVar = "aString"
    expect_error (vb (myVar),
                  "Value 'aString' used for input variable myVar must be boolean",
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

