context("placeholder test until you create real tests")

test_that("spurious test of equality to make CHECK happy", {
expect_equal(TRUE,TRUE)
})

test_that("error happens", {
    expect_error (stop("a msg at stop time"))
})


