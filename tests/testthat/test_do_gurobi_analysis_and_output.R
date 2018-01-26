#===============================================================================

                    #  test_do_gurobi_analysis_and_output.R

#===============================================================================

library (bdpg)

context ("do_gurobi_analysis_and_output")

#===============================================================================

gen_dummy_bpm <- function (num_spp, num_PUs)
    {
    bpm = matrix (0, nrow=num_spp, ncol=num_PUs)

    cat ("\ndim(bpm) = ", dim(bpm))
    for (cur_spp in 1:num_spp)
        {
        occ_PUs_for_this_spp = sample (1:num_PUs, 2, replace=FALSE)

        bpm [cur_spp, occ_PUs_for_this_spp] = 1
        }

    return (bpm)
    }

#===============================================================================

test_gurobi <- function (use_gap_limit,
                         gap_limit,

                         use_given_time_as_limit,
                         time_limit,

                         use_marxan_time_as_limit,
                         marxan_elapsed_time,

                         seed = 456,
                         num_spp = 10,
                         num_PUs = 5,

                        PU_costs        = rep (1, num_PUs),
                        spp_rep_targets = rep (1, num_spp)
                         )
    {
    set.seed (seed)

    # PU_costs        = rep (1, num_PUs)
    # spp_rep_targets = rep (1, num_spp)

    bpm_with_spp_rows_PU_cols = gen_dummy_bpm (num_spp, num_PUs)

    result = run_gurobi (num_spp,
                         num_PUs,
                         bpm_with_spp_rows_PU_cols,
                         PU_costs,
                         spp_rep_targets,

                         use_gap_limit,
                         gap_limit,

                         use_given_time_as_limit,
                         time_limit,

                         use_marxan_time_as_limit,
                         marxan_elapsed_time)

                        # rsrun = NULL,
                        # topdir = NULL,            #= parameters$fullOutputDir_NO_slash
                        # save_inputs = FALSE,
                        # save_outputs = FALSE
                        # )

    return (result)
    }

#===============================================================================

correct_solution_vector = c(2,5)    # raw gurobi output form - c(0,1,0,0,1)
correct_status = "OPTIMAL"
correct_objval = 2

test_that("run_gurobi: GAP ONLY", {

    result = test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA)
    expect_false (is.null (result))

        #  Solution vector
    expect_equal (correct_solution_vector, result$gurobi_solution_vector)

        #  Output scalars that are reproducible
    expect_equal (correct_status, result$gurobi_status)
    expect_equal (correct_objval, result$gurobi_objval)

        #  Input scalars
    expect_equal (10,    result$gurobi_num_spp)
    expect_equal (5,     result$gurobi_num_PUs)

    expect_equal (TRUE,  result$gurobi_use_gap_limit)
    expect_equal (0.005, result$gurobi_gap_limit_input)
    expect_equal (0.005, result$gurobi_gap_limit_used)

    expect_equal (FALSE, result$gurobi_use_given_time_as_limit)
    expect_equal (60,    result$gurobi_time_limit_input)

    expect_equal (FALSE, result$gurobi_use_marxan_time_as_limit)
    expect_equal (NA,    result$gurobi_marxan_elapsed_time_input)

    expect_equal (NULL,  result$gurobi_time_limit_used)
    })

test_that("run_gurobi: TIME ONLY", {

    result = test_gurobi (
                          use_gap_limit = FALSE, gap_limit = 0.005,
                          use_given_time_as_limit = TRUE, time_limit = 60,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA)
    expect_false (is.null (result))

        #  Solution vector
    expect_equal (correct_solution_vector, result$gurobi_solution_vector)

        #  Output scalars that are reproducible
    expect_equal (correct_status, result$gurobi_status)
    expect_equal (correct_objval, result$gurobi_objval)

        #  Input scalars
    expect_equal (10,    result$gurobi_num_spp)
    expect_equal (5,     result$gurobi_num_PUs)

    expect_equal (FALSE, result$gurobi_use_gap_limit)
    expect_equal (0.005, result$gurobi_gap_limit_input)
    expect_equal (NULL,  result$gurobi_gap_limit_used)

    expect_equal (TRUE,  result$gurobi_use_given_time_as_limit)
    expect_equal (60,    result$gurobi_time_limit_input)

    expect_equal (FALSE, result$gurobi_use_marxan_time_as_limit)
    expect_equal (NA,    result$gurobi_marxan_elapsed_time_input)

    expect_equal (60,    result$gurobi_time_limit_used)
    })

test_that("run_gurobi: GAP+TIME", {

    result = test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = TRUE, time_limit = 60,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA)
    expect_false (is.null (result))

        #  Solution vector
    expect_equal (correct_solution_vector, result$gurobi_solution_vector)

        #  Output scalars that are reproducible
    expect_equal (correct_status, result$gurobi_status)
    expect_equal (correct_objval, result$gurobi_objval)

        #  Input scalars
    expect_equal (10,    result$gurobi_num_spp)
    expect_equal (5,     result$gurobi_num_PUs)

    expect_equal (TRUE,  result$gurobi_use_gap_limit)
    expect_equal (0.005, result$gurobi_gap_limit_input)
    expect_equal (0.005, result$gurobi_gap_limit_used)

    expect_equal (TRUE,  result$gurobi_use_given_time_as_limit)
    expect_equal (60,    result$gurobi_time_limit_input)

    expect_equal (FALSE, result$gurobi_use_marxan_time_as_limit)
    expect_equal (NA,    result$gurobi_marxan_elapsed_time_input)

    expect_equal (60,    result$gurobi_time_limit_used)
    })

test_that("run_gurobi: MARXAN TIME ONLY", {

    result = test_gurobi (
                          use_gap_limit = FALSE, gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = TRUE,
                          marxan_elapsed_time = 21.03)
    expect_false (is.null (result))

        #  Solution vector
    expect_equal (correct_solution_vector, result$gurobi_solution_vector)

        #  Output scalars that are reproducible
    expect_equal (correct_status, result$gurobi_status)
    expect_equal (correct_objval, result$gurobi_objval)

        #  Input scalars
    expect_equal (10,    result$gurobi_num_spp)
    expect_equal (5,     result$gurobi_num_PUs)

    expect_equal (FALSE, result$gurobi_use_gap_limit)
    expect_equal (0.005, result$gurobi_gap_limit_input)
    expect_equal (NULL,  result$gurobi_gap_limit_used)

    expect_equal (FALSE, result$gurobi_use_given_time_as_limit)
    expect_equal (60,    result$gurobi_time_limit_input)

    expect_equal (TRUE,  result$gurobi_use_marxan_time_as_limit)
    expect_equal (21.03, result$gurobi_marxan_elapsed_time_input)

    expect_equal (22,    result$gurobi_time_limit_used)
    })

test_that("run_gurobi: GAP+MARXAN", {

    result = test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = TRUE,
                          marxan_elapsed_time = 21.03)
    expect_false (is.null (result))

        #  Solution vector
    expect_equal (correct_solution_vector, result$gurobi_solution_vector)

        #  Output scalars that are reproducible
    expect_equal (correct_status, result$gurobi_status)
    expect_equal (correct_objval, result$gurobi_objval)

        #  Input scalars
    expect_equal (10,    result$gurobi_num_spp)
    expect_equal (5,     result$gurobi_num_PUs)

    expect_equal (TRUE,  result$gurobi_use_gap_limit)
    expect_equal (0.005, result$gurobi_gap_limit_input)
    expect_equal (0.005, result$gurobi_gap_limit_used)

    expect_equal (FALSE, result$gurobi_use_given_time_as_limit)
    expect_equal (60,    result$gurobi_time_limit_input)

    expect_equal (TRUE,  result$gurobi_use_marxan_time_as_limit)
    expect_equal (21.03, result$gurobi_marxan_elapsed_time_input)

    expect_equal (22,    result$gurobi_time_limit_used)
    })

#-------------------------------------------------------------------------------

test_that("run_gurobi: ERROR - TIME+MARXAN", {

    expect_error (test_gurobi (
                          use_gap_limit = FALSE, gap_limit = 0.005,
                          use_given_time_as_limit = TRUE, time_limit = 60,
                          use_marxan_time_as_limit = TRUE,
                          marxan_elapsed_time = 21.03),
                  "Can't set both use_given_time_as_limit and use_marxan_time_as_limit to TRUE.",
                  fixed=TRUE)
    })

#-----------------------------------

test_that("run_gurobi: BAD COMBINATION GAP+TIME+MARXAN", {

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = TRUE, time_limit = 60,
                          use_marxan_time_as_limit = TRUE,
                          marxan_elapsed_time = 21.03),
                  "Can't set both use_given_time_as_limit and use_marxan_time_as_limit to TRUE.",
                  fixed=TRUE)
    })


#-----------------------------------

test_that("run_gurobi: BAD GAP", {

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = "blah",
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA),
                  "Validating 'blah' used for input variable gap_limit, must be numeric",
                  fixed=TRUE)

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = -1,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA),
                  "Validating '-1' used for input variable gap_limit, must be >= range_lo = '0'",
                  fixed=TRUE)
    })

#-----------------------------------

test_that("run_gurobi: BAD TIME LIMIT", {

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = TRUE, time_limit = "duh",
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA),
                  "Validating 'duh' used for input variable time_limit, must be numeric",
                  fixed=TRUE)

    expect_error (test_gurobi (
                          use_gap_limit = FALSE, gap_limit = 0.005,
                          use_given_time_as_limit = TRUE, time_limit = -2.8,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA),
                  "Validating '-2.8' used for input variable time_limit, must be > range_lo = '0'",
                  fixed=TRUE)
    })

#-----------------------------------

test_that("run_gurobi: BAD MARXAN ELAPSED TIME", {

    expect_error (test_gurobi (
                          use_gap_limit = FALSE, gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = TRUE,
                          marxan_elapsed_time = "ack"),
                  "Validating 'ack' used for input variable marxan_elapsed_time, must be numeric",
                  fixed=TRUE)

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = TRUE,
                          marxan_elapsed_time = -2.8),
                  "Validating '-2.8' used for input variable marxan_elapsed_time, must be > range_lo = '0'",
                  fixed=TRUE)

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = TRUE,
                          marxan_elapsed_time = NA),
                  "Validating 'NA' used for input variable marxan_elapsed_time, must be numeric",
                  fixed=TRUE)
    })

#-----------------------------------

test_that("run_gurobi: BAD BOOLEAN INPUTS", {

    expect_error (test_gurobi (
                          use_gap_limit = "T", gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA),
                  "Value 'T' used for input variable use_gap_limit must be boolean",
                  fixed=TRUE)

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = "F", time_limit = 60,
                          use_marxan_time_as_limit = FALSE,
                          marxan_elapsed_time = NA),
                  "Value 'F' used for input variable use_given_time_as_limit must be boolean",
                  fixed=TRUE)

    expect_error (test_gurobi (
                          use_gap_limit = TRUE, gap_limit = 0.005,
                          use_given_time_as_limit = FALSE, time_limit = 60,
                          use_marxan_time_as_limit = 1000,
                          marxan_elapsed_time = NA),
                  "Numeric value '1000' used for input variable use_marxan_time_as_limit must be boolean",
                  fixed=TRUE)
    })


#===============================================================================

