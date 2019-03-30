#===============================================================================

                    #  test_gscp_14b_compute_solution_scores.R

#===============================================================================

library (bdpg)

context ("compute_solution_scores")

#===============================================================================

    #  bpm is spp rows by PU cols

num_spp = 6
num_PUs = 5

bpm = matrix (c (1,1,1,1,1,
                 1,1,1,1,0,
                 1,1,1,0,0,
                 1,1,0,0,0,
                 1,0,0,0,0,
                 0,0,0,0,1), nrow=num_spp, ncol=num_PUs, byrow=TRUE)

spp_rep_targets = c (1,2,3,4,5,1)
cand_sol_PU_IDs = c (3,4,5)

    # reduced bpm : PU cols 3,4,5 : num occs in 3,4,5 : (target for each spp)
    # 1 1 1 -> 3  (1)
    # 1 1 0 -> 2  (2)
    # 1 0 0 -> 1  (3)
    # 0 0 0 -> 0  (4)
    # 0 0 0 -> 0  (5)
    # 0 0 1 -> 1  (1)
    #
    #
    # cand sol pu ids
    # 3 4 5
    #
    # spp rep targets
    # 1 2 3 4 5 1
    #
    # spp reps in sol
    # 3 2 1 0 0 1
    #
    # spp covered
    # 1 2 6
    #
    # num spp covered
    # 3
    #
    # frac_spp_covered
    # 3/6 = 0.5
    #
    # spp_rep_shortfall
    # 1-0.5 - 0.5


#===============================================================================

test_that ("compute_num_spp_covered_by_solution works", {

num_spp_covered =
    compute_num_spp_covered_by_solution (cand_sol_PU_IDs,
                                         bpm,
                                         spp_rep_targets)


expect_equal (3, num_spp_covered)
})

#===============================================================================

test_that ("compute_and_verify_rep_scores_wrt works", {

results_list = compute_and_verify_rep_scores_wrt (bpm,
                                                  cand_sol_PU_IDs,
                                                  spp_rep_targets,
                                                  num_spp)

expect_equal (0.5, results_list$spp_rep_shortfall)
expect_equal (3, results_list$num_spp_covered)
expect_equal (0.5, results_list$frac_spp_covered)
})

#===============================================================================

test_that ("compute_euc_out_err_frac works", {

results_list = compute_euc_out_err_frac (cor_or_app_str = "COR",
                                         solution_cost_err_frac = 0.3,
                                         frac_spp_covered = 0.6)
expect_equal (0.5, results_list$rsr_COR_euc_out_err_frac)


#----------

results_list = compute_euc_out_err_frac (cor_or_app_str = "APP",
                                         solution_cost_err_frac = 0.4,
                                         frac_spp_covered = 0.7)
expect_equal (0.5, results_list$rsr_APP_euc_out_err_frac)

#----------

# if (exists ("bdpg.emulating_tzar"))
#     temp____bdpg.emulating_tzar = bdpg.emulating_tzar
expect_error (compute_euc_out_err_frac (cor_or_app_str = "junk",
                                         solution_cost_err_frac = 0.4,
                                         frac_spp_covered = 0.7),
                "cor_or_app_str = 'junk'.  Must be 'COR' or 'APP'",
                fixed=TRUE)
})

#===============================================================================

    # pretend the correct solution is PU 2,3
    # then,
cor_optimum_cost = 7     #  4+3
PU_costs_vec = c (5,4,3,2,1)
    # sol_cost = 5+3+2 = 10
    # tot_landscape_cost = 5+4+3+2+1 = 15

#-------------------------------------------------------------------------------

test_that ("compute_solution_cost works", {

rs_solution_PU_IDs_vec = c (1,3,4)

expect_equal (10, compute_solution_cost (rs_solution_PU_IDs_vec, PU_costs_vec))
})

#-------------------------------------------------------------------------------

test_that ("compute_RS_solution_cost_scores_wrt_COR_costs_vec OVER opt cost works", {

rs_solution_PU_IDs_vec = c (1,3,4)

results_list =
    compute_RS_solution_cost_scores_wrt_COR_costs_vec (rs_solution_PU_IDs_vec,
                                                       cor_optimum_cost,
                                                       PU_costs_vec)
expect_equal (cor_optimum_cost, results_list$cor_optimum_cost)
expect_equal (10, results_list$rs_solution_cost)

#  (10-7)/7 = 3/7 = 0.4285714
# expect_equal (0.4285714, results_list$rs_solution_cost_err_frac)
# expect_equal (0.4285714, results_list$abs_rs_solution_cost_err_frac)
expect_equal (3/7, results_list$rs_solution_cost_err_frac)
expect_equal (3/7, results_list$abs_rs_solution_cost_err_frac)

# (10-7)/(15-7) = 3/8 = 0.375
expect_equal (0.375, results_list$rs_over_opt_cost_err_frac_of_possible_overcost)
expect_true (is.na (results_list$rs_under_opt_cost_err_frac_of_possible_undercost))

})

#-------------------------------------------------------------------------------

test_that ("compute_RS_solution_cost_scores_wrt_COR_costs_vec UNDER opt cost works", {

rs_solution_PU_IDs_vec = c (4,5)

results_list =
    compute_RS_solution_cost_scores_wrt_COR_costs_vec (rs_solution_PU_IDs_vec,
                                                       cor_optimum_cost,
                                                       PU_costs_vec)
expect_equal (cor_optimum_cost, results_list$cor_optimum_cost)
expect_equal (3, results_list$rs_solution_cost)

    #  NOTE that I had to use the original fractions rather than the
    #  decimal value that results from them to get the expect_equal()
    #  calls to think these things are equal.
#  (3-7)/7 = -4/7
expect_equal (-4/7, results_list$rs_solution_cost_err_frac)
expect_equal (4/7, results_list$abs_rs_solution_cost_err_frac)

# -(3-7)/(7-0) = 4/7
expect_true (is.na (results_list$rs_over_opt_cost_err_frac_of_possible_overcost))
expect_equal (4/7, results_list$rs_under_opt_cost_err_frac_of_possible_undercost)

})

#-------------------------------------------------------------------------------

test_that ("compute_RS_solution_cost_scores_wrt_COR_costs_vec EQUAL opt cost works", {

rs_solution_PU_IDs_vec = c (2,3)

results_list =
    compute_RS_solution_cost_scores_wrt_COR_costs_vec (rs_solution_PU_IDs_vec,
                                                       cor_optimum_cost,
                                                       PU_costs_vec)
expect_equal (cor_optimum_cost, results_list$cor_optimum_cost)
expect_equal (7, results_list$rs_solution_cost)

expect_equal (0, results_list$rs_solution_cost_err_frac)
expect_equal (0, results_list$abs_rs_solution_cost_err_frac)

expect_true (is.na (results_list$rs_over_opt_cost_err_frac_of_possible_overcost))
expect_true (is.na (results_list$rs_under_opt_cost_err_frac_of_possible_undercost))

})

#===============================================================================

