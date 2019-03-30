#===============================================================================

#                           test_gen_single_bdprob_app.R

#===============================================================================

library (bdpg)

context ("gen_single_bdprob_app")

#===============================================================================
#===============================================================================
#                               APP tests
#===============================================================================
#===============================================================================

#-----------------------------------
#  APP base
#-----------------------------------

test_that("gen_single_bdprob: test_gen_single_bdprob_APP_BASE", {

    parameters = init_test_parameters_1_cor_base ()

    # Xu_bdprob_cor_base = test_gen_single_bdprob_COR (parameters)
    Xu_bdprob_COR_BASE =
        gen_single_bdprob_COR (parameters,
                               starting_dir          = parameters$starting_dir,  #"~/Downloads",
                               parameters$integerize,
                               base_prob_name_stem   = "base_prob",
                               cor_dir_name_stem     = "cor"
                               )

    #--------------------

    parameters$gen_cost_errors = FALSE
    parameters$gen_FP_FN_errors = TRUE

    parameters$err_amt = 0.10
    parameters$spp_occ_FP_error_type = "CONSTANT"
    parameters$spp_occ_FP_const_rate = 0.1  #0.1

    parameters$spp_occ_FN_error_type = "CONSTANT"
    parameters$spp_occ_FN_const_rate = 0.1  #0.1

    parameters$match_error_counts = FALSE
    parameters$compound_err_name  = "05-FP_and_FN_not_matched_NO_cost_err"

    #----------

    Xu_bdprob_APP_BASE = gen_single_bdprob_APP (Xu_bdprob_COR_BASE,
                                                parameters,
                                                parameters$starting_dir,
                                                parameters$gen_cost_errors,
                                                parameters$gen_FP_FN_errors,
                                                compound_err_name = parameters$compound_err_name,
                                                ret_vals_from_build_const_err = NULL,
                                                ret_vals_from_apply_cost_errors = NULL)

    #--------------------

    testsFor_Xu_bdprob_APP_BASE_1 (Xu_bdprob_APP_BASE)
    })


#===============================================================================
#===============================================================================

#-----------------------------------
#  APP wrap
#-----------------------------------

# test_that("gen_single_bdprob: test_gen_single_bdprob_APP_WRAP", {
#
#     parameters = init_test_parameters_1_cor_base ()
#
#     Xu_bdprob_cor_base = test_gen_single_bdprob_COR (parameters)
#
#     Xu_bdprob_cor_wrap = gen_single_bdprob_WRAP (Xu_bdprob_cor_base,
#                                                  parameters,
#                                                  parameters$starting_dir)
#
#     Xu_bdprob_app_wrap = gen_single_bdprob_APP (Xu_bdprob_cor_wrap,
#                                                 parameters,
#                                                 parameters$starting_dir,
#                                                 gen_cost_errors,
#                                                 gen_FP_FN_errors,
#                                                 compound_err_name = NULL,
#                                                 ret_vals_from_build_const_err = NULL,
#                                                 ret_vals_from_apply_cost_errors = NULL)
#
#     testsFor_Xu_bdprob_app_wrap_1 (Xu_bdprob_cor_wrap)
#
#     })

#===============================================================================

