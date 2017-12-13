#===============================================================================

                        #  gen_bdprob_tests.R

#===============================================================================

TEST_CREATE_ELIGIBLE_PU_SET = FALSE
if (TEST_CREATE_ELIGIBLE_PU_SET)
    {
    test_create_eligible_PU_set ()
    }

#===============================================================================

# test_remove_Xu_base_spp_from_wrapping_distribution = function ()
#     {
#     extra_abundances = c(5, 4, 5, 2, 3, 2, 2, 4, 4, 4)
#
#     cat ("\n\nextra_abundances = ", extra_abundances)
#
#     cat ("\n    For num_base_spp = 2, new_extras_dist = ")
#     new_extras_dist = remove_Xu_base_spp_from_wrapping_distribution (extra_abundances, 2)
#     cat (new_extras_dist,
#          "\n        SHOULD = 5 4 5 3 2 4 4 4")
#
#     cat ("\n    For num_base_spp = 3, new_extras_dist = ")
#     new_extras_dist = remove_Xu_base_spp_from_wrapping_distribution (extra_abundances, 3)
#     cat (new_extras_dist,
#          "\n        SHOULD = 5 4 5 3 4 4 4")
#
#     # cat ("\n    For num_base_spp = 4, new_extras_dist = ","
#     #      \nSHOULD ABORT")
#     # new_extras_dist = remove_Xu_base_spp_from_wrapping_distribution (extra_abundances, 4)
#     # cat (new_extras_dist)
#     }
#
# TEST_REMOVE_XU_BASE_SPP_FROM_WRAPPING_DISTRIBUTION = FALSE
# if (TEST_REMOVE_XU_BASE_SPP_FROM_WRAPPING_DISTRIBUTION)
#     {
#     test_remove_Xu_base_spp_from_wrapping_distribution ()
#     }

#===============================================================================

test_trimmed_abundances = function ()
    {
    rounded_abundances = 1:10

    trimmed_abundances_2_10 = trim_abundances (rounded_abundances, min_abund=2)
    trimmed_abundances_4_7 = trim_abundances (rounded_abundances, min_abund=4, max_abund=7)
    trimmed_abundances_1_10 = trim_abundances (rounded_abundances, min_abund=1, max_abund=10)

    browser()
    }

TEST_TRIMMED_ABUNDANCES = FALSE
if (TEST_TRIMMED_ABUNDANCES)
    test_trimmed_abundances()

#===============================================================================

TEST_WRAP_ABUNDANCES_AROUND_ELIGIBLE_SET = TRUE
if (TEST_WRAP_ABUNDANCES_AROUND_ELIGIBLE_SET)
    {
    test_wrap = function ()
        {
        ind_set   = 101:110
        dep_set   = 111:120
        num_base_spp = 15

        extra_set = 121:130
        eligible_PUs = extra_set    #  c(dep_set, extra_set)

#        rounded_abundances = c(1:5)   Should cause error "tot_num_spp_on_exactly_2_PUs in wrapped distribution = 1 < num_base_spp = 10.
        rounded_abundances = c(rep(1:5,2), rep(2,num_base_spp))

        Xu_PU_spp_table = data.frame (PU_ID = c(101:120, 111:115, 116:120),
                                      spp_ID = c(1:10,1:10,11:15,11:15))


        wrapped_PU_spp_table =
            wrap_abundances_around_eligible_set (dep_set,
                                                  eligible_PUs,
                                                  rounded_abundances,
                                                  num_base_spp,
                                                  Xu_PU_spp_table,
                                                  min_allowed_abundance = 2)

        cat ("\n\nwrapped_PU_spp_table = \n")
        head (wrapped_PU_spp_table)
#        print (wrapped_PU_spp_table)
        cat ("\n\n")

#browser()

        #  Tests:
        #  1)  no PU repeats within a species
        #  2)  no PUs from the independent set
        #  3)  only one PU from dependent set if eligible set is only the extra set
        }

    test_wrap ()

    }  #  end if - TRUE

#===============================================================================

