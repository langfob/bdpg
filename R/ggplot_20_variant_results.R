#===============================================================================
#
#                           ggplot_20_variant_results.R
#
#  This function is not meant to be called.
#  It's only here as a dummy wrapper to keep R from executing this code when
#  it builds the package.
#  The code here is currently just a bunch of hacking around to do learn
#  something about plotting with ggplot and at the same time, look at some
#  very preliminary bdpg outputs from getting things working on nectar.
#  It will probably evolve into real plotting code eventually, but right now,
#  it's just a graveyard for code as I explore.
#
#===============================================================================

gg_20 <- function ()
{
#===============================================================================
#===============================================================================
#===============================================================================
#  Fooling around with the 20 outputs from 100 gen_20_basic_variants() runs
#  on nectar and copied to glass.
#===============================================================================

### Load necessary libraries.

library (tidyverse)


### Load the marxan data set.

#infile = "/Users/bill/Downloads/bdpgout/197xx/Marxan_SA.csv"    #  0.05 err_amt
#infile = "/Users/bill/Downloads/bdpgout/198xx/Marxan_SA.csv"    #  0.02 err_amt

infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_05_err_amt/default_runset/Gurobi.csv"
    #  Need to find and load this value from somewhere in the results file,
    #  but I can't remember how it's labelled there at the moment, so
    #  I'm loading it by hand for now.
ref_y = 0.05

# infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_02_err_amt/default_runset/Gurobi.csv"
# ref_y = 0.02

msa_dt         = read.csv (infile, header=TRUE, stringsAsFactors = FALSE)
msa_tib        = as.tibble (msa_dt)
sorted_msa_tib = arrange (msa_tib, rsp_combined_err_label)
sorted_msa_tib$idx = rep (1:200, 10)    #1:dim(sorted_msa_tib)[1]

#----------------------------------------

ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
#                            color = rsp_combined_err_label)) +
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=5  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=10  #, linetype, color, size
               )





ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
#                            color = rsp_combined_err_label)) +
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=5  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=10  #, linetype, color, size
               )









    #  Need to do some filtering and sorting and/or regression wrt
    #  output error and problem size as measured by various functions of
    #  the num_spp and num_PUs and by some graph measures like redundancy.
    #  Here's a quick hack for looking at what values the spp & PU cts take,
    #  but no effort to plot them in relation to various output errors.
tot_spp_pu = sorted_msa_tib$rsp_num_spp + sorted_msa_tib$rsp_num_PUs
plot (tot_spp_pu)
plot (sorted_msa_tib$rsp_num_spp_per_PU)

    #  NOTE: the result of selecting for NA gives 201 instead of 200 results.
    #  I would expect 200 since there are 200 correct values that have no
    #  input error.  Not sure what 1 extra NA is from.

err_mag_euc_Ftot_in_vs_euc_Ftot_out =
    sorted_msa_tib$rsr_COR_euc_out_err_frac /
    sorted_msa_tib$rsp_euc_realized_Ftot_and_cost_in_err_frac
err_mag_euc_Ftot_in_vs_euc_Ftot_out [is.na (err_mag_euc_Ftot_in_vs_euc_Ftot_out)] = 0

ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac, y = err_mag_euc_Ftot_in_vs_euc_Ftot_out,
#                            color = rsp_combined_err_label)) +
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=5  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=10  #, linetype, color, size
               )


    #  The graph from the previous call is bizarre and uninformative because
    #  you get some huge magnifications (up to 250) in the small input values.
    #  Here's a bit of code to select just the examples with magnification
    #  of 5 or less.
    #  Might be good to select for > 0 as well, or just remove the COR problems
    #  since magnification of a base of 0 is odd.
    #  Then again, maybe I need to redefine magnification there in some way
    #  because sometimes you DO get output error even without any input error.
    #  Not sure what the appropriate value would be there.  Maybe magnification
    #  is the wrong idea there.  Maybe I should just separate outputs for COR
    #  into a separate plot that just shows the error itself and then
    #  do magnification plots for APP values only.

    #  One other issue is that the input euc error is based on Ftot, which
    #  implicitly gives a different answer based on the number of coordinates
    #  that go into the formula, e.g., if you use (2) vs (2,2) vs (2,2,2) gives
    #  you 2, 2.828427, 3.464102.  Need to figure out how to make sense of
    #  magnification in that context.  You can't just replace Ftot with the
    #  euc combination of FP and FN because they're each weighted by the
    #  number of "pixels" involved.  Maybe I can use something from how Ftot
    #  is calculated.

    #  Need to figure out how to do this with a filter() call since ggplot()
    #  doesn't work the way that I've subsetted for just the small mag values.
    #  For the moment, just doing a quick plot() call instead of ggplot(),
    #  just to get a sense of what it looks like.
indices_of_mags_le_5 = which (err_mag_euc_Ftot_in_vs_euc_Ftot_out <= 5)
in_err = sorted_msa_tib$rsp_euc_realized_Ftot_and_cost_in_err_frac [indices_of_mags_le_5]
mag    = err_mag_euc_Ftot_in_vs_euc_Ftot_out [indices_of_mags_le_5]
plot (in_err, mag)

#  This ggplot call gives the following error:
# Warning: Ignoring unknown aesthetics: NA
# Error: Aesthetics must be either length 1 or the same as the data (2000): x, y, colour
# ggplot (data = sorted_msa_tib) +
#   geom_point (mapping = aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac [indices_of_mags_le_5],
#                             y = err_mag_euc_Ftot_in_vs_euc_Ftot_out [indices_of_mags_le_5],
# #                            color = rsp_combined_err_label)) +
#                             color = rsp_base_wrap_str) [indices_of_mags_le_5]) +
#   geom_hline (yintercept = ref_y, linetype="dashed",
#                 color = "black", size=0.5) +
#   geom_abline (intercept=0, slope=1  #, linetype, color, size
#                )

#----------------------------------------

ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_FP_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=5  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=10  #, linetype, color, size
               )





ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_FN_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=5  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=10  #, linetype, color, size
               )


ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_realized_median_abs_cost_err_frac,
                            y = rsr_COR_euc_out_err_frac,
#                            color = rsp_combined_err_label)) +
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
                                #  Need to draw the 1:1 line because the plot
                                #  is deceptive right now with roughly the
                                #  same digits appearing on both axes, but
                                #  the y values are 10 TIMES the x values.
                                #  The 1:1 line is almost wholly below the
                                #  dashed reference line, so the errors are
                                #  quite big.
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=5  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=10  #, linetype, color, size
               )




#----------------------------------------

    #  Make 2 plots, 1 for Base and 1 for Wrap.
    #  Put the index of the base problem (from 1 to 100) along the x axis.
    #  At each index on each plot, plot all 10 y values for that set of variants.

ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac,
                            color = rsp_combined_err_label)) +
  facet_wrap (~ rsp_base_wrap_str, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5)






# ggplot (data = sorted_msa_tib) +
#   geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac,
#                             color = rsp_base_wrap_str)) +
#   geom_hline (yintercept = ref_y, linetype="dashed",
#                 color = "black", size=0.5)




ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac,
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5)



# ggplot (data = sorted_msa_tib) +
#   geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac,
#                             color = rsp_base_wrap_str)) +
#   facet_wrap (~ rsp_combined_err_label, nrow = 3) +
#   geom_hline (yintercept = ref_y, linetype="dashed",
#                 color = "black", size=0.5)




ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rs_solution_cost_err_frac,
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_hline (yintercept = -ref_y, linetype="dashed",
                color = "black", size=0.5)


ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = abs_rs_solution_cost_err_frac,
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5)



ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rsr_APP_spp_rep_shortfall,
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5)



#===============================================================================
#  Fooling around with the 20 outputs from 1 gen_20_basic_variants() run.
#===============================================================================

    #  Some relevant result column names to remember for use in the plots below.
# rsp_realized_FP_rate
# rsp_realized_FN_rate
# rsp_realized_Ftot_rate
# rsp_euc_realized_FP_and_cost_in_err_frac
# rsp_euc_realized_FN_and_cost_in_err_frac
# rsp_euc_realized_Ftot_and_cost_in_err_frac
#
# rsr_COR_euc_out_err_frac

# rsp_realized_median_abs_cost_err_frac
# rsp_realized_mean_abs_cost_err_frac
# rsp_realized_sd_abs_cost_err_frac


library (tidyverse)

#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/18_easy.completedTzarEmulation/SR_Forward.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/19_easy.completedTzarEmulation/Marxan_SA.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/24_easy.completedTzarEmulation/ZL_Backward.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/25_easy.completedTzarEmulation/Marxan_SA.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_but_gurobi_1_easy_base/default_runset/39_easy.completedTzarEmulation/Marxan_SA.csv"
infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_05_err_amt/default_runset/Gurobi.csv"

msa_dt         = read.csv (infile, header=TRUE, stringsAsFactors = FALSE)
msa_tib        = as.tibble (msa_dt)
sorted_msa_tib = arrange (msa_tib, rsp_combined_err_label)
sorted_msa_tib$idx = 1:20    #1:dim(sorted_msa_tib)[1]

    #  Need to find and load this value from somewhere in the results file,
    #  but I can't remember how it's labelled there at the moment, so
    #  I'm loading it by hand for now.
ref_y = 0.05
#ref_y = 0.02

ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_realized_median_abs_cost_err_frac,
                            y = rsr_COR_euc_out_err_frac,
#                            color = rsp_combined_err_label)) +
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
                                #  Need to draw the 1:1 line because the plot
                                #  is deceptive right now with roughly the
                                #  same digits appearing on both axes, but
                                #  the y values are 10 TIMES the x values.
                                #  The 1:1 line is almost wholly below the
                                #  dashed reference line, so the errors are
                                #  quite big.
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               )


ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
#                            color = rsp_combined_err_label)) +
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               )


ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_FP_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               )

ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_FN_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
                            color = rsp_base_wrap_str)) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               )

#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================


work_tib = select (msa_tib, rsr_UUID, rsp_UUID, rsp_cor_or_app_str, rsp_base_wrap_str, rsp_combined_err_label, rsp_file_name_prefix, rsp_prob_is_ok, rsp_num_PUs, rsp_num_spp, rsp_num_spp_per_PU, rsp_cost_error_bound, rsp_original_FP_const_rate, rsp_original_FN_const_rate, rsp_match_error_counts, rsp_FP_const_rate, rsp_FN_const_rate, rsp_realized_FP_rate, rsp_realized_FN_rate, rsp_realized_Ftot_rate, rsp_app_num_spp, rsp_app_num_PUs, cor_optimum_cost, rs_solution_cost, rs_solution_cost_err_frac, abs_rs_solution_cost_err_frac, rs_over_opt_cost_err_frac_of_possible_overcost, rs_under_opt_cost_err_frac_of_possible_undercost, rsr_COR_euc_out_err_frac, rsr_APP_spp_rep_shortfall, rsr_APP_solution_NUM_spp_covered, rsr_APP_solution_FRAC_spp_covered, RS_user_time, RS_system_time, RS_elapsed_time, RS_user_child_time, RS_sys_child_time)

err_tib = select (work_tib, rsr_UUID, rsp_UUID, rsp_cor_or_app_str, rsp_base_wrap_str, rsp_combined_err_label, rsp_cost_error_bound, rsp_original_FP_const_rate, rsp_original_FN_const_rate, rsp_match_error_counts, rsp_FP_const_rate, rsp_FN_const_rate, rsp_realized_FP_rate, rsp_realized_FN_rate, rsp_realized_Ftot_rate, rs_solution_cost_err_frac, abs_rs_solution_cost_err_frac, rs_over_opt_cost_err_frac_of_possible_overcost, rs_under_opt_cost_err_frac_of_possible_undercost, rsr_COR_euc_out_err_frac, rsr_APP_spp_rep_shortfall, rsr_APP_solution_NUM_spp_covered, rsr_APP_solution_FRAC_spp_covered, RS_user_time)

err_tib = arrange (err_tib, rsp_combined_err_label)


### Create an index value to use in making plots with a default x axis

idx = 1:dim(err_tib)[1]
err_tib$idx = idx


### Plot the absolute value of the cost error fraction against the apparent fraction of species covered.

ggplot(data=err_tib) + geom_point(mapping = aes(x = abs_rs_solution_cost_err_frac, y = rsr_APP_solution_FRAC_spp_covered), color = "blue")



### Facets

#ggplot (data = err_tib) +
ggplot (data = msa_tib) +
  geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac))
#, color = rsp_base_wrap_str)

#ggplot (data = err_tib) +
#  geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac), color = #rsp_base_wrap_str) +
#  facet_wrap (~ rsp_combined_err_label, nrow = 10)
}

#===============================================================================


