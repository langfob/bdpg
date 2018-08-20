library (dplyr)

#===============================================================================

#  Set file paths

# # # base_path = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/"
#
    #  Easy
base_path = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base_2nd_attempt/bdpg_20_variants_all_rs_easy_base_2nd_try_Combined_err_amts/"
#
#     #  Hard
# base_path = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_HARD_base_first_attempt/bdpg_20_variants_all_rs_HARD_base_1st_try_Combined_err_amts/"

#     #  Hard - 2, 5, 7.5, 10 % error
# base_path = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_HARD_base_first_attempt/bdpg_20_variants_all_rs_HARD_base_02_05_075_10_Combined_err_amts/"

suffix = ".combined_results.csv"

#===============================================================================

#  Define functions

load_input_data <- function (rs_name, base_path, suffix)
    {
    infile = paste0 (base_path, rs_name, suffix)

        #  I can't remember why I called these "msa".  It might stand for
        #  multi-set analysis...

    msa_dt         = read.csv (infile, header=TRUE, stringsAsFactors = FALSE)

    return (msa_dt)
    }

convert_input_dt_to_sorted_tibble <- function (msa_dt)
    {
    msa_tib        = as.tibble (msa_dt)
    sorted_msa_tib = arrange (msa_tib, rsp_combined_err_label)

    unique_out_errs = unique (sorted_msa_tib$rsr_COR_euc_out_err_frac)
    length (unique_out_errs)
    sorted_unique_out_errs = sort (unique_out_errs)

    cat ("\n\nhead (sorted_unique_OUT_errs) = \n")
    print (head (sorted_unique_out_errs))

    cat ("\ntail (sorted_unique_OUT_errs) = \n")
    print (tail (sorted_unique_out_errs))
    cat ("\n-----------------------")


    unique_in_errs = unique (sorted_msa_tib$rsp_euc_realized_Ftot_and_cost_in_err_frac)
    length (unique_in_errs)
    sorted_unique_in_errs = sort (unique_in_errs)

    cat ("\n\nhead (sorted_unique_IN_errs) = \n")
    print (head (sorted_unique_in_errs))

    cat ("\ntail (sorted_unique_IN_errs) = \n")
    print (tail (sorted_unique_in_errs))
    cat ("\n-----------------------")

    return (sorted_msa_tib)
    }

#===============================================================================

#  Load input data

rs_name = "Gurobi"
#rs_name = "Marxan_SA"
#rs_name = "Marxan_SA_SS"
#rs_name = "ZL_Backward"
#rs_name = "ZL_Forward"
#rs_name = "SR_Backward"
#rs_name = "SR_Forward"
#rs_name = "UR_Backward"
#rs_name = "UR_Forward"

msa_dt = load_input_data (rs_name, base_path, suffix)

glimpse (msa_dt)


#Need to convert error type column to factors?


#  Break data up into related groups of columns

hdr_data = select (msa_dt,

                   rs_method_name,
                   rsp_combined_err_label,
                   rsp_cor_or_app_str,
                   rsp_base_wrap_str,

                   rsr_tzar_run_ID,
                   rsr_UUID,

                   rsp_file_name_prefix,
                   rsp_rand_seed,

                   rsp_UUID,
                   rsp_UUID_of_base_problem_that_has_err_added,
                   rsp_UUID_of_base_problem_that_is_wrapped,

                   rsr_checksum,
                   rsr_rand_seed,
                   rsp_tzar_run_ID,
                   rsp_checksum,
                   rsp_prob_is_ok,
                   rsp_prob_generator_params_known,
                   rsp_read_Xu_problem_from_Xu_file,
                   rsp_infile_name,
                   rsp_correct_solution_vector_is_known,

                   rsp_compute_network_metrics,
                   rsp_use_igraph_metrics,
                   rsp_use_bipartite_metrics,
                   rsp_bipartite_metrics_to_use
                   )
glimpse (hdr_data)


    #  Parameters from here on are things that could be used in a model.
    #  The hdr_data is stuff that would probably never be an input to a model.

    #  Parameters used to control generation of the problem, e.g., Xu params.
probGen_inputs = select (msa_dt,
                         rsp_cost_error_bound,
                         rsp_original_FP_const_rate,
                         rsp_original_FN_const_rate,
                         rsp_match_error_counts,
                         rsp_allow_imperfect_wrap,
                         rsp_opt_solution_as_frac_of_tot_num_nodes,
                         rsp_alpha__,
                         rsp_n__num_groups,
                         rsp_p__prop_of_links_between_groups,
                         rsp_r__density,
                         rsp_alpha___lower_bound,
                         rsp_alpha___upper_bound,
                         rsp_derive_alpha_from_n__num_groups_and_opt_frac_0.5,
                         rsp_use_unif_rand_alpha__,
                         rsp_n__num_groups_lower_bound,
                         rsp_n__num_groups_upper_bound,
                         rsp_use_unif_rand_n__num_groups,
                         rsp_num_independent_nodes_per_group,
                         rsp_use_unif_rand_p__prop_of_links_between_groups,
                         rsp_p__prop_of_links_between_groups_lower_bound,
                         rsp_p__prop_of_links_between_groups_upper_bound,
                         rsp_base_for_target_num_links_between_2_groups_per_round,
                         rsp_at_least_1_for_target_num_links_between_2_groups_per_round,
                         rsp_use_unif_rand_r__density,
                         rsp_r__density_lower_bound,
                         rsp_r__density_upper_bound,
                         rsp_num_nodes_per_group,
                         rsp_num_rounds_of_linking_between_groups,
                         rsp_target_num_links_between_2_groups_per_round,
                         rsp_num_links_within_one_group,
                         rsp_tot_num_links_inside_groups,
                         rsp_max_possible_num_links_between_groups,
                         rsp_max_possible_tot_num_links,
                         rsp_max_possible_tot_num_node_link_pairs
                         )
glimpse (probGen_inputs)


    #  Parameters that result from problem generation, but can't be seen by
    #  the end user.
probGen_deducedValues = select (msa_dt,

                                rsp_num_PUs,
                                rsp_num_spp,
                                rsp_num_spp_per_PU,
                                rsp_correct_solution_cost,

                                rsp_realized_median_abs_cost_err_frac,
                                rsp_realized_mean_abs_cost_err_frac,
                                rsp_realized_sd_abs_cost_err_frac,
                                rsp_FP_const_rate,
                                rsp_FN_const_rate,
                                rsp_realized_FP_rate,
                                rsp_realized_FN_rate,
                                rsp_realized_Ftot_rate,
                                rsp_euc_realized_FP_and_cost_in_err_frac,
                                rsp_euc_realized_FN_and_cost_in_err_frac,
                                rsp_euc_realized_Ftot_and_cost_in_err_frac,
                                rsp_wrap_is_imperfect,

                                    #  Xu only
                                rsp_tot_num_nodes,
                                rsp_num_independent_set_nodes,
                                rsp_num_dependent_set_nodes,

                                cor_optimum_cost
                                )
glimpse (probGen_deducedValues)


    #  Knowable attributes of the apparent problem, i.e., things that you can
    #  count or measure about any problem without knowing anything about how
    #  it was generated.  This is essentially the information that anyone has
    #  whenthey need to predict the output quality of reserve selection for
    #  any problem.
postGen_probDesc_data = select (msa_dt,

                                rsp_app_num_spp,
                                rsp_app_num_PUs,

                                    #  igraph package metrics
                                ig_rsp_UUID,
                                ig_top,
                                ig_bottom,
                                ig_num_edges_m,
                                ig_ktop,
                                ig_kbottom,
                                ig_bidens,
                                ig_lcctop,
                                ig_lccbottom,
                                ig_distop,
                                ig_disbottom,
                                ig_cctop,
                                ig_ccbottom,
                                ig_cclowdottop,
                                ig_cclowdotbottom,
                                ig_cctopdottop,
                                ig_cctopdotbottom,
                                ig_mean_bottom_bg_redundancy,
                                ig_median_bottom_bg_redundancy,
                                ig_mean_top_bg_redundancy,
                                ig_median_top_bg_redundancy,

                                ig_user_time,
                                ig_system_time,
                                ig_elapsed_time,
                                ig_user_child_time,
                                ig_sys_child_time,

                                    #  bipartite package metrics
                                bip_rsp_UUID,
                                connectance,
                                number.of.PUs,
                                number.of.Spp,

                                bip_user_time,
                                bip_system_time,
                                bip_elapsed_time,
                                bip_user_child_time,
                                bip_sys_child_time
                                )
glimpse (postGen_probDesc_data)


rs_data = select (msa_dt,

            rs_method_name,
            rsp_combined_err_label,
            rsp_cor_or_app_str,
            rsp_base_wrap_str,

            rsr_tzar_run_ID,
            rsr_UUID,

                  rs_solution_cost,
                  rs_solution_cost_err_frac,
                  abs_rs_solution_cost_err_frac,

                  rs_over_opt_cost_err_frac_of_possible_overcost,
                  rs_under_opt_cost_err_frac_of_possible_undercost,

                  rsr_COR_euc_out_err_frac,

                  rsr_APP_spp_rep_shortfall,
                  rsr_APP_solution_NUM_spp_covered,
                  rsr_APP_solution_FRAC_spp_covered,

                        #  run time information
                  RS_user_time,
                  RS_system_time,
                  RS_elapsed_time,
                  RS_user_child_time,
                  RS_sys_child_time
                  )
glimpse (rs_data)


rs_cls_err_measures = select (msa_dt,
                              rsr_APP_TP,
                              rsr_APP_TN,
                              rsr_APP_FP,
                              rsr_APP_FN,
                              rsr_APP_cSe,
                              rsr_APP_cSp,
                              rsr_APP_cPPV,
                              rsr_APP_cNPV,
                              rsr_APP_acc_frac,
                              rsr_APP_acc_err_frac,
                              rsr_APP_cost_savings,
                              rsr_APP_opt_cost_savings,
                              rsr_APP_TSS,
                              rsr_APP_max_cSe_cSp,
                              rsr_APP_min_cSe_cSp,
                              rsr_APP_mean_cSe_cSp,
                              rsr_APP_prod_cSe_cSp,
                              rsr_APP_euc_cSe_cSp,
                              rsr_APP_acc_err_mag,
                              rsr_COR_spp_rep_shortfall,
                              rsr_COR_solution_NUM_spp_covered,
                              rsr_COR_solution_FRAC_spp_covered,
                              rsr_APP_TP.1,
                              rsr_APP_TN.1,
                              rsr_APP_FP.1,
                              rsr_APP_FN.1,
                              rsr_APP_cSe.1,
                              rsr_APP_cSp.1,
                              rsr_APP_cPPV.1,
                              rsr_APP_cNPV.1,
                              rsr_APP_acc_frac.1,
                              rsr_APP_acc_err_frac.1,
                              rsr_APP_cost_savings.1,
                              rsr_APP_opt_cost_savings.1,
                              rsr_APP_TSS.1,
                              rsr_APP_max_cSe_cSp.1,
                              rsr_APP_min_cSe_cSp.1,
                              rsr_APP_mean_cSe_cSp.1,
                              rsr_APP_prod_cSe_cSp.1,
                              rsr_APP_euc_cSe_cSp.1,
                              rsr_APP_acc_err_mag.1
                              )
glimpse (rs_cls_err_measures)

# if (rs_name == "Gurobi")
#     {
#     gurobi_only_values = select (msa_dt,
#                                  gurobi_status,
#                                  gurobi_objval,
#                                  gurobi_objbound,
#                                  gurobi_runtime,
#                                  gurobi_itercount,
#                                  gurobi_baritercount,
#                                  gurobi_nodecount,
#                                  gurobi_num_spp,
#                                  gurobi_num_PUs,
#                                  gurobi_use_gap_limit,
#                                  gurobi_gap_limit_input,
#                                  gurobi_gap_limit_used,
#                                  gurobi_use_given_time_as_limit,
#                                  gurobi_time_limit_input,
#                                  gurobi_use_marxan_time_as_limit,
#                                  gurobi_marxan_elapsed_time_input,
#                                  gurobi_time_limit_used,
#                                  gurobi_runtime.1,
#                                  gurobi_poolobjbound,
#                                  gurobi_objbound.1,
#                                  gurobi_objboundc,
#                                  gurobi_mipgap
#                                  )
#     glimpse (gurobi_only_values)
#     }

if ((rs_name == "Marxan_SA") | (rs_name == "Marxan_SA_SS"))
    {
    marxan_sa_only_values = select (msa_dt,
                                    marxan_PROP,
                                    marxan_RANDSEED,
                                    marxan_NUMREPS,
                                    marxan_NUMITNS,
                                    marxan_STARTTEMP,
                                    marxan_NUMTEMP,
                                    marxan_COSTTHRESH,
                                    marxan_THRESHPEN1,
                                    marxan_THRESHPEN2,
                                    marxan_RUNMODE,
                                    marxan_MISSLEVEL,
                                    marxan_ITIMPTYPE,
                                    marxan_HEURTYPE,
                                    marxan_CLUMPTYPE,
                                    spf_const
                                    )
    glimpse (marxan_sa_only_values)
    }


working_data = select (msa_dt,

        #  Problem and reserve selector labels
                # rs_method_name,
                # rsp_combined_err_label,
                # rsp_cor_or_app_str,
                # rsp_base_wrap_str,
                #
                # rsr_tzar_run_ID,
                # rsr_UUID,
                #
                # rsp_file_name_prefix,
                #
                # rsp_UUID,
                # rsp_UUID_of_base_problem_that_has_err_added,
                # rsp_UUID_of_base_problem_that_is_wrapped,


        #  Input descriptors
                rsp_num_PUs,
                rsp_num_spp,
                rsp_num_spp_per_PU,

#  2018 08 20 - Not sure why I thought this was missing in easy data.
#  Doing data cleaning now and it seems like the values ARE there, not missing...
##!!  MISSING IN EASY PROBLEM DATA!!  !!##                rsp_correct_solution_cost,
rsp_correct_solution_cost,

        #  Post-gen knowable problem descriptors

            #  Species and PU counts
                # rsp_app_num_spp,
                # rsp_app_num_PUs,

            #  igraph package metrics
#                ig_rsp_UUID,

                ig_top,
                ig_bottom,
                ig_num_edges_m,
                ig_ktop,
                ig_kbottom,
                ig_bidens,
                ig_lcctop,
                ig_lccbottom,
                ig_distop,
                ig_disbottom,
                ig_cctop,
                ig_ccbottom,
                ig_cclowdottop,
                ig_cclowdotbottom,
                ig_cctopdottop,
                ig_cctopdotbottom,
                ig_mean_bottom_bg_redundancy,
                ig_median_bottom_bg_redundancy,
                ig_mean_top_bg_redundancy,
                ig_median_top_bg_redundancy,

                # ig_user_time,
                # ig_system_time,
                # ig_elapsed_time,
                # ig_user_child_time,
                # ig_sys_child_time,

            #  bipartite package metrics
#                bip_rsp_UUID,

                connectance,
                number.of.PUs,
                number.of.Spp,

                # bip_user_time,
                # bip_system_time,
                # bip_elapsed_time,
                # bip_user_child_time,
                # bip_sys_child_time,


            #  Realized input error values
                rsp_realized_median_abs_cost_err_frac,
                rsp_realized_mean_abs_cost_err_frac,
                rsp_realized_sd_abs_cost_err_frac,
                rsp_FP_const_rate,
                rsp_FN_const_rate,

                # rsp_realized_FP_rate,
                # rsp_realized_FN_rate,
                # rsp_realized_Ftot_rate,
                # rsp_euc_realized_FP_and_cost_in_err_frac,
                # rsp_euc_realized_FN_and_cost_in_err_frac,
                # rsp_euc_realized_Ftot_and_cost_in_err_frac,
                #
                # rsp_wrap_is_imperfect,


            #  Results and their errors
                rs_solution_cost,
                rs_solution_cost_err_frac,
                abs_rs_solution_cost_err_frac,

                # rs_over_opt_cost_err_frac_of_possible_overcost,
                # rs_under_opt_cost_err_frac_of_possible_undercost,

                rsr_COR_euc_out_err_frac,

                rsr_APP_spp_rep_shortfall,
                rsr_APP_solution_NUM_spp_covered,
                rsr_APP_solution_FRAC_spp_covered
#,

            #  Reserve selector run times
                # RS_user_time,
                # RS_system_time,
                # RS_elapsed_time,
                # RS_user_child_time,
                # RS_sys_child_time

                  )
glimpse (working_data)

#===============================================================================

#  Examine gurobi-specific data

#  See how many times gurobi timed out and what the distribution of gaps was.

#  if (gurobi...
# {
# plyr::count (gurobi_only_values$gurobi_status)
#
# hist (gurobi_only_values$gurobi_mipgap [gurobi_only_values$gurobi_mipgap > 0])
# }
#===============================================================================

#  Plot run times

# It looks like igraph is the main time hog (ignoring gurobi).  It's taking orders of magnitude more time than reserve selection or the bipartite graph routines.  However, bipartite isn't really doing much at the moment since I messed up the option selection there so that it only computes one quick thing.
#
# In any case, it does suggest that it might be worth restructuring the way that runs are done so that the graph routines (and possibly gurobi) are run as a group on machines with lots of memory.  Reserve selections can be run afterwards.
#
# Still, maybe that's not much of an idea, since there is one igraph run for every single reserve selection group of 8 very quick reserve selectors and gurobi.  The bulk of all the work would be going into doing the igraph calculations.
#
# This also suggests that it may be worth doing something to speed up the graph calculations or weed out which ones are useful.
# - For example, it seems like the redundancy() calculation often causes a choke; does it actually provide useful predictive information?
# - Or, run the python version of igraph instead of the R version?
#
# Note that I'm plotting these times with number of species on the x axis.  Might be interesting to plot them with some of the graph measures on the x axis to see if those are more closely correlated with the run times.  Could check this easily first by just plotting a matrix of correlation pairs of problem descriptors and run times.
# - Have now tried that with ig_median_top_bg_redundancy and ig_mean_top_bg_redundancy and they're not very predictve (visually) except for reserve selection elapsed time, where they're of similar predictive value as number of species.
#     - Interestingly though, the redundancy values clump into about 8 different groups.  I wonder if these are related to anything else, such as error type.
# - Have now also tried ig_median_bottom_bg_redundancy and ig_mean_bottom_bg_redundancy.  Neither of those are particularly useful predictors of run times either.  Interestingly, they don't clum into groups the way that the top redundancies do.  I have no idea what that means.


###  Plot User run times

plot (msa_dt$rsp_num_spp, msa_dt$RS_user_time)
plot (msa_dt$rsp_num_spp, msa_dt$ig_user_time)
plot (msa_dt$rsp_num_spp, msa_dt$bip_user_time)

plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$RS_user_time)
plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$ig_user_time)
plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$bip_user_time)

###  Plot System run times

plot (msa_dt$rsp_num_spp, msa_dt$RS_system_time)
plot (msa_dt$rsp_num_spp, msa_dt$ig_system_time)
plot (msa_dt$rsp_num_spp, msa_dt$bip_system_time)

plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$RS_system_time)
plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$ig_system_time)
plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$bip_system_time)

###  Plot Elapsed run times

plot (msa_dt$rsp_num_spp, msa_dt$RS_elapsed_time)
plot (msa_dt$rsp_num_spp, msa_dt$ig_elapsed_time)
plot (msa_dt$rsp_num_spp, msa_dt$bip_elapsed_time)

plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$RS_elapsed_time)
plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$ig_elapsed_time)
plot (msa_dt$ig_mean_bottom_bg_redundancy, msa_dt$bip_elapsed_time)

#===============================================================================

#  Compute magnifications

# msa_dt %>%
#   mutate (err_mag = rsr_COR_euc_out_err_frac /
#                     rsp_euc_realized_Ftot_and_cost_in_err_frac)


emag = msa_dt$rsr_COR_euc_out_err_frac /
       msa_dt$rsp_euc_realized_Ftot_and_cost_in_err_frac
emag [is.na (emag)] = 0
msa_dt$err_mag = emag
working_data$err_mag = emag

#===============================================================================

#  Compute correlations among all working data variables

library (corrplot)

correlations = cor (working_data)
dim (correlations)
#correlations [21:25, 36]    #  for hard data
correlations [20:25, 35:36]    #  for easy data

corrplot (correlations
#          , order = "hclust"    #  This fails because of NAs or NaNs in the data...
          )

#===============================================================================

#  Compute PU,spp vars

# msa_dt %>%
#  mutate (sppPUsum = rsp_app_num_spp + rsp_app_num_PUs,
#          sppPUprod = rsp_app_num_spp * rsp_app_num_PUs)

sppPUsum = msa_dt$rsp_num_spp + msa_dt$rsp_num_PUs
msa_dt$sppPUsum = sppPUsum

sppPUprod = msa_dt$rsp_num_spp * msa_dt$rsp_num_PUs
msa_dt$sppPUprod = sppPUprod

cat (length (msa_dt$sppPUsum))
cat (length (msa_dt$sppPUprod))

#===============================================================================

#  Group by error type and give counts

# The result of this looks like there are many failed runs in Wrap problems containing both FP and FN errors.

msa_dt %>%
  group_by (rsp_cor_or_app_str, rsp_base_wrap_str, rsp_combined_err_label) %>%
  summarize (n())

#===============================================================================

#  Group by error type and give median err mags

msa_dt %>%
  group_by (rsp_cor_or_app_str, rsp_base_wrap_str, rsp_combined_err_label) %>%
  summarize (median (err_mag))

#===============================================================================

#  What is causing the line of huge output errors ( > 5)?

# There is a band of output errors > 5 across the top of the plots.  Not only are these strangely separated large errors, but I think that they're responsible for the huge magnifications as well.

###  See which err types are responsible for huge errors

msa_dt %>%
  filter (rsr_COR_euc_out_err_frac > 2) %>%
  group_by (rsp_base_wrap_str, rsp_combined_err_label) %>%
  summarize (n())

###  Count the number of unique huge error values

# Not sure if that straight line of points across the top of the error graphs is really a bunch of identical values or just similar values squashed together by the scale of the plot.

plyr::count (msa_dt$rsr_COR_euc_out_err_frac [msa_dt$rsr_COR_euc_out_err_frac > 2])

###  See if these are all cases where some COR spp don't occur in APP

# Not sure if that straight line of points across the top of the error graphs is really a bunch of identical values or just similar values squashed together by the scale of the plot.
#
# If you look to see if the correct and apparent numbers of species different in any of the problems, it comes up as no, but if I remember right, at some point I decided to set the two values equal.  There are some coding issues that cause problems if the two occurrrence matrices aren't the same size (e.g., in computing errors?).

plyr::count (msa_dt$rsp_app_num_spp != msa_dt$rsp_num_spp)

###  Implied Bug Fix Required for missing apparent species error calcs

# So, I may have to modify the code and redo ALL runs to be able to figure this out.  More likely would be to just fix it before doing the full random set and just consider this to be part of why I'm doing this initial good-sized shakedown cruise through the easy and hard problem sets.

#===============================================================================

#  What is causing the line of output errors ~= 1?

# There is a band of output errors that look to exactly equal 1.  They seem too regular in the midst of all the other random-looking errors.
# - One possibility is that these are problems where the solution is 50% of the landscape, but the RS needs to get the entire landscape to get a legal solution (e.g., if there was one or more missing species due to FNs).  In that case, the error would be 100%, i.e., 1.0 on the plots.

###  See which err types are responsible for huge errors

msa_dt %>%
  filter (rsr_COR_euc_out_err_frac == 1) %>%
  group_by (rsp_base_wrap_str, rsp_combined_err_label) %>%
  summarize (n())

###  Count the number of unique error values near 1

# Not sure if that straight line of points across the top of the error graphs is really a bunch of identical values or just similar values squashed together by the scale of the plot.

plyr::count (msa_dt$rsr_COR_euc_out_err_frac [abs (1 - msa_dt$rsr_COR_euc_out_err_frac) < 0.00001])

###  Not sure what that result implies

# All of the values are from APP Base problems that contain FNs, but that doesn't tell me what the problem is.  One clue is that the two singleton cases are both when there is FP and FN but not matched, i.e., when the are lots of FPs relative to FNs.  So, again, it seems to be something to do with FN error.

#===============================================================================

#  Separate out CORRECT data

msa_dt_COR = msa_dt [msa_dt$rsp_cor_or_app_str == "COR", ]
glimpse (msa_dt_COR)

cat ("\nNumber of NA values in COR INPUT errors = ",
     length (which (is.na (msa_dt_COR$rsp_euc_realized_Ftot_and_cost_in_err_frac))))
cat ("\nNumber of 0 values in COR INPUT errors = ",
     length (which (msa_dt_COR$rsp_euc_realized_Ftot_and_cost_in_err_frac == 0)))
cat ("\nLength of COR INPUT errors = ",
     length (msa_dt_COR$rsp_euc_realized_Ftot_and_cost_in_err_frac))

cat ("\nNumber of NA values in COR OUTPUT errors = ",
     length (which (is.na (msa_dt_COR$rsr_COR_euc_out_err_frac))))
cat ("\nNumber of 0 values in COR OUTPUT errors = ",
     length (which (msa_dt_COR$rsr_COR_euc_out_err_frac == 0)))
cat ("\nLength of COR OUTPUT errors = ",
     length (msa_dt_COR$rsr_COR_euc_out_err_frac))

cat ("\nNumber of NA values in COR err_mag = ",
     length (which (is.na (msa_dt_COR$err_mag))))
cat ("\nNumber of Inf values in COR err_mag = ",
     length (which (is.infinite (msa_dt_COR$err_mag))))

plot (msa_dt_COR$rsr_COR_euc_out_err_frac)

msa_dt_COR %>%
  group_by (rsp_base_wrap_str, rsp_combined_err_label) %>%
#  summarize (n(), median (rsr_COR_euc_out_err_frac, na.rm = TRUE))
    summarize (median (rsr_COR_euc_out_err_frac, na.rm = TRUE),
               median (err_mag, na.rm = TRUE))

#===============================================================================

#  Separate out APPARENT data

msa_dt_APP = msa_dt [msa_dt$rsp_cor_or_app_str == "APP", ]
glimpse (msa_dt_APP)

cat ("\nNumber of NA values in APP INPUT errors = ",
     length (which (is.na (msa_dt_APP$rsp_euc_realized_Ftot_and_cost_in_err_frac))))
cat ("\nNumber of 0 values in APP INPUT errors = ",
     length (which (msa_dt_APP$rsp_euc_realized_Ftot_and_cost_in_err_frac == 0)))
cat ("\nLength of APP INPUT errors = ",
     length (msa_dt_APP$rsp_euc_realized_Ftot_and_cost_in_err_frac))

cat ("\nNumber of NA values in APP OUTPUT errors = ",
     length (which (is.na (msa_dt_APP$rsr_COR_euc_out_err_frac))))
cat ("\nNumber of 0 values in APP OUTPUT errors = ",
     length (which (msa_dt_APP$rsr_COR_euc_out_err_frac == 0)))
cat ("\nLength of APP OUTPUT errors = ",
     length (msa_dt_APP$rsr_COR_euc_out_err_frac))

cat ("\nNumber of NA values in APP err_mag = ",
     length (which (is.na (msa_dt_APP$err_mag))))
cat ("\nNumber of Inf values in APP err_mag = ",
     length (which (is.infinite (msa_dt_APP$err_mag))))

msa_dt_APP %>%
  group_by (rsp_base_wrap_str, rsp_combined_err_label) %>%
#  summarize (n(), median (rsr_COR_euc_out_err_frac, na.rm = TRUE))
  summarize (median (rsr_COR_euc_out_err_frac, na.rm = TRUE),
             median (err_mag, na.rm = TRUE))

plot (msa_dt_APP$rsr_COR_euc_out_err_frac)
plot (msa_dt_APP$rsp_euc_realized_Ftot_and_cost_in_err_frac, msa_dt_APP$err_mag)
plot (msa_dt_APP$rsp_euc_realized_Ftot_and_cost_in_err_frac, msa_dt_APP$rsr_COR_euc_out_err_frac)

#===============================================================================

#  Does numSpp and/or numPUs predict output error?

# These plots seem to show no relationship at all between output error and the number of species and/or number of PUs or some combination of the two.

plot (msa_dt_APP$rsp_num_spp, msa_dt_APP$rsr_COR_euc_out_err_frac)
plot (msa_dt_APP$rsp_num_PUs, msa_dt_APP$rsr_COR_euc_out_err_frac)


plot (msa_dt_APP$sppPUsum, msa_dt_APP$rsr_COR_euc_out_err_frac)
plot (msa_dt_APP$sppPUprod, msa_dt_APP$rsr_COR_euc_out_err_frac)

plot (msa_dt_APP$rsp_num_spp_per_PU, msa_dt_APP$rsr_COR_euc_out_err_frac)

#===============================================================================

#  Plot ERROR MAGS as a function of redundancy values and connectance

# These look pretty useless, except for one thing.  Every huge magnification occurs at a tiny value of redundancy. That could imply that it's a useful filter, i.e., to say if redundancy is greater than r, magnification is almost never going to be huge.

###  Plot all error mags for APP

plot (msa_dt_APP$ig_median_top_bg_redundancy, msa_dt_APP$err_mag)
plot (msa_dt_APP$ig_mean_top_bg_redundancy, msa_dt_APP$err_mag)
plot (msa_dt_APP$ig_median_bottom_bg_redundancy, msa_dt_APP$err_mag)
plot (msa_dt_APP$ig_mean_bottom_bg_redundancy, msa_dt_APP$err_mag)
plot (msa_dt_APP$connectance, msa_dt_APP$err_mag)

###  Plot error mags for APP whose err mag is not huge

not_huge_mag_idxs = which (msa_dt_APP$err_mag < 10)

plot (msa_dt_APP$ig_median_top_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$err_mag [not_huge_mag_idxs])
plot (msa_dt_APP$ig_mean_top_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$err_mag [not_huge_mag_idxs])
plot (msa_dt_APP$ig_median_bottom_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$err_mag [not_huge_mag_idxs])
plot (msa_dt_APP$ig_mean_bottom_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$err_mag [not_huge_mag_idxs])
plot (msa_dt_APP$connectance [not_huge_mag_idxs], msa_dt_APP$err_mag [not_huge_mag_idxs])

#===============================================================================

#  Plot OUTPUT ERRORS as a function of redundancy values and connectance

# Interesting.  Percentage output error for non-huge errors __does__ seem to be somewhat correlated with redundancy (particularly __top__ redundancy and particularly as that redundancy increases).

###  Plot all output errors for APP

plot (msa_dt_APP$ig_median_top_bg_redundancy, msa_dt_APP$rsr_COR_euc_out_err_frac)
plot (msa_dt_APP$ig_mean_top_bg_redundancy, msa_dt_APP$rsr_COR_euc_out_err_frac)
plot (msa_dt_APP$ig_median_bottom_bg_redundancy, msa_dt_APP$rsr_COR_euc_out_err_frac)
plot (msa_dt_APP$ig_mean_bottom_bg_redundancy, msa_dt_APP$rsr_COR_euc_out_err_frac)
plot (msa_dt_APP$connectance, msa_dt_APP$rsr_COR_euc_out_err_frac)

###  Plot output errors for APP whose __MAGNIFICATION__ is not huge

not_huge_mag_idxs = which (msa_dt_APP$err_mag < 10)

plot (msa_dt_APP$ig_median_top_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_mag_idxs])
plot (msa_dt_APP$ig_mean_top_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_mag_idxs])
plot (msa_dt_APP$ig_median_bottom_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_mag_idxs])
plot (msa_dt_APP$ig_mean_bottom_bg_redundancy [not_huge_mag_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_mag_idxs])
plot (msa_dt_APP$connectance [not_huge_mag_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_mag_idxs])

###  Plot output errors for APP whose __OUTPUT ERROR__ is not huge

not_huge_outErr_idxs = which (msa_dt_APP$rsr_COR_euc_out_err_frac < 5)

plot (msa_dt_APP$ig_median_top_bg_redundancy [not_huge_outErr_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_outErr_idxs])
plot (msa_dt_APP$ig_mean_top_bg_redundancy [not_huge_outErr_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_outErr_idxs])
plot (msa_dt_APP$ig_median_bottom_bg_redundancy [not_huge_outErr_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_outErr_idxs])
plot (msa_dt_APP$ig_mean_bottom_bg_redundancy [not_huge_outErr_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_outErr_idxs])
plot (msa_dt_APP$connectance [not_huge_outErr_idxs], msa_dt_APP$rsr_COR_euc_out_err_frac [not_huge_outErr_idxs])

#  Are there problem clusters?

# It almost looks like there are several subsets of problems here that a decision tree might be able to find.  For example,
# - All huge magnifications have tiny redundancies.
# - Output errors are somewhat positively correlated with top redundancy.

#===============================================================================











