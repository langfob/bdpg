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

load_input_data <- function (rs_name, base_path, suffix,
                             num_sets = 4,
                             num_variants_per_set = 20,
                             num_combined_err_labels = 10)
    {
    infile = paste0 (base_path, rs_name, suffix)

    msa_dt         = read.csv (infile, header=TRUE, stringsAsFactors = FALSE)
    msa_tib        = as.tibble (msa_dt)
    sorted_msa_tib = arrange (msa_tib, rsp_combined_err_label)

        #  Now that they are sorted by the 10 combined error labels,
        #  give an index to each rsrun within each of the combined error labels
        #  so that they can be spread out by index across a single facet plot.
        #  The index has no meaning other than to give it a unique identifier
        #  to spread along the x axis within each facet.

    num_indices_per_combined_err_label =
        num_sets * num_variants_per_set * num_combined_err_labels

    sorted_msa_tib$idx = rep (1:num_indices_per_combined_err_label,
                              num_combined_err_labels)

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

#' @importFrom stats mad quantile sd

compute_and_print_na_inf_stats <- function (rs_name,
                                            out_err,
                                            inp_err,
                                            err_mag,
                                            print_heading_str)
    {
    cat ("\n\n------------------------------------------------")
    cat (paste0 ("\n\n-----  ", print_heading_str, "  -----"))

    #----------

    num_inp_na = length (which (is.na (inp_err)))
        cat ("\n\nnum_inp_na for err_mag = ", num_inp_na)

    num_inp_inf = length (which (is.infinite (inp_err)))
        cat ("\nnum_inp_inf for err_mag = ", num_inp_inf)

    num_inp_0 = length (which (inp_err == 0))
        cat ("\nnum_inp_0 for err_mag = ", num_inp_0, "\n\n")

    #----------

    num_out_na = length (which (is.na (out_err)))
        cat ("\n\nnum_out_na for err_mag = ", num_out_na)

    num_out_inf = length (which (is.infinite (out_err)))
        cat ("\nnum_out_inf for err_mag = ", num_out_inf)

    num_out_0 = length (which (inp_err == 0))
        cat ("\nnum_out_0 for err_mag = ", num_out_0, "\n\n")

    #----------

    num_mag_na = length (which (is.na (err_mag)))
        cat ("\n\nnum_mag_na for err_mag = ", num_mag_na)

    num_mag_inf = length (which (is.infinite (err_mag)))
        cat ("\nnum_mag_inf for err_mag = ", num_mag_inf)

    num_mag_0 = length (which (inp_err == 0))
        cat ("\nnum_mag_0 for err_mag = ", num_mag_0, "\n\n")

    #----------

        #  Set infinite values to NA.
    err_mag [is.infinite (err_mag)] = NA

    mean_err_mag = mean (err_mag, na.rm = TRUE)
    sd_err_mag = sd (err_mag, na.rm = TRUE)

    median_err_mag = median (err_mag, na.rm = TRUE)
    mad_err_mag = mad (err_mag, na.rm = TRUE)

    cat (rs_name, " - Error magnification statistics (i.e., outErr/inErr):",
         "\n\n  mean = ", mean_err_mag,
         "\n    sd = ", sd_err_mag,
         "\n\n  median = ", median_err_mag,
         "\n    mad = ", mad_err_mag,
         "\n\n", sep='')

    cat ("Quantiles:\n")
    print (quantile (err_mag, na.rm = TRUE))

#browser()
    }

compute_and_print_mag_stats <- function (rs_name,
                                         out_err,
                                         inp_err

                                         , rsr_UUID
                                         , rsp_combined_err_label
                                         )
    {
    err_mag = out_err / inp_err

inf_locs = which (is.infinite (err_mag))
if (length (inf_locs) > 0)
    {
    for (iii in 1:length (inf_locs))
        {
        cat ("\nInf err_mag [", inf_locs[iii],
             "], rsr_UUID = ", rsr_UUID [inf_locs[iii]], ", ", rsp_combined_err_label [inf_locs[iii]], ":  inp_err = ", inp_err[inf_locs[iii]], ",
             out_err = ", out_err[inf_locs[iii]])
        }
    }


    compute_and_print_na_inf_stats (rs_name,
                                    out_err,
                                    inp_err,
                                    err_mag,
                                    "FOR ALL DATA")

    # cat ("\n\n------------------------------------------------")
    # cat ("\n\n-----  FOR ALL DATA  -----")
    # num_mag_na = length (which (is.na (err_mag)))
    # cat ("\n\nnum_mag_na for err_mag = ", num_mag_na)
    # num_inf = length (which (is.infinite (err_mag)))
    # cat ("\nnum_inf for err_mag = ", num_inf, "\n\n")
    # err_mag [is.infinite (err_mag)] = NA
    #
    # mean_err_mag = mean (err_mag, na.rm = TRUE)
    # sd_err_mag = sd (err_mag, na.rm = TRUE)
    #
    # median_err_mag = median (err_mag, na.rm = TRUE)
    # mad_err_mag = mad (err_mag, na.rm = TRUE)
    #
    # cat (rs_name, " - Error magnification statistics (i.e., outErr/inErr):",
    #      "\n\n  mean = ", mean_err_mag,
    #      "\n    sd = ", sd_err_mag,
    #      "\n\n  median = ", median_err_mag,
    #      "\n    mad = ", mad_err_mag,
    #      "\n\n", sep='')
    #
    # cat ("Quantiles:\n")
    # print (quantile (err_mag, na.rm = TRUE))

#--------------------

    # cat ("\n\n------------------------------------------------")
    # cat ("\n\n-----  FOR input error >= 0.01  -----")

    ge1_indices = which (inp_err >= 0.01)
    num_indices_ge1 = length (ge1_indices)
    cat ("\n\nnum_indices_ge1 = ", num_indices_ge1)
    err_mag = err_mag [ge1_indices]

    compute_and_print_na_inf_stats (rs_name,
                                    out_err,
                                    inp_err,
                                    err_mag,
                                    "FOR input error >= 0.01")

    # num_mag_na = length (which (is.na (err_mag)))
    # cat ("\n\nnum_mag_na for err_mag = ", num_mag_na)
    #
    # num_inf = length (which (is.infinite (err_mag)))
    # cat ("\nnum_inf for err_mag = ", num_inf, "\n\n")
    # err_mag [is.infinite (err_mag)] = NA
    #
    # mean_err_mag = mean (err_mag, na.rm = TRUE)
    # sd_err_mag = sd (err_mag, na.rm = TRUE)
    #
    # median_err_mag = median (err_mag, na.rm = TRUE)
    # mad_err_mag = mad (err_mag, na.rm = TRUE)
    #
    # cat (rs_name, " - Error magnification statistics (i.e., outErr/inErr):",
    #      "\n\n  mean = ", mean_err_mag,
    #      "\n    sd = ", sd_err_mag,
    #      "\n\n  median = ", median_err_mag,
    #      "\n    mad = ", mad_err_mag,
    #      "\n\n", sep='')
    #
    # cat ("Quantiles:\n")
    # print (quantile (err_mag, na.rm = TRUE))
    }

#===============================================================================

gg_multiple_err_amts <- function (rs_name, base_path, suffix,
                                  num_sets = 4,
                                  num_variants_per_set = 20,
                                  num_combined_err_labels = 10)
    {
    sorted_msa_tib = load_input_data (rs_name, base_path, suffix)

    compute_and_print_mag_stats (rs_name,
                                 sorted_msa_tib$rsr_COR_euc_out_err_frac,
                                 sorted_msa_tib$rsp_euc_realized_Ftot_and_cost_in_err_frac

                                 , sorted_msa_tib$rsr_UUID
                                 , sorted_msa_tib$rsp_combined_err_label)

    return (sorted_msa_tib)
    }

#===============================================================================

gg_eucInTot_vs_eucOutTot_all_on_1 <- function (rs_name,
                                               sorted_msa_tib,
                                               ref_y = 0)
    {
ggplot (data = sorted_msa_tib) +
        aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac,
                                    y = rsr_COR_euc_out_err_frac,
        #                            color = rsp_combined_err_label)) +
                                    color = rsp_base_wrap_str) +
        geom_point() +
        scale_color_manual(breaks = c("Base", "Wrap"), values=c("red", "blue")) +
        stat_smooth(method = "lm", col = "green") +

    # ggplot (data = sorted_msa_tib) +
    #       geom_point (mapping = ) +
    #     #scale_color_viridis(discrete=TRUE) +
    #     scale_color_manual(breaks = c("Base", "Wrap"), values=c("red", "blue")) +
    #
        ggtitle (paste0 (rs_name, " - Total input error vs. Total output error")) +
        theme(plot.title = element_text(hjust = 0.5)) +    #  To center the title

          geom_hline (yintercept = ref_y, linetype="dashed",
                        color = "black", size=0.5) +
          geom_abline (intercept=0, slope=1  #, linetype, color, size
                       ) +
          geom_abline (intercept=0, slope=5  #, linetype, color, size
                       ) +
          geom_abline (intercept=0, slope=10  #, linetype, color, size
                       )
    }

#===============================================================================

gg_eucInTot_vs_eucOutTot_facetted_by_err_label <- function (rs_name,
                                                            sorted_msa_tib,
                                                            ref_y = 0)
    {
    ggplot (data = sorted_msa_tib) +
      geom_point (mapping = aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac,
                                y = rsr_COR_euc_out_err_frac,
    #                            color = rsp_combined_err_label)) +
                                color = rsp_base_wrap_str)) +
    #scale_color_viridis(discrete=TRUE) +
    #scale_color_brewer(palette="Set1") +
    scale_color_manual(breaks = c("Base", "Wrap"), values=c("red", "blue")) +

    ggtitle (paste0 (rs_name, " - Total input error vs. Total output error by Error Class")) +
    theme(plot.title = element_text(hjust = 0.5)) +    #  To center the title

      facet_wrap (~ rsp_combined_err_label, nrow = 5) +
      geom_hline (yintercept = ref_y, linetype="dashed",
                    color = "black", size=0.5) +
      geom_abline (intercept=0, slope=1  #, linetype, color, size
                   ) +
      geom_abline (intercept=0, slope=5  #, linetype, color, size
                   ) +
      geom_abline (intercept=0, slope=10  #, linetype, color, size
                   )
    }

#===============================================================================

dummy <- function()
{
#library (tidyverse)

base_path = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/"
suffix = ".combined_results.csv"

rs_name = "Gurobi"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)

rs_name = "Marxan_SA"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)

rs_name = "ZL_Backward"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)

rs_name = "ZL_Forward"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)

rs_name = "UR_Backward"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)

rs_name = "UR_Forward"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)

rs_name = "SR_Backward"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)

rs_name = "SR_Forward"
sorted_msa_tib = gg_multiple_err_amts (rs_name, base_path, suffix)
gg_eucInTot_vs_eucOutTot_all_on_1 (rs_name, sorted_msa_tib, ref_y = 0)
gg_eucInTot_vs_eucOutTot_facetted_by_err_label (rs_name, sorted_msa_tib, ref_y = 0)
}













#===============================================================================

#===============================================================================

#===============================================================================

gg_20 <- function ()
{
#===============================================================================
#===============================================================================
#===============================================================================
#  4 sets of 20 outputs from 100 gen_20_basic_variants() runs
#  on nectar and copied to glass.
#===============================================================================

### Load necessary libraries.

#library (tidyverse)

# rs_name = "Gurobi"
# infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/Gurobi.combined_results.csv"
infile = paste0 (base_path, rs_name, suffix)

# rs_name = "Marxan_SA"
# infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/Marxan_SA.combined_results.csv"

# rs_name = "ZL_Backward"
# infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/ZL_Backward.combined_results.csv"

# rs_name = "ZL_Forward"
# infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/ZL_Forward.combined_results.csv"

# rs_name = "SR_Forward"
# infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/SR_Forward.combined_results.csv"

# rs_name = "UR_Forward"
# infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/UR_Forward.combined_results.csv"

# rs_name = "UR_Backward"
# infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/UR_Backward.combined_results.csv"

rs_name = "SR_Backward"
infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_Combined_err_amts/SR_Backward.combined_results.csv"

ref_y = 0

msa_dt         = read.csv (infile, header=TRUE, stringsAsFactors = FALSE)
msa_tib        = as.tibble (msa_dt)
sorted_msa_tib = arrange (msa_tib, rsp_combined_err_label)
    #  Now that they are sorted by the 10 combined error labels,
    #  give an index to each rsrun within each of the combined error labels
    #  so that they can be spread out by index across a single facet plot.
    #  The index has no meaning other than to give it a unique identifier
    #  to spread along the x axis within each facet.
sorted_msa_tib$idx = rep (1:800, 10)    #1:dim(sorted_msa_tib)[1]    #  For the 4 combined sets 0f 100 runs each

#----------

    #  NOTE: the result of selecting for NA gives 201 instead of 200 results.
    #  I would expect 200 since there are 200 correct values that have no
    #  input error.  Not sure what 1 extra NA is from.

err_mag_euc_Ftot_in_vs_euc_Ftot_out =
    sorted_msa_tib$rsr_COR_euc_out_err_frac /
    sorted_msa_tib$rsp_euc_realized_Ftot_and_cost_in_err_frac
err_mag_euc_Ftot_in_vs_euc_Ftot_out [is.na (err_mag_euc_Ftot_in_vs_euc_Ftot_out)] = 0

mean_err_mag_euc_Ftot_in_vs_euc_Ftot_out = mean (err_mag_euc_Ftot_in_vs_euc_Ftot_out)
sd_err_mag_euc_Ftot_in_vs_euc_Ftot_out = sd (err_mag_euc_Ftot_in_vs_euc_Ftot_out)
median_err_mag_euc_Ftot_in_vs_euc_Ftot_out = median (err_mag_euc_Ftot_in_vs_euc_Ftot_out)
mad_err_mag_euc_Ftot_in_vs_euc_Ftot_out = mad (err_mag_euc_Ftot_in_vs_euc_Ftot_out)

cat ("\n\n", rs_name, " - err_mag_euc_Ftot_in_vs_euc_Ftot_out statistics:",
     "\n\n  mean = ", mean_err_mag_euc_Ftot_in_vs_euc_Ftot_out,
     "\n    sd = ", sd_err_mag_euc_Ftot_in_vs_euc_Ftot_out,
     "\n\n  median = ", median_err_mag_euc_Ftot_in_vs_euc_Ftot_out,
     "\n    mad = ", mad_err_mag_euc_Ftot_in_vs_euc_Ftot_out,
     "\n", sep='')

#===============================================================================
#===============================================================================
#===============================================================================

#library (tidyverse)

rs_name = "Gurobi"
ref_y = 0

infile_02 = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_02_err_amt/default_runset/Gurobi.csv"
msa_dt_02         = read.csv (infile_02, header=TRUE, stringsAsFactors = FALSE)

infile_05 = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_05_err_amt/default_runset/Gurobi.csv"
msa_dt_05         = read.csv (infile_05, header=TRUE, stringsAsFactors = FALSE)

#infile_075 = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_075_err_amt/default_runset/Gurobi.csv"
#msa_dt_075         = read.csv (infile_075, header=TRUE, stringsAsFactors = FALSE)
infile = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_075_err_amt/default_runset/Gurobi.csv"
msa_dt         = read.csv (infile, header=TRUE, stringsAsFactors = FALSE)

infile_10 = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/Gurobi.csv"
msa_dt_10         = read.csv (infile_10, header=TRUE, stringsAsFactors = FALSE)

infile_15 = "/Users/bill/D/Projects/ProblemDifficulty/Results/bdpg_20_variants_all_rs_easy_base/bdpg_20_variants_all_rs_easy_base_15_err_amt/default_runset/Gurobi.csv"
msa_dt_15         = read.csv (infile_15, header=TRUE, stringsAsFactors = FALSE)


msa_dt = rbind (msa_dt_02, msa_dt_05, msa_dt_075)

msa_tib        = as.tibble (msa_dt)
sorted_msa_tib = arrange (msa_tib, rsp_combined_err_label)
    #  Now that they are sorted by the 10 combined error labels,
    #  give an index to each rsrun within each of the combined error labels
    #  so that they can be spread out by index across a single facet plot.
    #  The index has no meaning other than to give it a unique identifier
    #  to spread along the x axis within each facet.
#sorted_msa_tib$idx = rep (1:600, 10)    #1:dim(sorted_msa_tib)[1]    #  For the single set of 100 runs
sorted_msa_tib$idx = rep (1:200, 10)    #1:dim(sorted_msa_tib)[1]    #  For the single set of 100 runs



msa_tib$rsp_combined_err_label [msa_tib$rsp_original_FP_const_rate == 0.05]
#===============================================================================
#===============================================================================
#===============================================================================
#  1 set of 20 outputs from 100 gen_20_basic_variants() runs
#  on nectar and copied to glass.
#===============================================================================

#library (tidyverse)
#library (viridis)

rs_name = "Gurobi"
infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/Gurobi.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/Marxan_SA.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/SR_Forward.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/SR_Backward.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/ZL_Backward.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/ZL_Forward.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/UR_Forward.csv"
#infile = "/Users/bill/tzar/outputdata/bdpg_20_variants_all_rs_easy_base_10_err_amt/default_runset/UR_Backward.csv"

ref_y = 0.10
msa_dt         = read.csv (infile, header=TRUE, stringsAsFactors = FALSE)

#===============================================================================
#===============================================================================
#===============================================================================

msa_tib        = as.tibble (msa_dt)
sorted_msa_tib = arrange (msa_tib, rsp_combined_err_label)
    #  Now that they are sorted by the 10 combined error labels,
    #  give an index to each rsrun within each of the combined error labels
    #  so that they can be spread out by index across a single facet plot.
    #  The index has no meaning other than to give it a unique identifier
    #  to spread along the x axis within each facet.
sorted_msa_tib$idx = rep (1:200, 10)    #1:dim(sorted_msa_tib)[1]    #  For the single set of 100 runs

#===============================================================================
#===============================================================================
#===============================================================================

ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = rsp_euc_realized_Ftot_and_cost_in_err_frac, y = rsr_COR_euc_out_err_frac,
#                            color = rsp_combined_err_label)) +
                            color = rsp_base_wrap_str)) +
#scale_color_viridis(discrete=TRUE) +
scale_color_manual(breaks = c("Base", "Wrap"), values=c("red", "blue")) +

ggtitle (paste0 (rs_name, " - Total input error vs. Total output error")) +
theme(plot.title = element_text(hjust = 0.5)) +    #  To center the title

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
#scale_color_viridis(discrete=TRUE) +
#scale_color_brewer(palette="Set1") +
scale_color_manual(breaks = c("Base", "Wrap"), values=c("red", "blue")) +

ggtitle (paste0 (rs_name, " - Total input error vs. Total output error by Error Class")) +
theme(plot.title = element_text(hjust = 0.5)) +    #  To center the title

  facet_wrap (~ rsp_combined_err_label, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_abline (intercept=0, slope=1  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=5  #, linetype, color, size
               ) +
  geom_abline (intercept=0, slope=10  #, linetype, color, size
               )




#----------


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
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------
#----------------------------------------

#  FP and cost in err vs tot out err
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




#  FN and cost in err vs tot out err
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

#  median abs cost err vs tot out err
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

# index vs tot out err colored by Err Label, facetted by Base/Wrap side by side
ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac,
                            color = rsp_combined_err_label)) +
#scale_color_viridis(discrete=TRUE) +
  facet_wrap (~ rsp_base_wrap_str) +
#  facet_wrap (~ rsp_base_wrap_str, nrow = 2) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5)

# index vs tot out colored by Err Label, facetted by Base/Wrap one above the other
ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac,
                            color = rsp_combined_err_label)) +
#scale_color_viridis(discrete=TRUE) +
#  facet_wrap (~ rsp_base_wrap_str) +
  facet_wrap (~ rsp_base_wrap_str, nrow = 2) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5)



#----------------------------------------




# ggplot (data = sorted_msa_tib) +
#   geom_point (mapping = aes(x = idx, y = rsr_COR_euc_out_err_frac,
#                             color = rsp_base_wrap_str)) +
#   geom_hline (yintercept = ref_y, linetype="dashed",
#                 color = "black", size=0.5)



# index vs tot out err facetted by Combined Err Label, colored by Base/Wrap
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



# index vs cost out err colored by Base/Wrap, facetted by Combined Err Label
ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rs_solution_cost_err_frac,
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5) +
  geom_hline (yintercept = -ref_y, linetype="dashed",
                color = "black", size=0.5)


# index vs abs cost out err colored by Base/Wrap, facetted by Combined Err Label
ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = abs_rs_solution_cost_err_frac,
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5)



# index vs rep shortfall err colored by Base/Wrap, facetted by Combined Err Label
ggplot (data = sorted_msa_tib) +
  geom_point (mapping = aes(x = idx, y = rsr_APP_spp_rep_shortfall,
                            color = rsp_base_wrap_str)) +
  facet_wrap (~ rsp_combined_err_label, nrow = 5) +
  geom_hline (yintercept = ref_y, linetype="dashed",
                color = "black", size=0.5)



#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
#===============================================================================
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


#library (tidyverse)

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


