#===============================================================================
#
#                        gen_lognormal_overlay_tests.R
#
#===============================================================================

test_calculate_mu <- function ()
    {
    cat ("\n\nstart test_calculate_mu()\n\n")

    for (num_PUs_per_spp_ie_rarity in c(2,10,50,100))    #c(2, 50))    #
        {
        for (num_spp_with_given_num_PUs in c(1, 10,50,100))    #c(1,50))    #
            {
            for (sigma in c(0.01, 0.1, 0.5, 1, 2, 5, 10))    #c(0.1, 1))    #
                {
                for (plusOrMinus1 in c(1, -1))
                    {
                    try (
                        {
                        mu <- calculate_mu (num_PUs_per_spp_ie_rarity,
                                            num_spp_with_given_num_PUs,
                                            sigma,
                                            plusOrMinus1)
                        cat ("mu = ", mu, "\n", sep='')
                        }
                        )  #  end - try()
                    }  #  end - for plusOrMinus1
                }  #  end - for sigma
            }  #  end - for num_spp_with_given_num_PUs
        }  #  end - for num_PUs_per_spp_ie_rarity

    cat ("\nend test_calculate_mu()\n\n")
    }  #  end - function test_mu_calc

TEST_MU_CALC = TRUE
if (TEST_MU_CALC)  test_calculate_mu()

#===============================================================================
#
#                               test_EF ()
#
#  Test function to exercise the evaluation function, i.e., EF().
#
#  Just calls the EF repeatedly for the same input parameters.
#  Plots the scores to verify that there is variation.

#  Does not do any optimization to search for a particular kind of result.
#
#===============================================================================

test_EF = function ()
    {
    seed_value = 17
    num_spp_to_generate = 100
    min_num_spp_on_2_PUs = 20
    max_max_abundance = 40
    target_max_abundance_ct = 30
    mean_sd_pair = c (1.3, 0.6)

    num_iterations = 20
    add_one_to_abundances = FALSE

    set.seed (119)

    min_meanlog = 0.5
    max_meanlog = 1.5
    meanlogs = runif (num_iterations, min=min_meanlog, max=max_meanlog)

    min_sdlog = 0.5
    max_sdlog = 1.0
    sdlogs   = runif (num_iterations, min=min_sdlog, max=max_sdlog)

    outfile = "/Users/bill/D/Projects/ProblemDifficulty/src/bdprobdiff/R/outfile.txt"
    cat ("seed_value,num_spp_to_generate,min_num_spp_on_2_PUs,max_max_abundance,target_max_abundance_ct,score",
         file=outfile, sep="\n")

    scores = rep (NA, num_iterations)

    for (cur_idx in 1:num_iterations)
        {
        cat ("\n\n------------------------------------------------------------")
        cat ("\n\ncur_idx = ", cur_idx)

        mean_sd_pair = c (meanlogs [cur_idx], sdlogs [cur_idx])

        ret_score = EF (seed_value,
                        num_spp_to_generate,
                        min_num_spp_on_2_PUs,
                        max_max_abundance,
                        target_max_abundance_ct,
                        add_one_to_abundances,
                        outfile,
                        mean_sd_pair)

        cat ("\n\n=====> ret_score = ", ret_score, "\n\n", sep='')

        scores [cur_idx] = ret_score
        }

        #----------------------------------------------------------------
        #  Plot the resulting scores.
        #  Some of the scores may be huge (e.g., ones that are supposed
        #  to be infinite) and this will blow up the plotting routine,
        #  so cap the scores at some arbitrary level for plotting.
        #----------------------------------------------------------------

    biggest_score_to_plot = 2
    big_score_indices = which (scores > biggest_score_to_plot)
    capped_scores = scores
    capped_scores [big_score_indices] = biggest_score_to_plot

    plot (capped_scores)

    }  #  end function - test_EF ()

#----------

if (FALSE)
    {
    test_EF ()
    }

#===============================================================================
#
#                           test search
#
#  Test the use of the optimizer to search for a meanlog, sdlog pair
#  that generates a lognormal distribution as close as possible to the
#  specified number of species on 2 PUs and largest abundance count.
#
#===============================================================================

test_search_for_lognormal <- function ()
    {
    #--------------------------------------------------------------------------
    #  Start by specifying the test parameters to be passed to the optimizer.
    #--------------------------------------------------------------------------

    seed_value = 17
    num_spp_to_generate = 100
    min_num_spp_on_exactly_2_PUs = 20
    max_max_abundance = 40
    target_max_abundance_ct = 30
    initial_meanlog = 0.5162258
    initial_sdlog = 0.7986151
    add_one_to_abundances = TRUE
    max_iterations = 500
    outfile = "/Users/bill/D/Projects/ProblemDifficulty/src/bdprobdiff/R/outfile.txt"

    search_for_approximating_lognormal (seed_value,
                                        num_spp_to_generate,
                                        min_num_spp_on_2_PUs,
                                        max_max_abundance,
                                        target_max_abundance_ct,
                                        initial_meanlog,
                                        initial_sdlog,
                                        max_iterations,
                                        add_one_to_abundances,
                                        outfile
                                        )

    }  #  end function - test_search_for_lognormal ()

#----------

if (FALSE)
    {
    lognormal_result = test_search_for_lognormal ()
    cat ("\n\nFinal lognormal_result = \n")
    print (lognormal_result)
    }

#===============================================================================

get_one_lognormal_distribution = function (seed_value,
                                           meanlog,
                                           sdlog,
                                           num_spp_to_generate,
                                           add_one_to_abundances)
    {
#     cat ("\n\nStarting EF:",
#          "\n    seed_value              = ", seed_value,
#          "\n    num_spp_to_generate     = ", num_spp_to_generate,
#          "\n    add_one_to_abundances   = ", add_one_to_abundances,
#          "\n    meanlog                 = ", meanlog,
#          "\n    sdlog                   = ", sdlog,
#          "\n", sep='')

    set.seed (seed_value)
    abundance_data = gen_rounded_abundances (num_spp_to_generate,
                                             meanlog,
                                             sdlog,
                                             add_one_to_abundances,
                                             plot_rounded_abundances=FALSE)

    num_spp_on_exactly_2_patches = abundance_data$num_spp_on_exactly_2_patches
    max_abundance_ct             = abundance_data$max_abundance_ct

#     cat ("\n\n                num_spp_on_exactly_2_patches        = ", num_spp_on_exactly_2_patches, sep='')
#     cat ("\n                max_abundance_ct                    = ", max_abundance_ct, sep='')

    return (list (rounded_abundances = abundance_data$rounded_abundances,
                  num_spp_on_exactly_2_patches = num_spp_on_exactly_2_patches,
                  max_abundance_ct = max_abundance_ct))
    }

#-------------------------------------------------------------------------------

library (tcltk)  #  https://ryouready.wordpress.com/2009/03/16/r-monitor-function-progress-with-a-progress-bar/

build_lognormal_table = function (num_spp_to_generate = 100,
                                  meanlogs = c(0,1,2),
                                  sdlogs = c(0,1,2),
                                  num_seed_reps = 100,
                                  table_file = "table.csv",
                                  summary_file = "summary.csv",
                                  append_to_existing_files = FALSE
                                  )
    {
#    num_pairs_meanlog_cross_sdlog = length (meanlogs) * length (sdlogs)
    total = length (meanlogs) * length (sdlogs)
        # create progress bar
    pb <- tkProgressBar (title = "progress bar", min = 0,
                         max = total,
                         width = 300)

    if (! is.null (table_file) & ! append_to_existing_files)
        {
            #  Write the column headers to the outfile.
        cat ("num_spp_to_generate,cur_meanlog,cur_sdlog,cur_seed_value,on_exactly_2,max_abund,on_exactly_2_plus_1,max_abund_plus_1",
             file=table_file, sep="\n")
        }

    if (! is.null (summary_file) & ! append_to_existing_files)
        {
            #  Write the column headers to the outfile.
        cat ("num_spp_to_generate,cur_meanlog,cur_sdlog,cur_seed_value,median_on_exactly_2,mad_on_exactly_2,mean_on_exactly_2,sd_on_exactly_2,median_max_abund,mad_max_abund,mean_max_abund,sd_max_abund,median_on_exactly_2_plus_1,mad_on_exactly_2_plus_1,mean_on_exactly_2_plus_1,sd_on_exactly_2_plus_1,median_max_abund_plus_1,mad_max_abund_plus_1,mean_max_abund_plus_1,sd_max_abund_plus_1)",
             file=summary_file, sep="\n")
        }

    cat ("\n\n")
    cur_mean_sd_pair_idx = 0
    for (cur_meanlog in meanlogs)
        {
        cat ("\n    cur_mean = ", cur_meanlog)
        for (cur_sdlog in sdlogs)
            {
            cat ("\n        cur_sdlog = ", cur_sdlog)

            on_exactly_2_reps = rep (NA, num_seed_reps)
            max_abund_reps = rep (NA, num_seed_reps)
            on_exactly_2_plus_1_reps = rep (NA, num_seed_reps)
            max_abund_plus_1_reps = rep (NA, num_seed_reps)

            for (cur_idx in 1:num_seed_reps)
                {
                cur_seed_value = cur_idx

#                cat ("\n            cur_seed_value = ", cur_seed_value)

                abund_data =
                    get_one_lognormal_distribution (cur_seed_value,
                                                    cur_meanlog,
                                                    cur_sdlog,
                                                    num_spp_to_generate,
                                                    add_one_to_abundances=FALSE)

                on_exactly_2_reps [cur_idx]      = abund_data$num_spp_on_exactly_2_patches
                max_abund_reps [cur_idx] = abund_data$max_abundance_ct

                rounded_abundances_plus_1              = abund_data$rounded_abundances + 1
                num_spp_on_exactly_2_patches_on_plus_1 = length (which (rounded_abundances_plus_1 == 2))
                max_abundance_ct_on_plus_1             = max (rounded_abundances_plus_1)
#                 cat ("\n                PLUS 1:")
#                 cat ("\n                num_spp_on_exactly_2_patches_on_plus_1        = ", num_spp_on_exactly_2_patches_on_plus_1, sep='')
#                 cat ("\n                max_abundance_ct_on_plus_1                    = ", max_abundance_ct_on_plus_1, sep='')
#                 cat ("\n")

                on_exactly_2_plus_1_reps [cur_idx]      = num_spp_on_exactly_2_patches_on_plus_1
                max_abund_plus_1_reps [cur_idx] = max_abundance_ct_on_plus_1

                reps_out_record = paste (num_spp_to_generate,
                                         cur_meanlog,
                                         cur_sdlog,
                                         cur_seed_value,

                                         on_exactly_2_reps [cur_idx],
                                         max_abund_reps [cur_idx],

                                         on_exactly_2_plus_1_reps [cur_idx],
                                         max_abund_plus_1_reps [cur_idx],

                                         sep=',')

                cat (reps_out_record, file=table_file, sep="\n", append=TRUE)

                }  #  end for - cur_idx in num_reps

            median_on_exactly_2 = median (on_exactly_2_reps)
            mad_on_exactly_2   = mad  (on_exactly_2_reps)
            mean_on_exactly_2 = mean (on_exactly_2_reps)
            sd_on_exactly_2   = sd (on_exactly_2_reps)

            median_max_abund    = median (max_abund_reps)
            mad_max_abund       = mad  (max_abund_reps)
            mean_max_abund    = mean (max_abund_reps)
            sd_max_abund      = sd (max_abund_reps)

#             cat ("\n\n        median_on_exactly_2 = ", median_on_exactly_2,
#                  ", mad_on_exactly_2 = ", mad_on_exactly_2)
#             cat ("\n        mean_on_exactly_2 = ", mean_on_exactly_2,
#                  ", sd_on_exactly_2 = ", sd_on_exactly_2)
#             cat ("\n        median_max_abund = ", median_max_abund,
#                  ", mad_max_abund = ", mad_max_abund)
#             cat ("\n        mean_max_abund = ", mean_max_abund,
#                  ", sd_max_abund = ", sd_max_abund)

            median_on_exactly_2_plus_1 = median (on_exactly_2_plus_1_reps)
            mad_on_exactly_2_plus_1   = mad  (on_exactly_2_plus_1_reps)
            mean_on_exactly_2_plus_1 = mean (on_exactly_2_plus_1_reps)
            sd_on_exactly_2_plus_1   = sd (on_exactly_2_plus_1_reps)

            median_max_abund_plus_1    = median (max_abund_plus_1_reps)
            mad_max_abund_plus_1       = mad  (max_abund_plus_1_reps)
            mean_max_abund_plus_1    = mean (max_abund_plus_1_reps)
            sd_max_abund_plus_1      = sd (max_abund_plus_1_reps)

#             cat ("\n        PLUS 1:")
#             cat ("\n        median_on_exactly_2_plus_1 = ", median_on_exactly_2_plus_1,
#                  ", mad_on_exactly_2_plus_1 = ", mad_on_exactly_2_plus_1)
#             cat ("\n        mean_on_exactly_2_plus_1 = ", mean_on_exactly_2_plus_1,
#                  ", sd_on_exactly_2_plus_1 = ", sd_on_exactly_2_plus_1)
#             cat ("\n        median_max_abund_plus_1 = ", median_max_abund_plus_1,
#                  ", mad_max_abund_plus_1 = ", mad_max_abund_plus_1)
#             cat ("\n        mean_max_abund_plus_1 = ", mean_max_abund_plus_1,
#                  ", sd_max_abund_plus_1 = ", sd_max_abund_plus_1)

            summary_out_record = paste (num_spp_to_generate,
                                                cur_meanlog,
                                                cur_sdlog,
                                                cur_seed_value,

                                                median_on_exactly_2,
                                                mad_on_exactly_2,
                                                mean_on_exactly_2,
                                                sd_on_exactly_2,
                                                median_max_abund,
                                                mad_max_abund,
                                                mean_max_abund,
                                                sd_max_abund,

                                                median_on_exactly_2_plus_1,
                                                mad_on_exactly_2_plus_1,
                                                mean_on_exactly_2_plus_1,
                                                sd_on_exactly_2_plus_1,

                                                median_max_abund_plus_1,
                                                mad_max_abund_plus_1,
                                                mean_max_abund_plus_1,
                                                sd_max_abund_plus_1,
                                        sep=',')

            cat (summary_out_record, file=summary_file, sep="\n", append=TRUE)

            cur_mean_sd_pair_idx = cur_mean_sd_pair_idx + 1
            setTkProgressBar (pb, cur_mean_sd_pair_idx,
                              label=paste( round((cur_mean_sd_pair_idx/total)*100, 0),
                                        "% done"))

            }  #  end for - cur_sdlog in sdlogs
        }  #  end for - cur_meanlog in meanlogs

    cat ("\n\n")
    }

#-------------------------------------------------------------------------------

if (FALSE)
    {
    meanlogs = seq (0, 10.0, 1)    #  Tiny divisions (0.01) => long run time
    sdlogs = seq (0, 10.0, 1)    #  Tiny divisions (0.01) => long run time

    build_lognormal_table (num_spp_to_generate = 100,
                          meanlogs = meanlogs,
                          sdlogs = sdlogs,
                          num_seed_reps = 100,
                          table_file = "table.csv",
                          summary_file = "summary.csv",
                          append_to_existing_files = FALSE)

        #  Look up a meanlog,sdlog pair in the table.

    xxx = read.csv("table.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
#     yyy = which ((xxx["on_exactly_2"] == 10) & (xxx["max_abund"] == 11))
#     length (yyy)
#     xxx [yyy, c("cur_meanlog","cur_sdlog","cur_seed_value","on_exactly_2","max_abund")]

    pair_ct = count (xxx, c("on_exactly_2","max_abund"))
    library(ggplot2)
    ggplot(data = pair_ct, aes(x=on_exactly_2, y=max_abund, fill=freq)) + geom_tile()

    yyy = which ((pair_ct [,"on_exactly_2"] < 10) & (pair_ct [,"max_abund"] < 100))
    zzz = pair_ct [yyy,]

        #  Just noticed this ggplot code that I should try too:
        #  http://is-r.tumblr.com/post/32387034930/simplest-possible-heatmap-with-ggplot2
    }

#  Here are the values that caused the search routine to fail to get
#  any score except the max:
#
# Browse[1]> seed_value_for_search
# [1] 11
# Browse[1]> num_spp_to_generate
# [1] 1628
# Browse[1]> min_num_spp_on_2_PUs
# [1] 814
# Browse[1]> max_max_abundance
# [1] 122
# Browse[1]> target_max_abundance_ct
# [1] 814
# Browse[1]> initial_meanlog_for_search
# [1] 0.5162258
# Browse[1]> initial_sdlog_for_search
# [1] 0.7986151

#===============================================================================

#  optimization
#       - http://zoonek.free.fr/blosxom/R/2012-06-01_Optimization.html

if (FALSE)    #  Show search trajectories for meanlog and sdlog over one search.
    {
                   #  search_outfile_name
    efout = read.csv("./R/outfile.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)

    plot (efout[,"meanlog"],xlim=c(0,40),ylim=c(0,1),type='l',col='blue',
          xlab='EF iteration', ylab='mean and sd log')
    lines (efout[,"sdlog"],col='red')
    legend ("bottomleft", legend = c("meanlog","sdlog"), col=c("blue","red"), lty=1) # optional legend
    }

#===============================================================================

