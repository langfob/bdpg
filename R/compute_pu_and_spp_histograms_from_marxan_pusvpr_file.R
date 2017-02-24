infile_name <- "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1834_marxan_simulated_annealing.completedTzarEmulation/RSrun_-COR-Wrap-Marxan_SA.408782d5-2bb6-4c09-8b18-f05ad3089b15/input/puvspr.dat"

pu_spp_amt_df <- read.csv (infile_name, header=TRUE)


    #  Currently, these ignore the amount of each species and just
    #  count each patch occurrence as an abundance of 1.
    #  Should probably do it both ways.


# grouped_by_pu <- dplyr::group_by (pu_spp_amt_df, pu)
# #num_spp_per_patch <- dplyr::count_ (grouped_by_pu, pu)
# num_spp_per_patch <- dplyr::count_ (pu_spp_amt_df, pu)
# hist (num_spp_per_patch$n)
#
# grouped_by_spp <- dplyr::group_by (pu_spp_amt_df, species)
# num_PUs_per_spp <- dplyr::count_ (grouped_by_spp, species)
# hist (num_PUs_per_spp$n)

num_spp_per_patch <- dplyr::count (pu_spp_amt_df, pu)
hist_num_spp_per_patch <- hist (num_spp_per_patch$n)

num_PUs_per_spp <- dplyr::count (pu_spp_amt_df, species)
hist_num_PUs_per_spp <- hist (num_PUs_per_spp$n)
