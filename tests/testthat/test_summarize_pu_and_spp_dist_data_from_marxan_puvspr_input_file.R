#===============================================================================

#    test_summarize_pu_and_spp_dist_data_from_marxan_puvspr_input_file.R

#===============================================================================

test_summarize_pu_and_spp_dist_data_from_marxan_puvspr_input_file <- function ()
    {
    infile_path <- "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1834_marxan_simulated_annealing.completedTzarEmulation/RSrun_-COR-Wrap-Marxan_SA.408782d5-2bb6-4c09-8b18-f05ad3089b15/input/puvspr.dat"
    pu_and_spp_dist_data <-
        summarize_pu_and_spp_dist_data_from_marxan_puvspr_input_file (infile_path)
    }

#===============================================================================

