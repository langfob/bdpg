test_compute_num_spp_covered_by_solution <- function ()
    {
    rs_solution = c(1,3)
    bpm = matrix (c (1,0,0,1, 0,1,0,0, 0,0,1,0), nrow=4, ncol=3)
    spp_rep_targets = c(1,1,1,1)

    compute_num_spp_covered_by_solution (rs_solution,
                                         bpm,
                                         spp_rep_targets)
    }

