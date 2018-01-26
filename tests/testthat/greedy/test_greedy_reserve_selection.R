#===============================================================================

                    #  test_greedy_reserve_selection.R

#===============================================================================

gen_dummy_bpm <- function (num_spp=4, num_PUs=3, seed = 456)
    {
    set.seed (seed)

    bpm = matrix (0, nrow=num_spp, ncol=num_PUs)
                                                                                if (verbose) {
                                                                                cat ("\ndim(bpm) = ", dim(bpm))
                                                                                }
    for (cur_spp in 1:num_spp)
        {
        occ_PUs_for_this_spp = sample (1:num_PUs, 2, replace=FALSE)

        bpm [cur_spp, occ_PUs_for_this_spp] = 1
        }

    return (bpm)
    }

#===============================================================================


