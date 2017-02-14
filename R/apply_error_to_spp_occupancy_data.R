#===============================================================================

                #  source ("apply_error_to_spp_occupancy_data.R")

#===============================================================================

#  History

#  BTL - 2015 04 30 - Created.

#===============================================================================

    #  Walk through the occupancy matrix (PU vs spp) and randomly
    #  choose to flip some of the TPs to FNs and TNs to FPs based on the
    #  given FP and FN error rates.
    #  Update the occupancy matrix (bpm) as you go along, but don't update
    #  the PU_spp_pair_indices until you know all locations that have
    #  flipped.  Update PU_spp_pair_indices at the end so that you don't
    #  have to constantly resize this big array as you go along.

apply_const_error_to_spp_occupancy_data =
        function (bpm, FP_rates, FN_rates, num_PUs, num_spp,
                  random_values,   #  passing these in to make it easier to test
                                   #  in a reproducible way
                  bdpg_error_codes
                  )
    {
    cat ("\nStarting apply_const_error_to_spp_occupancy_data loop.\n\n")

    for (cur_spp_row in 1:num_spp)
        {
        for (cur_PU_col in 1:num_PUs)
            {
#             cat ("\n[", cur_spp_row, ",",
#                  cur_PU_col,
#                  "]", sep='')
            if (bpm [cur_spp_row, cur_PU_col])
                {
                    #  TP:  This species DOES exist on this planning unit.
                    #       Randomly choose whether to replace a given TP
                    #       with a false negative (FN)
                    #       i.e., simulate not detecting that spp on that PU.
                if (random_values [cur_spp_row, cur_PU_col] < FN_rates [cur_spp_row, cur_PU_col])
                    bpm [cur_spp_row, cur_PU_col] = 0

                }  else
                {
                    #  TN:  This species does NOT exist on this planning unit.
                    #       Randomly choose whether to replace a given TN
                    #       with a false positive (FP).
                if (random_values [cur_spp_row, cur_PU_col] < FP_rates [cur_spp_row, cur_PU_col])
                    bpm [cur_spp_row, cur_PU_col] = 1

               }  #  end else - TN so set FP
            }  #  end for - all PU cols
        }  #  end for - all spp rows

    return (bpm)
    }

#===============================================================================

set_const_FP_and_FN_rates = function (parameters, bdpg_error_codes)
    {
        #----------------------------
        #  Set False Positive rate.
        #----------------------------

    spp_occ_FP_error_type = parameters$spp_occ_FP_error_type

    FP_const_rate = NA
    if (spp_occ_FP_error_type == "CONSTANT")
        {
        FP_const_rate = parameters$spp_occ_FP_const_rate

        } else if (spp_occ_FP_error_type == "RANDOM_UNIFORM_CONSTANT")
        {
        lower_bound = parameters$spp_occ_FP_rate_lower_bound
        upper_bound = parameters$spp_occ_FP_rate_upper_bound

        FP_const_rate = runif (1, min=lower_bound, max=upper_bound)

        } else  #  unknown type of error to add
        {
        cat ("\n\nERROR: Unknown spp_occ_FP_error_type = '",
             spp_occ_FP_error_type, "'.\n", sep='')
        quit (save="no", bdpg_error_codes$ERROR_STATUS_unknown_spp_occ_FP_error_type)
        }

        #----------------------------
        #  Set False Negative rate.
        #----------------------------

    spp_occ_FN_error_type = parameters$spp_occ_FN_error_type

    FN_const_rate = NA
    if (spp_occ_FN_error_type == "CONSTANT")
        {
        FN_const_rate = parameters$spp_occ_FN_const_rate

        } else if (spp_occ_FN_error_type == "RANDOM_UNIFORM_CONSTANT")
        {
        lower_bound = parameters$spp_occ_FN_rate_lower_bound
        upper_bound = parameters$spp_occ_FN_rate_upper_bound

        FN_const_rate = runif (1, min=lower_bound, max=upper_bound)

        } else  #  unknown type of error to add
        {
        cat ("\n\nERROR: Unknown spp_occ_FN_error_type = '",
             spp_occ_FN_error_type, "'.\n", sep='')
        quit (save="no", bdpg_error_codes$ERROR_STATUS_unknown_spp_occ_FN_error_type)
        }

    #--------------------

    return (list (FP_const_rate = FP_const_rate,
                  FN_const_rate = FN_const_rate))
    }

#===============================================================================

    #  Usually, the number of TNs and TPs will be unbalanced.
    #  Therefore, there will be far more opportunities to inject
    #  FPs than FNs or vice versa.
    #  Consequently, even if the FP and FN rates are set to the
    #  same value, there are likely to be far more FPs than FNs
    #  or vice versa in the apparent matrix.
    #  If you want to keep the opportunities for each of them
    #  to be more balanced, then you can multiply the dominant
    #  one by the lesser one's fraction of occurrence and
    #  reset the rate for the dominant so that both end up
    #  with the same counts.
    #
    #  Example: if there are
    #       - 100 entries total
    #       - 70 TNs
    #       - 30 TPs
    #  and you want 0.1 probability of FN, then you should get
    #  approximately 3 FNs.  If you want the FPs to match the FNs,
    #  then x * 70 FPs must equal 3 FPs too.
    #  So, the multiplier x = 3 / 70 ~ 0.0429
    #  i.e., the adjusted_P(FP) = num_FNs / num_TNs

match_FP_and_FN_counts_to_smaller_of_the_two = function (num_TPs, num_TNs,
                                      FP_const_rate, FN_const_rate)
    {
    approx_num_FNs = round (FN_const_rate * num_TPs)
    approx_num_FPs = round (FP_const_rate * num_TNs)

    cat ("\n\nBefore matching of FP and FN const_rates:",
         "\n\tnum_TPs = ", num_TPs,
         "\n\tnum_TNs = ", num_TNs,
         "\n\tapprox_num_FNs = ", approx_num_FNs,
         "\n\tapprox_num_FPs = ", approx_num_FPs,
         "\n")

    if ((num_TNs > 0) & (num_TPs > 0))
        {
        if (num_TNs > num_TPs)
            {
            FP_const_rate = approx_num_FNs / num_TNs

            } else
            {
            FN_const_rate = approx_num_FPs / num_TPs
            }
        } else
        {
        cat ("\n\nNot matching FP and FN counts since num_TNs or num_TPs = 0.\n")
        }

    approx_num_FNs = round (FN_const_rate * num_TPs)
    approx_num_FPs = round (FP_const_rate * num_TNs)

    cat ("\n\nAfter matching of FP and FN const_rates:",
         "\n\tFP_const_rate = ", FP_const_rate,
         "\n\tFN_const_rate = ", FN_const_rate,
         "\n\tapprox_num_FNs = ", approx_num_FNs,
         "\n\tapprox_num_FPs = ", approx_num_FPs,
         "\n")


    return (list (FP_const_rate = FP_const_rate,
                  FN_const_rate = FN_const_rate))
    }

#===============================================================================

apply_error_to_spp_occupancy_data =
        function (parameters, cor_bpm, cor_num_PU_spp_pairs,
                  cor_num_PUs, cor_num_spp,
                  bdpg_error_codes)
    {
    FP_and_FN_const_rates = set_const_FP_and_FN_rates (parameters,
                                                       bdpg_error_codes)

    FP_const_rate = FP_and_FN_const_rates$FP_const_rate
    FN_const_rate = FP_and_FN_const_rates$FN_const_rate

    match_error_counts = FALSE
    if (! is.null (parameters$match_error_counts))
        match_error_counts = parameters$match_error_counts

    if (match_error_counts)
        {
        num_TPs = sum (cor_bpm)
        num_TNs = length (cor_bpm) - num_TPs

        FP_FN_const_rate_pair =
            match_FP_and_FN_counts_to_smaller_of_the_two (num_TPs, num_TNs,
                                                          FP_const_rate,
                                                          FN_const_rate)

        FP_const_rate = FP_FN_const_rate_pair$FP_const_rate
        FN_const_rate = FP_FN_const_rate_pair$FN_const_rate
        }

    FP_rates = matrix (rep (FP_const_rate, (cor_num_PUs * cor_num_spp)),
                        nrow=cor_num_spp,
                        ncol=cor_num_PUs,
                        byrow=TRUE)

    FN_rates = matrix (rep (FN_const_rate, (cor_num_PUs * cor_num_spp)),
                        nrow=cor_num_spp,
                        ncol=cor_num_PUs,
                        byrow=TRUE)

    random_values = matrix (runif (cor_num_PUs * cor_num_spp),
                            nrow=cor_num_spp,
                            ncol=cor_num_PUs,
                            byrow=TRUE)

    app_spp_occupancy_data =
        apply_const_error_to_spp_occupancy_data (cor_bpm,
                                                FP_rates, FN_rates,
                                                cor_num_PUs, cor_num_spp,
                                                random_values,
                                               bdpg_error_codes)

    app_PU_spp_pair_indices =
        build_PU_spp_pair_indices_from_occ_matrix (app_spp_occupancy_data,
                                                    cor_num_PUs, cor_num_spp)

    app_num_spp = length (unique (app_PU_spp_pair_indices [,"spp_ID"]))
    app_num_PUs = length (unique (app_PU_spp_pair_indices [,"PU_ID"]))

    if (app_num_spp != cor_num_spp)
        cat ("\n\nAfter adding error:  app_num_spp (", app_num_spp,
             ") now differs from original cor_num_spp (", cor_num_spp,
             ").")

    if (app_num_PUs != cor_num_PUs)
        cat ("\n\nAfter adding error:  app_num_PUs (", app_num_PUs,
             ") now differs from original cor_num_PUs (", cor_num_PUs,
             ").")
#browser()

    return (list (original_FP_const_rate = FP_and_FN_const_rates$FP_const_rate,
                  original_FN_const_rate = FP_and_FN_const_rates$FN_const_rate,
                  match_error_counts = match_error_counts,
                  FP_const_rate = FP_const_rate,
                  FN_const_rate = FN_const_rate,
                  app_PU_spp_pair_indices = app_PU_spp_pair_indices,
                  app_spp_occupancy_data = app_spp_occupancy_data,
                  app_num_spp = app_num_spp,
                  app_num_PUs = app_num_PUs))
    }

#===============================================================================

