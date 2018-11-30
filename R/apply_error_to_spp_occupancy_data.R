#===============================================================================

                #  source ("apply_error_to_spp_occupancy_data.R")

#===============================================================================

set_const_err_rate = function (err_type, const_rate,
                               rate_lower_bound, rate_upper_bound,
                               err_name_string)
    {
    const_err_rate = NA
    if (err_type            == "CONSTANT")
        {
        const_err_rate = vn (const_rate)

        } else if (err_type == "RANDOM_UNIFORM_CONSTANT")
        {
        lower_bound = vn (rate_lower_bound)
        upper_bound = vn (rate_upper_bound)

        const_err_rate = runif (1, min=lower_bound, max=upper_bound)

        } else                           #  unknown type of error to add
        {
        stop_bdpg (paste0 ("Unknown error_type for '", err_name_string,
                           "' = '", err_type, "'"))
        }

    return (const_err_rate)
    }

#===============================================================================

set_const_FP_and_FN_err_rates = function (spp_occ_FP_error_type,
                                          spp_occ_FP_const_rate,
                                          spp_occ_FP_rate_lower_bound,
                                          spp_occ_FP_rate_upper_bound,

                                          spp_occ_FN_error_type,
                                          spp_occ_FN_const_rate,
                                          spp_occ_FN_rate_lower_bound,
                                          spp_occ_FN_rate_upper_bound)
    {
        #----------------------------
        #  Set False Positive rate.
        #----------------------------

    FP_const_rate = set_const_err_rate (spp_occ_FP_error_type,
                                        spp_occ_FP_const_rate,
                                        spp_occ_FP_rate_lower_bound,
                                        spp_occ_FP_rate_upper_bound,
                                        "spp_occ_FP_error_type")

        #----------------------------
        #  Set False Negative rate.
        #----------------------------

    FN_const_rate = set_const_err_rate (spp_occ_FN_error_type,
                                        spp_occ_FN_const_rate,
                                        spp_occ_FN_rate_lower_bound,
                                        spp_occ_FN_rate_upper_bound,
                                        "spp_occ_FN_error_type")

    #--------------------

    FP_and_FN_const_rates <- list (FP_const_rate = FP_const_rate,
                                   FN_const_rate = FN_const_rate)

    return (FP_and_FN_const_rates)
    }

#===============================================================================

#' Set constant FP and FN rates
#'
#' Set the False Positive and False Negative error rate to either a
#' given constant value or a constant value chosen from a uniform random
#' distribution whose upper and lower bounds are given.
#'
#' As a result, the False
#' Positive error rate for every PU/spp pair in the problem will be identical.
#' The False Negative error rate for every PU/spp pair will also be identical
#' but not necessarily the same value as the False Positive rate.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns FP_and_FN_const_rates list

#-------------------------------------------------------------------------------

set_const_FP_and_FN_rates = function (parameters)
    {
    FP_and_FN_const_rates =
        set_const_FP_and_FN_err_rates (parameters$spp_occ_FP_error_type,
                                       parameters$spp_occ_FP_const_rate,
                                       parameters$spp_occ_FP_rate_lower_bound,
                                       parameters$spp_occ_FP_rate_upper_bound,

                                       parameters$spp_occ_FN_error_type,
                                       parameters$spp_occ_FN_const_rate,
                                       parameters$spp_occ_FN_rate_lower_bound,
                                       parameters$spp_occ_FN_rate_upper_bound)

    return (FP_and_FN_const_rates)
    }

#===============================================================================

make_sure_no_spp_eradicated_by_setting_FNs <-
            function (bpm, old_occ_cts_for_spp, min_allowed_num_occ_per_spp = 1)
    {
    new_occ_cts_for_spp = rowSums (bpm)
    flag_removed_spp = old_occ_cts_for_spp + new_occ_cts_for_spp
    indices_of_spp_wholly_removed = which (flag_removed_spp <= 0)

    for (cur_spp in indices_of_spp_wholly_removed)
        {
        cur_flipped_locs = which (bpm [cur_spp, ] == -1)
        # cat ("\nFor cur_spp = ", cur_spp, ", cur_flipped_locs = ")
        # print (cur_flipped_locs)

        if (length (cur_flipped_locs) >= min_allowed_num_occ_per_spp)
            {
            indices_to_flip_back =
                safe_sample (cur_flipped_locs, min_allowed_num_occ_per_spp, replace = FALSE)
            bpm [cur_spp, indices_to_flip_back] = 1

            } else
            {
            stop_bdpg (
                paste0 ("\nFor cur_spp = '", cur_spp,
                        ", length (cur_flipped_locs) = '",
                        length (cur_flipped_locs),
                        "' must be >= min_allowed_num_occ_per_spp = '",
                        min_allowed_num_occ_per_spp, "'\n"))
            }
        }  #  end for - all wholly removed spp

    # print (bpm)

        #  Recheck all row sums now to be sure that there are no spp removed.

    new_occ_cts_for_spp = rowSums (bpm)
    flag_removed_spp = old_occ_cts_for_spp + new_occ_cts_for_spp
    indices_of_spp_wholly_removed = which (flag_removed_spp <= 0)
    num_spp_not_reintroduced = length (indices_of_spp_wholly_removed)

    if (num_spp_not_reintroduced > 0)
        {
        stop_bdpg (paste0 ("\nFailed to reintroduce the following spp that ",
                           "had been wholly removed: '",
                           indices_of_spp_wholly_removed, "'\n"))
        }

        #  Now all spp should have at least 1 occurrence and you can switch
        #  all remaining -1 values to be 0 so that those occurrences are removed.

    bpm [bpm == -1] = 0
    # print (bpm)

    return (bpm)
    }

#-------------------------------------------------------------------------------

#' Apply error to species occupancy data.
#'
#'  Walk through the occupancy matrix (PU vs spp) and randomly
#'  choose to flip some of the TPs to FNs and TNs to FPs based on the
#'  given FP and FN error rates.
#'  Update the occupancy matrix (bpm) as you go along, but don't update
#'  the PU_spp_pair_indices until you know all locations that have
#'  flipped.  Update PU_spp_pair_indices at the end so that you don't
#'  have to constantly resize this big array as you go along.
#'
#-------------------------------------------------------------------------------

#' @param FP_rates numeric vector
#' @param FN_rates numeric vector
#' @param random_values numeric vector
#' @param no_empty_spp_allowed boolean
#' @param min_allowed_num_occ_per_spp integer
#' @inheritParams std_param_defns
#'
#' @return Returns bpm matrix

#-------------------------------------------------------------------------------

apply_error_to_spp_occupancy_data =
        function (bpm, FP_rates, FN_rates, num_PUs, num_spp,
                  random_values,   #  passing these in to make it easier to test
                                  #  in a reproducible way

                      ##### ADDED 2018 11 29 - BTL
                  no_empty_spp_allowed = TRUE,
                  min_allowed_num_occ_per_spp = 1
                  )
    {
    cat ("\nStarting apply_error_to_spp_occupancy_data loop.\n\n")

##### ADDED 2018 11 29 - BTL
old_occ_cts_for_spp = rowSums (bpm)
FN_occ_replacement_value = if (no_empty_spp_allowed) -1 else 0

        #  NOTE: I also tried recoding this routine usin mapply() instead of a
        #        for loop and for some reason, mapply() was 10 times slower.
        #        The test and its results are recorded in notes file for
        #        2018 02 03.
    for (cur_spp_row in 1:num_spp)
        {
        for (cur_PU_col in 1:num_PUs)
            {
#             cat ("\n[", cur_spp_row, ",",
#                  cur_PU_col,
#                  "]", sep='')
            if (bpm [cur_spp_row, cur_PU_col])    #  is this spp on this PU?
                {
                    #  TP:  This species DOES exist on this planning unit.
                    #       Randomly choose whether to replace a given TP
                    #       with a false negative (FN)
                    #       i.e., simulate not detecting that spp on that PU.
                if (random_values [cur_spp_row, cur_PU_col] < FN_rates [cur_spp_row, cur_PU_col])
##### CHANGED 2018 11 29 - BTL
#####                    bpm [cur_spp_row, cur_PU_col] = 0
                    bpm [cur_spp_row, cur_PU_col] = FN_occ_replacement_value

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

##### ADDED 2018 11 29 - BTL
if (no_empty_spp_allowed)
    {
    bpm = make_sure_no_spp_eradicated_by_setting_FNs (bpm,
                                                            old_occ_cts_for_spp,
                                                            min_allowed_num_occ_per_spp)

        #  Verify that no species has been completely wiped out by injection
        #  of FNs.
    new_occ_cts_for_spp = rowSums (bpm)
    empty_spp = which (new_occ_cts_for_spp <= 0)
    num_empty_spp = length (empty_spp)

    if (num_empty_spp > 0)
        {
        cat ("\n\nempty_spp indices = ")
        print (empty_spp)

        stop_bdpg (
            paste0 ("\n\nAt least one species has been completely wiped out ",
                    "in applying error (probably by addition of FNs).\n",
                    "num_empty_spp = '", num_empty_spp, "'\n"))
        }
    }

    return (bpm)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#'  Match FP and FN counts to smaller of the two
#'
#'  Usually, the number of TNs and TPs will be unbalanced (i.e., there will
#'  usually be far more absences (TNs) than presences (TPs).
#'  Therefore, there will be far more opportunities to inject
#'  FPs than FNs or vice versa.
#'  Consequently, even if the FP and FN rates are set to the
#'  same value, there are likely to be far more FPs than FNs
#'  or vice versa in the apparent matrix.  In many ecological cases, this would
#'  yield an unreasonable number of False Positives since False Positives
#'  are generally much less likely than False Negatives in field studies.
#'
#'  So, this routine will adjust the error rate of the dominant value to
#'  yield the same _count_ as the other, e.g., if TNs are dominant, then the
#'  adjusted_P(FP) = num_FNs / num_TNs
#'
#-------------------------------------------------------------------------------

#'
#' @param num_TPs integer
#' @param num_TNs integer
#' @param FP_const_rate numeric
#' @param FN_const_rate numeric
#'
#' @return Returns FP_FN_const_rate_pair list

#-------------------------------------------------------------------------------

match_FP_and_FN_counts_to_smaller_of_the_two <- function (num_TPs,
                                                          num_TNs,
                                                          FP_const_rate,
                                                          FN_const_rate)
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


    FP_FN_const_rate_pair <- list (FP_const_rate = FP_const_rate,
                                   FN_const_rate = FN_const_rate)

    return (FP_FN_const_rate_pair)
    }

#===============================================================================

match_FP_and_FN_counts_if_necessary <- function (match_error_counts,
                                                 num_TPs,            #sum (cor_bpm)
                                                 num_TPs_and_TNs,    #length (cor_bpm)
                                                 FP_const_rate,
                                                 FN_const_rate)
    {
    match_error_counts = vb (match_error_counts,
                             def_on_empty = TRUE, def = FALSE)

    if (match_error_counts)
        {
#        num_TPs = sum_bpm                 #sum (cor_bpm)
#        num_TNs = length_bpm - num_TPs    #length (cor_bpm) - num_TPs
        num_TNs = num_TPs_and_TNs - num_TPs    #length (cor_bpm) - num_TPs

        FP_FN_const_rate_pair =
          match_FP_and_FN_counts_to_smaller_of_the_two (num_TPs,
                                                        num_TNs,
                                                        FP_const_rate,
                                                        FN_const_rate)

        FP_const_rate = FP_FN_const_rate_pair$FP_const_rate
        FN_const_rate = FP_FN_const_rate_pair$FN_const_rate
        }

    return (list (FP_const_rate = FP_const_rate,
                  FN_const_rate = FN_const_rate))
    }

#===============================================================================

build_const_err_FP_and_FN_matrices <- function (num_TPs,            #  sum (cor_bpm)
                                                num_TPs_and_TNs,    #  length (cor_bpm)

                                                cor_num_PUs,
                                                cor_num_spp,

                                                spp_occ_FP_error_type,
                                                spp_occ_FP_const_rate,
                                                spp_occ_FP_rate_lower_bound,
                                                spp_occ_FP_rate_upper_bound,

                                                spp_occ_FN_error_type,
                                                spp_occ_FN_const_rate,
                                                spp_occ_FN_rate_lower_bound,
                                                spp_occ_FN_rate_upper_bound,

                                                match_error_counts)
    {
cat ("\n\nIN build_const_err_FP_and_FN_matrices()\n\n")

#    FP_and_FN_const_rates = set_const_FP_and_FN_rates (parameters)
    FP_and_FN_const_rates =
        set_const_FP_and_FN_err_rates (spp_occ_FP_error_type,
                                       spp_occ_FP_const_rate,
                                       spp_occ_FP_rate_lower_bound,
                                       spp_occ_FP_rate_upper_bound,

                                       spp_occ_FN_error_type,
                                       spp_occ_FN_const_rate,
                                       spp_occ_FN_rate_lower_bound,
                                       spp_occ_FN_rate_upper_bound)

    FP_const_rate = FP_and_FN_const_rates$FP_const_rate
    FN_const_rate = FP_and_FN_const_rates$FN_const_rate

    #----------

    FP_and_FN_const_rates =
        match_FP_and_FN_counts_if_necessary (match_error_counts,
                                             num_TPs,            #sum (cor_bpm),
                                             num_TPs_and_TNs,    #length (cor_bpm),
                                             FP_const_rate,
                                             FN_const_rate)

    FP_const_rate = FP_and_FN_const_rates$FP_const_rate
    FN_const_rate = FP_and_FN_const_rates$FN_const_rate

    #----------

    FP_rates_matrix = matrix (rep (FP_const_rate, (cor_num_PUs * cor_num_spp)),
                          nrow=cor_num_spp,
                          ncol=cor_num_PUs,
                          byrow=TRUE)

    FN_rates_matrix = matrix (rep (FN_const_rate, (cor_num_PUs * cor_num_spp)),
                          nrow=cor_num_spp,
                          ncol=cor_num_PUs,
                          byrow=TRUE)

    #----------

    ret_vals_from_build_const_err_FP_and_FN_matrices =
        list (original_FP_const_rate  = FP_and_FN_const_rates$FP_const_rate,
              original_FN_const_rate  = FP_and_FN_const_rates$FN_const_rate,
              match_error_counts      = match_error_counts,
              FP_const_rate           = FP_const_rate,
              FN_const_rate           = FN_const_rate,
              FP_rates_matrix         = FP_rates_matrix,
              FN_rates_matrix         = FN_rates_matrix)

    return (ret_vals_from_build_const_err_FP_and_FN_matrices)
    }

#===============================================================================

#-------------------------------------------------------------------------------

# Compute realized error rates in apparent problem

#  Since errors are generated stochastically, the realized error rates
#  in the apparent spp vs PU matrix are unlikely to exactly match
#  the error generator's target rates, so measure the error rates
#  that actually resulted after adding error.

#-------------------------------------------------------------------------------

compute_realized_error_rates <- function (cor_bpm, app_bpm,
                                          target_FP_rate=NA,  # optional, only for display
                                          target_FN_rate=NA)  # optional, only for display
    {
    num_TPs  = length (which (cor_bpm > 0))    #  Allow for future possibility of abundances instead of just 0/1 values
    num_TNs  = length (cor_bpm) - num_TPs
    num_Ttot = length (cor_bpm)

    num_FNs  = length (which (cor_bpm > app_bpm))    #sum (cor_bpm > app_bpm)
    num_FPs  = length (which (cor_bpm < app_bpm))    #sum (cor_bpm < app_bpm)
    num_Ftot = num_FNs + num_FPs

    FN_rate   = num_FNs / num_TPs
    FP_rate   = num_FPs / num_TNs
    Ftot_rate = num_Ftot / num_Ttot

        #  Echo results for verification.
    cat ("\n-----  Realized error rates  -----\n")
    cat ("\nnum_TPs   = ", num_TPs)
    cat ("\nnum_TNs   = ", num_TNs)
    cat ("\nnum_Ttot  = ", num_Ttot)

    cat ("\nnum_FNs  = ", num_FNs)
    cat ("\nnum_FPs  = ", num_FPs)
    cat ("\nnum_Ftot = ", num_Ftot)

    cat ("\nFN_rate   = ", FN_rate)
    cat ("\nFP_rate   = ", FP_rate)
    cat ("\nFtot_rate = ", Ftot_rate)

    if (! is.na (target_FP_rate))
        cat ("\ntarget_FP_rate = ", target_FP_rate)
    if (! is.na (target_FP_rate))
        cat ("\ntarget_FN_rate = ", target_FN_rate)
    cat ("\n\n-----  End realized error rates  -----\n")

    realized_error_rates = list (FN_ct=num_FNs,    FN_rate=FN_rate,
                                 FP_ct=num_FPs,    FP_rate=FP_rate,
                                 Ftot_ct=num_Ftot, Ftot_rate=Ftot_rate)
    return (realized_error_rates)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Apply constant error to spp occ data
#'
#-------------------------------------------------------------------------------

#' @param cor_bpm matrix
#' @param cor_num_PUs integer
#' @param cor_num_spp integer
#' @param FP_rates_matrix float matrix of False Positive error rates for each
#' species/planning unit combination
#' @param FN_rates_matrix float matrix of False Negative error rates for each
#' species/planning unit combination
#' @inheritParams std_param_defns
#'
#' @return Returns ret_vals_from_apply_errors list

#-------------------------------------------------------------------------------

apply_const_error_to_spp_occupancy_data <- function (cor_num_PUs,
                                               cor_num_spp,
                                               cor_bpm,
                                               FP_rates_matrix,
                                               FN_rates_matrix)
    {
cat ("\n\nIN apply_const_error_to_spp_occupancy_data()\n\n")

    random_values = matrix (runif (cor_num_PUs * cor_num_spp),
                          nrow=cor_num_spp,
                          ncol=cor_num_PUs,
                          byrow=TRUE)

        #--------------------------------------------------------------------
        #  Ready to apply the errors now and create both the occupancy
        #  matrix
        #  app_spp_occupancy_data is the cor_bpm matrix with error added
        #  to it.
        #--------------------------------------------------------------------

    app_spp_occupancy_data =
        apply_error_to_spp_occupancy_data (cor_bpm,
                                            FP_rates_matrix,
                                            FN_rates_matrix,
                                            cor_num_PUs,
                                            cor_num_spp,
                                            random_values)

        #-----------------------------------------------------------------
        #  Since the errors are generated stochastically, the realized
        #  error rates in the apparent bpm are unlikely to exactly match
        #  the error generator's target rates, so measure the realized
        #  error rates now.
        #-----------------------------------------------------------------

    realized_error_rates = compute_realized_error_rates (cor_bpm,
                                                         app_spp_occupancy_data)

        #---------------------------------------------------------------
        #  Now create the and the erroneous PU_spp_pair table from the
        #  erroneous bpm matrix.
        #---------------------------------------------------------------

    app_PU_spp_pair_indices =
        build_PU_spp_pair_indices_from_occ_matrix (app_spp_occupancy_data)

        #--------------------------------------------------------------------
        #  Make sure the spp and PU counts are still OK after adding error.
        #  This should never be a problem, but this check should be made
        #  to avoid downstream errors where the counts are assumed to be
        #  unchanged.
        #  Also, check both counts before deciding whether to stop so that
        #  you don't have to re-run everything to find out that both of
        #  the counts were wrong.
        #
        #  2017 06 09 - BTL
        #  Have changed the setting of app_num_xxx to just use the
        #  cor_num_xxx values as the app_num_xxx values to get downstream
        #  dimensions of various structures to match for comparisons.
        #  Should probably just remove this section of computing the values
        #  here.  However, it might still be useful if instead of
        #  checking for not equal, you only check to make sure that the
        #  app values don't exceed the cor values because that should not
        #  ever happen, i.e., it would be a real error since no PUs or
        #  or spp should ever be created (though in the future, it might
        #  be that an error type is added to allow for identifying a spp
        #  that isn't really there, i.e., misidentifying a cryptic spp).
        #--------------------------------------------------------------------

    app_num_spp = length (unique (app_PU_spp_pair_indices [,"spp_ID"]))
    app_num_PUs = length (unique (app_PU_spp_pair_indices [,"PU_ID"]))

#    app_ct_error = FALSE
    if (app_num_spp != cor_num_spp)
        {
        cat ("\n\nAfter adding error:  app_num_spp (", app_num_spp,
             ") now differs from original cor_num_spp (", cor_num_spp,
             ").\nWill still set app_num_spp to old cor_num_spp.\n")
#        app_ct_error = TRUE
        }

    if (app_num_PUs != cor_num_PUs)
        {
        cat ("\n\nAfter adding error:  app_num_PUs (", app_num_PUs,
             ") now differs from original cor_num_PUs (", cor_num_PUs,
             ").\nWill still set app_num_PUs to old cor_num_PUs.\n")
#        app_ct_error = TRUE
        }

#  2017 06 09 - BTL
#  Have decided to just set the app_num_xxx values to cor_num_xxx values so that
#  dimensions of cor and app structures match in comparisons downstream.
#     if (app_ct_error)  stop_bdpg()

        #--------------------------------------------------------------------

    ret_vals_from_apply_const_error_to_spp_occupancy_data <-
        list (
                app_PU_spp_pair_indices = app_PU_spp_pair_indices,
                app_spp_occupancy_data  = app_spp_occupancy_data,

                realized_FP_rate        = realized_error_rates$FP_rate,
                realized_FN_rate        = realized_error_rates$FN_rate,
                realized_Ftot_rate      = realized_error_rates$Ftot_rate,

                app_num_spp             = cor_num_spp,     #app_num_spp,
                app_num_PUs             = cor_num_PUs      #app_num_PUs
                )

    return (ret_vals_from_apply_const_error_to_spp_occupancy_data)
    }

#===============================================================================

