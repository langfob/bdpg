#===============================================================================
#                           Generate a wrapped problem.
#===============================================================================


compute_PU_spp_table_attributes_by_spp <- function (PU_IDs_for_one_spp_as_df,
                                                    dep_set)
    {
    num_pu_this_spp        = length (PU_IDs_for_one_spp_as_df)
    num_unique_pu_this_spp = length (unique (PU_IDs_for_one_spp_as_df))
    num_pu_in_dep_set      = sum (PU_IDs_for_one_spp_as_df %in% dep_set)

    return (data.frame (num_pu_this_spp, num_unique_pu_this_spp, num_pu_in_dep_set))
    }

#===============================================================================

#' export

validate_wrap <- function (extra_abund,
                           dep_set_PUs_eligible,
                           PU_spp_table,
                           dep_set)
    {
        #  Apply compute_PU_spp_table_attributes_by_spp() function to PU_spp_table by spp_ID group
        #  Form of this call is taken from:
        #  http://www.milanor.net/blog/dplyr-do-tips-for-using-and-programming/
    values_by_spp =
        PU_spp_table %>%
        dplyr::group_by (spp_ID) %>%
        dplyr::do (compute_PU_spp_table_attributes_by_spp (PU_IDs_for_one_spp_as_df = .$PU_ID,
                                                           dep_set))

    num_extra_spp = length (extra_abund)

        #  Rules that PU_spp_table for wrapped spp must satisfy to be valid:

    num_test_rules = 6
    ok = rep (FALSE, num_test_rules)

        #  Rule 1:  At least one occurrence of each species ID must be on a PU in the dep set
    ok [1] = all (values_by_spp$num_pu_in_dep_set > 0)
    if (! ok[1])
        {
        message ("\nWrapping rule 1 violation:")
        cat ("\nRule 1:  At least one occurrence of each species ID must be on a PU in the dep set",
             "\nnum_extra_spp = ", num_extra_spp,
             "\nsum (values_by_spp$num_pu_in_dep_set > 0) = ",
             sum (values_by_spp$num_pu_in_dep_set > 0),
             "\nwhich (values_by_spp$num_pu_in_dep_set <= 0) = '",
             which (values_by_spp$num_pu_in_dep_set <= 0), "'\n")
        print (values_by_spp$num_pu_in_dep_set)
        }

        #  Rule 2:  If dep set not eligible, then one and only one occurrence of the species can be in dep set
    ok [2] = TRUE
    if (! dep_set_PUs_eligible)
        ok [2] = all (values_by_spp$num_pu_in_dep_set == 1)
    if (! ok[2])
        {
        message ("\nWrapping rule 2 violation:")
        cat ("\nRule 2:  If dep set not eligible, then one and only one occurrence of the species can be in dep set",
             "\nnum_extra_spp = ", num_extra_spp,
             "\nsum (values_by_spp$num_pu_in_dep_set == 1) = ",
             sum (values_by_spp$num_pu_in_dep_set == 1),
             "\nwhich (values_by_spp$num_pu_in_dep_set != 1) = '",
             which (values_by_spp$num_pu_in_dep_set != 1), "'\n")
        print (values_by_spp$num_pu_in_dep_set)
        }

        #  Rule 3:  No PU can occur more than once within a species
    ok [3] = all (values_by_spp$num_pu_this_spp == values_by_spp$num_unique_pu_this_spp)
    if (! ok[3])
        {
        cts_not_same = which (values_by_spp$num_pu_this_spp !=
                                  values_by_spp$num_unique_pu_this_spp)
        message ("\nWrapping rule 3 violation:")
        cat ("\nRule 3:  No PU can occur more than once within a species",
             "\nduplicates at index(s): '", cts_not_same, "'")
        cat ("\nvalues_by_spp$num_pu_this_spp = \n")
        print (values_by_spp$num_pu_this_spp)
        cat ("\nvalues_by_spp$num_unique_pu_this_spp = \n")
        print (values_by_spp$num_unique_pu_this_spp)
    }

        #  Rule 4:  All species must occur in result table
    ok [4] = (num_extra_spp == length (values_by_spp$spp_ID))
    if (! ok[4])
        {
        message ("\nWrapping rule 4 violation:")
        cat ("\nRule 4:  All species must occur in result table",
             "\nnum_extra_spp = ", num_extra_spp,
             "\nlength (values_by_spp$spp_ID) = ", length (values_by_spp$spp_ID))
        }

        #  Rule 5:  All species must occur the number of times specified in their abundance
            #  Had to use isTRUE() here because all.equal doesn't seem to return
            #  a boolean.  For some reason, it returns a string saying
            #  something like "Mean relative difference: ..." when they
            #  don't match.  When they do match, it does return TRUE though...
    ok [5] = isTRUE (all.equal (extra_abund, values_by_spp$num_unique_pu_this_spp))
    if (! ok[5])
        {
        message ("\nWrapping rule 5 violation:")
        cat ("\nRule 5:  All species must occur the number of times specified in their abundance")
        if (length (extra_abund) == length (values_by_spp$num_unique_pu_this_spp))
            {
            indices_of_mismatches = which (extra_abund != values_by_spp$num_unique_pu_this_spp)
            cat ("\nindices_of_mismatches = '", indices_of_mismatches, "'")
            }
        cat ("\nextra_abund = \n")
        print (extra_abund)
        cat ("\nvalues_by_spp$num_unique_pu_this_spp = \n")
        print (values_by_spp$num_unique_pu_this_spp)
        }

        #  Rule 6:  Total number of lines in the result table must equal total number of occurrences
    ok [6] = (sum (extra_abund) == nrow (PU_spp_table))
    if (! ok[6])
        {
        message ("\nWrapping rule 6 violation:")
        cat ("\nRule 6:  Total number of lines in the result table must equal total number of occurrences",
             "\ntotal num spp = sum (extra_abund) = ", sum (extra_abund),
             "\ntotal num lines in result table = ", nrow (PU_spp_table))
        cat ("\nextra_abund = \n")
        print (extra_abund)
        cat ("\nPU_spp_table = \n")
        print (PU_spp_table)
        }

    all_ok = all (ok)
    if (! all_ok)
        {
        cat ("\nRule(s) ", which (!ok),
                      " violated in wrapped species PU_spp_table.\n")
        stop()
        }

    return (all_ok)
    }

#===============================================================================

    #  Need to do some sanity checks on various wrap parameters
    #  to make sure that they can't generate nonsense or a crash
    #
    #  Just a dummy placeholder routine until I know what needs checking.

#' Do sanity checks on parameters for wrapping a biodiversity problem
#'
#' Currently just a placeholder...

#-------------------------------------------------------------------------------

do_sanity_checks <- function ()
    {
    cat ("\n\nDummy sanity check for wrap_...().\n\n")
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Create set of planning units eligible for adding species into
#'
#' When wrapping a species distribution around a Xu biodiversity problem,
#' the first instance of any new species is added to the solution set of the
#' original Xu problem that is being wrapped.  However, if more than one
#' instance of the new species is being added in the wrapped problem, the
#' extra instances could go in a) just the new planning units outside the
#' original Xu problem added for the wrapper, or b) in both the original
#' solution set and the newly added planning units.  This function builds
#' that target set of planning units based on the value of the
#' dep_set_PUs_eligible flag passed to it.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{dep_set_PUs_eligible}{
#' \preformatted{
#' dep_set_PUs_eligible :  logi FALSE
#' }}
#' \subsection{eligible_PUs}{
#' \preformatted{
#' eligible_PUs :  int [1:285] 123 124 125 126 127 128 129 130 131 132 ...
#' }}
#' \subsection{extra_PUs}{
#' \preformatted{
#' extra_PUs :  int [1:285] 123 124 125 126 127 128 129 130 131 132 ...
#' }}
#' \subsection{Xu_dep_set}{
#' \preformatted{
#' Xu_dep_set :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param Xu_dep_set integer vector of planning unit IDs for the solution set
#'     of the original Xu problem being wrapped around
#' @param extra_PUs integer vector of plannning unit IDs being added to the
#'     original Xu problem to make the wrapped problem
#' @param dep_set_PUs_eligible boolean indicating whether new species instances
#'     can be added to the original Xu problem's solution set in addition to
#'     the adding them to the newly added planning units; TRUE implies new
#'     species instances can be added to both sets; FALSE implies new species
#'     instances can only be added to the newly added planning units
#'
#' @return set of planning unit IDs where new species can be placed

#-------------------------------------------------------------------------------

create_eligible_PU_set <- function (Xu_dep_set,
                                    extra_PUs,
                                    dep_set_PUs_eligible
                                    )
    {
    if (dep_set_PUs_eligible)
        eligible_PUs = c(Xu_dep_set, extra_PUs)
    else
        eligible_PUs = extra_PUs


#docaids::doc_vars_in_this_func_once ()
    return (eligible_PUs)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#'  Drop spp from abund dist if on too many or too few patches
#'
#'  This function is primarily to get rid of species that occur on
#'  only 0 or 1 patches, however, some distribution generator might also
#'  generate species that are too common as well, so this routine will
#'  axe those too.
#'
#'  Abundances are expressed here as a vector of count values where each value
#'  is the count of the number of patches a particular species occurs on.
#'  For example, [1 1 1 2 6 6] would represent 3 species that occur
#'  on exactly one patch, 1 species that occurs on 2 patches, and 2 species
#'  that occur on 6 patches each.  Calling this routine with a min_abund of 2
#'  and a max_abund of 5 would return a 1 element vector, i.e., the vector [2].
#'
#'  This doesn't really need to be a function since it's only one line
#'  long, but I built an earlier version that was more verbose so that
#'  I could monitor how many things were being cut out.
#'  That version is in the git repository in case it needs to be
#'  resurrected for some reason.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{max_abund}{
#' \preformatted{
#' max_abund :  num 1.8e+308
#' }}
#' \subsection{min_abund}{
#' \preformatted{
#' min_abund :  num 2
#' }}
#' \subsection{rounded_abundances}{
#' \preformatted{
#' rounded_abundances :  num [1:1628] 2 2 1 1 3 1 3 2 2 1 ...
#' }}
#' \subsection{trimmed_rounded_abund_per_spp}{
#' \preformatted{
#' trimmed_rounded_abund_per_spp :  num [1:1277] 2 2 3 3 2 2 2 2 2 2 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param rounded_abundances vector of abundances to be trimmed
#' @param min_abund lowest abundance to allow in the trimmed set
#' @param max_abund largest abundance to allow in the trimmed set
#'
#' @return vector of abundances whose values lie in the specified range

#-------------------------------------------------------------------------------

trim_abundances = function (rounded_abundances,
                            min_abund=2,
                            max_abund=.Machine$double.xmax
                            )
    {
    trimmed_rounded_abund_per_spp <-
        rounded_abundances [(rounded_abundances <= max_abund) &
                            (rounded_abundances >= min_abund), drop=FALSE]

#docaids::doc_vars_in_this_func_once ()
    return (trimmed_rounded_abund_per_spp)
    }

#===============================================================================

gen_raw_histogram_of_wrapped_dist <- function (Xu_PU_spp_table,
                                               trimmed_rounded_abund_per_spp,
                                               spp_col_name)
    {
                    verbose_remove_base = FALSE    #  just for debugging now...
                    if (verbose_remove_base)
                            {
                            cat ("\n\nStarting ",
                                 "gen_raw_histogram_of_wrapped_dist():")
                                            cat ("\n    Xu_PU_spp_table = \n")
                                            show (Xu_PU_spp_table)
                                            cat ("\n    trimmed_rounded_abund_per_spp = \n")
                                            show (trimmed_rounded_abund_per_spp)
                                            cat ("\n    spp_col_name = \n")
                                            show (spp_col_name)
                            }
# Xu_PU_spp_table =
#     PU_ID spp_ID
# 1       1      1
# 2       2      1
# 3       3      2
# 4       4      2
# ...
# 155   155     78
# 156   156     78
# 157   157     79
# 158   158     79
#
#
# trimmed_rounded_abund_per_spp =
#   [1] 5 2 2 2 2 3 2 3 3 2 2 3 2 6 2 4 2 2 2 2 2 2 2 2 2 2 2 3 3 2 2 3 2 4 2 3 3
#  [38] 2 3 2 2 2 3 4 2 3 3 2 2 2 2 3 2 2 2 2 3 2 4 2 3 3 2 2 3 4 2 2 2 2 2 3 2 3
#  [75] 3 3 2 3 3 2 2 3 3 2 3 2 2 3 3 2 2 2 2 3 2 6 2 3 2 4 2 2 3 2 2 3 2 2 3 2 3
# [112] 4 4 2 2 3 3 2 2 4 2 2 3 3 2 2 3 3 2 3 3 2 2 2 5 2 3 2 2 2 4 2 3 3 2 2
#
# spp_col_name =
# [1] "spp_ID"

        #-----------------------------------------------------------------------
        #  Count the number of occurrences of each species in the base problem.
        #-----------------------------------------------------------------------

    base_abund_by_spp = plyr::count (Xu_PU_spp_table [,spp_col_name])

                        if (verbose_remove_base)
                            {
                            cat ("\n\n    base_abund_by_spp = \n")
                            show (base_abund_by_spp)
                            }
# base_abund_by_spp =
#     x freq
# 1   1    2
# 2   2    2
# ...
# 78 78    2
# 79 79    2

        #-----------------------------------------------------------------------
        #  Count how many times each number of occurrences appears in that set
        #  (e.g., how many species occur on 2 patches, on 3 patches, etc.).
        #  Should always be 2 for every species in the Xu base problem.
        #-----------------------------------------------------------------------

    base_abund_hist = plyr::count (base_abund_by_spp [,"freq"])

                        if (verbose_remove_base)
                            {
                            cat ("\n\n    base_abund_hist = \n")
                            show (base_abund_hist)
                            }
# base_abund_hist =
#   x freq
# 1 2   79

        #-----------------------------------------------------------------------
        #  Do the same for the wrapping abundances.
        #  They are given as a vector of numbers of occurrences for each
        #  species in the wrapping distribution, but unlike the base problem
        #  abundances, no species ID has been assigned yet to these wrapping
        #  counts.
        #-----------------------------------------------------------------------

    wrapping_abund_hist = plyr::count (trimmed_rounded_abund_per_spp)

                        if (verbose_remove_base)
                            {
                            cat ("\n\n    wrapping_abund_hist = \n")
                            show (wrapping_abund_hist)
                            }
# wrapping_abund_hist =
#   x freq
# 1 2   86
# 2 3   46
# 3 4   10
# 4 5    2
# 5 6    2

        #-----------------------------------------------------------------------
        #  There will probably be some different elements in the two histograms.
        #  For example, the original Xu histogram will only have one entry,
        #  i.e., the count for abundance = 2, since all species occur on
        #  exactly 2 patches.  The wrapping histogram will probably (though
        #  not necessarily) have entries for other patch counts and may have
        #  a different value than the Xu histogram for the number of species
        #  occurring on 2 patches.
        #
        #  We need to make a single histogram that merges the two sets of
        #  possible counts.  The merge() function does this by making a single
        #  data.frame with a column for each of the 2 input histograms,
        #  labelling one column as x and the other as y.  It adds a row for
        #  each matching entry in the inputs (e.g., a row for abundance = 2,
        #  with the x column giving the count for the wrapping histogram and
        #  the y column giving the count for the Xu histogram).
        #
        #  It also adds a row for each entry in either input that doesn't
        #  appear in the other input (e.g., a row for every abundance other
        #  than 2 in the wrapping histogram when it's wrapping around an
        #  original Xu histogram that only has the value for abundance = 2).
        #-----------------------------------------------------------------------

    wrapped_extra_spp_abund_merge = merge (x=wrapping_abund_hist,
                                           y=base_abund_hist,
                                           by="x", all=TRUE)

                        if (verbose_remove_base)
                            {
                            cat ("\n\n    wrapped_extra_spp_abund_merge = \n")
                            show (wrapped_extra_spp_abund_merge)
                        }
# wrapped_extra_spp_abund_merge =
#   x freq.x freq.y
# 1 2     86     79
# 2 3     46     NA
# 3 4     10     NA
# 4 5      2     NA
# 5 6      2     NA

    return (wrapped_extra_spp_abund_merge)
    }

#===============================================================================

check_for_imperfect_wrap <- function (final_wrapped_extra_spp_abund_hist,
                                      allow_imperfect_wrap)
    {
# final_wrapped_extra_spp_abund_hist =
#   abund freq
# 1     2    7     <<<<<-----  perfect:  wrap fully encloses base problem
# 2     3   46
# 3     4   10
# 4     5    2
# 5     6    2

#  OR

# final_wrapped_extra_spp_abund_hist =
#   abund freq
# 1     2   -20     <<<<<-----  imperfect: does not fully enclose base problem
# 2     3   46
# 3     4   10
# 4     5    2
# 5     6    2

#browser()
    num_neg_abund_freqs = sum (final_wrapped_extra_spp_abund_hist [,'freq'] < 0)
    if (num_neg_abund_freqs > 0)
        {
            #  There is at least one negative abundance frequency, i.e.,
            #  abundance level where the number of species with that abundance
            #  in the problem being wrapped is greater than in the wrapping
            #  wrapping distribution.
            #  For example, if it's not a perfect wrapping distribution,
            #  then there might only be 10 species that occur on exactly two
            #  PUs while the Xu base problem has 15 species occurring on
            #  exactly two PUs.

        if (allow_imperfect_wrap)
            {
            error_or_warning_on_wrap_str = "- bdpg WARNING"
            } else
            {
            error_or_warning_on_wrap_str = "- bdpg FATAL ERROR"
            }

            #-----------------------------------------------------------------------
            #  Write a warning message in 2 ways.
            #  First, write it using cat() so that it ends up in the console log.
            #  Second, write it using message() so that it shows up on the console
            #  in red to have more chance of making the user aware of it.
            #  Originally, I just used the message() call but it doesn't show up
            #  in the console log file (I think it goes to stderr or something.)
            #-----------------------------------------------------------------------

        msg_string = paste0 ("\n\nIMPERFECT WRAP ", error_or_warning_on_wrap_str,
                         "\n", num_neg_abund_freqs, " element of the wrapping distribution (freq.x) is smaller than",
                         "\nthe corresponding element in the wrapped distribution (freq.y).",
                         "\nThis would lead to a negative frequency for at least one level of abundance, ",
                         "\ni.e., a negative number of species having the given abundance.",
                         "\n\nwrapped_extra_spp_abund_merge = \n")
        cat (msg_string)
        message (msg_string)
        print (final_wrapped_extra_spp_abund_hist)

        if (allow_imperfect_wrap)
            {
            indices_of_negative_freqs = which (final_wrapped_extra_spp_abund_hist [,'freq'] < 0)
            final_wrapped_extra_spp_abund_hist [indices_of_negative_freqs, 'freq'] = 0
            }

        cat ("\nResulting final_wrapped_extra_spp_abund_hist = \n")
        print (final_wrapped_extra_spp_abund_hist)
        cat ("\n")

        if (! allow_imperfect_wrap) stop ("Fail on imperfect wrap of bdproblem")

        }  #  end if - num_neg_abund_freqs > 0

    return (final_wrapped_extra_spp_abund_hist)
    }

#===============================================================================

compute_final_wrapped_extra_spp_abund_hist <- function (wrapped_extra_spp_abund_merge,
                                                        allow_imperfect_wrap)
    {
                    verbose_remove_base = FALSE    #  just for debugging now...
                    if (verbose_remove_base)
                            {
                            cat ("\n\nStarting ",
                                 "compute_final_wrapped_extra_spp_abund_hist():")
                                            cat ("\nallow_imperfect_wrap = ", allow_imperfect_wrap,
                                                 "\nwrapped_extra_spp_abund_merge = \n")
                                            show (wrapped_extra_spp_abund_merge)
                            }
#browser()
# wrapped_extra_spp_abund_merge =
#   x freq.x freq.y
# 1 2     86     79
# 2 3     46     NA
# 3 4     10     NA
# 4 5      2     NA
# 5 6      2     NA

        #-----------------------------------------------------------------------
        #  Now we need to clean up this data frame so that NAs are replaced
        #  with 0's and so that any missing abundance values are added to
        #  the data.frame (e.g., if the highest abundance value was 10 but
        #  neither input histogram had any species that occurred on 3, 4, or
        #  9 PUs).
        #-----------------------------------------------------------------------

            #  Replace NA counts with 0s.
    wrapped_extra_spp_abund_merge [is.na (wrapped_extra_spp_abund_merge)] = 0

                        if (verbose_remove_base)
                            {
                            cat ("\n\n    After NA replacement with 0, wrapped_extra_spp_abund_merge = \n")
                            show (wrapped_extra_spp_abund_merge)
                            }
# After NA replacement with 0, wrapped_extra_spp_abund_merge =
#   x freq.x freq.y
# 1 2     86     79
# 2 3     46      0
# 3 4     10      0
# 4 5      2      0
# 5 6      2      0


    final_wrapped_extra_spp_abund_hist =
        as.data.frame (cbind (wrapped_extra_spp_abund_merge [,"x"],
                              wrapped_extra_spp_abund_merge [, "freq.x"] -
                                  wrapped_extra_spp_abund_merge [, "freq.y"]
                              ))

    names (final_wrapped_extra_spp_abund_hist) = c("abund","freq")

                        if (verbose_remove_base)
                            {
                            cat ("\n\n    final_wrapped_extra_spp_abund_hist = \n")
                            show (final_wrapped_extra_spp_abund_hist)
                        }

# final_wrapped_extra_spp_abund_hist =
#   abund freq
# 1     2    7
# 2     3   46
# 3     4   10
# 4     5    2
# 5     6    2

    final_wrapped_extra_spp_abund_hist =
        check_for_imperfect_wrap (final_wrapped_extra_spp_abund_hist,
                                  allow_imperfect_wrap)

    return (final_wrapped_extra_spp_abund_hist)
    }

#===============================================================================

build_vec_of_extra_spp_and_their_abundances <- function (wrapped_extra_spp_abund_hist)
    {
    num_extra_spp = sum (wrapped_extra_spp_abund_hist [,"freq"])
# num_extra_spp =
# 767
    extra_spp_abund = rep (NA, num_extra_spp)
# extra_spp_abund = logi [1:767] NA NA NA NA NA NA ...
    num_abund_rows = length (wrapped_extra_spp_abund_hist[,"abund"])
# num_abund_rows =
# 5

                        verbose_remove_base = TRUE    #  just for debugging now...
                        if (verbose_remove_base)
                            {
                            cat ("\nIn build_vec_of_extra_spp_and_their_abundances() just before loop: ",
                                 "\nnum_extra_spp = ", num_extra_spp,
                                 "\nnum_abund_rows = ", num_abund_rows,
                                 "\ninitial extra_spp_abund = \n")
                            print (extra_spp_abund)
                            }

    start_idx = 1
    for (cur_idx in 1:num_abund_rows)
        {
        if (wrapped_extra_spp_abund_hist [cur_idx, "freq"] > 0)    #  Should always be true, but just in case...
            {
            end_idx = start_idx + wrapped_extra_spp_abund_hist [cur_idx, "freq"] - 1
            extra_spp_abund [start_idx:end_idx] = wrapped_extra_spp_abund_hist [cur_idx, "abund"]

                        if (verbose_remove_base)
                            {
                            cat ("\nAt bottom of cur_idx = ", cur_idx, ",
                                 start_idx = ", start_idx,
                                 "end_idx = ", end_idx,
                                 "extra_spp_abund = \n")
                            print (extra_spp_abund)
                            }

            start_idx = end_idx + 1
            }
    }

    return (extra_spp_abund)
    }

#===============================================================================

clean_up_wrapped_abund_dist <- function (wrapped_extra_spp_abund_merge,
                                         allow_imperfect_wrap)
    {
                    verbose_remove_base = TRUE    #  just for debugging now...
                    if (verbose_remove_base)
                            {
                            cat ("\n\nStarting ",
                                 "clean_up_wrapped_abund_dist():")
                                            cat ("\nallow_imperfect_wrap = \n", allow_imperfect_wrap,
                                                 "\nwrapped_extra_spp_abund_merge = \n")
                                            show (wrapped_extra_spp_abund_merge)
                            }
#browser()
# wrapped_extra_spp_abund_merge =
#   x freq.x freq.y
# 1 2     86     79
# 2 3     46     NA
# 3 4     10     NA
# 4 5      2     NA
# 5 6      2     NA

        #-----------------------------------------------------------------------
        #  Now we need to clean up this data frame so that NAs are replaced
        #  with 0's and so that any missing abundance values are added to
        #  the data.frame (e.g., if the highest abundance value was 10 but
        #  neither input histogram had any species that occurred on 3, 4, or
        #  9 PUs).
        #-----------------------------------------------------------------------

    wrapped_extra_spp_abund_hist =
        compute_final_wrapped_extra_spp_abund_hist (wrapped_extra_spp_abund_merge,
                                                    allow_imperfect_wrap)


        #-----------------------------------------------------------------------
        #  Now build a vector of the abundances for just the extra species
        #  based on the histogram of abundances of just the extra species
        #  (i.e., the species in the wrapped distribution but not in the
        #  base problem's distribution.
        #-----------------------------------------------------------------------

    extra_spp_abund =
        build_vec_of_extra_spp_and_their_abundances (wrapped_extra_spp_abund_hist)

#docaids::doc_vars_in_this_func_once ()
    return (extra_spp_abund)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#  Remove base species abundances from wrapping distribution

#' Remove base problem's species from the wrapping problems set of species to
#' be distributed over the planning units
#'
#' When wrapping one distribution around another, the outside distribution
#' needs to contain the inside distribution as a proper subset, however,
#' when it's time to distribute the wrapping distribution over the landscape,
#' the inside distribution has already been distributed.  That means that the
#' inside distribution needs to be removed from full wrapping distribution
#' before its instances are spread across the landscape.  This function
#' strips that inside distribution out of the full wrapped distribution and
#' returns a set of abundances ready for spreading.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{base_abund_by_spp}{
#' \preformatted{
#' base_abund_by_spp : 'data.frame':	814 obs. of  2 variables:
#'  $ x   : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ freq: int  2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{base_abund_hist}{
#' \preformatted{
#' base_abund_hist : 'data.frame':	1 obs. of  2 variables:
#'  $ x   : int 2
#'  $ freq: int 814
#' }}
#' \subsection{cur_idx}{
#' \preformatted{
#' cur_idx :  int 6
#' }}
#' \subsection{end_idx}{
#' \preformatted{
#' end_idx :  num 463
#' }}
#' \subsection{extra_spp_abund}{
#' \preformatted{
#' extra_spp_abund :  num [1:463] 2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{num_abund_rows}{
#' \preformatted{
#' num_abund_rows :  int 6
#' }}
#' \subsection{num_extra_spp}{
#' \preformatted{
#' num_extra_spp :  num 463
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#' \subsection{start_idx}{
#' \preformatted{
#' start_idx :  num 464
#' }}
#' \subsection{trimmed_rounded_abund_per_spp}{
#' \preformatted{
#' trimmed_rounded_abund_per_spp :  num [1:1277] 2 2 3 3 2 2 2 2 2 2 ...
#' }}
#' \subsection{verbose_remove_base}{
#' \preformatted{
#' verbose_remove_base :  logi FALSE
#' }}
#' \subsection{wrapped_extra_spp_abund_hist}{
#' \preformatted{
#' wrapped_extra_spp_abund_hist : 'data.frame':	6 obs. of  2 variables:
#'  $ abund: num  2 3 4 5 6 7
#'  $ freq : num  77 311 57 15 2 1
#' }}
#' \subsection{wrapped_extra_spp_abund_merge}{
#' \preformatted{
#' wrapped_extra_spp_abund_merge : 'data.frame':	6 obs. of  3 variables:
#'  $ x     : num  2 3 4 5 6 7
#'  $ freq.x: int  891 311 57 15 2 1
#'  $ freq.y: num  814 0 0 0 0 0
#' }}
#' \subsection{wrapping_abund_hist}{
#' \preformatted{
#' wrapping_abund_hist : 'data.frame':	6 obs. of  2 variables:
#'  $ x   : num  2 3 4 5 6 7
#'  $ freq: int  891 311 57 15 2 1
#' }}
#' \subsection{Xu_PU_spp_table}{
#' \preformatted{
#' Xu_PU_spp_table : 'data.frame':	1628 obs. of  2 variables:
#'  $ PU_ID : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: int  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param Xu_PU_spp_table PU_spp table for original Xu problem being wrapped
#'     around
#' @param trimmed_rounded_abund_per_spp vector of abundances of all species in
#'     the full wrapped distribution, i.e., including the original Xu problem
#'     abundances
#' @param spp_col_name string containing the name of the column in the PU_spp
#'     table that contains the species IDs
#'
#' @return vector of abundances in the wrapping abundance distribution that are
#'     not also in the original Xu problem

#-------------------------------------------------------------------------------

remove_base_spp_abundances_from_wrapping_distribution <-
    function (Xu_PU_spp_table,
              trimmed_rounded_abund_per_spp,
              spp_col_name,
              allow_imperfect_wrap)
    {
                    verbose_remove_base = FALSE    #  just for debugging now...
                    if (verbose_remove_base)
                            {
                            cat ("\n\nStarting ",
                                 "remove_base_spp_abundances_from_wrapping_distribution():")
                                            cat ("\n    Xu_PU_spp_table = \n")
                                            show (Xu_PU_spp_table)
                                            cat ("\n    trimmed_rounded_abund_per_spp = \n")
                                            show (trimmed_rounded_abund_per_spp)
                                            cat ("\n    spp_col_name = \n")
                                            show (spp_col_name)
                                            cat ("\nallow_imperfect_wrap = \n")
                                            show (allow_imperfect_wrap)
                            }

    wrapped_extra_spp_abund_merge =
        gen_raw_histogram_of_wrapped_dist (Xu_PU_spp_table,
                                           trimmed_rounded_abund_per_spp,
                                           spp_col_name)

    extra_spp_abund = clean_up_wrapped_abund_dist (wrapped_extra_spp_abund_merge,
                                                   allow_imperfect_wrap)

    return (extra_spp_abund)
    }

#===============================================================================

#' @export
#'
create_wrapping_spp_PU_spp_table <- function (extra_abund,
                                              dep_set,
                                              eligible_set,

            #  NOTE:  These 2 args are ONLY for use when called by unit test
            #         routines that need to reproduce random values exactly.
            #         You should omit them in all other cases.
            #         See tests/testthat/test_gen_wrapped_bdprob.R for the
            #         only places where they are and should be used.
                                              use_testing_only_rand_seed = FALSE,
                                              testing_only_rand_seed = 17)
    {
    if (use_testing_only_rand_seed) set.seed (testing_only_rand_seed)

        #-----------------------------------------------------------------------
        #  Create and initialize a table showing each occurrence of each extra
        #  species (i.e., wrapping species) on a particular PU, i.e., one
        #  PU_ID/spp_ID pair for each occurrence.
        #-----------------------------------------------------------------------

    num_extra_occurrences = sum (extra_abund)
    num_extra_spp         = length (extra_abund)

    PU_spp_table = data.frame (PU_ID =  rep (NA, num_extra_occurrences),
                               spp_ID = rep (NA, num_extra_occurrences))

        #-----------------------------------------------------------------------
        #  Now, for each extra species (i.e., species in the wrapping set),
        #  randomly draw planning units where that species occurs in the
        #  eligible set of planning units.
        #  Then, load each of those occurrences into the PU_spp_table.
        #-----------------------------------------------------------------------

    cur_row = 1
    for (cur_spp_idx in 1:num_extra_spp)
        {
        num_PUs_to_draw = extra_abund [cur_spp_idx]

            #-------------------------------------------------------------------
            #  Draw the one mandatory PU in the dependent set for this species
            #  and add a PU_ID/spp_ID pair for it in the PU_spp_table.
            #-------------------------------------------------------------------

        cur_dep_set_PU = safe_sample (dep_set, 1, replace=FALSE)
        PU_spp_table [cur_row, "PU_ID"] = cur_dep_set_PU
        PU_spp_table [cur_row, "spp_ID"] = cur_spp_idx
        cur_row = cur_row + 1

            #-------------------------------------------------------------------
            #  If that PU was part of the overall eligible set to draw from
            #  (e.g., the eligible set was the union of the dependent set and
            #   the extra set rather than just being the extra set),
            #  remove it from the set of PUs eligible for any remaining draws.
            #-------------------------------------------------------------------

        idx_of_cur_dep_set_PU = which (eligible_set == cur_dep_set_PU)
        cur_eligible_set =
            if (length (idx_of_cur_dep_set_PU) > 0)
                eligible_set [-idx_of_cur_dep_set_PU] else eligible_set

            #-------------------------------------------------------------
            #  Now draw all remaining occurrences of the current species
            #  from the eligible set of PUs and load a PU_ID/spp_ID pair
            #  for each one into the PU_spp_table.
            #-------------------------------------------------------------

        num_PUs_to_draw = num_PUs_to_draw - 1
        if (num_PUs_to_draw > 0)
            {
            extra_PUs_for_cur_spp = safe_sample (cur_eligible_set,
                                                 num_PUs_to_draw,
                                                 replace=FALSE)

            end_row = cur_row + num_PUs_to_draw - 1
            PU_spp_table [cur_row:end_row, "PU_ID"]  = extra_PUs_for_cur_spp
            PU_spp_table [cur_row:end_row, "spp_ID"] = cur_spp_idx

            cur_row = end_row + 1

            }  #  end if - num_PUs_to_draw
        }  #  end for - cur_spp_idx

    return (PU_spp_table)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Wrap abundances around eligible set
#'
#' Take a given distribution (e.g., from a Xu problem) and add more species
#' (and probably more planning units too) to the problem so that the new
#' problem has a specified distribution, but retains the original problem's
#' solution.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{cur_eligible_set}{
#' \preformatted{
#' cur_eligible_set :  int [1:285] 123 124 125 126 127 128 129 130 131 132 ...
#' }}
#' \subsection{cur_row}{
#' \preformatted{
#' cur_row :  num 1410
#' }}
#' \subsection{cur_spp_idx}{
#' \preformatted{
#' cur_spp_idx :  int 463
#' }}
#' \subsection{dep_set}{
#' \preformatted{
#' dep_set :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{dep_set_PU}{
#' \preformatted{
#' dep_set_PU :  int 42
#' }}
#' \subsection{eligible_set}{
#' \preformatted{
#' eligible_set :  int [1:285] 123 124 125 126 127 128 129 130 131 132 ...
#' }}
#' \subsection{end_row}{
#' \preformatted{
#' end_row :  num 1409
#' }}
#' \subsection{extra_abund}{
#' \preformatted{
#' extra_abund :  num [1:463] 2 2 2 2 2 2 2 2 2 2 ...
#' }}
#' \subsection{extra_PUs_for_cur_spp}{
#' \preformatted{
#' extra_PUs_for_cur_spp :  int [1:6] 397 278 292 178 316 324
#' }}
#' \subsection{min_allowed_abundance}{
#' \preformatted{
#' min_allowed_abundance :  num 2
#' }}
#' \subsection{num_base_spp}{
#' \preformatted{
#' num_base_spp :  int 814
#' }}
#' \subsection{num_extra_occurrences}{
#' \preformatted{
#' num_extra_occurrences :  num 1409
#' }}
#' \subsection{num_extra_spp}{
#' \preformatted{
#' num_extra_spp :  int 463
#' }}
#' \subsection{num_PUs_to_draw}{
#' \preformatted{
#' num_PUs_to_draw :  num 6
#' }}
#' \subsection{PU_col_name}{
#' \preformatted{
#' PU_col_name :  chr "PU_ID"
#' }}
#' \subsection{PU_spp_table}{
#' \preformatted{
#' PU_spp_table : 'data.frame':	3037 obs. of  2 variables:
#'  $ PU_ID : num  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: num  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#' \subsection{rounded_abund_per_spp}{
#' \preformatted{
#' rounded_abund_per_spp :  num [1:1628] 2 2 1 1 3 1 3 2 2 1 ...
#' }}
#' \subsection{spp_col_name}{
#' \preformatted{
#' spp_col_name :  chr "spp_ID"
#' }}
#' \subsection{trimmed_rounded_abund_per_spp}{
#' \preformatted{
#' trimmed_rounded_abund_per_spp :  num [1:1277] 2 2 3 3 2 2 2 2 2 2 ...
#' }}
#' \subsection{x}{
#' \preformatted{
#' x :  int(0)
#' }}
#' \subsection{Xu_PU_spp_table}{
#' \preformatted{
#' Xu_PU_spp_table : 'data.frame':	1628 obs. of  2 variables:
#'  $ PU_ID : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: int  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @param dep_set integer vector
#' @param eligible_set integer vector
#' @param rounded_abund_per_spp integer vector
#' @param num_base_spp integer
#' @param Xu_PU_spp_table data frame
#' @param allow_imperfect_wrap boolean
#' @param min_allowed_abundance integer
#' @param PU_col_name character string
#' @param spp_col_name character string
#' @param use_testing_only_rand_seed boolean
#' @param testing_only_rand_seed integer
#'
#' @return Returns PU_spp_table data frame
#' @export

#-------------------------------------------------------------------------------

wrap_abundances_around_eligible_set <- function (
                                                dep_set,
                                                eligible_set,
                                                rounded_abund_per_spp,
                                                num_base_spp,
                                                Xu_PU_spp_table,
                                                allow_imperfect_wrap,
                                                min_allowed_abundance = 2,
                                                PU_col_name = "PU_ID",
                                                spp_col_name = "spp_ID",
                                                use_testing_only_rand_seed = FALSE,
                                                testing_only_rand_seed = 17
                                                )
    {
    cat ("\n\nStarting wrap_abundances_around_eligible_set()\n", sep='')

        #-----------------------------------------------------------------------
        #  2016 06 21 - BTL
        #  NEED TO REMOVE ANY SPP WHOSE ABUNDANCE IS 1,
        #  BECAUSE THEY MAKE THE PROBLEM MUCH EASIER, I.E., ALL PUs CONTAINING
        #  ANY SPECIES WITH ABUNDANCE = 1 MUST AUTOMATICALLY BE INCLUDED IN
        #  CORRECT SOLUTION.
        #  THEREFORE, ANY PROBLEM CAN AUTOMATICALLY BE REDUCED TO A DIFFERENT
        #  AND HARDER PROBLEM BY REMOVING ALL SINGLETON SPECIES.
        #  The same would apply to any other target value if the target
        #  was > 1.  For example, if a species had a target of 3 and it only
        #  occurred on 3 patches or less, that species would need to be
        #  removed.  This doesn't happen in these initial Xu problems since
        #  the generator currently only works for having all targets = 1.
        #-----------------------------------------------------------------------

    trimmed_rounded_abund_per_spp = trim_abundances (rounded_abund_per_spp,
                                                     min_abund=min_allowed_abundance)
    plot (sort(trimmed_rounded_abund_per_spp, decreasing = TRUE))

        #-----------------------------------------------------------------
        #  Build a vector containing the abundances of each of the extra
        #  species.
        #-----------------------------------------------------------------

    extra_abund =
        remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                               trimmed_rounded_abund_per_spp,
                                                               spp_col_name,
                                                               allow_imperfect_wrap)

    PU_spp_table =
        create_wrapping_spp_PU_spp_table (extra_abund,
                                           dep_set,
                                           eligible_set,
                                                #  Only for unit tests.
                                                #  Normally just use default
                                                #  values from arg list above.
                                           use_testing_only_rand_seed,
                                           testing_only_rand_seed
                                          )

                    cat ("\n\n--------------------------------------\n\n", "PU_spp_table = \n")#    print (PU_spp_table)    #  usually too long to print...
                    print (head (PU_spp_table))    #  usually too long to print entire table...
                    cat ("\n\n")

        #-------------------------------------------------------------------
        #  Combine the PU_spp_table just created for the wrapping species
        #  with the PU_spp_table from the original problem.
        #  Since both the extra species and the base species started their
        #  species numbering at 1, need to offset the numbering of one or
        #  the other of these two groups.  Will change the numbering of
        #  the extra species to begin just after the last of the base
        #  species.
        #  This is a fairly arbitrary choice, but it may help debugging
        #  sometime since the base species will have the same spp IDs in
        #  the wrapped set as in the base set.
        #-------------------------------------------------------------------

    PU_spp_table [, "spp_ID"] = PU_spp_table [, "spp_ID"] + num_base_spp
    PU_spp_table = rbind (Xu_PU_spp_table, PU_spp_table)

                    cat ("\n\nAfter rbind(), PU_spp_table = \n")
                    print (head (PU_spp_table))    #  usually too long to print entire table...
                    cat ("\n\n")

#docaids::doc_vars_in_this_func_once ()
    return (PU_spp_table)
    }  #  end function - wrap_abundances_around_eligible_set

#===============================================================================

#-------------------------------------------------------------------------------

#' Wrap abundance distribution around Xu problem
#'
#'  Note that this function doesn't care where you got the abundances,
#'  i.e., they don't have to have come from the lognormal generator.
#'  It can be any abundance set that you want, as long as it contains
#'  at least as many species sitting on exactly 2 patches as the base
#'  Xu problem has.  It can have more than the Xu problem, but not less.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{all_PU_IDs}{
#' \preformatted{
#' all_PU_IDs :  int [1:407] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{all_spp_IDs}{
#' \preformatted{
#' all_spp_IDs :  int [1:1277] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{bdpg_error_codes}{
#' \preformatted{
#' bdpg_error_codes : List of 6
#'  $ ERROR_STATUS_num_inside_or_within_group_links_less_than_one: num 1001
#'  $ ERROR_STATUS_optimal_solution_is_not_optimal               : num 1002
#'  $ ERROR_STATUS_num_nodes_per_group_must_be_at_least_2        : num 1003
#'  $ ERROR_STATUS_duplicate_spp_in_Xu_input_file                : num 1004
#'  $ ERROR_STATUS_unknown_spp_occ_FP_error_type                 : num 1005
#'  $ ERROR_STATUS_unknown_spp_occ_FN_error_type                 : num 1006
#' }}
#' \subsection{cor_dir_name_stem}{
#' \preformatted{
#' cor_dir_name_stem :  chr "cor"
#' }}
#' \subsection{dep_set_PUs_eligible}{
#' \preformatted{
#' dep_set_PUs_eligible :  logi FALSE
#' }}
#' \subsection{eligible_PUs}{
#' \preformatted{
#' eligible_PUs :  int [1:285] 123 124 125 126 127 128 129 130 131 132 ...
#' }}
#' \subsection{extra_nodes}{
#' \preformatted{
#' extra_nodes : 'data.frame':	285 obs. of  3 variables:
#'  $ node_ID             : int  123 124 125 126 127 128 129 130 131 132 ...
#'  $ group_ID            : logi  NA NA NA NA NA NA ...
#'  $ dependent_set_member: logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#' }}
#' \subsection{extra_PUs}{
#' \preformatted{
#' extra_PUs :  int [1:285] 123 124 125 126 127 128 129 130 131 132 ...
#' }}
#' \subsection{largest_PU_ID}{
#' \preformatted{
#' largest_PU_ID :  int 122
#' }}
#' \subsection{num_extra_PUs}{
#' \preformatted{
#' num_extra_PUs :  int 285
#' }}
#' \subsection{num_used_extra_PUs}{
#' \preformatted{
#' num_used_extra_PUs :  int 271
#' }}
#' \subsection{rounded_abundances}{
#' \preformatted{
#' rounded_abundances :  num [1:1628] 2 2 1 1 3 1 3 2 2 1 ...
#' }}
#' \subsection{starting_dir}{
#' \preformatted{
#' starting_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{tot_num_PUs_in_landscape}{
#' \preformatted{
#' tot_num_PUs_in_landscape :  num 407
#' }}
#' \subsection{unique_wrapped_PUs}{
#' \preformatted{
#' unique_wrapped_PUs :  num [1:393] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{unique_wrapped_spp}{
#' \preformatted{
#' unique_wrapped_spp :  num [1:1277] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{used_extra_PUs}{
#' \preformatted{
#' used_extra_PUs :  num [1:271] 123 124 125 126 127 128 129 130 131 132 ...
#' }}
#' \subsection{wrap_prob_name_stem}{
#' \preformatted{
#' wrap_prob_name_stem :  chr "wrap_prob"
#' }}
#' \subsection{wrapped_bdprob}{
#' \preformatted{
#' wrapped_bdprob : Formal class 'Xu_wrapped_bd_problem' [package "bdpg"] with 36 slots
#' }}
#' \subsection{wrapped_bpm}{
#' \preformatted{
#' wrapped_bpm :  num [1:1277, 1:407] 1 0 0 0 0 0 0 0 0 0 ...
#' }}
#' \subsection{wrapped_highest_PU_ID}{
#' \preformatted{
#' wrapped_highest_PU_ID :  num 407
#' }}
#' \subsection{wrapped_highest_spp_ID}{
#' \preformatted{
#' wrapped_highest_spp_ID :  num 1277
#' }}
#' \subsection{wrapped_nodes}{
#' \preformatted{
#' wrapped_nodes : 'data.frame':	407 obs. of  3 variables:
#'  $ node_ID             : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ group_ID            : num  1 1 2 2 3 3 4 4 5 5 ...
#'  $ dependent_set_member: logi  FALSE TRUE FALSE TRUE FALSE TRUE ...
#' }}
#' \subsection{wrapped_num_PUs}{
#' \preformatted{
#' wrapped_num_PUs :  num 407
#' }}
#' \subsection{wrapped_num_spp}{
#' \preformatted{
#' wrapped_num_spp :  int 1277
#' }}
#' \subsection{wrapped_PU_costs}{
#' \preformatted{
#' wrapped_PU_costs :  num [1:407] 1 1 1 1 1 1 1 1 1 1 ...
#' }}
#' \subsection{wrapped_PU_spp_indices}{
#' \preformatted{
#' wrapped_PU_spp_indices : 'data.frame':	3037 obs. of  2 variables:
#'  $ PU_ID : num  1 2 3 4 5 6 7 8 9 10 ...
#'  $ spp_ID: num  1 1 2 2 3 3 4 4 5 5 ...
#' }}
#' \subsection{wrapped_PU_vector}{
#' \preformatted{
#' wrapped_PU_vector :  num [1:3037] 1 2 3 4 5 6 7 8 9 10 ...
#' }}
#' \subsection{wrapped_spp_vector}{
#' \preformatted{
#' wrapped_spp_vector :  num [1:3037] 1 1 2 2 3 3 4 4 5 5 ...
#' }}
#' \subsection{Xu_bdprob}{
#' \preformatted{
#' Xu_bdprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{Xu_dep_and_indep_set}{
#' \preformatted{
#' Xu_dep_and_indep_set :  int [1:122] 1 3 5 7 9 11 13 15 17 19 ...
#' }}
#' \subsection{Xu_dep_set}{
#' \preformatted{
#' Xu_dep_set :  int [1:61] 2 4 6 8 10 12 14 16 18 20 ...
#' }}
#' \subsection{Xu_nodes}{
#' \preformatted{
#' Xu_nodes : 'data.frame':	122 obs. of  3 variables:
#'  $ node_ID             : int  1 2 3 4 5 6 7 8 9 10 ...
#'  $ group_ID            : num  1 1 2 2 3 3 4 4 5 5 ...
#'  $ dependent_set_member: logi  FALSE TRUE FALSE TRUE FALSE TRUE ...
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#' @param starting_dir character string
#' @param rounded_abundances DESCRIPTION.
#' @param Xu_bdprob DESCRIPTION.
#' @param dep_set_PUs_eligible DESCRIPTION.
#' @param tot_num_PUs_in_landscape DESCRIPTION.
#' @param search_outfile_name_base character string
#' @param search_outfile_name character string
#'
#' @return Returns a Xu_wrapped_bd_problem
#' @export

#-------------------------------------------------------------------------------

wrap_abundance_dist_around_Xu_problem <- function (starting_dir,
                                                  compute_network_metrics_for_this_prob,
                                                  rounded_abundances,
                                                  Xu_bdprob,
                                                  dep_set_PUs_eligible,
                                                  tot_num_PUs_in_landscape,
#                            seed_value_for_search,
                            seed_value_for_search_list,
                                        allow_imperfect_wrap,

                                                  bdpg_error_codes,
                                                  search_outfile_name_base,
                                                  search_outfile_name,
                                            wrap_prob_name_stem = "wrap_prob",
                                            cor_dir_name_stem = "cor"
                                                  )
    {
        #------------------------------------------------------------
        #  Get values for local variables to be used throughout the
        #  computations in this function.
        #------------------------------------------------------------

    Xu_nodes = Xu_bdprob@nodes
    Xu_dep_set = get_dependent_node_IDs (Xu_nodes)
    Xu_dep_and_indep_set = c(get_independent_node_IDs (Xu_nodes), Xu_dep_set)

    largest_PU_ID = max (Xu_nodes$node_ID)
    #    extra_PUs = (Xu_bdprob@num_PUs + 1) : tot_num_PUs_in_landscape
    extra_PUs = (largest_PU_ID + 1) : tot_num_PUs_in_landscape

    eligible_PUs =
        create_eligible_PU_set (Xu_dep_set, extra_PUs, dep_set_PUs_eligible)

    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    #---------------------------------------------------------------------------

        #---------------------------------------------------------------
        #  Given the rounded distribution of species abundances to
        #  wrap around the given Xu problem, do the wrap to generate a
        #  table of pairs of indices of PUs and species.
        #---------------------------------------------------------------

    wrapped_PU_spp_indices =
        wrap_abundances_around_eligible_set (Xu_dep_set,
                                             eligible_PUs,
                                             rounded_abundances,
                                             Xu_bdprob@num_spp,  #Xu_tot_num_spp,

#                                             Xu_bdprob@PU_spp_pair_indices,
                                             Xu_bdprob@cor_PU_spp_pair_indices,

                                             allow_imperfect_wrap,
                                             min_allowed_abundance = 2,

                                             Xu_bdprob@PU_col_name,
                                             Xu_bdprob@spp_col_name)

      #----------------------------------

    wrapped_PU_vector = wrapped_PU_spp_indices [, Xu_bdprob@PU_col_name]
    unique_wrapped_PUs = sort (unique (wrapped_PU_vector))

        #-------------------------------------------------------------------
        #  Highest PU_ID may not be occupied, so old counts like
        #  wrapped_highest_PU_ID and wrapped_num_PUs are wrong.
        #  This is especially important because these are the numbers that
        #  drive the dimensions of the bpm and costs and
        #  the length of marxan boolean solution indicator vectors.
        #-------------------------------------------------------------------

    wrapped_highest_PU_ID = tot_num_PUs_in_landscape    #max (unique_wrapped_PUs)
    wrapped_num_PUs = tot_num_PUs_in_landscape          #length (unique_wrapped_PUs)

    if (wrapped_num_PUs != wrapped_highest_PU_ID)
        {
        cat ("\n\nXu_bdprob@num_PUs = \n", Xu_bdprob@num_PUs)
        cat ("\nsetdiff (unique_wrapped_PUs, 1:wrapped_highest_PU_ID) = \n")
        print (setdiff (unique_wrapped_PUs, 1:wrapped_highest_PU_ID))

        missing_PU_IDs = setdiff (1:wrapped_highest_PU_ID, unique_wrapped_PUs)

        cat ("\nMissing ", length (missing_PU_IDs),
           " PU_IDs = setdiff (1:wrapped_highest_PU_ID, unique_wrapped_PUs) = \n")
        print (setdiff (1:wrapped_highest_PU_ID, unique_wrapped_PUs))
        cat ("\n")

        message (paste0 ("\n\nwrapped_num_PUs (", wrapped_num_PUs,
                         ") != wrapped_highest_PU_ID (", wrapped_highest_PU_ID,
                         ")")
                )

        if (min (missing_PU_IDs) > Xu_bdprob@num_PUs)
            {
            message (paste0 ("\nProbably just didn't draw some PU_IDs in ",
                             "extra set when placing wrapping species, since ",
                             "lowest missing PU_ID (", min (missing_PU_IDs),
                             " is larger than highest PU_ID in base problem (",
                             Xu_bdprob@num_PUs, ").\n\n"))
            } else
            {
            stop (paste0 ("\nlowest missing PU_ID (", min (missing_PU_IDs),
                             " is <= highest PU_ID in base problem (",
                             Xu_bdprob@num_PUs, ").\n\n"))
            }
        }

    used_extra_PUs = setdiff (unique_wrapped_PUs, Xu_dep_and_indep_set)
    num_used_extra_PUs = length (used_extra_PUs)
    #    unused_extra_PUs = setdiff (extra_PUs, used_extra_PUs)

num_extra_PUs = length (extra_PUs)

    extra_nodes =
            data.frame (node_ID = extra_PUs,                                 #used_extra_PUs,
                      group_ID = rep (NA, num_extra_PUs),                    #num_used_extra_PUs),
                      dependent_set_member = rep (FALSE, num_extra_PUs))     #num_used_extra_PUs))
    wrapped_nodes = rbind (Xu_bdprob@nodes, extra_nodes)

    all_PU_IDs = 1:wrapped_highest_PU_ID

      #----------------------------------

cat ("\n\nJust after loading wrapped_nodes:\n")
#browser()
# nodes = data.frame (node_ID = node_IDs,
#                     group_ID = group_IDs,
#                     dependent_set_member = dependent_set_members)

      #----------------------------------

    wrapped_spp_vector = wrapped_PU_spp_indices [, Xu_bdprob@spp_col_name]
    unique_wrapped_spp = sort (unique (wrapped_spp_vector))
    wrapped_num_spp = length (unique_wrapped_spp)
    wrapped_highest_spp_ID = max (unique_wrapped_spp)

    if (wrapped_num_spp != wrapped_highest_spp_ID)
        {
        cat ("\n\nWARNING: wrapped_num_spp (", wrapped_num_spp,
           ") != wrapped_highest_spp_ID (", wrapped_highest_spp_ID, ")")
        cat ("\n    setdiff (unique_wrapped_spp, 1:wrapped_highest_spp_ID) = ",
           setdiff (unique_wrapped_spp, 1:wrapped_highest_spp_ID))
        cat ("\n    setdiff (1:wrapped_highest_spp_ID, unique_wrapped_spp) = ",
           setdiff (1:wrapped_highest_spp_ID, unique_wrapped_spp))
        cat ("\n\n")
        }

    all_spp_IDs = 1:wrapped_highest_spp_ID

      #---------------------------------------------------------------------------
      #---------------------------------------------------------------------------
      #---------------------------------------------------------------------------

              #-----------------------------------------------------------
              #  Convert PU/spp data structure into other formats needed
              #  downstream.
              #
              #  NOTE:  I'm using wrapped_highest_???_ID instead of
              #         wrapped_num_??? in this call because I think
              #         that there may be runs where not all of the
              #         extra_PUs have something land on them.
              #         If that's the case, then the bpm adjacency
              #         matrix might end up being too small if it's
              #         dimensioned from the counts instead of from the
              #         highest ID number.  Probably won't matter for
              #         spp, but I'm doing the same for species just
              #         in case.
              #
              #         2017 12 15 - BTL
              #         This reminds me that I need to note and test the
              #         things I assume to be true about each data
              #         structure, e.g., using assertions.
              #-----------------------------------------------------------

    wrapped_PU_costs = get_PU_costs (wrapped_num_PUs)
    wrapped_bpm =
        create_adj_matrix_with_spp_rows_vs_PU_cols (
                                    wrapped_highest_spp_ID, #wrapped_num_spp,
                                    wrapped_highest_PU_ID,  #wrapped_num_PUs,
                                    wrapped_PU_spp_indices,  #wrapped_PU_spp_pair_indices,
                        wrapped_PU_costs,
                                    Xu_bdprob@spp_col_name,
                                    Xu_bdprob@PU_col_name,
                                    Xu_dep_set,
                                    Xu_bdprob@correct_solution_vector_is_known,
                                    bdpg_error_codes
                                    )

        #----------------------------------------------------------------------
        #  Now have a completed problem, so build the structure describing it
        #  for return to the caller.
        #----------------------------------------------------------------------

    wrapped_bdprob = new ("Xu_wrapped_bd_problem")

        #------------------------------------------------------------------
        #  Assign a unique identifier to this newly generated problem.
        #  These IDs are useful when combining or adding error to
        #  problems so that you can identify exactly which problems
        #  were combined or used as a base when provenance might get
        #  confusing.
        #  Also save the UUID of the problem that's being wrapped around.
        #------------------------------------------------------------------

    wrapped_bdprob@UUID                                 = uuid::UUIDgenerate()
    wrapped_bdprob@UUID_of_base_problem_that_is_wrapped = Xu_bdprob@UUID

    wrapped_bdprob@prob_is_ok                           = FALSE
    wrapped_bdprob@rand_seed                            = seed_value_for_search_list$seed_value
    wrapped_bdprob@R_internal_seed_array                = seed_value_for_search_list$R_internal_seed_array

    #----------

#*****************************************************************************************
#  Possible bug (2017 02 09):
#
#  THIS SECTION MAY BE AN ISSUE SINCE IT'S ABOUT SAVING THE PROBLEM GENERATION
#  PARAMETERS IF THEY'RE KNOWN.
#  IF WRAPPING AROUND A XU PROBLEM GENERATED FROM SCRATCH, THEN THOSE PARS ARE KNOWN.
#  IF WRAPPING AROUND A XU PROBLEM READ FROM A XU BENCHMARK FILE, THEY'RE NOT KNOWN.
#  OTHER GENERATION PARAMETERS THAT WE MIGHT WANT TO KNOW ARE THINGS ABOUT THE WRAPPING,
#  E.G., WAS A LOGNORMAL THE WRAPPER AND IF SO, WHAT WERE ITS PARAMETERS?
#  THIS ALL SUGGESTS THAT I PROBABLY NEED TO CREATE ANOTHER PROB_GEN_INFO CLASS.
#  For the moment, I'll just copy in the entries from the problem that's being wrapped.
#*****************************************************************************************

            #  Replace old Xu information with an indirection.
#    wrapped_bdprob@read_Xu_problem_from_Xu_file         = Xu_bdprob@read_Xu_problem_from_Xu_file
#    wrapped_bdprob@Xu_parameters                = Xu_bdprob@Xu_parameters

###  Is this even necessary?  Is it ever used?
    wrapped_bdprob@prob_type     = Xu_bdprob@prob_type    #  "Xu_prob_gen_info_class"

###  Should this be base_prob_gen_info instead of prob_gen_info?
    wrapped_bdprob@prob_gen_info = Xu_bdprob@prob_gen_info

###  Make a new class for wrap_gen_info?
###  Also, need to add this slot to the wrap class definition itself?
###  Would this ever be used?  Maybe in generation of problem features in g15?
###      wrapped_bdprob@wrap_gen_info = Xu_bdprob@wrap_gen_info

#*****************************************************************************************

    wrapped_bdprob@prob_generator_params_known          = Xu_bdprob@prob_generator_params_known
    wrapped_bdprob@correct_solution_vector_is_known     = Xu_bdprob@correct_solution_vector_is_known

    wrapped_bdprob@cor_PU_spp_pair_indices          = wrapped_PU_spp_indices

    wrapped_bdprob@all_PU_IDs                   = all_PU_IDs
    wrapped_bdprob@all_spp_IDs                  = all_spp_IDs

    wrapped_bdprob@PU_col_name                  = Xu_bdprob@PU_col_name
    wrapped_bdprob@spp_col_name                 = Xu_bdprob@spp_col_name
    wrapped_bdprob@num_PUs                      = wrapped_num_PUs
    wrapped_bdprob@num_spp                      = wrapped_num_spp

        #-----------------------------------------------------------------------
        #  Since the problem is wrapped around the Xu problem,
        #  the correct optimum cost is the same as for the Xu problem.
        #  The vector of costs is longer though, since there are more PUs
        #  in the wrapped problem.
        #  At the moment, those PU costs are all dummied to 1 anyway.
        #  If that ever changes, then get_PU_costs() is going to have to change.
        #-----------------------------------------------------------------------

    wrapped_bdprob@correct_solution_cost             = Xu_bdprob@correct_solution_cost  #correct_solution_cost

    wrapped_bdprob@PU_costs                     = wrapped_PU_costs    #get_PU_costs (wrapped_num_PUs)
    wrapped_bdprob@nodes                        = wrapped_nodes

    wrapped_bdprob@bpm                          = wrapped_bpm

        #-------------------------------------------------------------
        #  Quit if there are any duplicate edges/spp in the problem.
        #
        #  NOTE:  2017 02 07 - BTL
        #         This was done for the basic Xu problem, but should
        #         it still be done for the wrapped problem.
        #         I don't think it was necessary for correctness
        #         in the base problem, more likely for increasing
        #         the difficulty.
        #         I'll leave the check in here for now, but may want
        #         to change this later or else change the underlying
        #         algorithm that generates the wrapping so that it
        #         doesn't generate any duplicates if it is
        #         generating them now.
        #-------------------------------------------------------------
        #  2017 02 14 - BTL
        #  This started crashing the whole R session when it found 1
        #  duplicate link, so I'm commenting it out for now.
        #-------------------------------------------------------------

    # see_if_there_are_any_duplicate_links (wrapped_bpm,
    #                                       wrapped_bdprob@num_spp,
    #                                       bdpg_error_codes)

        #----------------------------------------
        #  Create directories for this problem.
        #----------------------------------------

    wrapped_bdprob@obj_type_str                     = "RSprob"
    wrapped_bdprob@cor_or_app_str                   = Xu_bdprob@cor_or_app_str
    wrapped_bdprob@basic_or_wrapped_or_comb_str     = "Wrap"

    wrapped_bdprob@file_name_prefix =
                            paste (wrapped_bdprob@obj_type_str,
                                   wrapped_bdprob@cor_or_app_str,
                                   wrapped_bdprob@basic_or_wrapped_or_comb_str,
                                   sep='-')

    create_RSprob_dir_and_subdirs (starting_dir,  #  parameters$fullOutputDir_NO_slash,  #  usually parameters$fullOutputDir_NO_slash
                                   wrapped_bdprob)

        #-----------------------------------------------------------------
        #  Tracking of details of the search for a wrapping distribution
        #  were written to a file in the top directory of the tzar run
        #  before the final directory for the wrapped problem was known.
        #  Move search tracking file to the newly created dir of the
        #  wrapped problem.
        #-----------------------------------------------------------------

    if (file.exists (search_outfile_name))
        {
        file.rename (search_outfile_name,
                     file.path (get_RSprob_path_topdir (wrapped_bdprob,
                                                        starting_dir),
                              search_outfile_name_base))
        }

        #-----------------------------------------------------------------
        #  Compute and save the distribution and network metrics for the
        #  problem.
        #-----------------------------------------------------------------

        #  Summarize and plot graph and distribution structure information.

    wrapped_bdprob@final_link_counts_for_each_node =
        summarize_and_plot_graph_and_distribution_structure_information (

                  wrapped_bdprob@cor_PU_spp_pair_indices,
                  "COR",
                  wrapped_bdprob@all_PU_IDs,    #####!!!!!#####all_correct_node_IDs,
                  get_RSprob_path_plots (wrapped_bdprob, starting_dir),
                  wrapped_bdprob@spp_col_name,
                  wrapped_bdprob@PU_col_name,
                  wrapped_bdprob@presences_col_name
                  )

        #----------------------------
        #  Compute network metrics.
        #----------------------------

    wrapped_bdprob =
        init_object_graph_data (
            wrapped_bdprob,
            starting_dir,
            value_or_FALSE_if_null (parameters$compute_network_metrics),
            compute_network_metrics_for_this_prob,
            value_or_FALSE_if_null (parameters$use_igraph_metrics),
            value_or_FALSE_if_null (parameters$use_bipartite_metrics),
            parameters$bipartite_metrics_to_use)

        #------------------------------------------------------------
        #  Everything seems to have worked.
        #  Save the bdprob to disk as a first cut at how to archive
        #  and retrieve problems in general.
        #  This particular bit of code may disappear later on, once
        #  it's clearer how to archive.
        #------------------------------------------------------------

    wrapped_bdprob@prob_is_ok                   = TRUE

    wrapped_bdprob = save_rsprob (wrapped_bdprob, starting_dir)
    save_rsprob_results_data (wrapped_bdprob, starting_dir, parameters)

#docaids::doc_vars_in_this_func_once ()

    return (wrapped_bdprob)  #  end function - wrap_abundance_dist_around_Xu_problem
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Generate a correct wrapped Xu biodiversity problem from a correct base problem
#'
#' Wrap a given distribution around a given Xu problem's distribution.
#' That is, add more planning units and species to the flat Xu distribution's
#' set of PUs and species so that the Xu distribution is a proper subset of
#' the larger distribution.
#'
#' This is intended as a way of embedding the Xu problem's unrealistic
#' species distribution (i.e., every species occurs on exactly 2 patches)
#' inside a more realistic distribution, but one that has exactly the same
#' correct solution set as the base Xu problem.
#'
#'@section Restrictions on wrapping distribution:
#' At the moment, the only wrapping distribution that there is code for
#' generating is the lognormal distribution.  However, the basic idea allows
#' for ANY distribution where the base set of Xu species and planning units
#' is a proper subset of the final distribution.
#'
#' To enhance the capabilities of this routine to allow other distributions,
#' you would just have to
#'
#' \itemize{
#'    \item{provide some kind of option(s) to choose what kind
#' of wrapping distribution to use}
#'    \item{provide a function to generate that wrapping distribution}
#'    \item{replace the call to find_lognormal_to_wrap_around_Xu() with
#'    the new function}
#'}
#'
#' One caveat is that all species that occur on one and only one planning unit
#' are removed from the distribution.  This is because those species would
#' automatically require their planning unit to be included in the final
#' solution and therefore, make the problem simpler for the optimizer.
#'
#-------------------------------------------------------------------------------

#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{add_one_to_lognormal_abundances}{
#' \preformatted{
#' add_one_to_lognormal_abundances :  logi FALSE
#' }}
#' \subsection{base_bdprob}{
#' \preformatted{
#' base_bdprob : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{bdpg_error_codes}{
#' \preformatted{
#' bdpg_error_codes : List of 6
#'  $ ERROR_STATUS_num_inside_or_within_group_links_less_than_one: num 1001
#'  $ ERROR_STATUS_optimal_solution_is_not_optimal               : num 1002
#'  $ ERROR_STATUS_num_nodes_per_group_must_be_at_least_2        : num 1003
#'  $ ERROR_STATUS_duplicate_spp_in_Xu_input_file                : num 1004
#'  $ ERROR_STATUS_unknown_spp_occ_FP_error_type                 : num 1005
#'  $ ERROR_STATUS_unknown_spp_occ_FN_error_type                 : num 1006
#' }}
#' \subsection{dep_set_PUs_eligible}{
#' \preformatted{
#' dep_set_PUs_eligible :  logi FALSE
#' }}
#' \subsection{desired_max_abundance_frac}{
#' \preformatted{
#' desired_max_abundance_frac :  num 0.7
#' }}
#' \subsection{desired_Xu_spp_frac_of_all_spp}{
#' \preformatted{
#' desired_Xu_spp_frac_of_all_spp :  num 0.5
#' }}
#' \subsection{max_search_iterations}{
#' \preformatted{
#' max_search_iterations :  num 500
#' }}
#' \subsection{parameters}{
#' \preformatted{
#' parameters : List of 66
#'  $ summary_without_run_id_filename                           : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/prob_diff_results_with_0_ru"| __truncated__
#'  ...
#'  $ fullOutputDir_NO_slash                                    : chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{rounded_abundances}{
#' \preformatted{
#' rounded_abundances :  num [1:1628] 2 2 1 1 3 1 3 2 2 1 ...
#' }}
#' \subsection{search_outfile_name}{
#' \preformatted{
#' search_outfile_name :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/wrap_search_outfile.csv"
#' }}
#' \subsection{seed_value_for_search}{
#' \preformatted{
#' seed_value_for_search :  num 11
#' }}
#' \subsection{solution_frac_of_landscape}{
#' \preformatted{
#' solution_frac_of_landscape :  num 0.3
#' }}
#' \subsection{starting_dir}{
#' \preformatted{
#' starting_dir :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress"
#' }}
#' \subsection{tot_num_PUs_in_landscape}{
#' \preformatted{
#' tot_num_PUs_in_landscape :  num 407
#' }}
#' \subsection{wrapped_bdprob_COR}{
#' \preformatted{
#' wrapped_bdprob_COR : Formal class 'Xu_wrapped_bd_problem' [package "bdpg"] with 36 slots
#' }}
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#' @param starting_dir character string
#' @param base_bdprob a correct Xu_bd_problem whose correct solution vector is
#' known
#'
#' @return Returns a correct wrapped biodiversity problem
#' @export

#-------------------------------------------------------------------------------

gen_wrapped_bdprob_COR <- function (starting_dir,
                                    compute_network_metrics_for_this_prob,
                                    parameters,
                                    base_bdprob,
                                    bdpg_error_codes)
    {
    if (options_are_legal_for_single_bdprob_WRAP (parameters))
        {
            #---------------------------------
            #  Control parameters from user.
            #---------------------------------

        desired_Xu_spp_frac_of_all_spp  = parameters$desired_Xu_spp_frac_of_all_spp
        solution_frac_of_landscape      = parameters$solution_frac_of_landscape
        desired_max_abundance_frac      = parameters$desired_max_abundance_frac
        dep_set_PUs_eligible            = value_or_FALSE_if_null (parameters$dep_set_PUs_eligible)
        add_one_to_lognormal_abundances = value_or_FALSE_if_null (parameters$add_one_to_lognormal_abundances)
        max_search_iterations           = parameters$max_search_iterations

            #-----------------------
            #  Derived parameters.
            #-----------------------

        tot_num_PUs_in_landscape = round (get_num_nodes (base_bdprob@nodes) /
                                          solution_frac_of_landscape)

        search_outfile_name_base = "wrap_search_outfile.csv"
        search_outfile_name      = file.path (starting_dir,
                                              search_outfile_name_base)

            #-----------------------------------------------------
            #  Set random seed if parameters specify doing that.
            #-----------------------------------------------------

        seed_value_for_search_list =
            set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
                                                      is_rsprob = TRUE,
                                                      parameters,
                                                      cor_or_app_str = "COR",
                                                      basic_or_wrapped_or_comb_str = "WRAP",
                                                      location_string = "Start of wrap_abundance_dist_around_Xu_problem(),COR,WRAP")

            #-----------------------------------------------------------
            #  Search for a set of lognormal parameters that fit the
            #  user's constraints while including the base Xu problem.
            #-----------------------------------------------------------

        rounded_abundances =
            find_lognormal_to_wrap_around_Xu (base_bdprob,
                                              parameters,
                                              desired_Xu_spp_frac_of_all_spp,
                                              solution_frac_of_landscape,
                                              desired_max_abundance_frac,
#                                seed_value_for_search,
                                seed_value_for_search_list$seed_value,
                                              max_search_iterations,
                                              add_one_to_lognormal_abundances,
                                              search_outfile_name)

            #--------------------------------------------------------------
            #  Wrap the generated lognormal around the Xu base problem.
            #
            #  Note that the wrap_abundance_dist_around_Xu_problem()
            #  function doesn't care where you got the abundances, i.e.,
            #  they don't have to have come from the lognormal generator.
            #
            #  It can be any abundance set that you want, as long as it
            #  contains at least as many species sitting on exactly
            #  2 patches as the base Xu problem has.
            #
            #  It can have more species that sit on exactly 2 patches
            #  than the base Xu problem, but not less.
            #--------------------------------------------------------------

        wrapped_bdprob_COR =
            wrap_abundance_dist_around_Xu_problem (starting_dir,
                                                   compute_network_metrics_for_this_prob,
                                                   rounded_abundances,
                                                   base_bdprob,
                                                   dep_set_PUs_eligible,
                                                   tot_num_PUs_in_landscape,
#                                seed_value_for_search,
                                seed_value_for_search_list,
                                        value_or_FALSE_if_null (parameters$allow_imperfect_wrap),

                                                   bdpg_error_codes,
                                                   search_outfile_name_base,
                                                   search_outfile_name)

        }  #  end if - options_are_legal_for_single_bdprob_WRAP
#docaids::doc_vars_in_this_func_once ()

    return (wrapped_bdprob_COR)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Check whether options for wrapping have legal values
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return boolean with TRUE if options are legal and FALSE otherwise

#-------------------------------------------------------------------------------

options_are_legal_for_single_bdprob_WRAP <- function (parameters)
    {
        #----------------------------------------------------------------------
        #  Make sure that the base problem for the multiproblem is not one of
        #  Xu's benchmark problems read in from a file, since they do not
        #  contain the correct solution set.  They only contain the correct
        #  solution cost.
        #----------------------------------------------------------------------

    if (value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file))
        {
        stop (paste0 ("\n\nParameter read_Xu_problem_from_Xu_file is TRUE.",
                    "\nCannot wrap around Xu problem read from file ",
                    "because correct solution IDs ",
                    "\nare not given with the file.",
                    "\nOnly the correct cost is given.",
                    "\nQuitting.\n\n")
            )
        }

        #----------------------------------------------------------------------
        #  Base problem is not a Xu benchmark problem, so try to do the wrap
        #  now.
        #  At the moment, the only kind of wrap that's available is the
        #  lognormal, so check to make sure that is the type that has been
        #  requested.  If not, then fail.  Otherwise, do the lognormal wrap
        #  now.
        #----------------------------------------------------------------------

    if (! value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu))
        {
            #-------------------------------------------------------------
            #  Wrap request is not for a lognormal distribution, so fail.
            #-------------------------------------------------------------

        stop (paste0 ("\n\nwrap_lognormal_dist_around_Xu is not set to TRUE.  ",
                    "\n    It is currently the only defined wrap function.\n")
            )
        }

    return (TRUE)
    }

#===============================================================================

#-------------------------------------------------------------------------------

#' Generate a single wrapped biodiversity problem from a given base problem
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#' @param starting_dir character string
#' @param base_bdprob a correct Xu_bd_problem whose correct solution vector is
#' known
#'
#' @return Returns a correct wrapped biodiversity problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_WRAP <- function (bdprob_to_wrap,
                                    parameters,
                                    bdpg_error_codes)
    {
    starting_dir =
        file.path (normalizePath (parameters$full_output_dir_with_slash))

    compute_network_metrics_for_this_prob =
        value_or_FALSE_if_null (parameters$compute_network_metrics_wrapped_COR)

    WRAP_prob =
        gen_wrapped_bdprob_COR (starting_dir,
                                compute_network_metrics_for_this_prob,
                                parameters,
                                bdprob_to_wrap,
                                bdpg_error_codes)

    return (WRAP_prob)
    }

#===============================================================================

