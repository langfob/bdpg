#===============================================================================
#                           Generate a wrapped problem.
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

#  New version:

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
              spp_col_name)
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
                            }

        #  Count the number of occurrences of each species in the base problem.
    base_abund_by_spp = plyr::count (Xu_PU_spp_table [,spp_col_name])
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    base_abund_by_spp = \n")
                            show (base_abund_by_spp)
                            }

        #  Count how many times each number of occurrences appears in that set
        #  (e.g., how many species occur on 2 patches, on 3 patches, etc.).
    base_abund_hist         = plyr::count (base_abund_by_spp [,"freq"])
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    base_abund_hist = \n")
                            show (base_abund_hist)
                            }

        #  Do the same for the wrapping abundances.
        #  They are given as a vector of numbers of occurrences for each
        #  species in the wrapping distribution, but unlike the base problem
        #  abundances, no species ID has been assigned yet to these wrapping
        #  counts.
    wrapping_abund_hist     = plyr::count (trimmed_rounded_abund_per_spp)
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    wrapping_abund_hist = \n")
                            show (wrapping_abund_hist)
                            }

        #  There will probably be some different elements in the two histograms.
        #  For example, the original Xu histogram will only have one entry,
        #  i.e., the count for abundance = 2, since all species occur on
        #  exactly 2 patches.  The wrapping histogram will probably (though
        #  not necessarily) have entries for other patch counts and may have
        #  a different value than the Xu histogram for the number of species
        #  occurring on 2 patches.
        #  We need to make a single histogram that merges the two sets of
        #  possible counts.  The merge() function does this by making a single
        #  data.frame with a column for each of the 2 input histograms,
        #  labelling one column as x and the other as y.  It adds a row for
        #  each matching entry in the inputs (e.g., a row for abundance = 2,
        #  with the x column giving the count for the wrapping histogram and
        #  the y column giving the count for the Xu histogram).
        #  It also adds a row for each entry in either input that doesn't
        #  appear in the other input (e.g., a row for every abundance other
        #  than 2 in the wrapping histogram when it's wrapping around an
        #  original Xu histogram that only has the value for abundance = 2).

    wrapped_extra_spp_abund_merge = merge (x=wrapping_abund_hist,
                                                y=base_abund_hist,
                                                by="x", all=TRUE)
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    wrapped_extra_spp_abund_merge = \n")
                            show (wrapped_extra_spp_abund_merge)
                            }

        #  Now we need to clean up this data frame so that NAs are replaced
        #  with 0's and so that any missing abundance values are added to
        #  the data.frame (e.g., if the highest abundance value was 10 but
        #  neither input histogram had any species that occurred on 3, 4, or
        #  9 PUs).




            #  Replace NA counts with 0s.
    wrapped_extra_spp_abund_merge [is.na (wrapped_extra_spp_abund_merge)] = 0
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    After NA replacement with 0, wrapped_extra_spp_abund_merge = \n")
                            show (wrapped_extra_spp_abund_merge)
                            }

    wrapped_extra_spp_abund_hist =
        as.data.frame (cbind (wrapped_extra_spp_abund_merge [,"x"],
                              wrapped_extra_spp_abund_merge [, "freq.x"] - wrapped_extra_spp_abund_merge [, "freq.y"]
                              ))
    names (wrapped_extra_spp_abund_hist) = c("abund","freq")
                        if (verbose_remove_base)
                            {
                            cat ("\n\n    Final wrapped_extra_spp_abund_hist = \n")
                            show (wrapped_extra_spp_abund_hist)
                            }


    num_extra_spp = sum (wrapped_extra_spp_abund_hist [,"freq"])
    extra_spp_abund = rep (NA, num_extra_spp)
    num_abund_rows = length (wrapped_extra_spp_abund_hist[,"abund"])
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

#docaids::doc_vars_in_this_func_once ()
    return (extra_spp_abund)
    }

#===============================================================================

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
#' @param min_allowed_abundance integer
#' @param PU_col_name character string
#' @param spp_col_name character string
#'
#' @return Returns PU_spp_table data frame

#-------------------------------------------------------------------------------

wrap_abundances_around_eligible_set <- function (dep_set,
                                                 eligible_set,
                                                 rounded_abund_per_spp,
                                                 num_base_spp,
                                                 Xu_PU_spp_table,
                                                 min_allowed_abundance = 2,
                                                 PU_col_name = "PU_ID",
                                                 spp_col_name = "spp_ID"
                                                )
    {
    cat ("\n\nStarting wrap_abundances_around_eligible_set()\n", sep='')

    #----------------------------------



        #  2016 06 21 - BTL
        #  NEED TO REMOVE ALL SPP WHOSE ABUNDANCE IS 1,
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


    trimmed_rounded_abund_per_spp = trim_abundances (rounded_abund_per_spp,
                                                     min_abund=min_allowed_abundance)
    plot (sort(trimmed_rounded_abund_per_spp, decreasing = TRUE))

    #----------------------------------

#  FIXED NOW?
 # BUG: THIS SECTION OF CODE ASSUMES THAT extra_abund IS A VECTOR CONTAINING
 #      THE ABUNDANCE OF EACH EXTRA SPECIES.
 #      AT THE MOMENT, extra_abund ACTUALLY CONTAINS THE NUMBER OF SPECIES
 #      HAVING EACH GIVEN ABUNDANCE.
 #      NEED TO CHANGE EXPAND WHAT'S RETURNED INTO THE EXPECTED VECTOR.
 #      CAN THAT BE DONE USING ONE OF THE PLYR THINGS LIKE MELT()?

                    extra_abund =
                        # remove_base_spp_abundances_from_wrapping_distribution (trimmed_rounded_abund_per_spp,
                        #                                                        num_base_spp,
                        #                                                        spp_col_name)
                        remove_base_spp_abundances_from_wrapping_distribution (Xu_PU_spp_table,
                                                                               trimmed_rounded_abund_per_spp,
                                                                               spp_col_name)

                    num_extra_occurrences = sum (extra_abund)
                    num_extra_spp         = length (extra_abund)

#  Should these be initialized to NA instead of 0?  Does it matter?
    PU_spp_table = data.frame (PU_ID = rep (0, num_extra_occurrences),
                               spp_ID = rep (0, num_extra_occurrences))

#browser()
    cur_row = 1

    for (cur_spp_idx in 1:num_extra_spp)
        {
#        cat ("\n\ncur_spp_idx = ", cur_spp_idx, "\n", sep='')

        num_PUs_to_draw = extra_abund [cur_spp_idx]

            #  Draw the one mandatory PU in the dependent set for this species.

        dep_set_PU = safe_sample (dep_set, 1, replace=FALSE)
        PU_spp_table [cur_row, "PU_ID"] = dep_set_PU
        PU_spp_table [cur_row, "spp_ID"] = cur_spp_idx
        cur_row = cur_row + 1

            #  If that PU was part of the overall eligible set
            #  (e.g., the eligible set was the union of the dependent set and
            #   the extra set rather than just being the extra set),
            #  remove it from the eligible set of PUs.

        x = which (eligible_set == dep_set_PU)
        cur_eligible_set =
            if (length (x) > 0) eligible_set [-x] else eligible_set

        num_PUs_to_draw = num_PUs_to_draw - 1
    # cat ("\nAbout to check num_PUs_to_draw > 0 for cur_spp_idx = ",
    #      cur_spp_idx, sep='')
#browser()
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

    cat ("\n\n--------------------------------------\n\n", "PU_spp_table = \n")
#    print (PU_spp_table)    #  usually too long to print...
    print (head (PU_spp_table))
    cat ("\n\n")

        #-------------------------------------------------------------------
        #  Add the occurrences from the original problem to the table
        #  just created for the wrapped problem.
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
#    print (PU_spp_table)    #  usually too long to print...
    print (head (PU_spp_table))
    cat ("\n\n")

#browser()

#docaids::doc_vars_in_this_func_once ()
    return (PU_spp_table)

    }  #  end function - wrap_abundances_around_eligible_set

#===============================================================================

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

wrap_abundance_dist_around_Xu_problem = function (starting_dir,
                                                  compute_network_metrics_for_this_prob,
                                                  rounded_abundances,
                                                  Xu_bdprob,
                                                  dep_set_PUs_eligible,
                                                  tot_num_PUs_in_landscape,
                            seed_value_for_search,
                                                  bdpg_error_codes,
                                                  search_outfile_name_base,
                                                  search_outfile_name,
                                            wrap_prob_name_stem = "wrap_prob",
                                            cor_dir_name_stem = "cor"
                                                  )
    {
        #  Get values for local variables to be used throughout the
        #  computations in this function.

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

        #  Given the rounded distribution of species abundances to
        #  wrap around the given Xu problem, do the wrap to generate a
        #  table of pairs of indices of PUs and species.

    wrapped_PU_spp_indices =
        wrap_abundances_around_eligible_set (Xu_dep_set,
                                             eligible_PUs,
                                             rounded_abundances,
                                             Xu_bdprob@num_spp,  #Xu_tot_num_spp,

#                                             Xu_bdprob@PU_spp_pair_indices,
                                             Xu_bdprob@cor_PU_spp_pair_indices,

                                             min_allowed_abundance = 2,
                                             Xu_bdprob@PU_col_name,
                                             Xu_bdprob@spp_col_name)

      #----------------------------------

    wrapped_PU_vector = wrapped_PU_spp_indices [, Xu_bdprob@PU_col_name]
    unique_wrapped_PUs = sort (unique (wrapped_PU_vector))

            #  Highest PU_ID may not be occupied, so old counts like
            #  wrapped_highest_PU_ID and wrapped_num_PUs are wrong.
            #  This is especially important because these are the numbers that
            #  drive the dimensions of the bpm and costs and
            #  the length of marxan boolean solution indicator vectors.

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
#                                    Xu_bdprob@bdpg_error_codes
                                    bdpg_error_codes
                                    )

      #---------------------------------------------------------------------------

          #  Now have a completed problem, so build the structure describing it
          #  for return to the caller.

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
    wrapped_bdprob@rand_seed                            = seed_value_for_search

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

#    wrapped_bdprob@PU_spp_pair_indices          = wrapped_PU_spp_indices
    wrapped_bdprob@cor_PU_spp_pair_indices          = wrapped_PU_spp_indices

    wrapped_bdprob@all_PU_IDs                   = all_PU_IDs
    wrapped_bdprob@all_spp_IDs                  = all_spp_IDs

    wrapped_bdprob@PU_col_name                  = Xu_bdprob@PU_col_name
    wrapped_bdprob@spp_col_name                 = Xu_bdprob@spp_col_name
    wrapped_bdprob@num_PUs                      = wrapped_num_PUs
    wrapped_bdprob@num_spp                      = wrapped_num_spp

          #  Since the problem is wrapped around the Xu problem,
          #  the correct optimum cost is the same as for the Xu problem.
          #  The vector of costs is longer though, since there are more PUs
          #  in the wrapped problem.
          #  At the moment, those PU costs are all dummied to 1 anyway.
          #  If that ever changes, then get_PU_costs() is going to have to change.

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

#                  wrapped_bdprob@PU_spp_pair_indices,
                  wrapped_bdprob@cor_PU_spp_pair_indices,

                  "COR",
                  wrapped_bdprob@all_PU_IDs,    #####!!!!!#####all_correct_node_IDs,

                  #wrapped_bdprob@derived_bdpg_dir_names$plot_output_dir,
                  get_RSprob_path_plots (wrapped_bdprob, starting_dir),

                  wrapped_bdprob@spp_col_name,
                  wrapped_bdprob@PU_col_name,
                  wrapped_bdprob@presences_col_name
                  )

        #  Compute network metrics.

    wrapped_bdprob =
        init_object_graph_data (
            wrapped_bdprob,
            starting_dir,
            value_or_FALSE_if_null (parameters$compute_network_metrics),
            #parameters$compute_network_metrics_wrapped_COR,
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

#     wrapped_bdprob@basic_or_wrapped_or_comb_str = "wrapped"
#
# #    wrapped_bdprob@full_saved_bdprob_path =
#         save_bdprob (wrapped_bdprob@basic_or_wrapped_str, "COR",
#                      wrapped_bdprob@UUID,
#                      get_RSprob_path_topdir (wrapped_bdprob, starting_dir),
#                      # wrapped_bdprob@prob_outdir,
#                      wrapped_bdprob)

    wrapped_bdprob <- save_rsprob (wrapped_bdprob, starting_dir)

#    save_rsprob_results_data_for_Xu_NOT_read_from_bench_file (wrapped_bdprob,
    save_rsprob_results_data (wrapped_bdprob, starting_dir, parameters)

#docaids::doc_vars_in_this_func_once ()

    return (wrapped_bdprob)  #  end function - wrap_abundance_dist_around_Xu_problem
    }

#===============================================================================

#' Generate COR wrapped bd problem
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
#' @param base_bdprob Xu_bd_problem
#'
#' @return Returns wrapped_bdprob_COR
#' @export

#-------------------------------------------------------------------------------

gen_wrapped_bdprob_COR <- function (starting_dir,
                                    compute_network_metrics_for_this_prob,
                                    parameters,
                                    base_bdprob,
                                    bdpg_error_codes)
    {
    if (value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu))
        {
            #---------------------------------
            #  Control parameters from user.
            #---------------------------------

        desired_Xu_spp_frac_of_all_spp  = parameters$desired_Xu_spp_frac_of_all_spp
        solution_frac_of_landscape      = parameters$solution_frac_of_landscape
        desired_max_abundance_frac      = parameters$desired_max_abundance_frac
        dep_set_PUs_eligible            = parameters$dep_set_PUs_eligible
        add_one_to_lognormal_abundances = parameters$add_one_to_lognormal_abundances
        max_search_iterations           = parameters$max_search_iterations


    # forced_seed =
    #     get_forced_seed_value_if_necessary (is_rsrun = FALSE,
    #                                         is_rsprob = TRUE,
    #                                         parameters,
    #                                         cor_or_app = "COR",
    #                                         basic_or_wrapped_or_comb_str = "WRAP")
    #
    # seed_value_for_search =
    #     set_new_or_forced_rand_seed_if_necessary (value_or_FALSE_if_null (parameters$set_rand_seed_at_creation_of_all_new_major_objects),
    #                                               "Start of wrap_abundance_dist_around_Xu_problem(),COR,WRAP",
    #                                               forced_seed)

    seed_value_for_search =
        set_new_or_forced_rand_seed_if_necessary (is_rsrun = FALSE,
                                                  is_rsprob = TRUE,
                                                  parameters,
                                                  cor_or_app_str = "COR",
                                                  basic_or_wrapped_or_comb_str = "WRAP",
                                                  location_string = "Start of wrap_abundance_dist_around_Xu_problem(),COR,WRAP")

            #-----------------------
            #  Derived parameters.
            #-----------------------

        tot_num_PUs_in_landscape = round (get_num_nodes (base_bdprob@nodes) /
                                          solution_frac_of_landscape)

        # search_outfile_name      = paste0 (parameters$full_output_dir_with_slash,
        #                                    "wrap_search_outfile.csv")
        search_outfile_name_base = "wrap_search_outfile.csv"
        search_outfile_name      = file.path (starting_dir,
                                              search_outfile_name_base)

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
seed_value_for_search,
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
                                        seed_value_for_search,
                                                   bdpg_error_codes,
                                                   search_outfile_name_base,
                                                   search_outfile_name)

        } else  #  lognormal wrap not chosen and that's the only kind allowed
        {       #  at the moment, so quit

        stop (paste0 ("\n\nIn gen_wrapped_bdprob_COR():  ",
                      "parameters$wrap_lognormal_dist_around_Xu == FALSE\n",
                      "    Currently, the only type of wrapping done is ",
                      "wrapping lognormal around Xu.\n\n"))
        }

#docaids::doc_vars_in_this_func_once ()

    return (wrapped_bdprob_COR)
    }

#===============================================================================

#' Generate a single wrapped Xu biodiversity problem
#'
#' @param bdprob_to_wrap a bdproblem to wrap a distribution around
#' @param src_rds_file_dir character string
#' @inheritParams std_param_defns
#'
#' @return Returns a wrapped Xu biodiversity problem
#' @export

#-------------------------------------------------------------------------------

gen_single_bdprob_WRAP <- function (bdprob_to_wrap,
                                    parameters,
                                    bdpg_error_codes,
                                    src_rds_file_dir=NULL)
    {
    wrap_lognormal_dist_around_Xu =
        value_or_FALSE_if_null (parameters$wrap_lognormal_dist_around_Xu)

    read_Xu_problem_from_Xu_bench_file =
        value_or_FALSE_if_null (parameters$read_Xu_problem_from_Xu_bench_file)

    #---------------------------------------------------------------------------

        #----------------------------------------------------------------------
        #  Make sure that the base problem for the multiproblem is not one of
        #  Xu's benchmark problems read in from a file, since they do not
        #  contain the correct solution set.  They only contain the correct
        #  solution cost.
        #----------------------------------------------------------------------

    if (wrap_lognormal_dist_around_Xu &   #(parameters$wrap_lognormal_around_Xu &
        read_Xu_problem_from_Xu_bench_file)   # parameters$read_Xu_problem_from_Xu_file)
        {
        stop (paste0 ("\n\nParameters wrap_lognormal_dist_around_Xu and ",
                    "read_Xu_problem_from_Xu_file ",
                    "\nare both true.",
                    "\nCannot wrap around Xu problem read from file ",
                    "because dependent node IDs ",
                    "\nare never given with the file.",
                    "\nQuitting.\n\n")
            )
        }

    #---------------------------------------------------------------------------

    if (wrap_lognormal_dist_around_Xu)  #parameters$wrap_lognormal_around_Xu)
        {
        starting_dir =
            file.path (normalizePath (parameters$full_output_dir_with_slash))
            # ,
            # "wrap_prob.1")

        compute_network_metrics_for_this_prob =
#            value_or_FALSE_if_null (parameters$compute_network_metrics_for_this_prob)
            value_or_FALSE_if_null (parameters$compute_network_metrics_wrapped_COR)

        WRAP_prob =
            gen_wrapped_bdprob_COR (starting_dir,
                                    compute_network_metrics_for_this_prob,
                                    parameters,
                                    bdprob_to_wrap,
                                    bdpg_error_codes)
        } else
        {
        stop (paste0 ("\n\nwrap_lognormal_dist_around_Xu is not set to TRUE.  ",
                    "\n    It is currently the only defined wrap function.\n")
            )
        }

    return (WRAP_prob)
    }

#===============================================================================

