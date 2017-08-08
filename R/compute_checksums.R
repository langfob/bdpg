#===============================================================================
#
#                           compute_checksums.R
#
#===============================================================================

#' Compute a checksum for the object
#'
#' The point of this function is to give a quick way to compare two complex
#' objects to see whether they are really the same object excluding their UUID
#' and checksum fields.  This is useful when running lots of experiments over
#' and over during development where the same random seed may end up
#' accidentally being re-used and generating identical experiments.  When lots
#' experiments are archived, it may be necessary to quickly cross-compare many
#' archived experiments against each other and having a stored checksum will
#' make it easier to do this and therfore, easier to spot accidental
#' duplicates.
#'
#' This function works by replacing the UUID and checksum fields of the local
#' copy of the object with empty strings and then writing the object copy to a
#' temporary file.  Then, a checksum is computed for that temporary file and
#' file.  The modified original object is not returned from the function.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{base_outdir_for_checksum}{
#' \preformatted{
#' base_outdir_for_checksum :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{checksum}{
#' \preformatted{
#' checksum :  Named chr "8e5ad448864db57de0e617aff6929882"
#' }}
#' \subsection{full_saved_obj_path}{
#' \preformatted{
#' full_saved_obj_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{obj_with_UUID_and_checksum}{
#' \preformatted{
#' obj_with_UUID_and_checksum : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{saved_obj_with_checksum_filename}{
#' \preformatted{
#' saved_obj_with_checksum_filename :  chr "tmp_for_checksum__ok_to_delete_if_left_after_run.rds"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return character string checksum of the object when object has been written
#'     out as a file

compute_obj_checksum <- function (obj_with_UUID_and_checksum,
                                  base_outdir_for_checksum=".")
    {
        #------------------------------------------------------------------
        #  Clear the UUID and checksum fields if they exist,
        #  since we don't want them to cause the checksum to be different
        #  when two objects are identical other than those two fields.
        #------------------------------------------------------------------

    if ("UUID" %in% methods::slotNames (obj_with_UUID_and_checksum))
        obj_with_UUID_and_checksum@UUID <- ""
    if ("checksum" %in% methods::slotNames (obj_with_UUID_and_checksum))
        obj_with_UUID_and_checksum@checksum <- ""

        #--------------------------------------------------------------
        #  Write the object to a temporary file that checksum can be
        #  computed over (checksum only works on files, not objects).
        #--------------------------------------------------------------

    saved_obj_with_checksum_filename = paste0 ("tmp_for_checksum__ok_to_delete_if_left_after_run.rds")
    full_saved_obj_path = file.path (normalizePath (base_outdir_for_checksum),
                                     saved_obj_with_checksum_filename)
    saveRDS (obj_with_UUID_and_checksum, full_saved_obj_path)

        #-----------------------------------------------------
        #  Now ready to compute the checksum
        #  and get rid of the temporary file created for it.
        #-----------------------------------------------------

    checksum = tools::md5sum (full_saved_obj_path)
    if (file.exists (full_saved_obj_path)) file.remove (full_saved_obj_path)

#docaids::doc_vars_in_this_func_once ()
    return (checksum)
    }

#===============================================================================

#' Convenience function for computing the object's checksum and setting that slot
#'
#' This function computes the checksum and sets the checksum slot in the object,
#' then returns the modified object.  It just packages those up for convenience.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{base_outdir_for_checksum}{
#' \preformatted{
#' base_outdir_for_checksum :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{obj_with_UUID_and_checksum}{
#' \preformatted{
#' obj_with_UUID_and_checksum : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return  the same object that was passed in except that its checksum is
#'     now set

compute_and_set_obj_checksum <- function (obj_with_UUID_and_checksum,
                                          base_outdir_for_checksum=".")
    {
    obj_with_UUID_and_checksum@checksum <-
        compute_obj_checksum (obj_with_UUID_and_checksum,
                              base_outdir_for_checksum)

#docaids::doc_vars_in_this_func_once ()
    return (obj_with_UUID_and_checksum)
    }

#===============================================================================

#' Save object with checksum
#'
#' Save the object to disk with its checksum slot set.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{base_outdir_for_checksum}{
#' \preformatted{
#' base_outdir_for_checksum :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{full_saved_obj_path}{
#' \preformatted{
#' full_saved_obj_path :  chr "/Users/bill/tzar/outputdata/biodivprobgen/default_runset/1837_marxan_simulated_annealing.inprogress/RSprob-COR-Base.d0729e1c-ea"| __truncated__
#' }}
#' \subsection{obj_with_UUID_and_checksum}{
#' \preformatted{
#' obj_with_UUID_and_checksum : Formal class 'Xu_bd_problem' [package "bdpg"] with 35 slots
#' }}
#' \subsection{saved_obj_with_checksum_filename}{
#' \preformatted{
#' saved_obj_with_checksum_filename :  chr "saved.RSprob-COR-Base.d0729e1c-eadc-4899-a382-8cb7ac2c08d7.rds"
#' }}
#'
#' @inheritParams std_param_defns
#'
#' @return Returns input object with its checksum slot filled

save_obj_with_checksum = function (obj_with_UUID_and_checksum,
#  no longer used?                                   saved_obj_with_checksum_filename,
                                   base_outdir_for_checksum)
    {
    obj_with_UUID_and_checksum =
        compute_and_set_obj_checksum (obj_with_UUID_and_checksum,
                                      base_outdir_for_checksum)

        #  This assumes that the object has a file_name_prefix and a UUID.

    saved_obj_with_checksum_filename =
        paste0 ("saved.",
                obj_with_UUID_and_checksum@file_name_prefix, ".",
                obj_with_UUID_and_checksum@UUID,
                ".rds")

    full_saved_obj_path = file.path (base_outdir_for_checksum,
                                     saved_obj_with_checksum_filename)

    saveRDS (obj_with_UUID_and_checksum, full_saved_obj_path)

#    reloaded_obj = readRDS (full_saved_obj_path)    #  testing only

    cat ("\n\n>>>>> '", obj_with_UUID_and_checksum@file_name_prefix, "' object saved to: \n    '",
         full_saved_obj_path, "'",
         "\nTo reload problem, use readRDS (full_saved_obj_path)\n\n", sep='')

#docaids::doc_vars_in_this_func_once ()
    return (obj_with_UUID_and_checksum)
    }

#===============================================================================

