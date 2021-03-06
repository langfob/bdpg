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
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return character string checksum of the object when object has been written
#'     out as a file

#-------------------------------------------------------------------------------

compute_obj_checksum <- function (obj_with_UUID_and_checksum,
                                  base_outdir_for_checksum=".")
    {
        #------------------------------------------------------------------
        #  Clear the UUID, checksum, and rand_seed fields if they exist,
        #  since we don't want them to cause the checksum to be different
        #  when two objects are identical other than those fields.
        #  Doesn't really matter what values these fields get, as long
        #  as they are the same for every object.
        #------------------------------------------------------------------

    if ("UUID" %in% methods::slotNames (obj_with_UUID_and_checksum))
        obj_with_UUID_and_checksum@UUID <- ""
    if ("checksum" %in% methods::slotNames (obj_with_UUID_and_checksum))
        obj_with_UUID_and_checksum@checksum <- ""
    if ("rand_seed" %in% methods::slotNames (obj_with_UUID_and_checksum))
        obj_with_UUID_and_checksum@rand_seed <- 0

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

        #--------------------------------------------------------------------
        #  2017 12 19 - BTL
        #  Printing checksum right at this point shows some odd string that
        #  contains the name of the temporary output file as well as the
        #  32 character checksum.  It doesn't seem to hurt anything at the
        #  moment, but it seems bizarre.
        #  I have no idea why, but it goes away if you just paste0()
        #  the checksum back into itself like the code below.
        #  I'll just leave it here in case there's a problem with the weird
        #  version of the checksum later on.
        #  (I have more notes about this in my daily log file entry for
        #  2017 12 19 if I need more information about it later.)
        #--------------------------------------------------------------------

    #checksum = paste0 (checksum)

    return (checksum)
    }

#===============================================================================

#' Convenience function for computing the object's checksum and setting that slot
#'
#' This function computes the checksum and sets the checksum slot in the object,
#' then returns the modified object.  It just packages those up for convenience.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return  the same object that was passed in except that its checksum is
#'     now set

#-------------------------------------------------------------------------------

compute_and_set_obj_checksum <- function (obj_with_UUID_and_checksum,
                                          base_outdir_for_checksum=".")
    {
        #-----------------------------------------------------------------------
        #  Compute the checksum but wrap as.character() around the result
        #  before assigning it to the checksum slot in the object because
        #  there is something weird about the object that the function
        #  returns.
        #  I expected the return to be a 32 character string holding the
        #  checksum but it's some very strange object that contains a filename
        #  followed by a newline, then the same number of blanks as the
        #  length of the filename and then the 32 character checksum
        #  surrounded by quotes.  At least that's the print representation
        #  of what comes back.  If you wrap as.character() around that, it
        #  turns into just the 32 characters of checksum.
        #  The problem with the original return value is that two objects
        #  can have the same 32 character checksums but have been computed
        #  on different temporary files and so the checksums fail to be "==".
        #  I haven't been able to find any explanation of this anywhere,
        #  so this is the best I can do to hack around it.
        #  2017 12 26 - BTL
        #-----------------------------------------------------------------------

    checksum <- compute_obj_checksum (obj_with_UUID_and_checksum,
                                      base_outdir_for_checksum)

    obj_with_UUID_and_checksum@checksum <- as.character (checksum)

    return (obj_with_UUID_and_checksum)
    }

#===============================================================================

#' Save object with checksum
#'
#' Save the object to disk with its checksum slot set.
#'
#' Note that there may be a way to compress these files when they're saved,
#' but I don't quite understand from the saveRDS() help file whether you
#' have to do the compression by calling gzfile() or some argument to saveRDS().
#' It may be worth looking into more if these objects get large because of
#' the adjacency matrix, since the examples in the help file for readRDS()
#' look like readRDS() can directly restore an R object that's been saved
#' using saveRDS() and then had gzip() applied to it.
#'
#-------------------------------------------------------------------------------

#' @inheritParams std_param_defns
#'
#' @return Returns input object with its checksum slot filled

#-------------------------------------------------------------------------------

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

    return (obj_with_UUID_and_checksum)
    }

#===============================================================================

