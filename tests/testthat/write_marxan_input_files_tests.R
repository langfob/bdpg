#===============================================================================

                #  source ('write_marxan_input_files_tests.R')

#===============================================================================
#                              Testing
#===============================================================================

# choose_random_spp_for_patch = function (num_spp)
#     {
#     num_spp_for_patch = sample (1:num_spp, 1)
#     spp_for_patch = sample (1:num_spp, num_spp_for_patch, replace=FALSE)
#
#     return (spp_for_patch)
#     }

#-------------------------------------------------------------------------------

#' Generate a random species vs. planning unit table.
#'
#' @param num_PUs An integer number of planning units to generate.
#' @param num_spp An integer number of species to generate.
#' @param sppAmount An integer amount of each species to generate on each patch.
#' @export
#' @return A data frame with column headings "species", "pu", and "amount".
#' @examples
#' \dontrun{
#' num_PUs = 10
#' num_spp = 10
#' gen_random_spp_PU_amount_table (num_PUs, num_spp)
#'          }

gen_random_spp_PU_amount_table =
        function (num_PUs, num_spp,
                  sppAmount = 1  #  Use same amount for every species
                 )
    {
    num_spp_in_PU = sample (1:num_spp, num_PUs, replace=TRUE)
    total_num_spp_PU_pairs = sum (num_spp_in_PU)

    spp_PU_amount_table =
        data.frame (species = rep (NA, total_num_spp_PU_pairs),
                    pu      = rep (NA, total_num_spp_PU_pairs),
                    amount  = rep (sppAmount, total_num_spp_PU_pairs))

    cur_table_row = 0
    for (cur_PU in 1:num_PUs)
        {
        spp_for_cur_PU = sample (1:num_spp,
                                 num_spp_in_PU [cur_PU],
                                 replace=FALSE)

        for (cur_spp in spp_for_cur_PU)
            {
            cur_table_row = cur_table_row + 1

            spp_PU_amount_table [cur_table_row, 1] = cur_spp
            spp_PU_amount_table [cur_table_row, 2] = cur_PU
            }
        }

    return (spp_PU_amount_table)
    }

#===============================================================================

test_write_marxan_input_files <- function ()
    {
    seed = 1
    set.seed (seed)

    num_PUs = 100
    num_spp = 3

    PU_IDs = 1:num_PUs
    spp_IDs = 1:num_spp

    spp_PU_amount_table = gen_random_spp_PU_amount_table (num_PUs, num_spp)
    write_all_marxan_input_files (PU_IDs, spp_IDs, spp_PU_amount_table)
    }

#===============================================================================

