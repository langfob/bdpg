#===============================================================================

#  Tests extracted from gen_single_bdprob.R on 2017 02 08.

#===============================================================================
test_create_base_dir_structure <- function ()
    {
    base_outdir = "."
    derived_bdpg_dir_names = create_base_dir_structure (base_outdir,
                                                        create_dirs=FALSE)

    return (derived_bdpg_dir_names)
    }

#===============================================================================

test_create_res_sel_replicate_dirs <- function ()
    {
    create_dirs=FALSE
    base_outdir = file.path (normalizePath ("."), "res_sel")  # For some reason, normalizePath("./res_sel", mustWork=FALSE) fails in that it just returns "./res_sel", even though it works correctly if res_sel does exist.
    bdpg_dir_names=list()

    for (iii in 1:3)
        {
        bdpg_dir_names = create_new_res_sel_replicate_subtree (bdpg_dir_names, "simpleRichness", base_outdir, create_dirs)
        }

    for (iii in 1:2)
        {
        bdpg_dir_names = create_new_res_sel_replicate_subtree (bdpg_dir_names, "marxan", base_outdir, create_dirs)
        }

    return (bdpg_dir_names)
    }

#===============================================================================
