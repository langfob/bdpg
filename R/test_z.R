#===============================================================================

                                #  test_z.R

#===============================================================================

test_z <- function (zmeths, seed = 456, num_spp = 4, num_PUs = 3)
    {
                                                                                if (verbose) {
                                                                                cat ("\n  num_spp = ", num_spp)
                                                                                cat ("\n  num_PUs = ", num_PUs)
                                                                                }
    bpm = gen_dummy_bpm (num_spp, num_PUs, seed)
                                                                                if (verbose) {
                                                                                cat ("\n\n  bpm = \n")
                                                                                print (bpm)
                                                                                }

    wt_spp_vec = rep (1, num_spp)    #c(2,3,4,5)    #rep (2, num_spp)    #  weight of species j
                                                                                if (verbose) {
                                                                                cat ("\n  wt_spp_vec = ", wt_spp_vec)
                                                                                }

    c_PU_vec = rep (1, num_PUs)      #c(10,20,30)    #rep (1, num_PUs)  #  cost of PU i
                                                                                if (verbose) {
                                                                                cat ("\n  c_PU_vec = ", c_PU_vec)
                                                                                }

zui = NULL
if ("inline" %in% zmeths)
    {
    set.seed (seed + 12345)
    cat ("\n------------------------------------------------------------------------\n")

timings_using_inline = system.time ({
    #OLD#    zui = z_using_inline (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
    zui = z (num_spp,
             num_PUs,
             wt_spp_vec,
             c_PU_vec,
             bpm,
             z_meth="inline")
})

    cat ("\n\ntimings_using_inline = \n")
    print (timings_using_inline)

    full_zui = zui$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_zui) = ", length (full_zui))
    print (full_zui)

    short_zui = zui$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_zui) = ", length (short_zui))
    cat ("\nshort_zui = \n")
    print (short_zui)
    }

zuf = NULL
if ("funcs" %in% zmeths)
    {
    set.seed (seed + 12345)
    timings_using_funcs = system.time ({
        #OLD#  zuf = z_using_funcs (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)                                            })
        zuf = z (num_spp,
                 num_PUs,
                 wt_spp_vec,
                 c_PU_vec,
                 bpm,
                 z_meth="funcs")
    })

    cat ("\n\ntimings_using_funcs = \n")
    print (timings_using_funcs)

    full_zuf = zuf$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_zuf) = ", length (full_zuf))
    print (full_zuf)

    short_zuf = zuf$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_zuf) = ", length (short_zuf))
    cat ("\nshort_zuf = \n")
    print (short_zuf)
    }

zufor = NULL
if ("for" %in% zmeths)
    {
    set.seed (seed + 12345)
    timings_using_for = system.time ({
        #OLD#  zufor = z_using_for (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)                                            })
        zufor = z (num_spp,
                 num_PUs,
                 wt_spp_vec,
                 c_PU_vec,
                 bpm,
                 z_meth="for")
    })

    cat ("\n\ntimings_using_for = \n")
    print (timings_using_for)

    full_zufor = zufor$full_ranked_solution_PU_IDs_vec

    cat ("\nlength(full_zufor) = ", length (full_zufor))
    print (full_zufor)

    short_zufor = zufor$short_ranked_solution_PU_IDs_vec
    cat ("\nlength(short_zufor) = ", length (short_zufor))
    cat ("\nshort_zufor = \n")
    print (short_zufor)
    }



    cat ("\n------------------------------------------------------------------------")
        cat ("\nzui == zuf: ", all.equal (zui, zuf))
        cat ("\nzui == zufor: ", all.equal (zui, zufor))
        cat ("\nzuf == zufor: ", all.equal (zuf, zufor), "\n")
    cat ("\n------------------------------------------------------------------------")

    return (list (zui_long  = zui$full_ranked_solution_PU_IDs_vec,
                  zui_short = zui$short_ranked_solution_PU_IDs_vec,

                  zuf_long  = zuf$full_ranked_solution_PU_IDs_vec,
                  zuf_short = zuf$short_ranked_solution_PU_IDs_vec,

                  zufor_long  = zufor$full_ranked_solution_PU_IDs_vec,
                  zufor_short = zufor$short_ranked_solution_PU_IDs_vec))
    }

#===============================================================================


