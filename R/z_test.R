#===============================================================================

                                #  z_test.R

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
    zui = z_using_inline (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_inline = \n")
    print (timings_using_inline)
    print (zui)
}

if ("funcs" %in% zmeths)
{
set.seed (seed + 12345)
cat ("\n------------------------------------------------------------------------\n")
                                            timings_using_funcs = system.time ({
    zuf = z_using_funcs (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_funcs = \n")
    print (timings_using_funcs)
    print (zuf)
}

if ("for" %in% zmeths)
{
set.seed (seed + 12345)
cat ("\n------------------------------------------------------------------------")
                                            timings_using_for = system.time ({
    zufor = z_using_for (num_spp, num_PUs, wt_spp_vec, c_PU_vec, bpm)
                                            })
    cat ("\n\ntimings_using_for = \n")
    print (timings_using_for)
    print (zufor)
}

cat ("\n------------------------------------------------------------------------")
    cat ("\nzui == zuf: ", all.equal (zui, zuf))
    cat ("\nzui == zufor: ", all.equal (zui, zufor))
    cat ("\nzuf == zufor: ", all.equal (zuf, zufor), "\n")
cat ("\n------------------------------------------------------------------------")

return (list (zui=zui, zuf=zuf, zufor=zufor))
    }

#===============================================================================

