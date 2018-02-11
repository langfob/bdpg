#-------------------------------------------------------------------------------

test_compute_realized_error_rates <- function ()
    {
    cor_bpm = matrix (c(1,1,1,0,0,0), nrow=2, ncol=3, byrow=TRUE)
    app_bpm = matrix (c(0,1,1,1,1,0), nrow=2, ncol=3, byrow=TRUE)

    compute_realized_error_rates (cor_bpm, app_bpm)

    #  SHOULD OUTPUT:
    #
    # -----  Realized error rates  -----
    #
    # num_TPs =  3
    # num_TNs =  3
    # num_FNs =  1
    # num_FPs =  2
    # FN_rate =  0.3333333
    # FP_rate =  0.6666667
    #
    # -----  End realized error rates  -----
    # $FN_ct
    # [1] 1
    #
    # $FN_rate
    # [1] 0.3333333
    #
    # $FP_ct
    # [1] 2
    #
    # $FP_rate
    # [1] 0.6666667
    #
    }

