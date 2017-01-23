#===============================================================================

            #  gscp_10c_build_adj_and_cooccurrence_matrices_tests.R

#===============================================================================

test_create_PU_spp_pair_names = function ()
    {
    PU_col_name ="PU_ID"
    spp_col_name = "spp_ID"
    cor_num_PUs = 3
    cor_num_spp = 3

        #  Have every species on every patch in the correct problem definition.
    cor_PU_spp_pair_indices = expand.grid (x=1:cor_num_PUs, y=1:cor_num_spp)
    names (cor_PU_spp_pair_indices) = c(PU_col_name, spp_col_name)


        #  Remove spp 3 and PU 2 to make the apparent problem.
    app_PU_spp_pair_indices = cor_PU_spp_pair_indices
    spp_3_occurrences = which (cor_PU_spp_pair_indices [,spp_col_name] == 3)
    app_PU_spp_pair_indices = app_PU_spp_pair_indices [-spp_3_occurrences,]

    PU_2_occurrences = which (app_PU_spp_pair_indices [,PU_col_name] == 2)
    app_PU_spp_pair_indices = app_PU_spp_pair_indices [-PU_2_occurrences,]

    app_PU_spp_pair_names =
        create_PU_spp_pair_names (
                                  #cor_num_PUs, cor_num_spp,
                                  app_PU_spp_pair_indices,
                                  PU_col_name, spp_col_name)

        #  Here is what the result should look like:
    true_PU_spp_pair_names = data.frame (PU_ID=c("p1","p3","p1","p3"),
                                         spp_ID=c("s1","s1","s2","s2"),
                                         stringsAsFactors=FALSE)
    true_PU_vertex_names = c("p1","p3")
    true_spp_vertex_names = c("s1","s2")
    true_app_PU_spp_pair_names =
        list (PU_spp_pair_names = true_PU_spp_pair_names,
              PU_vertex_names = true_PU_vertex_names,
              spp_vertex_names = true_spp_vertex_names)

        #  Finally, compare result to what it should look like.
    expect_equal (app_PU_spp_pair_names, true_app_PU_spp_pair_names)
    }

#test_create_PU_spp_pair_names ()

#===============================================================================


