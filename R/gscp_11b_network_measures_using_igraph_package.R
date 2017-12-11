#===============================================================================

            #  gscp_11b_network_measures_using_igraph_package.R

#===============================================================================

                    #  Code for using igraph package.

#===============================================================================

#' ccLowDot
#'
#' ccLowDot
#'
#' The last clustering measures relies on the functions ccBip
#' written by Gabor Csardi, while ccLowDot and ccTopDot are
#' essentially the same function with only a minor change
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{bg}{
#' \preformatted{
#' bg : IGRAPH UN-B 936 1628 --
#' }}
#'
#' @param bg not sure
#'
#' @return not sure
#'
#' @import igraph
ccBip <- function (bg)
    {
    if (! "name" %in% list.vertex.attributes (bg))
        {
        V(bg)$name <- seq_len (vcount(bg))
        }
    neib <- get.adjlist (bg)
    names (neib) <- V(bg)$name
    proj <- bipartite.projection(bg)

    ccBip_retval <-    lapply (proj,
            function(x)
                {
                el <- get.edgelist(x)
                sapply (V(x)$name,
                        function (v)
                            {
                            subs <- el[,1]==v | el[,2]==v

                            f <- function (un, vn) length (union (un, vn))

                            vals <- E(x)[subs]$weight /
                                    unlist (mapply (f,
                                                    neib[el[subs,1]],
                                                    neib[el[subs,2]]))
                            mean (vals)
                            }
                        )
                }
            )
#docaids::doc_vars_in_this_func_once ()
    return (ccBip_retval)
    }


#' ccLowDot
#'
#' ccLowDot
#'
#' The last clustering measures relies on the functions ccBip
#' written by Gabor Csardi, while ccLowDot and ccTopDot are
#' essentially the same function with only a minor change
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{bg}{
#' \preformatted{
#' bg : IGRAPH UN-B 936 1628 --
#' }}
#'
#' @param bg the graph
#'
#' @import igraph

ccLowDot <- function (bg)
    {
    if (! "name" %in% list.vertex.attributes(bg))
        {
        V(bg)$name <- seq_len(vcount(bg))
        }

    neib <- get.adjlist (bg)
    names (neib) <- V(bg)$name
    proj <- bipartite.projection (bg)
ccLowDot_retval <-    lapply (proj,
            function(x)
                {
                el <- get.edgelist (x)
                sapply (V(x)$name,
                        function(v)
                            {
                            subs <- el[,1]==v | el[,2]==v
                            f <- function (un, vn) min (length(un),
                                                        length(vn))
                            vals <- E(x)[subs]$weight /
                                    unlist (mapply (f, neib[el[subs,1]],
                                                    neib[el[subs,2]]))
                            mean(vals)
                            }
                        )
                }
            )
#docaids::doc_vars_in_this_func_once ()
    return (ccLowDot_retval)
    }

#' ccTopDot
#'
#' ccTopDot
#'
#' The last clustering measures relies on the functions ccBip
#' written by Gabor Csardi, while ccLowDot and ccTopDot are
#' essentially the same function with only a minor change
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{bg}{
#' \preformatted{
#' bg : IGRAPH UN-B 936 1628 --
#' }}
#'
#' @param bg the graph
#'
#' @import igraph

ccTopDot <- function (bg)
    {
    if (! "name" %in% list.vertex.attributes(bg))
        {
        V(bg)$name <- seq_len(vcount(bg))
        }

    neib <- get.adjlist (bg)
    names (neib) <- V(bg)$name
    proj <- bipartite.projection (bg)
    ccTopDot_retval <- lapply(proj,
           function(x)
               {
                el <- get.edgelist(x)
                sapply (V(x)$name,
                        function(v)
                            {
                            subs <- el[,1]==v | el[,2]==v
                            f <- function(un, vn) max(length(un), length(vn))
                            vals <- E(x)[subs]$weight /
                                    unlist (mapply (f, neib[el[subs,1]],
                                                    neib[el[subs,2]]))
                            mean (vals)
                            }
                        )
                }
           )
#docaids::doc_vars_in_this_func_once ()
    return (ccTopDot_retval)
    }

#===============================================================================

#' Latapy et al redundancy measure
#'
#' Latapy et al redundancy measure
#'
#'  The code above is missing the code for the final (and possibly most important)
#'  metric in the Latapy et al paper, i.e., the redundancy coefficient.  That
#'  measure is provided in the python NetworkX package:
#'
#'  http://networkx.lanl.gov/reference/generated/networkx.algorithms.bipartite.redundancy.node_redundancy.html
#'
#'  Python source code for the function is at:
#'
#'  http://networkx.lanl.gov/_modules/networkx/algorithms/bipartite/redundancy.html#node_redundancy
#'
#'  Simone Gabbriellini has tried to code the redundancy function in R and asked
#'  some questions about it on at least one mailing list.  The exchanges are
#'  pretty long, so I won't insert them right here.  I'll insert a couple of the
#'  posts and followups at the bottom of this file so that you don't have to
#'  go chase up the sources for what I've done here.
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{c}{
#' \preformatted{
#' c :  int 1
#' }}
#' \subsection{comb}{
#' \preformatted{
#' comb :  int [1:2, 1] 868 926
#' }}
#' \subsection{g}{
#' \preformatted{
#' g : IGRAPH UN-B 936 1628 --
#' }}
#'
#' @param g graph
#' @param top_bottom_vertex_type not sure...
#'
#' @import igraph

redundancy <- function (g
                            #  Added by BTL since this only seemed to do bottom.
                        , top_bottom_vertex_type=FALSE
                        )
    {
    redundancy <- c()

        #  type test added by BTL since this only seemed to do bottom...
#   for (i in V(g)[which(V(g)$type==FALSE)])
    for (i in V(g)[which(V(g)$type==top_bottom_vertex_type)])
        {

        overlap <- 0
        nei <- neighbors(g,i)

            #  Correction suggested by Tamas Nepusz
        #if(length(nei)>0){
        if(length (nei)>1)
            {
            comb <- combn(nei, 2)
            for(c in seq(1:dim(comb)[2]))
                {
                unei <- neighbors(g,comb[1,c])
                wnei <- neighbors(g,comb[2,c])

                    #  Correction suggested by Tamas Nepusz
                #redund <- Reduce(union, list(unei,wnei))
                #redund <- Reduce(setdiff, list(redund,i))
                redund <- setdiff(intersect(unei, wnei), i)

                if(length(redund)>0)
                    {
                        overlap <- overlap + 1
                    }
                }
            }
        if (overlap > 0)
            {
            n <- length (nei)
            norm <- 2.0 / (n * (n - 1))
            } else
            {
            norm <- 1
            }
        redundancy <- append(redundancy, overlap*norm)
        }

#docaids::doc_vars_in_this_func_once ()
    return(redundancy)
    }

#===============================================================================

#' Compute igraph-related network measures
#'
#' Compute igraph-related network measures
#'
#'@section Local Variable Structures and examples:
#'Here is the output of str() for each variable visible in the function.
#'Note that the particular counts and values given are just examples to show
#'what the data might look like.
#'
#' \subsection{bg}{
#' \preformatted{
#' bg : IGRAPH UN-B 936 1628 --
#' }}
#'
#' @param rsprob a reserve selection problem
#' @param top_dir character string
#'
#' @import igraph

compute_igraph_related_network_measures <-
                    #  2016 03 29 - BTL.
                    #  Trying to make this routine more generic so that it
                    #  can be run on either cor_ or app_.
                    #  Not certain about whether these changes will work yet,
                    #  so leaving reminders of what went before.
#     function (spp_vertex_names,
#               PU_vertex_names,
#               num_spp,
#               num_PUs,
#               app_PU_spp_pair_indices,
#               PU_spp_pair_names,
#               emulating_tzar,
#               DEBUG_LEVEL,
#               network_output_dir
#               )
    function (rsprob,
              top_dir
             )
#               ,
#
#                     #  2016 03 29 - BTL.
#                     #  Removed from arg list since now computed below,
#                     #  by call to create_PU_spp_pair_names().
# #              spp_vertex_names,
# #              PU_vertex_names,
# ###              num_spp,
# ###              num_PUs,
#                     #  2016 03 29 - BTL.
#                     #  Probably needs to be generic, not app_, so that you
#                     #  can run this routine on both cor_ and app_ values.
#                     #  At the moment, I'm not sure about that.
# #              app_PU_spp_pair_indices,
#               PU_spp_pair_indices,
#
# #####              PU_spp_pair_names,
#               network_output_dir,
#                     #  Added 2016 03 29 - BTL.
#                     #  May not need the num_spp, num_PUs args up above if
#                     #  everything in here always requires the cor_ counts.
#                     #  At the moment, I'm not sure about that.
# #####              cor_num_PUs,   #  app_ ???
# #####              cor_num_spp,
#               PU_col_name,
#               spp_col_name
#               )
    {
        #-------------------------------------------------------------------
        #  These used to be arguments to the function, but now that rsprob
        #  is being passed in, they can all be derived here.
        #-------------------------------------------------------------------

    if (rsprob@cor_or_app_str == "APP")
        {
        PU_spp_pair_indices = rsprob@APP_prob_info@app_PU_spp_pair_indices
        } else
        {
        PU_spp_pair_indices = rsprob@cor_PU_spp_pair_indices
        }

    network_output_dir  = get_RSprob_path_networks (rsprob, top_dir)
    PU_col_name         = rsprob@PU_col_name
    spp_col_name        = rsprob@spp_col_name

        #---------------------------------------------------------------------
        #  I had been drawing plots of the networks, but they're so big and
        #  convoluted and the nodes are drawn all over the top of each other
        #  that it's not worth doing anymore.
        #  I'll leave the code for it in here though, in case it becomes
        #  useful at some later time.  I'll set a flag here to control that,
        #  but it could be put in the project.yaml file if that becomes
        #  useful.
        #---------------------------------------------------------------------

    draw_network_plots = FALSE

        #--------------------------------------------------------------------
        #  Generate the PU_spp_pair_names with names instead of indices.
        #
        #  I'm doing this because names instead of indices are better
        #  for igraph, if I remember correctly.
        #  See comment at start of code defining create_PU_spp_pair_names()
        #  for more details.
        #
        #  2016 03 29 - BTL
        #  NOTE:  Moving this call to create_PU_spp_pair_names() in here
        #           from the mainline because this seems to be the only
        #           place the returned values are used.
        #--------------------------------------------------------------------

    PU_spp_pair_names_triple = create_PU_spp_pair_names (#####num_PUs,  #####cor_num_PUs,   #  app_ ???
                                                         #####num_spp,  #####cor_num_spp,  #  app_ ???
                                                         PU_spp_pair_indices, #  app_ ???
                                                         PU_col_name,
                                                         spp_col_name
                                                         )

    PU_spp_pair_names = PU_spp_pair_names_triple$PU_spp_pair_names
    PU_vertex_names = PU_spp_pair_names_triple$PU_vertex_names
    spp_vertex_names = PU_spp_pair_names_triple$spp_vertex_names

        #--------------------------------------------------------------------
#browser()
    vertices = data.frame (name=c(spp_vertex_names, PU_vertex_names),
#                          type=c(rep(FALSE, num_spp),
#                                 rep(TRUE, num_PUs)))
                         type=c(rep(FALSE, length(spp_vertex_names)),
                                rep(TRUE, length(PU_vertex_names))))

      #  Does igraph require me to rename the columns to say
      #  "from" and "to instead of "PU_ID" and "spp_ID"?
    # bg = graph.data.frame (edge_df, directed=FALSE, vertices=vertices)

    cat ("\n\nJust before graph.data.frame()\n")

    cat ("\nnames (PU_spp_pair_indices = \n")
    print (names (PU_spp_pair_indices))

    cat ("\nnames (PU_spp_pair_names = \n")
    print (names (PU_spp_pair_names))

    #names (PU_spp_pair_indices) = c("from", "to")
    #cat ("\n\nAfter changing names\n")
    #cat ("\nnames (PU_spp_pair_indices = \n")
    #print (names (PU_spp_pair_indices))

    #bg = graph.data.frame (PU_spp_pair_indices, directed=FALSE, vertices=vertices)
    bg = graph.data.frame (PU_spp_pair_names, directed=FALSE, vertices=vertices)

    #names (PU_spp_pair_indices) = c(PU_col_name, spp_col_name)
    #cat ("\n\nAfter reinstating old names\n")
    #cat ("\nnames (PU_spp_pair_indices = \n")
    #print (names (PU_spp_pair_indices))
    #}

    #===========================================================================

    cat ("\n\n=====>  Under igraph, is.bipartite (bg) = ", is.bipartite (bg), "\n")
    #Echo results...

    if (getOption ("bdpg.emulating_tzar", default=FALSE) &
        (getOption ("bdpg.DEBUG_LEVEL", default=0) > 0))
        {
        print(bg)

        print (V(bg))
        print (V(bg)$type)
        print (E(bg))
        }

        #-----------------------------------------------------------------------
        #  Write out the bipartite graph as an edgelist, then plot it to a file.
        #-----------------------------------------------------------------------

    write.graph (bg,
               file.path (network_output_dir, "bg-bipartite_graph_edgelist.txt"),
               format="edgelist")

    if (draw_network_plots)
        {
        pdf (file.path (network_output_dir, "bg-bipartite_graph.pdf"))
        plot (bg)
        dev.off()
        }

        #---------------------------------------------------------------
        #  Create the 2 projections of the bipartite graph, i.e.,
        #  a graph of just species and a graph of just planning units.
        #---------------------------------------------------------------

    bgp = bipartite.projection (bg)

        #-----------------------------------------------------------------------
        #  Write out the PU projection, i.e., projection 1, then plot it to a file.
        #-----------------------------------------------------------------------

    bgp_proj1_PUs = bgp$proj1
    write.graph (bgp_proj1_PUs,
               file.path (network_output_dir, "bgp_proj1_PUs_edgelist.txt"),
               format="edgelist")

    if (draw_network_plots)
        {
        pdf (file.path (network_output_dir, "bgp_proj1_PUs.pdf"))
        plot (bgp_proj1_PUs)
        dev.off()
        }

        #-----------------------------------------------------------------------
        #  Write out the spp projection, i.e., projection 2, then plot it to a file.
        #-----------------------------------------------------------------------

    bgp_proj2_spp = bgp$proj2
    write.graph (bgp_proj2_spp,
               file.path (network_output_dir, "bgp_proj2_spp_edgelist.txt"),
               format="edgelist")

    if (draw_network_plots)
        {
        pdf (file.path (network_output_dir, "bgp_proj2_spp.pdf"))
        plot (bgp_proj2_spp)
        dev.off()
        }

    #===========================================================================

    ###  Compute the bipartite measures from the Latapy et al 2008 paper.
    #  **Reference: Matthieu Latapy, Clemence Magnien, Nathalie Del Vecchio,
    #    Basic notions for the analysis of large two-mode networks,
    #    Social Networks 30 (2008) 31–48**

    #  Everything from here down (and some function definitions that I have moved
    #  up to the start of this file) is copied directly (other than reformatting and
    #  some minor corrections) from point 6 of the R Recipes page of the igraph
    #  wiki, except for the redundancy code.  The sources for that are listed
    #  down below, where the redundancy() function is defined.  Here is the url
    #  for the igraph wiki page for everything but the redundancy() code:
    #
    #  http://igraph.wikidot.com/r-recipes
    #
    #  NOTE:  The code doesn't work as it is given on the web page.  For example, it
    #  starts off by referencing a graph g that has not been defined.  Later, it
    #  starts in on network transitivity and references "target", which has not been
    #  defined either.  Finally, it references several functions before they're
    #  defined, e.g., ccBip().  In each case, I've done a little hack to get it to
    #  work here.

    #===========================================================================

      # Number of top and bottom nodes
    top <- length(V(bg)[type==FALSE])
    bottom <- length(V(bg)[type==TRUE])

      # Number of edges
    m <- ecount (bg)

      # Mean degree for top and bottom nodes
    ktop <- m/top
    kbottom <- m/bottom

      # Density for bipartite network
    bidens <- m/(top*bottom)

      # Largest connected component for top and bottom nodes:
    gclust <- clusters (bg, mode='weak')
    lcc <- induced.subgraph (bg, V(bg)[gclust$membership==1])
    lcctop <- length (V(lcc)[type==FALSE])
    lccbottom <- length (V(lcc)[type==TRUE])

      # Mean distance for top and bottom nodes
    distop <- mean (shortest.paths (lcc,
                                  v=V(lcc)[type==FALSE],
                                  to=V(lcc)[type==FALSE],
                                  mode = 'all'))
    disbottom <- mean (shortest.paths (lcc,
                                     v=V(lcc)[type==TRUE],
                                     to=V(lcc)[type==TRUE],
                                     mode = 'all'))

    #===========================================================================

        #  BTL additions to get it to work...
    target = bg

        # Network transitivity
    trtarget <- graph.motifs (target, 4)
    ccglobal <- (2 * trtarget[20]) / trtarget[14]  #  Not sure why they calculated this...

        #  BTL - Had to move this code down below the definitions of the functions
        #        used here (ccBip(), etc.).

        # Clustering coefficients (cc, cclowdot, cctopdot) for top and bottom nodes
    ccPointTarg <- ccBip(bg)
    cctop <- mean(ccPointTarg$proj1, na.rm=TRUE)
    ccbottom <- mean(ccPointTarg$proj2, na.rm=TRUE)
    ccLowTarg <- ccLowDot(bg)
    cclowdottop <- mean(ccLowTarg$proj1, na.rm=TRUE)
    cclowdotbottom <- mean(ccLowTarg$proj2, na.rm=TRUE)
    ccTopTarg <- ccTopDot(bg)
    cctopdottop <- mean(ccTopTarg$proj1, na.rm=TRUE)
    cctopdotbottom <- mean(ccTopTarg$proj2, na.rm=TRUE)

    #===============================================================================

    bottom_bg_redundancy = redundancy (bg, FALSE)
    cat ("\n\nbottom bg_redundancy = \n")
    print (bottom_bg_redundancy)

    mean_bottom_bg_redundancy = mean (bottom_bg_redundancy)
    cat ("\nmean_bottom_bg_redundancy = ", mean_bottom_bg_redundancy)
    median_bottom_bg_redundancy = median (bottom_bg_redundancy)
    cat ("\nmedian_bottom_bg_redundancy = ", median_bottom_bg_redundancy)

    top_bg_redundancy = redundancy (bg, TRUE)
    cat ("\n\ntop bg_redundancy = \n")
    print (top_bg_redundancy)

    mean_top_bg_redundancy = mean (top_bg_redundancy)
    cat ("\nmean_top_bg_redundancy = ", mean_top_bg_redundancy)
    median_top_bg_redundancy = median (top_bg_redundancy)
    cat ("\nmedian_top_bg_redundancy = ", median_top_bg_redundancy)

    cat ("\n")

    #===============================================================================

    bipartite_metrics_from_igraph_package_df =
        data.frame (ig_rsp_UUID = rsprob@UUID,
                  ig_top = top,
                  ig_bottom = bottom,
                  ig_num_edges_m = m,
                  ig_ktop = ktop,
                  ig_kbottom = kbottom,
                  ig_bidens = bidens,
        #                ig_gclust = gclust,    #  a list...
        #                ig_lcc = lcc,      #  an igraph...
                  ig_lcctop = lcctop,
                  ig_lccbottom = lccbottom,
                  ig_distop = distop,
                  ig_disbottom = disbottom,

        #                ig_trtarget = trtarget,        #  vector of length 11 in test...
        #                ig_ccglobal = ccglobal,        #  weird computation - yields NA
        #                ig_ccPointTarg = ccPointTarg,  #  a list...
                  ig_cctop = cctop,
                  ig_ccbottom = ccbottom,
        #                ig_ccLowTarg = ccLowTarg,      #  a list...
                  ig_cclowdottop = cclowdottop,
                  ig_cclowdotbottom = cclowdotbottom,
        #                ig_ccTopTarg = ccTopTarg,      #  a list...
                  ig_cctopdottop = cctopdottop,
                  ig_cctopdotbottom = cctopdotbottom,

                  ig_mean_bottom_bg_redundancy = mean_bottom_bg_redundancy,
                  ig_median_bottom_bg_redundancy = median_bottom_bg_redundancy,
                  ig_mean_top_bg_redundancy = mean_top_bg_redundancy,
                  ig_median_top_bg_redundancy = median_top_bg_redundancy
                  )

        #-----------------------------------------------------------------
        #  Add UUID of the problem as the first column and then save the
        #  graph results data frame to disk.
        #-----------------------------------------------------------------

    # uuid_col = data.frame (prob_UUID=rsprob@UUID)
    # bipartite_metrics_from_igraph_package_df =
    #     cbind (uuid_col, bipartite_metrics_from_igraph_package_df)
    #
    # cat ("\n\nfinal bipartite_metrics_from_igraph_package_df including prob_UUID column = \n")
    # print (bipartite_metrics_from_igraph_package_df)
    # cat ("\n\n")

    igraph_metrics_csv_file_name =
        file.path (network_output_dir,
#                   "bipartite_metrics_from_igraph_package_df")
#                   "bipartite_metrics_from_igraph_package_df.csv")
                   paste0 (rsprob@igraph_metrics_file_name_stem, ".csv"))

    write.csv (bipartite_metrics_from_igraph_package_df,
               file = igraph_metrics_csv_file_name,
               # col.names=TRUE,
               row.names=FALSE
               )

#docaids::doc_vars_in_this_func_once ()
    return (bipartite_metrics_from_igraph_package_df)
    }

#===============================================================================

#  Here are posts and followups mentioned above that are related to the Latapy
#  et al redundancy measure coding up by Simone Gabbriellini with suggestions
#  by Tamas Napusz.

#===============================================================================

# http://lists.nongnu.org/archive/html/igraph-help/2013-02/msg00073.html
#
# ```
# [igraph] redundancy in bipartite networks
# From:     Simone Gabbriellini
# Subject:     [igraph] redundancy in bipartite networks
# Date:     Sat, 16 Feb 2013 11:41:41 +0100
#
# Dear List,
#
# I have coded this function to calculate redundancy in bipartite
# networks as specified in Latapy, Magnien, Del Vecchio "Basic notions
# for the analysis of large two-mode networks", Social Networks 30
# (2008) 31–48:
# redundancy<-function(g){
#         redundancy<-c()
#         for(i in V(g)[which(V(g)$type==FALSE)]){
#                 overlap <- 0
#                 nei<-neighbors(g,i)
#                 if(length(nei)>0){
#                         comb<-combn(nei, 2)
#                         for(c in seq(1:dim(comb)[2])){
#                                 unei<-neighbors(g,comb[1,c])
#                                 wnei<-neighbors(g,comb[2,c])
#                                 redund<-Reduce(union, list(unei,wnei))
#                                 redund<-Reduce(setdiff, list(redund,i))
#                                 if(length(redund)>0){
#                                         overlap <- overlap + 1
#                                 }
#                         }
#                 }
#                 if(overlap > 0){
#                         n <- length(nei)
#                         norm<-2.0/(n*(n-1))
#                 } else {
#                         norm <- 1
#                 }
#                 redundancy<-append(redundancy, overlap*norm)
#         }
#         return(redundancy)
# }
#
# I quote here their words:
# "In other words, the redundancy coefficient of v is the fraction of
# pairs of neighbours of v linked to another node than v. In the
# projection, these nodes would be linked together even if v were not
# there, see Fig. 9; this is why we call this the redundancy. If it is
# equal to 1 then the projection would be exactly the same without v; if
# it is 0 it means that none of its neighbours would be linked together
# in the projection."
#
# and:
# "the notion of redundancy we propose here is equivalent to the
# generalisation of the notion of clustering coefficient to squares,
# denoted by C4( ), proposed independently in Lind et al. (2005): it is
# the probability, when a node has two neighbours, that these two nodes
# have (another) neighbour in common."
#
# and here's the reference for Lind (2005):
# Lind, P.G., Gonzlez, M.C., Herrmann, H.J., 2005. Cycles and clustering
# in bipartite networks. ArXiV preprint cond-mat/0504241.
#
# The problem I have is that this function reports always 1, no matter
# the network I use... is there something wrong in the way I call igraph
# nodes?
#
# Any advice or help are more than welcome...
#
# Best regards,
# Simone

#===============================================================================

# A reply to that post from Tamas Nepusz:
#
# http://lists.nongnu.org/archive/html/igraph-help/2013-02/msg00085.html
#
# ```
# Re: [igraph] redundancy in bipartite networks
# From:     Tamas Nepusz
# Subject:     Re: [igraph] redundancy in bipartite networks
# Date: 	Wed, 20 Feb 2013 16:45:43 +0100
# User-agent: 	Mozilla/5.0 (X11; Linux x86_64; rv:17.0) Gecko/20130106 Thunderbird/17.0.2
#
# Hi Simone,
#
# >                               redund<-Reduce(union, list(unei,wnei))
# >                               redund<-Reduce(setdiff, list(redund,i))
# First, you need "intersect" here, not "union". Second, just out of
# curiosity, why don't you simply do this:
#
# redund <- setdiff(intersect(unei, wnei), i)
#
# Also, a possible bug: if(length(nei)>0) should be replaced by
# if(length(nei)>1), since there are no "neighbor pairs" if the node has only
# one neighbor.
#
# --
# T.

#===============================================================================

