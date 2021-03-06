% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/gscp_11b_network_measures_using_igraph_package.R
\name{redundancy}
\alias{redundancy}
\title{Latapy et al redundancy measure}
\usage{
redundancy(g, top_bottom_vertex_type = FALSE)
}
\arguments{
\item{g}{graph}

\item{top_bottom_vertex_type}{not sure...}
}
\description{
Latapy et al redundancy measure
}
\details{
The code above is missing the code for the final (and possibly most important)
 metric in the Latapy et al paper, i.e., the redundancy coefficient.  That
 measure is provided in the python NetworkX package:

 http://networkx.lanl.gov/reference/generated/networkx.algorithms.bipartite.redundancy.node_redundancy.html

 Python source code for the function is at:

 http://networkx.lanl.gov/_modules/networkx/algorithms/bipartite/redundancy.html#node_redundancy

 Simone Gabbriellini has tried to code the redundancy function in R and asked
 some questions about it on at least one mailing list.  The exchanges are
 pretty long, so I won't insert them right here.  I'll insert a couple of the
 posts and followups at the bottom of this file so that you don't have to
 go chase up the sources for what I've done here.
}
