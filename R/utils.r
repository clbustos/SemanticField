#' Trim a graph using its arcs weights
#' @param gr graph
#' @param p minimum weight accepted before trim 
#' @import igraph
trimByWeight<-function(gr,p=2) {
  pesos_val<-igraph::get.edge.attribute(gr,"weight")>=p
  sg<-igraph::subgraph.edges(gr,which(pesos_val))
  #edges_a_mantener<-get.edges(gr,1:length(pesos_val))[pesos_val,]
  #edges_a_mantener
  sg
}
