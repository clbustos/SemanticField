#' Trim by weight
#' @param gr Corpus
#' @param p level of trim 
#' @return subgraph
#' @export
#' @import igraph
trimByWeight<-function(gr,p=2) {
  if(p<=1) {
    return(gr)
  }
  pesos_val<-igraph::get.edge.attribute(gr,"weight")>=p
  sg<-igraph::subgraph.edges(gr,which(pesos_val))
  sg
}
