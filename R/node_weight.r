#' Calculates the sum and mean of edge weight for all nodes
#' @param x graph 
#' @return data.frame with nodes and sums on weights
#' @export
#' @import igraph

nodeWeight<-function(x) {
  node.weights<-get.edge.attribute(x,"weight")
  edges.l<-lapply(get.adjedgelist(x),function(edges) {node.weights[edges]})
  data.frame(word=names(edges.l),weigth.sum=sapply(edges.l,sum), weigth.mean=sapply(edges.l,mean),degree=degree(x))
}
