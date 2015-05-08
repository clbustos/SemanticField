#' Lexical availability index
#' @param corpus object
#' @return data.frame with words and its lexical availability
#' @export

LAI<-function(x,type="lexmath",lambda=0.9) {
  name.function<-paste0("LAI.",type)
  get(name.function)(x,lambda)
}

#' Lexical availability index, LEXMATH version
#' @param corpus object
#' @return data.frame with words and its lexical availability
#' @export
LAI.lexmath<-function(x,lambda=0.9) {
  lambda=0.9
  factor.lai=lambda^(0:(x$max.answer-1))/x$n.subjects
  
  #print(length(factor.lai))
  #print(dim(x$words.position))
  
  res.lai<-as.matrix(x$words.position)%*% matrix(factor.lai,length(factor.lai),1)
  lai<-as.numeric(res.lai)
  names(lai)<-rownames(res.lai)
  invisible(lai[order(names(lai))])
}
