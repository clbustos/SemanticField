#' Generate a base object to analyze corpus as 
#' a graph and its lexical availability
#' @param x a data.frame with the following structure 
#'        \describe{
#'        \item{id}{Identifier for the subject}
#'        \item{order}{Order of the answer}
#'        \item{value}{Word for subject \code{id}, order \code{order}}
#'        }
#' @param lambda Lambda coefficient for LAI
#' @return a corpus object 
#' @import reshape
generateCorpus<-function(x,lambda=0.9) {
  
  n.cases<-length(unique(x$id))
  words<-unique(x$value)
  n.answer<-aggregate(x$id,list(id=x$id),length)
  colnames(n.answer)<-c("id","n")
  max.answer<-max(n.answer$n)
  
  r1<-aggregate(x$value,list(word=x$value,order=x$order),length)
  
  words.position<-cast(r1,word~order,fill=0,value="x")
  
  lambda=0.9
  factor.lai=lambda^(0:(max.answer-1))/n.cases
  
  res.lai<-as.matrix(words.position)%*% matrix(factor.lai,length(factor.lai),1)
  
  lai<-as.numeric(res.lai)
  names(lai)<-rownames(res.lai)
  
  #idl<-data.frame(palabra=unicas,idl=as.numeric(res.idl))
  out<-list(
    corpus=x,
    n.cases=n.cases,
    max.answer=max.answer,
    n.answer=n.answer,
    mean.answer=mean(n.answer$n),
    sum.answer=sum(n.answer$n),
    words=words,
    words.position=words.position,
    words.frequency=rowSums(words.position),
    lai=lai
  )
  class(out)<-"corpus"
  invisible(out)
}
