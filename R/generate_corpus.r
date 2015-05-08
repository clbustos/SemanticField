#' Generate a base object to analyze corpus as 
#' a graph and its lexical availability
#' @param x a data.frame with the following structure 
#'        \describe{
#'        \item{id}{Identifier for the subject}
#'        \item{order}{Order of the answer}
#'        \item{word}{Word for subject \code{id}, order \code{order}}
#'        }
#' @param lambda Lambda coefficient for LAI
#' @return a corpus object 
#'        \itemize{
#'        \item \code{corpus} Original data.frame
#'        \item \code{n.subjects} Number of persons
#'        \item \code{max.answer} Maximum number of answer per person
#'        \item \code{answers.per.subject} Number of answer per subject 
#'        \item \code{mean.answers} Mean number of answers per subject 
#'        \item \code{sum.answers} Total number of answers for all subjects
#'        \item \code{words} List of words
#'        }

#' @import reshape
generateCorpus<-function(x) {
#  print(names(x) %in% c("id","order","word"))
  if(sum(names(x) %in% c("id","order","word"))!=3) {
    stop("You should use correct names of data.frame")
  }
  n.cases<-length(unique(x$id))
  words<-unique(x$word)
  n.answer<-aggregate(x$id,list(id=x$id),length)
  colnames(n.answer)<-c("id","n")
  min.answer<-min(n.answer$n)
  max.answer<-max(n.answer$n)
  
  r1<-aggregate(x$word,list(word=x$word,order=x$order),length)
  #print(r1)
  words.position<-cast(r1,word~order,fill=0,value="x")
  
  
  #idl<-data.frame(palabra=unicas,idl=as.numeric(res.idl))
  out<-list(
    corpus=x,
    n.subjects=n.cases,
    min.answer=min.answer,
    max.answer=max.answer,
    answers.per.subject=n.answer,
    mean.answers=mean(n.answer$n),
    sum.answers=sum(n.answer$n),
    words=words,
    words.position=words.position,
    words.frequency=data.frame(words=words.position[,1],n=rowSums(words.position[,-1]))
  )
  class(out)<-"corpus"
  invisible(out)
}
