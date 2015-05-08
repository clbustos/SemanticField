load("../extdata/cc.Rdata")
corpus.cc<-generateCorpus(cc)

expect_that(corpus.cc$n.subjects,equals(4))
expect_that(corpus.cc$max.answer,equals(3))
expect_that(corpus.cc$answers.per.subject,equals(data.frame(id=c(1,2,3,4),n=c(3,3,3,2))))

