# exploratory analysis script for Capstone task 0

# a better way of loading in the 3 corpora with tm?
# 1 VCorpus for each file
# TextRepository for all three files
# not better: much (x20) bigger datastructure

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)

eng <- VCorpus(DirSource("data/small/en_US/"))

Corpus.summary <- function(corpus) as.data.frame(t(sapply(corpus, function(x) c(TextDocument=meta(x,"id"), 
                                                                                NLines=length(content(x)), 
                                                                                NWords=length(words(x)), # from NLP package
                                                                                NChars=sum(nchar(content(x)))) ) ))
Corpus.summary(eng)

# other way as list of corpora, individual blogs, tweets, newselements are textdocuments (20 times larger...)
corpl <- lapply(list.files("data/small/en_US",full.names = TRUE),
                function(f) {
                  vc <- VCorpus(VectorSource(readLines(f)))
                  meta(vc,tag="id",type="corpus") <- f
                  vc    
                })

str(corpl[[1]])

Corpus.summary <- function(corp) c(Name=basename(meta(corp,tag="id",type="corpus")), 
                 NElements=length(corp), 
                 NWords=sum(unlist(lapply(corp, function(x) length(words(x))))),# number of words
                 NChars=sum(unlist(lapply(corp, function(x) nchar(content(x))))) # number of characters
)

as.data.frame(t(sapply(corpl, Corpus.summary))) 
