# exploratory analysis script for Capstone task 1

# exploring profanity filters on small subset for English


setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)

# read an english corpus
#eng <- VCorpus(DirSource("data/final/en_US/"))
eng <- VCorpus(DirSource("data/small/en_US/"))
#eng <- VCorpus(DirSource("data/micro/en_US/"))

Corpus.summary <- function(corpus) as.data.frame(t(sapply(corpus, 
                                                          function(x) { ellen <- nchar(content(x))
                                                                        wordlist <- words(x)  # from NLP package
                                                                        c(TextDocument=meta(x,"id"),
                                                                          NChars=sum(ellen), NElements=length(content(x)),
                                                                          MedianElement=median(ellen), MaxElement=max(ellen),
                                                                          NWords2=length(wordlist), NVoc=length(unique(wordlist)) 
                                                                        )} )))
# summary statistics
Corpus.summary(eng)

# 1384 bad words from CMU
fileUrl <- "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
download.file(fileUrl,destfile="./data/en-US-bad-words.txt",method="curl")
bad <- readLines("./data/en-US-bad-words.txt")

removeWords("you cocksucking bitch", bad)

eng <- VCorpus(DirSource("data/small/en_US/"))
Corpus.summary(eng)
eng <- tm_map(eng,removeWords, bad)
Corpus.summary(eng)

# Save the data
