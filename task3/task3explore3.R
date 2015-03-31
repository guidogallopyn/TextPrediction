# test script for Capstone task3
#test NGramLM class

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)
library(dplyr)
library(slam)
library(hash)


options(mc.cores=1) # RWeka bug workaround 

Corpus.summary <- function(corpus) 
  ldply(lapply(corpus, function(x) { ellen <- nchar(content(x))
                                     wordlist <- RWeka::WordTokenizer(x)  
                                     data.frame(TextDocument=meta(x,"id"),
                                                NChars=sum(ellen), NElements=length(content(x)),
                                                MedianElement=median(ellen), MaxElement=max(ellen),
                                                NWords=length(wordlist), NVoc=length(unique(wordlist)))} ), 
        data.frame)
#
# load the NGram class and helper functions 
#
source("NGramLM.R")

# read an english corpus
#corpus <- VCorpus(DirSource("data/final/en_US/"))
#small <- VCorpus(DirSource("data/small/en_US/"))
small <- VCorpus(DirSource("data/micro/en_US/"))

# summary statistics
Corpus.summary(small)

# corpus preprocessing and cleaning
small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
#small <- tm_map(small, removeWords, readLines("data/en-US-bad-words.txt"))
small <- tm_map(small, stripWhitespace)

# summary statistics clean corpus
Corpus.summary(small)

#
# usage testing with 1..4 grams on micro corpus
#
mod1 <- NGramLM(small,1)
str(mod1)

mod2 <- NGramLM(small,2)
str(mod2)

mod3 <- NGramLM(small,3)
str(mod3)

mod4 <- NGramLM(small,4)  
str(mod4)


#
# get words out of LM
head(getWords(mod1),12)
head(getWords(mod2),12)


# number of 1grams and 2grams
getNumberNGrams(mod1,1)
getNumberNGrams(mod2,2)
getNumberNGrams(mod3,3)
getNumberNGrams(mod4,4)

# how many 1-grams in training corpus
sum(values(mod1[["1-grams"]]))
sum(values(mod2[["1-grams"]]))


# show highes probability unigrams
head(sort(unlist(as.list(mod1[["Pbo1"]])),decreasing=TRUE),12)

# do all unigram probs sum to 1? yes
sum(values(mod1[["Pbo1"]]))

#howmany 2-grams in training corpus
sum(values(mod2[["2-grams"]]))
sum(values(mod3[["2-grams"]]))

# show highes probability biigrams
head(sort(unlist(as.list(mod2[["Pbo2"]])),decreasing=TRUE),12) # only one bigram for the start word, prob=1 
get2grams(mod2,c("took","dear"))


# which bigrams start with "the" or "a" ?
get2grams(mod2,c("the","a"))

# get 2gram counts bigrams starting with "the"
values(mod2[["2-grams"]], keys=get2grams(mod2,"the")[[1]])
sum(values(mod2[["2-grams"]], keys=get2grams(mod2,"the")[[1]])) # number of bigram occurances starting with "the"
mod2[["1-grams"]][["the"]] # number of unigram occurances" here no left-over probability mass for "the"

# so bigram probs starting with the word "the" should add up to 1" yes
values(mod2[["Pbo2"]], keys=get2grams(mod2,"the")[[1]]) 
sum(values(mod2[["Pbo2"]], keys=get2grams(mod2,"the")[[1]]) )


# unigram model testing
# Probability of word "auto" with no history for the three models
KatzPbo.NGramLM(mod1, history=NULL, "auto") #should give unigram probability of "auto" 
mod1[["Pbo1"]][["auto"]]
mod1[["Pbo1"]][["the"]]
predict(mod1,"this is a history",c("auto","the"))  # history is ignored by unigram model
predict(mod1,"this is a history",c("auto","the","OOV"))  
predict(mod1,"this is a history",c("OOV1","OOV2","OOV3")) # three OOV, no good choice
predict(mod1,"this is a history") # returns word with the max unigram prob of all words in volcab

# null history ngram should be same as unigram model
KatzPbo.NGramLM(mod2, history=NULL, "auto") #should give unigram probability of "auto" 
mod2[["Pbo1"]][["auto"]]
KatzPbo.NGramLM(mod3, history=NULL, "auto") #should give unigram probability of "auto" 
mod3[["Pbo1"]][["auto"]]
KatzPbo.NGramLM(mod4, history=NULL, "auto") #should give unigram probability of "auto" 
mod4[["Pbo1"]][["auto"]]

# Probability of word "auto" with history "the" for the bigram and trigram models
KatzPbo.NGramLM(mod2, "the", "auto") 
mod2[["Pbo2"]][["the auto"]]   # existing bigram

KatzPbo.NGramLM(mod2,"the", "than") 
mod2[["Pbo2"]][["the than"]] # NULL no bigram 
mod2[["Pbo1"]][["the"]]    # unigram exists
mod2[["alpha1"]][["the"]]  # bow=0, because no left-over probability mass for "the"
predict(mod2,"the", "than") 

predict(mod2, "this is the", c("than","auto"))
sapply(c("than","auto"), function(w) KatzPbo(mod2,"the", w) )
names(which.max(sapply(c("than","auto"), function(w) KatzPbo(mod2,"the", w) )))

# test with non zero bow
head(sort(unlist(as.list(mod2[["alpha1"]])),decreasing=TRUE), 100) # lets pick "place"
get2grams(mod2,"place")
mod2[["alpha1"]][["place"]]

predict(mod2, "place", c("for","the"))
mod2[["alpha1"]][["place"]]
mod2[["Pbo1"]][["the"]]
mod2[["alpha1"]][["place"]]*mod2[["Pbo1"]][["the"]]
mod2[["Pbo2"]][["place for"]]

# trigram tests
mod3[["Pbo3"]][["and just about"]]
mod3[["Pbo3"]][["and just lost"]]
get2grams(mod3,"just")
predict(mod3,"and just",c("about","got","the"))
KatzPbo.NGramLM(mod3, c("and", "just"), "about") # works if trigram exists
KatzPbo.NGramLM(mod3, c("and", "just"), "got") 
mod3[["Pbo2"]][["just got"]]
mod3[["alpha2"]][["and just"]]
KatzPbo.NGramLM(mod3, c("and", "just"), "the") 
mod3[["Pbo1"]][["the"]]
mod3[["alpha1"]][["just"]]
mod3[["alpha2"]][["and just"]]

