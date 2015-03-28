# exploring good-turning discounting

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)
library(dplyr)
library(slam)
library(hash)

small <- VCorpus(DirSource("data/micro/en_US/"))

small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
#small <- tm_map(small, removeWords, readLines("data/en-US-bad-words.txt"))
small <- tm_map(small, stripWhitespace)

# summary statistics clean corpus
Corpus.summary(small)

cnt <-row_sums(tm::TermDocumentMatrix(small, 
                                control=list( tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)),
                                              wordLengths=c(1, Inf))))

(tab<-table(cnt))

# good-turing discounting
GoodTuringDisc <- function (cnt) {
  tab <- table(cnt)
  (cnt+1) * approx(as.integer(names(tab)),tab,cnt+1)$y / approx(as.integer(names(tab)),tab,cnt)$y
}

#absolute discounting
(cnt-.75)/cnt


head(GoodTuringDisc(cnt))
head(GoodTuringDisc(cnt)/cnt)
head(cnt)
tab

# with smoothing 
GoodTuringDisc <- function (cnt) {
  tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) 
  fit <- lm(log(Freq) ~ log(cnt), data=tab)
  (cnt+1) * exp(predict(fit, data.frame(cnt=cnt+1))) / exp(predict(fit, data.frame(cnt=cnt)))
}


head(cnt,20)
head(GoodTuringDisc(cnt),20)
GoodTuringDisc(cnt)

# debugging smoothing
length(cnt)
(tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) )
fit <- lm(log(Freq) ~ log(cnt), data=tab)
fhat <- exp(predict(fit, tab)) 
cstar <- (cnt+1) * predict(fit, data.frame(cnt=cnt+1)) / predict(fit, data.frame(cnt=cnt))


source("NGramLM.R")

mod <- train(control=list(data="micro",N=3, method="Katz", threshold=0))


