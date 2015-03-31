# exploring good-turning discounting

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)
library(dplyr)
library(slam)
library(hash)

options(mc.cores=1) # RWeka bug workaround 


small <- VCorpus(DirSource("data/small/en_US/"))

small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
#small <- tm_map(small, removeWords, readLines("data/en-US-bad-words.txt"))
small <- tm_map(small, stripWhitespace)

cnt <-row_sums(tm::TermDocumentMatrix(small, 
                                control=list( tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)),
                                              wordLengths=c(1, Inf))))

(tab<-table(cnt))

# good-turing discounting with smoothing 
GoodTuringDisc <- function (cnt) {
  tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) 
  fit <- lm(log(Freq) ~ log(cnt), weights=Freq, data=tab)
  (1 + 1/cnt) * exp(predict(fit, data.frame(cnt=cnt+1))) / exp(predict(fit, data.frame(cnt=cnt)))
}

head(GoodTuringDisc(cnt))
head(GoodTuringDisc(cnt)*cnt)
head(cnt)
summary(cnt-GoodTuringDisc(cnt)*cnt)

# debugging smoothing ==> use of weights
length(cnt)
(tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) )
fit <- lm(log(Freq) ~ log(cnt), data=tab)
fhat <- exp(predict(fit, tab)) 
cstar <- (cnt+1) * predict(fit, data.frame(cnt=cnt+1)) / predict(fit, data.frame(cnt=cnt))
summary(cstar-cnt)

plot(tab$cnt,tab$Freq,log="xy")
lines(tab$cnt,fhat)

#absolute discounting
AbsoluteDisc <- function (cnt) (cnt - 0.5)/cnt

# Is this correct for bigrams, n-grams?

cnt <-row_sums(tm::TermDocumentMatrix(small, 
                                      control=list( tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)),
                                                    wordLengths=c(1, Inf))))
length(cnt)
tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt))
fit <- lm(log(Freq) ~ log(cnt), data=tab)
fhat <- exp(predict(fit, tab)) 
cstar <- (cnt+1) * predict(fit, data.frame(cnt=cnt+1)) / predict(fit, data.frame(cnt=cnt))
summary(cstar-cnt)
plot(tab$cnt,tab$Freq,log="xy")
lines(tab$cnt,fhat)


