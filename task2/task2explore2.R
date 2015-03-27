# exploratory analysis script for Capstone task 2

# Questions to consider
# Some words are more frequent than others - what are the distributions of word frequencies? 
# ---> What are the frequencies of 2-grams and 3-grams in the dataset? 
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 
# How do you evaluate how many of the words come from foreign languages? 
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(rJava)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
#library(openNLP)
#library(wordcloud)
library(ggplot2)
library(plyr)
library(dplyr)
#require(RColorBrewer)

# read an english corpus
#eng <- VCorpus(DirSource("data/final/en_US/"))
eng <- VCorpus(DirSource("data/small/en_US/"))
#eng <- VCorpus(DirSource("data/micro/en_US/"))


Corpus.summary <- function(corpus) 
  ldply(lapply(corpus, function(x) { ellen <- nchar(content(x))
                                     wordlist <- RWeka::WordTokenizer(x)  
                                     data.frame(TextDocument=meta(x,"id"),
                                                NChars=sum(ellen), NElements=length(content(x)),
                                                MedianElement=median(ellen), MaxElement=max(ellen),
                                                NWords=length(wordlist), NVoc=length(unique(wordlist)))} ), 
        data.frame)

# summary statistics
Corpus.summary(eng)

# Questions
# 2) What are the frequencies of 2-grams and 3-grams in the dataset? 

# cleaning the data
eng <- tm_map(eng, content_transformer(tolower))
eng <- tm_map(eng, removePunctuation, preserve_intra_word_dashes = TRUE)
eng <- tm_map(eng, removeNumbers)
eng <- tm_map(eng, removeWords, readLines("data/en-US-bad-words.txt"))
eng <- tm_map(eng, stripWhitespace)

#eng <- tm_map(eng, removeWords, stopwords("english"))
#todo: filter profanity

# clean corpus summary statistics
Corpus.summary(eng)

#RWeka::WOW("NGramTokenizer")
options(mc.cores=4) # bug workaround 
#dtm <- TermDocumentMatrix(eng, control=list(tokenize = words)) 

# get unigrams, bigrams and trigrams
ngrams <- list(
  "1gram"=TermDocumentMatrix(eng, control=list(tokenize = RWeka::WordTokenizer)), # BUG workaround
  "2gram"=TermDocumentMatrix(eng, control=list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)))), # BUG workaround
  "3gram"=TermDocumentMatrix(eng, control=list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)))) # BUG workaround
)
ngrams

# calculate n gram frequencies and ranks
freqnrank <- function(tdm) {
  v <- sort(slam::row_sums(tdm), decreasing=TRUE)
  data.frame(Word=names(v), Freq=v, RelFreq=v/sum(v), Rank=rank(-v))
}
d <- ldply(lapply(names(ngrams), function(x) transform(freqnrank(ngrams[[x]]), Ngram=x)))

# summary for NGrams using dplyr
d %>% group_by(Ngram) %>% summarise(Count=sum(Freq), Unique=n()) 

# plot N-Gram probabilities
ggplot(d, aes(x=Rank, y=RelFreq, colour=Ngram)) + geom_line() + 
  scale_y_log10(breaks=10^(-7:0)) + scale_x_log10(breaks=10^(0:5)) +   
  xlab("NGram Rank") + ylab("NGram Probability Mass") + ggtitle("NGram probability in function of their rank") +
  stat_smooth(method = "lm", aes(weight=1/Rank, ,colour=Ngram))

# global rank: rank of all 1,2,3 grams mixed together
ggplot(transform(d,GRank=rank(-Freq)), aes(x=GRank, y=Freq)) + geom_point() + geom_line() + 
  scale_y_log10() + scale_x_log10() +   
  xlab("NGram Rank") + ylab("NGram Count") + ggtitle("NGram probability in function of their rank") +  
  stat_smooth(method = "lm", aes(weight=1/GRank),colour="red")

# show in top NGrams in wordcloud
par(mfrow=c(1,2))
with(filter(d,Ngram=="2gram"),wordcloud(Word, Freq, max.words=100, min.freq=2,colors=brewer.pal(8,"Dark2")))
with(filter(d,Ngram=="3gram"),wordcloud(Word, Freq, max.words=100, min.freq=2,colors=brewer.pal(8,"Dark2")))

# show in top NGrams excluding stopwords in wordcloud (not very useful)
eng <- tm_map(eng, removeWords, stopwords("english"))
# get unigrams, bigrams and trigrams
ngrams <- list(
  "1gram"=TermDocumentMatrix(eng, control=list(tokenize = RWeka::WordTokenizer)), # BUG workaround
  "2gram"=TermDocumentMatrix(eng, control=list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2)))), # BUG workaround
  "3gram"=TermDocumentMatrix(eng, control=list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3)))) # BUG workaround
)
d <- ldply(lapply(names(ngrams), function(x) transform(freqnrank(ngrams[[x]]),Ngram=x)))
d %>% group_by(Ngram) %>% summarise(N=sum(Freq), Count=n()) 
par(mfrow=c(1,2))
with(filter(d,Ngram=="2gram"),wordcloud(Word, Freq, max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2")))
with(filter(d,Ngram=="3gram"),wordcloud(Word, Freq, max.words=100, random.order=FALSE,colors=brewer.pal(8,"Dark2")))



