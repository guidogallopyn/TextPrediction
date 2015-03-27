# exploratory analysis script for Capstone task 2

# Questions to consider
# Some words are more frequent than others - what are the distributions of word frequencies? 
# What are the frequencies of 2-grams and 3-grams in the dataset? 
# How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 
# ---> How do you evaluate how many of the words come from foreign languages? 
# Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(rJava)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)

# Questions
# 4)  How do you evaluate how many of the words come from foreign languages? 
# use dictionaries, if not in english dictionary then possibly non english 

# read an english corpus
#corpus <- VCorpus(DirSource("data/final/en_US/"))
corpus <- VCorpus(DirSource("data/small/en_US/"))
#corpus <- VCorpus(DirSource("data/micro/en_US/"))


Corpus.summary <- function(corpus) 
  ldply(lapply(corpus, function(x) { ellen <- nchar(content(x))
                                     wordlist <- RWeka::WordTokenizer(x)  
                                     data.frame(TextDocument=meta(x,"id"),
                                                NChars=sum(ellen), NElements=length(content(x)),
                                                MedianElement=median(ellen), MaxElement=max(ellen),
                                                NWords=length(wordlist), NVoc=length(unique(wordlist)))} ), 
        data.frame)

# summary statistics
Corpus.summary(corpus)

# cleaning
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

#funs <- list(stripWhitespace, content_transformer(tolower), removePunctuation, )
# corpus <- tm_map(corpus, tm_reduce, tmFuns=funs)

Corpus.summary(corpus)

# read english dictionary from SIL
fileUrl <- "http://www-01.sil.org/linguistics/wordlists/english/wordlist/wordsEn.txt"
download.file(fileUrl,destfile="./data/en-US-dict.txt",method="curl")
eng <- WordListDocument("./data/en-US-dict.txt",
                        meta=list(id="dictionary",language="en",origin=fileUrl))

#remove all english words and see what is left 
residue <- tm_map(corpus, removeWords, content(eng)[1:3000]) #doesn't work for more then 3000 words, regexp too large

# extract wordlist from corpus
wordlist <- PlainTextDocument(Terms(TermDocumentMatrix(corpus, control=list(tokenize = RWeka::WordTokenizer))),
                              id="wordlist",language="en")

residue <- setdiff(content(wordlist), content(eng))

length(content(wordlist)) #56582 unique words in corpus
length(content(eng))  #109584 words dictionary

length(residue) #27817 WOW words in corpus voc outside 100k dictionary 
# combination of junk characters, typos, names, acronyms, urban, collocations, foreign language etc

# idea: use an english dictionary when tokenizing for LM building !!!! threshold in number of observations
tdm <- TermDocumentMatrix(corpus, control=list(tokenize = RWeka::WordTokenizer, 
                                               dictionary=content(eng)))
tdm <- tdm[slam::row_sums(tdm)>0,]
nTerms(tdm)

# other idea: filter on word occurances at least twice
tdm2 <- TermDocumentMatrix(corpus, control=list(tokenize = RWeka::WordTokenizer))
tdm2 <- tdm2[slam::row_sums(tdm2)>1,]
nTerms(tdm)  # 28765
nTerms(tdm2) # 25429

length(intersect(Terms(tdm), Terms(tdm2))) # 19276
length(setdiff(Terms(tdm), Terms(tdm2)))   # 9489
length(setdiff(Terms(tdm2), Terms(tdm)))   # 6153 # terms occur at least twice in corpus but are not in english dictionary

tdm3 <- TermDocumentMatrix(corpus, control=list(tokenize = RWeka::WordTokenizer))
tdm3 <- tdm3[slam::row_sums(tdm3)>2,]
nTerms(tdm)  # 28765
nTerms(tdm3) # 18096

length(intersect(Terms(tdm), Terms(tdm3))) # 15015
length(setdiff(Terms(tdm), Terms(tdm3)))   # 13750
length(setdiff(Terms(tdm3), Terms(tdm)))   # 3081 # terms occur at least 3 times in corpus but are not in english dictionary


tdm4 <- TermDocumentMatrix(corpus, control=list(tokenize = RWeka::WordTokenizer))
tdm4 <- tdm4[slam::row_sums(tdm4)>3,]
nTerms(tdm)  # 28765
nTerms(tdm4) # 14419

length(intersect(Terms(tdm), Terms(tdm4))) # 12523
length(setdiff(Terms(tdm), Terms(tdm4)))   # 16242
length(setdiff(Terms(tdm4), Terms(tdm)))   # 1896 # terms occur at least 4 times in corpus but are not in english dictionary



