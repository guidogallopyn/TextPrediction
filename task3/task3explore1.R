# exploratory analysis script for Capstone task 3

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)
library(dplyr)
library(slam)

# Questions
# what is needed ? 1
# 1) efficiently store a sparse matrix p(wj|wi...wk) or multi dim for n-gram
# 2) fast retrieval of argmax  p(wj|wi...wk) given p(wj|wi...wk)



options(mc.cores=1) # RWeka bug workaround 

Corpus.summary <- function(corpus) 
  ldply(lapply(corpus, function(x) { ellen <- nchar(content(x))
                                     wordlist <- RWeka::WordTokenizer(x)  
                                     data.frame(TextDocument=meta(x,"id"),
                                                NChars=sum(ellen), NElements=length(content(x)),
                                                MedianElement=median(ellen), MaxElement=max(ellen),
                                                NWords=length(wordlist), NVoc=length(unique(wordlist)))} ), 
        data.frame)

# read an english corpus
#corpus <- VCorpus(DirSource("data/final/en_US/"))
small <- VCorpus(DirSource("data/small/en_US/"))
#small <- VCorpus(DirSource("data/micro/en_US/"))

# summary statistics
Corpus.summary(small)

# cleaning
small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
small <- tm_map(small, removeWords, readLines("data/en-US-bad-words.txt"))
small <- tm_map(small, stripWhitespace)

# summary statistics clean corpus
Corpus.summary(small)

# NGram counting
unigramTokenizer  <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))
bigramTokenizer   <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))
trigramTokenizer  <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))

#ngrams <- list( 
#  "1gram"=TermDocumentMatrix(small, control=list(tokenize = RWeka::WordTokenizer)), 
#  "2gram"=TermDocumentMatrix(small, control=list(tokenize = bigramTokenizer)), 
#  "3gram"=TermDocumentMatrix(small, control=list(tokenize = trigramTokenizer)) 
#)


# first model = No LM, use Ngram counts, during predicton find nGram with max count, might be a bit slow during prediction
unigramCounts <- row_sums(TermDocumentMatrix(small, control=list(tokenize = unigramTokenizer)))
bigramCounts  <- row_sums(TermDocumentMatrix(small, control=list(tokenize = bigramTokenizer)))

# look at most frequent unigrams and bigrams
head(sort(unigramCounts,decreasing=TRUE),22)
head(sort(bigramCounts,decreasing=TRUE),22)

unigramCounts["the"]
bigramCounts["of the"]



# simple predictor function: get the max count of all bigrams with given start word
predict2 <- function(text) {
  lastword <- tail(unigramTokenizer(text),1)           
  pattern <- paste0("^",lastword,"[[:space:]]+")
  followers <- bigramCounts[grepl(pattern,names(bigramCounts))] # find all bigrams with a starting word the last word of the input text
  unigramTokenizer(names(which.max(followers)))[2]
}

predict2(" this is a test ")
predict2(" my name is ")
predict2(" tell me why")
predict2(" guido ")

# what if last word is OOV? prediction is NA 
predict2(" tell me maritza")


# extending to trigrams
trigramCounts  <- row_sums(TermDocumentMatrix(small, control=list(tokenize = trigramTokenizer)))

# simple predictor function: get the max count of all trigrams with given start word
predict3 <- function(text) {
  last2gram <- tail(bigramTokenizer(text),1)           
  pattern <- paste0("^",last2gram,"[[:space:]]+")
  followers <- trigramCounts[grepl(pattern,names(trigramCounts))] # find all trigrams with a starting bigram the last bigram of the input text
  unigramTokenizer(names(which.max(followers)))[3]
}

predict3(" this is a test ")
predict3(" my name is ")
predict3(" tell me why")

# what if last bigram is OOV? we get a NA
predict3(" tell me maritza")
predict3(" tell me guido")


#quiz sentences
quiz<- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
        "You're the reason why I smile everyday. Can you follow me please? It would mean the",
        "Hey sunshine, can you follow me and make me the",
        "Very early observations on the Bills game: Offense still struggling but the",
        "Go on a romantic date at the",
        "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
        "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
        "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
        "Be grateful for the good times and keep the faith during the",
        "If this isn't the cutest thing you've ever seen, then you must be"
)

lapply(quiz, predict3) # score 3/10 

# write a predict function that takes a list of alternatives

alternatives <- list(
  c("soda", "cheese","pretzels","beer"),
  c("best", "universe", "world", "most"),
  c("happiest","smelliest","bluest","saddest"),
  c("referees","players","crowd","defense"),
  c("mall","movies","beach","grocery"),
  c("motorcycle","way","horse","phone"),
  c("years","thing","weeks","time"),
  c("ears","toes","eyes","fingers"),
  c("hard","sad","worse","bad"),
  c("callous","asleep","insensitive","insane")
)

predict3a <- function(text, alternatives=NULL) {
  last2gram <- tail(bigramTokenizer(text),1)           
  pattern <- paste0("^",last2gram,"[[:space:]]+")
  followers <- trigramCounts[grepl(pattern,names(trigramCounts))] # find all trigrams with a starting bigram the last bigram of the input text
  pattern <- paste(alternatives,collapse="|")
  if (length(alternatives) > 0) followers <- followers[grepl(pattern,names(followers))] 
  unigramTokenizer(names(which.max(followers)))[3]  # todo
}

Map(predict3a, quiz, alternatives) # only 4/10 with an not NA answer


# lets try 4 grams

quadgramTokenizer  <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 4, max = 4))
quadgramCounts  <- row_sums(TermDocumentMatrix(small, control=list(tokenize = quadgramTokenizer)))

predict4a <- function(text, alternatives=NULL) {
  last3gram <- paste0("^",tail(trigramTokenizer(text),1))         
  followers <- quadgramCounts[grepl(last3gram,names(quadgramCounts))] # find all 4grams with a starting 3gram the last 3gram of the input text
  pattern <- paste(alternatives,collapse="|")
  if (length(alternatives) > 0) followers <- followers[grepl(pattern,names(followers))] 
  unigramTokenizer(names(which.max(followers)))[4]  # todo
}

Map(predict4a, quiz, alternatives) # only 1/10 with an not NA answer: problem is OOV, need a backoff strategy or interpolation


# Katz backoff http://en.wikipedia.org/wiki/Katz%27s_back-off_model
 
  
# bigram predictor with Katz backoff, starting from ngram counts
predict2bo <- function(text, alts=NULL) {
  alts <- ifelse (length(alts) == 0, names(unigramCounts), intersect(alts,names(unigramCounts))) # for which words to predict # can this be pruned ???             
  last1gram <- tail(unigramTokenizer(text),1)          # last word of text
  startpattern <- paste0("^",last1gram,"[[:space:]]+")
  followers <- bigramCounts[grepl(startpattern, names(bigramCounts))] # find all bigrams with a starting word the last word of the input text
  words <- sub(startpattern, "", names(followers))      # all second words in bigrams starting with last1gram  
  
  beta1 <- 1 - sum(followers)/unigramCounts[last1gram]                # left-over probability mass for unigram
  alpha1 <- beta1 /((1-sum(unigramCounts[words])/sum(unigramCounts)))   # bow: back-off weight
  
  Pbo <- setNames(rep(0,length(alts)), alts)
  
  for (w in alts) {  
    Pbo[w]  <- ifelse( w %in% words,
                       followers[paste(last1gram,w)] / unigramCounts[last1gram],
                       alpha1 * unigramCounts[w] / sum (unigramCounts) )
  }
  names(which.max(Pbo))
}

Map(predict2bo, quiz, alternatives) # only 7/10 with but 3 NULL answers, which may be due to OOV in alts? No
# important unigram like "of", "in", "an", "a", "i",  is missing!!! why??? seems all 1 and 2 letter words are missing from unigrams 
# huge tokenization bug termFreq default is min wordlength of 3 (correction made above)

unigramCounts <- row_sums(TermDocumentMatrix(small, control=list(tokenize = unigramTokenizer, wordLengths=c(1, Inf))))
bigramCounts  <- row_sums(TermDocumentMatrix(small, control=list(tokenize = bigramTokenizer, wordLengths=c(1, Inf))))
trigramCounts  <- row_sums(TermDocumentMatrix(small, control=list(tokenize = trigramTokenizer, wordLengths=c(1, Inf))))
quadgramCounts  <- row_sums(TermDocumentMatrix(small, control=list(tokenize = quadgramTokenizer, wordLengths=c(1, Inf))))


#now try again
Map(predict2bo, quiz, alternatives) # only 10/10 with but nonsensical ansers for a few, so lets try tri grams with backoff

# making trigram backoff LM needs to be precomputed we'll explore creting an LM S3 class, train it and use it as a predictor



# medium size corpus 10 million words
corpus <- VCorpus(DirSource("data/final/en_US/"))
medium <- tm_map(corpus , content_transformer(function(x) x[sample(length(x),length(x)/10)])) 
writeCorpus(medium, path = "data/medium/en_US/")

