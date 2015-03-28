# exploratory analysis script for Capstone task 3

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)
library(dplyr)
library(slam)
library(hash)

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
#small <- VCorpus(DirSource("data/small/en_US/"))
small <- VCorpus(DirSource("data/micro/en_US/"))

# summary statistics
Corpus.summary(small)

# cleaning
small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
#small <- tm_map(small, removeWords, readLines("data/en-US-bad-words.txt"))
small <- tm_map(small, stripWhitespace)

# summary statistics clean corpus
Corpus.summary(small)

# making trigram backoff LM needs to be precomputed we'll explore creting an LM S3 class, train it and use it as a predictor
# requres tm, RWeka and slam

NGramLM <- function(corpus, N=2, threshold=0) 
{
  model <- list( "N" = N )
  class(model) <- append(class(model),"NGramLM")

  for( n in 1:N ) {
    print(paste0("Counting ",n,"-grams..."))
    cnt <- row_sums(tm::TermDocumentMatrix(corpus, 
                                           control=list( tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n)),
                                                         wordLengths=c(1, Inf))))
    cnt <- cnt[cnt > threshold]  
    model[[paste0(n,"-grams")]] <- hash(cnt) 
  }
  
  # Katz back-off probability calculation
  print(paste0("Calculating ", getNumberNGrams(model,1), " unigram probabilities..."))
  V <- sum(values(model[["1-grams"]]))
  model[["Pbo1"]] <- hash(lapply(model[["1-grams"]], function (x) x/V))
  if(N<2) return(model)
  
  print(paste0("Calculating ",getNumberNGrams(model,2)," bigram sets...")) # this is O(|V|^3) worst case, O(|V|^2) when #bigrams is proportional to #unigrams
  bigramsets <- lapply(keys(model[["1-grams"]]), 
                       function(start) grep(paste0("^",start,"[[:space:]]+"),
                                            keys(model[["2-grams"]]),value=TRUE))
  
  print(paste0("Calculating ",getNumberNGrams(model,1)," unigram back off weights..."))
  model[["alpha1"]] <- hash(Map(function (start,bgs) ifelse(length(bgs) == 0, 1, 
                                                            (1 - sum(values(model[["2-grams"]],keys=bgs)) / model[["1-grams"]][[start]]) /
                                                              (1 - sum(values(model[["Pbo1"]], keys=sapply(bgs, function(x) (unlist(strsplit(x," "))[2])))))),
                                getWords(model), bigramsets))
  print(paste0("Calculating ",getNumberNGrams(model,2)," bigram probabilities..."))
  model[["Pbo2"]] <- hash(Map(function (bg,cnt) cnt / model[["1-grams"]][[ unlist(strsplit(bg," "))[1] ]],
                              keys(model[["2-grams"]]), values(model[["2-grams"]])))
  
  
  return(model)
}


# class methods
summary.NGramLM <- function(model) return(model$N)

# get the number of Ngrams in the LM
getNumberNGrams <- function(model,N) UseMethod("getNumberNGrams",model)
getNumberNGrams.NGramLM <- function(model,N) length(model[[paste0(N,"-grams")]])


# get all the words in the LM
getWords <- function(x) UseMethod("getWords",x)
getWords.NGramLM <- function(model) keys(model[["1-grams"]])

# get all the bigrams starting with a word, work on a list of words
get2grams <- function(model,startlist) UseMethod("get2grams",model)
get2grams.NGramLM <- function(model,startlist) 
  lapply(startlist, function(start) grep(paste0("^",start,"[[:space:]]+"),
                                         keys(model[["2-grams"]]),value=TRUE))


# Katz back-off Probability of word sequence
KatzPbo <- function(model,history,w) UseMethod("KatzPbo",model)
KatzPbo.NGramLM <- function(model, history, w) {
  if(model$N == 1) return(model[["Pbo1"]][[w]]) 
  bg <- paste(history,w)
  ifelse(has.key(bg, model[["Pbo2"]]), 
         model[["Pbo2"]][[bg]], 
         model[["alpha1"]][[history]] * model[["Pbo1"]][[w]])
}



predict.NGramLM <- function(model, text, alts=NULL)
{ 
#  alts <- ifelse (length(alts) == 0, getWords(model), intersect(alts,getWords(model)))
  if(length(alts) == 0)  alts <- getWords(model)
  last1gram <- tail(RWeka::WordTokenizer(text),1)   # last word of text
  names(which.max(sapply(alts, function(w) KatzPbo(model,last1gram, w) )))
}

#
# usage
#
mod2 <- NGramLM(small,2)
str(mod2)

#
# get words out of LM
head(getWords(mod2),12)

# number of 1grams and 2grams
getNumberNGrams(mod2,1)
getNumberNGrams(mod2,2)

# howmany 1-grams in training corpus
sum(values(mod2[["1-grams"]]))

# do all unigram probs sum to 1? yes
sum(values(mod2[["Pbo1"]]))

#howmany 2-grams in training corpus
sum(values(mod2[["2-grams"]]))

# with bigrams start with "the" or "a" 
get2grams(mod2,c("the","a"))

values(mod2[["2-grams"]], keys=get2grams(mod2,"the")[[1]])
mod2[["1-grams"]][["the"]]
sum(values(mod2[["2-grams"]], keys=get2grams(mod2,"the")[[1]])) # this adds up

values(mod2[["Pbo2"]], keys=get2grams(mod2,"the")[[1]]) 

# do bigram probs starting with the word "the" add up to 1" yes
sum(values(mod2[["Pbo2"]], keys=get2grams(mod2,"the")[[1]]) )


# Probability of word "auto" in context of "the"
KatzPbo.NGramLM(mod2,"the", "auto") 
mod2[["Pbo2"]][["the auto"]]

KatzPbo.NGramLM(mod2,"the", "than") 
mod2[["Pbo2"]][["the than"]] # NULL no bigram 
mod2[["Pbo1"]][["the"]]
mod2[["alpha1"]][["the"]]

predict(mod2, "this is the", c("andy","auto"))
intersect(c("day","auto"), getWords(mod2)) # both words are in vocabulary
KatzPbo.NGramLM(mod2,"the", "auto") 
KatzPbo.NGramLM(mod2,"the", "andy") 
mod2[["Pbo2"]][["the auto"]]
mod2[["Pbo2"]][["the andy"]]

sapply(c("andy","auto"), function(w) KatzPbo(mod2,"the", w) )


names(which.max(sapply(c("andy","auto"), function(w) KatzPbo(mod2,"the", w) )))


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

# write a predict function that takes a list of alternatives

alternatives <- list(
  c("soda", "cheese","pretzels","beer"),        # beer
  c("best", "universe", "world", "most"),       # world
  c("happiest","smelliest","bluest","saddest"), # happiest
  c("referees","players","crowd","defense"),    # not crowd
  c("mall","movies","beach","grocery"),         # beach
  c("motorcycle","way","horse","phone"),        # way
  c("years","thing","weeks","time"),            # time
  c("ears","toes","eyes","fingers"),            # not eyes, likely fingers
  c("hard","sad","worse","bad"),                # not hard
  c("callous","asleep","insensitive","insane")  # not asleep
)

small <- VCorpus(DirSource("data/small/en_US/"))

# summary statistics
Corpus.summary(small)

# cleaning
small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
#small <- tm_map(small, removeWords, readLines("data/en-US-bad-words.txt"))
small <- tm_map(small, stripWhitespace)

# summary statistics clean corpus
Corpus.summary(small)

mod <- NGramLM(small, N=1, threshold=5)
str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)

mod <- NGramLM(small, N=2, threshold=5)
str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
save(mod,file="LM.en_US_small.5.RData")

#
# get words out of LM
head(getWords(mod),12)

# howmany 1-grams in training corpus
sum(values(mod[["1-grams"]]))

# do all unigram probs sum to 1? yes
sum(values(mod[["Pbo1"]]))

#howmany 2-grams in training corpus
sum(values(mod2[["2-grams"]]))

# with bigrams start with "the" or "a" 
get2grams(mod,c("the","a"))

values(mod[["2-grams"]], keys=get2grams(mod,"the")[[1]])
mod[["1-grams"]][["the"]]  # number of occurances of the unigram "the"
sum(values(mod[["2-grams"]], keys=get2grams(mod,"the")[[1]])) # number of occurances of the bigrams starting with "the"

values(mod[["Pbo2"]], keys=get2grams(mod,"the")[[1]]) 

# do bigram probs starting with the word "the" add up to 1-beta1? can't tell
sum(values(mod[["Pbo2"]], keys=get2grams(mod,"the")[[1]]) )
mod[["alpha1"]][["the"]]

Map(function(x,y) predict(mod,x,y), quiz, alternatives) #6/10 on quiz 

#does threshold 10 change the result? yes a bit 
mod <- NGramLM(small, N=2, threshold=10)
Map(function(x,y) predict(mod,x,y), quiz, alternatives) #5/10 on quiz , one NULL result

#does threshold 4 change the result? no same as threshold 5, will need trigrams and faster way to do bigram and trigram sets (need a tree)
mod <- NGramLM(small, N=2, threshold=4)
save(mod,file="LM.en_US.small.Katx.2.4.RData")
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 6/10 on quiz


# implementation with faster bigramset calculation via tree based calc
NGramLM <- function(corpus, N=2, threshold=0) 
{
  model <- list( "N" = N )
  class(model) <- append(class(model),"NGramLM")
  
  for( n in 1:N ) {
    print(paste0("Counting ",n,"-grams..."))
    cnt <- row_sums(tm::TermDocumentMatrix(corpus, 
                                           control=list( tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n)),
                                                         wordLengths=c(1, Inf))))
    cnt <- cnt[cnt > threshold]  
    model[[paste0(n,"-grams")]] <- hash(cnt) 
  }
  
  # Katz back-off probability calculation
  print(paste0("Calculating ", getNumberNGrams(model,1), " unigram probabilities..."))
  V <- sum(values(model[["1-grams"]]))
  model[["Pbo1"]] <- hash(lapply(model[["1-grams"]], function (x) x/V))
  if(N<2) return(model)
  
  print(paste0("Calculating ",getNumberNGrams(model,2)," bigram sets...")) # this is O(|V|^3) worst case, O(|V|^2) when #bigrams is proportional to #unigrams

  # hash table with followers unigram -> bigram followers
  followers <- hash()
  for( bg in keys(model[["2-grams"]])) {
    key <- unlist(strsplit(bg," "))[1]
    followers[[ key ]] <- append(followers[[ key ]], bg)  
  }
  
  print(paste0("Calculating ",getNumberNGrams(model,1)," unigram back off weights..."))
  model[["alpha1"]] <- hash(Map(function (start)
                                ifelse(!has.key(start, followers), 
                                       1, # no following bigrams
                                       (1 - sum(values(model[["2-grams"]], keys=followers[[start]])) / model[["1-grams"]][[start]]) /
                                       (1 - sum(values(model[["Pbo1"]], keys=sapply(followers[[start]], function(x) (unlist(strsplit(x," "))[2])))))),
                                getWords(model)))
  
  print(paste0("Calculating ",getNumberNGrams(model,2)," bigram probabilities..."))
  model[["Pbo2"]] <- hash(Map(function (bg,cnt) cnt / model[["1-grams"]][[ unlist(strsplit(bg," "))[1] ]],
                              keys(model[["2-grams"]]), values(model[["2-grams"]])))
  
  clear(followers)
  return(model)
}

small <- VCorpus(DirSource("data/micro/en_US/"))
small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
small <- tm_map(small, stripWhitespace)
Corpus.summary(small)

mod <- NGramLM(small, N=2, threshold=0)

str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
sum(values(mod[["1-grams"]])) # howmany 1-grams in training corpus
sum(values(mod[["Pbo1"]])) # do all unigram probs sum to 1? yes
sum(values(mod[["2-grams"]])) #howmany 2-grams in training corpus
get2grams(mod,c("the","a")) # with bigrams start with "the" or "a" 
values(mod[["2-grams"]], keys=get2grams(mod,"the")[[1]])
mod[["1-grams"]][["the"]]
sum(values(mod[["2-grams"]], keys=get2grams(mod,"the")[[1]])) # this adds up
values(mod[["Pbo2"]], keys=get2grams(mod,"the")[[1]]) 
sum(values(mod[["Pbo2"]], keys=get2grams(mod,"the")[[1]]) ) # do bigram probs starting with the word "the" add up to 1" yes
KatzPbo.NGramLM(mod,"the", "auto") # Probability of word "auto" in context of "the"
mod[["Pbo2"]][["the auto"]]
KatzPbo.NGramLM(mod,"the", "than") 
mod[["Pbo2"]][["the than"]] # NULL no bigram 
mod[["Pbo1"]][["the"]]
mod[["alpha1"]][["the"]]
predict(mod, "this is the", c("andy","auto"))
intersect(c("day","auto"), getWords(mod)) # both words are in vocabulary
KatzPbo.NGramLM(mod,"the", "auto") 
KatzPbo.NGramLM(mod,"the", "andy") 
mod[["Pbo2"]][["the auto"]]
mod[["Pbo2"]][["the andy"]]
sapply(c("andy","auto"), function(w) KatzPbo(mod,"the", w) )
names(which.max(sapply(c("andy","auto"), function(w) KatzPbo(mod,"the", w) )))


# now with small corpus
small <- VCorpus(DirSource("data/small/en_US/"))

small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
small <- tm_map(small, stripWhitespace)
Corpus.summary(small)

mod <- NGramLM(small, N=2, threshold=5)
str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
save(mod,file="LM.en_US.small.katz.2.5.r2.RData")
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 6/10 on quiz same result but way faster 632kb

mod <- NGramLM(small, N=2, threshold=3)
str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
save(mod,file="LM.en_US.small.katz.2.3.r2.RData")
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 6/10 on quiz, same result good speed, 938kb

mod <- NGramLM(small, N=2, threshold=0)
str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
save(mod,file="LM.en_US.small.katz.2.0.r2.RData")
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 7/10 on quiz, best, result much slower training speed, 8.3Mb

mod <- NGramLM(small, N=2, threshold=1)
str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
save(mod,file="LM.en_US.small.katz.2.1.r2.RData")
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 6/10 on quiz, decent training speed 2.1Mb

# conclusion: we need trigrams with backoff (we allready knew that)

# convenience ngram sgtring functions
nghead <- function(ng) { tmp <- unlist(strsplit(ng," ",fixed=TRUE)); paste(head(tmp,length(tmp)-1),collapse=" ")}   
ngtail <- function(ng) { tmp <- unlist(strsplit(ng," ",fixed=TRUE)); paste(tail(tmp,length(tmp)-1),collapse=" ")}   
ngfirst <- function(ng) head(unlist(strsplit(ng," ",fixed=TRUE)),1) # first word of n gram 
nglast  <- function(ng) tail(unlist(strsplit(ng," ",fixed=TRUE)),1) # last word of n gram 


# implementation with faster bigramset calculation via tree based calc
NGramLM <- function(corpus, N=2, threshold=0) 
{
  model <- list( "N" = N )
  class(model) <- append(class(model),"NGramLM")
  
  for( n in 1:N ) {
    print(paste0("Counting ",n,"-grams..."))
    cnt <- row_sums(tm::TermDocumentMatrix(corpus, 
                                           control=list( tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n)),
                                                         wordLengths=c(1, Inf))))
    cnt <- cnt[cnt > threshold]  
    model[[paste0(n,"-grams")]] <- hash(cnt) 
  }
  
  # Katz back-off probability calculation
  print(paste0("Calculating ", getNumberNGrams(model,1), " unigram probabilities..."))
  V <- sum(values(model[["1-grams"]]))
  model[["Pbo1"]] <- hash(lapply(model[["1-grams"]], function (x) x/V))
  if(N<2) return(model)

  # hash table with followers unigram -> bigram followers
  followers <- hash()
  
  # add (n-1)gram followers to hastable:  (n-1)gram + word --> ngram 
  for(n in 2:N) {
    print(paste0("Calculating ",getNumberNGrams(model,n)," ",n,"-gram sets...")) 
    for( ng in keys(model[[paste0(n,"-grams")]])) {
      key <- nghead(ng)
      followers[[ key ]] <- append(followers[[ key ]], ng)  
    }
    
    print(paste0("Calculating ",getNumberNGrams(model,n-1)," ",n-1,"-gram back off weights..."))
    model[[paste0("alpha",n-1)]] <- 
      hash(Map(function (start) ifelse(!has.key(start, followers), 
                                       1, # no following trigrams
                                       (1 - sum(values(model[[paste0(n,"-grams")]], keys=followers[[start]])) / model[[paste0(n-1,"-grams")]][[start]]) /
                                         (1 - sum(values(model[[paste0("Pbo",2)]], keys=sapply(followers[[start]], ngtail))))),
               keys(model[[paste0("2-grams")]]) ))
    
    print(paste0("Calculating ",getNumberNGrams(model,n)," ",n,"-gram probabilities..."))
    model[[paste0("Pbo",n)]] <- hash(Map(function (ng,cnt) cnt / model[[paste0(n-1,"-grams")]][[ nghead(ng) ]],
                                         keys(model[[paste0(n,"-grams")]]), values(model[[paste0(n,"-grams")]])))
    
  }
  clear(followers)
  return(model)
}

# test bench
small <- VCorpus(DirSource("data/micro/en_US/"))
small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
small <- tm_map(small, stripWhitespace)
Corpus.summary(small)

mod <- NGramLM(small, N=3, threshold=0)

str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
sum(values(mod[["1-grams"]])) # howmany 1-grams in training corpus
sum(values(mod[["Pbo1"]])) # do all unigram probs sum to 1? yes
sum(values(mod[["2-grams"]])) #howmany 2-grams in training corpus
get2grams(mod,c("the","a")) # with bigrams start with "the" or "a" 
values(mod[["2-grams"]], keys=get2grams(mod,"the")[[1]])
mod[["1-grams"]][["the"]]
sum(values(mod[["2-grams"]], keys=get2grams(mod,"the")[[1]])) # this adds up
values(mod[["Pbo2"]], keys=get2grams(mod,"the")[[1]]) 
sum(values(mod[["Pbo2"]], keys=get2grams(mod,"the")[[1]]) ) # do bigram probs starting with the word "the" add up to 1" yes
KatzPbo.NGramLM(mod,"the", "auto") # Probability of word "auto" in context of "the"
mod[["Pbo2"]][["the auto"]]
KatzPbo.NGramLM(mod,"the", "than") 
mod[["Pbo2"]][["the than"]] # NULL no bigram 
mod[["Pbo1"]][["the"]]
mod[["alpha1"]][["the"]]
predict(mod, "this is the", c("andy","auto"))
intersect(c("day","auto"), getWords(mod)) # both words are in vocabulary
KatzPbo.NGramLM(mod,"the", "auto") 
KatzPbo.NGramLM(mod,"the", "andy") 
mod[["Pbo2"]][["the auto"]]
mod[["Pbo2"]][["the andy"]]
sapply(c("andy","auto"), function(w) KatzPbo(mod,"the", w) )
names(which.max(sapply(c("andy","auto"), function(w) KatzPbo(mod,"the", w) )))

# now with small corpus
small <- VCorpus(DirSource("data/small/en_US/"))

small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
small <- tm_map(small, stripWhitespace)
Corpus.summary(small)
mod <- NGramLM(small, N=3, threshold=5)
str(mod)
getNumberNGrams(mod,1)
getNumberNGrams(mod,2)
getNumberNGrams(mod,3)
save(mod,file="LM.en_US.small.katz.3.5.r3.RData")
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 6/10 on quiz, decent training speed 2.1Mb





