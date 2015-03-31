# source file for the NGramLM class

# see http://www.speech.sri.com/projects/srilm/manpages/ngram-discount.7.html


require(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
require(slam)
require(hash)

# convenience ngram hash-key functions
nghead <- function(ng,k=1) { tmp <- unlist(strsplit(ng," ",fixed=TRUE))
                             return(paste(head(tmp,length(tmp)-k),collapse=" "))}   
ngtail <- function(ng,k=1) { tmp <- unlist(strsplit(ng," ",fixed=TRUE))
                             return(paste(tail(tmp,length(tmp)-k),collapse=" "))}   
ngfirst <- function(ng,k=1) return(paste(head(unlist(strsplit(ng," ",fixed=TRUE)),k),collapse=" ")) # first k words  of n gram 
nglast  <- function(ng,k=1) return(paste(tail(unlist(strsplit(ng," ",fixed=TRUE)),k),collapse=" ")) # last k words of n gram 


# good-turing discounting with smoothing 
GoodTuringDisc <- function (cnt) {
  tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) 
  fit <- lm(log(Freq) ~ log(cnt), data=tab)
  (1 + 1/cnt) * exp(predict(fit, data.frame(cnt=cnt+1))) / exp(predict(fit, data.frame(cnt=cnt)))
}
# good-turing probability estimate for number of OOV given 
GoodTuringOOV <- function (cnt,threshold) {
  tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) 
  fit <- lm(log(Freq) ~ log(cnt), data=tab)
  sum(exp(predict(fit, data.frame(cnt=1:(threshold+1)))) 
}


# NGram LM R class constructor
NGramLM <- function(corpus, N=2, threshold=0, debug=FALSE) 
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
    model[[paste0("disc",n)]] <- hash(GoodTuringDisc(cnt))
  }
  
  # Katz back-off probability calculation
  print(paste0("Calculating ", getNumberNGrams(model,1), " unigram probabilities..."))
  V <- sum(values(model[["1-grams"]]))
  ngkeys <- keys(model[["1-grams"]])
  model[["Pbo1"]] <- hash(Map(function (cnt,d) d * cnt / V, 
                              values(model[["1-grams"]],keys=ngkeys), 
                              values(model[["disc1"]],keys=ngkeys)))
  
  #calculate OOV prob estimate (Good-Turing)
  model[["Pbo1"]][["<OOV>"]] <- / V
  if(N<2) return(model)
  
  # hash table with followers (n-1)gram + word -> ngram followers
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
                                         (1 - sum(values(model[[paste0("Pbo",n-1)]], keys=sapply(followers[[start]], ngtail))))),
               keys(model[[paste0(n-1,"-grams")]]) ))
    
    print(paste0("Calculating ",getNumberNGrams(model,n)," ",n,"-gram probabilities..."))
    ngkeys <- keys(model[[paste0(n,"-grams")]])
    model[[paste0("Pbo",n)]] <- hash(Map(function (ng,cnt,d) d * cnt / model[[paste0(n-1,"-grams")]][[ nghead(ng) ]],
                                         ngkeys, 
                                         values(model[[paste0(n,"-grams")]],keys=ngkeys),
                                         values(model[[paste0("disc",n)]],keys=ngkeys))) 
  }
  clear(followers)
  # clear counts and discounts,  we keep them for debugging
  if(!debug) sapply(grep("grams|disc",names(model),value=TRUE),function(h) clear(model[[h]]))
  return(model)
}

# NGramLM class methods: summary
summary.NGramLM <- function(model) return(paste("NGramLM language model, N =",model$N))

# NGramLM class methods: print
print.NGramLM <- function(model) return(paste("NGramLM language model, N =",model$N))

# NGramLM class method: clear: removing hash R environments 
clear.NGramLM <- function(model) {
sapply(grep("grams|Pbo|disc|alpha",names(model),value=TRUE),
       function(h) clear(model[[h]]))
}

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


#' Katz back-off Probability of a word given a history of n-1 words
#' 
#' @param model   : an NGramLM model
#' @param history : character vector with history  c("w1", "w2", ... )
#' @param w       : word 
#' @return Pbo ( w | history ) the probability of observing w given history

KatzPbo <- function(model, history ,w) UseMethod("KatzPbo",model)
KatzPbo.NGramLM <- function(model, history, w) { 
  if(length(history) == 0) return(ifelse(has.key(w,model[["Pbo1"]]), # unigram probability or good-turing for OOV
                                         model[["Pbo1"]][[w]],
                                         model[["Pbo1"]][["<OOV>"]]))  
  if(length(history) > model$N-1) history <- tail(history, model$N-1)    # truncate history to max N-1gram (if needed)
  n <- length(history) + 1 
  ng <- paste(c(history,w),collapse=" ")  # ngram hash key
  n1g <- paste(history,collapse=" ")      # history (n-1)gram hash key 
  ifelse(has.key(ng, model[[paste0("Pbo",n)]]), 
         model[[paste0("Pbo",n)]][[ng]],  # ngram probability
         ifelse(has.key(n1g, model[[ paste0("alpha",n-1) ]]), model[[ paste0("alpha",n-1) ]][[ n1g ]] , 1) * 
            KatzPbo.NGramLM(model, tail(history, length(history)-1), w))   # backoff * (n-1)gram probability
}

#' Perplexity of a token sequence with NGram model
#' 
#' @param model   : an NGramLM model
#' @param tokens  : character vector with words  c("w1", "w2", ... )
#' @return Perplexity of the word sequence ie. geometric mean of the 1/probabilities of the individual words in context

Perplexity <- function(model, tokens) UseMethod("Perplexity",model)
Perplexity.NGramLM <- function (model,tokens) exp(-sum(mapply(function(w,history) log(KatzPbo.NGramLM(model, history, w)),
                                                              tokens,
                                                              lapply(1:length(tokens),function(n) tail(head(tokens,n-1),model$N-1)) )) /length(tokens))

# Perplexity on a corpus using a NGram model
CorpusPerplexity  <- function(model, corpus)  UseMethod("CorpusPerplexity",model)
CorpusPerplexity.NGramLM  <- function(model, corpus) {
  mergedcorp <- VCorpus(VectorSource(unlist(lapply(corpus,content))))  # merge corpus so that every line in all docs is seperate document
  tokenCorp <- tm_map(mergedcorp, RWeka::WordTokenizer)                # tokenize the corpus: each doc is now a token vector
  tokenCorp <- tm_filter(tokenCorp, function(x) (length(x) > 0))       # remove zero length docs
  exp(weighted.mean(log(sapply(tokenCorp, function(tokens) Perplexity(model,tokens))), sapply(tokenCorp,length))) 
}



predict.NGramLM <- function(model, text, alts=NULL)
{ 
  if(length(alts) == 0)  alts <- getWords(model)
  else alts <- intersect(alts,getWords(model))
  if(length(alts) == 0) return(NULL)  
  history <- RWeka::WordTokenizer(text)
  names(which.max(sapply(alts, function(w) KatzPbo(model, history, w) )))
}



# LM training receipe with controls
train <- function(control, path="") {
  modelFile <- paste0(path,"models/LM.en_US.", control$data,".Katz.",control$N,".",control$threshold,".RData")
  if(!file.exists(modelFile)) {
    corpus <- VCorpus(DirSource(paste0(path,"data/",control$data,"/en_US/")))
    corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
    corpus <- tm_map(corpus, removeNumbers)
    #corpus <- tm_map(corpus, removeWords, readLines("data/en-US-bad-words.txt"))
    corpus <- tm_map(corpus, stripWhitespace)
    mod <- NGramLM(corpus, N=control$N, threshold=control$threshold)
    save(mod,file=modelFile)
  } else load(file=modelFile)
  return(mod)
}
