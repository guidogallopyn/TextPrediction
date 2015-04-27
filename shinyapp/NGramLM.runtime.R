# source file for the NGramLM class
# runtime without training


options( java.parameters = "-Xmx4g" ) # more jave heap space for RWeka see http://www.bramschoenmakers.nl/en/node/726
options(mc.cores=1) # RWeka bug workaround 
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
require(tm)
require(slam)
require(hash)

#
# convenience ngram hash-key functions
#

#' Get the n-k word prefix of the N-gram a_z hashkey.
#' 
#' @param a_z     : an n gram hash key
#' @param k       : default 1
#' @return a_ hash key of n-k gram prefix 
ngprefix <- function(a_z,k=1) { tmp <- unlist(strsplit(a_z," ",fixed=TRUE))  # The n-k word prefix of the N-gram a_z.
                                return(paste(head(tmp,length(tmp)-k),collapse=" "))}  

#' Get the n-k word suffix of the N-gram a_z hashkey.
#' 
#' @param a_z     : an n gram hash key
#' @param k       : default 1
#' @return _z hash key of n-k gram suffix
#'  
ngsuffix <- function(a_z,k=1) { tmp <- unlist(strsplit(a_z," ",fixed=TRUE))  # The n-k word suffix of the N-gram a_z.
                               return(paste(tail(tmp,length(tmp)-k),collapse=" "))}   

#' Get the first word of the N-gram a_z hashkey.
#' 
#' @param a_z     : an n gram hash key
#' @param k       : default 1
#' @return a hash key of the first word
#'  
ngfirst <- function(ng,k=1) return(paste(head(unlist(strsplit(ng," ",fixed=TRUE)),k),collapse=" ")) # first k words  of n gram 

#' Get the last word of the N-gram a_z hashkey.
#' 
#' @param a_z     : an vector of n gram hash keys
#' @param k       : default 1
#' @return z hash key of the last word
#' 
#nglast  <- function(ng,k=1) return(paste(tail(unlist(strsplit(ng," ",fixed=TRUE)),k),collapse=" ")) # last k words of n gram 
nglast  <- function(ngs,k=1) { if(is.null(ngs)) return(NULL) 
  sapply(strsplit(ngs," ",fixed=TRUE), function(s) paste(tail(s,k),collapse=" ")) # last k words of n gram 
}  

# good-turing discounting with smoothing 
GoodTuringDisc <- function (cnt) {
  tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) 
  fit <- lm(log(Freq) ~ log(cnt), data=tab)
  (1 + 1/cnt) * exp(predict(fit, data.frame(cnt=cnt+1))) / exp(predict(fit, data.frame(cnt=cnt)))
}


# tm corpus subsample function

subsample.TextDocument <- function(doc,frac) 
  PlainTextDocument( sample(content(doc), frac * length(content(doc))), 
                     id = meta(doc,"id") )


# constructs an LM model file name ecoding model info
modelfilename <- function(data, method="Katz",N=3,K=3,debug=FALSE) {
  s <- paste0("LM.en_US.", data,".",method,".N",N,".K",K)
  if(debug) s <- paste0(s,".d")
  paste0(s,".RData")
}

#parses a model filename and returns a list of model properties
LMparams <- function(names) {
  chunks <- strsplit(names,".",fixed=TRUE)
#  if(chunks[1] != "LM") return (NULL)  # remove non LM files to do
  df <- data.frame(data     = sapply(chunks,"[", 3),
                   language = sapply(chunks,"[", 2),
                   method   = sapply(chunks,"[", 4),
                   N        = sapply(chunks, function(x) as.integer(sub("N","",x[5]))),
                   K        = sapply(chunks, function(x) as.integer(sub("K","",x[6]))),
                   debug    = sapply(chunks, function(x) (x[7] =="d")),
                   name     = names)
  return(df)
}



# NGramLM class methods: summary
summary.NGramLM <- function(model) {
  s <- list("Markov Order" = model$N, 
            "Method"       = "Katz back-off",
            "Smoothing"    = "Good-Turing",
            "stats"        = model$stats)
  class(s) <- c("NGramLM.summary", class(s))
  return(s)
}

print.NGramLM.summary <- function(x) {
  print(noquote("NGramLM language model"))
  sapply(1:3,function(n) print(noquote(paste(names(x)[n],":",x[[n]]))))
  print(x$stats)
}


# NGramLM class methods: print
print.NGramLM <- function(model) return(paste("NGramLM language model, N =",model$N))



# NGramLM class method: clear: removing hash R environments 
clear <- function(model) UseMethod("clear",model)
clear.NGramLM <- function(model) 
                    for( h in grep("grams|^p|disc|alpha|logp|bow|followers|trie",names(model),value=TRUE) )
                        hash::clear(model[[h]]) 



# get the number of unique Ngrams of order n in the LM
getNumberNGrams <- function(model,n) UseMethod("getNumberNGrams",model)
getNumberNGrams.NGramLM <- function(model,n) length(model[[paste0(n,"-grams")]])



# get the Ngrams counts of order n in the LM
getNGramCount <- function(model,n) UseMethod("getNGramCount",model)
getNGramCount.NGramLM <- function(model,n) sum(values(model[[paste0(n,"-grams")]]))



# get all the words in the LM
getWords <- function(x) UseMethod("getWords",x)
getWords.NGramLM <- function(model) keys(model[["1-grams"]])



# get all the words in the LM
Voc <- function(x) UseMethod("Voc",x)
Voc.NGramLM <- function(model) keys(model$logp1)



# get number of words in the LM
nVoc <- function(x) UseMethod("nVoc",x)
nVoc.NGramLM <- function(model) length(model$logp1)




#' LM score = log(p) of a word given a history of n-1 words
#' 
#' @param model   : an NGramLM model
#' @param history : character vector with history  c("w1", "w2", ... )
#' @param w       : character vector of word to score (TODO, works with 1 now)
#' @return log p( w | history ) vector of log probabilities of observing w given history

LMScore <- function(model, history ,w) UseMethod("LMScore",model)

LMScore.NGramLM <- function(model, history, w) { 
  
  if(length(w) == 0) return(numeric(0))
  
  if(length(history) == 0) history <- ""
  
  if(model$N==1 | (length(history) == 1 & history[1] == "")) { # unigram
    ws <- w
    ws[!has.key(ws,model$logp1)] <- "<UNK>"
    scores <- values(model$logp1, keys=ws)
    names(scores) <- w
    return(unlist(scores))
  }
  
  if(length(history) > model$N-1) history <- tail(history, model$N-1)    # truncate history to max N-1 gram   
  n <- length(history) + 1 
  
  n1g <- paste(history,collapse=" ")      # history (n-1)gram hash key 
  ngs <- paste(n1g,w)                     # ngram hash keys 
  logpn <- paste0("logp",n)
  bown1 <- paste0("bow",n-1) 
  
  lmcovered <- has.key(ngs, model[[logpn]])
  scores <- c(values(model[[logpn]], keys=ngs[ lmcovered ] ), 
              ifelse(has.key(n1g, model[[ bown1 ]]), model[[ bown1 ]][[ n1g ]] , 0) +
              LMScore.NGramLM(model, tail(history, length(history)-1), nglast(ngs[ !lmcovered ])))
  names(scores) <- nglast(names(scores)) 
  return(unlist(scores))
}

# non recurseive implementation

LMbest.NGramLM <- function(model, history, prefix="") { 
  
  if(length(history) > model$N-1) history <- tail(history, model$N-1)    # truncate history to max N-1 gram   
  N <- length(history) + 1 
  
  if(length(history) == 0) history <- ""
  
  if (prefix != "") {  
    voc <- model$trie[[prefix]]
    if (has.key(prefix,model$logp1)) voc <- c(prefix,voc)
  } else voc <- Voc(model)       # no prefix, then consider full vocabulary
  
  if( N==1 )  { # unigram
    maxscore <- values(model$logp1, keys=names(which.max(values(model$logp1, keys=voc)))) # named score
    return( maxscore ) 
  }
  
  maxscore <- c(UNK= -Inf)
  n1g <- paste(history,collapse=" ")                               # history unigram hash key 
  bown <- 0
  for(n in N:2) {    
    logpn <- paste0("logp",n)
    bown1 <- paste0("bow",n-1)
    prefwords <- intersect(nglast(model$followers[[n1g]]), voc)
    if(length(prefwords)>0) { 
      voc <- setdiff(voc,prefwords)
      ngs <- paste(n1g,prefwords)
      nscore <- values(model[[logpn]], keys=names(which.max(values(model[[logpn]], keys=ngs)))) # named score
      scores <- c(maxscore,  nscore + bown) 
      names(scores) <-nglast(names(scores))
      maxscore <- scores[which.max(scores)]
    }
    if(has.key(n1g, model[[bown1]])) bown <- bown + model[[bown1]][[ n1g ]] 
    n1g <- ngsuffix(n1g)
  }   
  nscore <- values(model$logp1, keys=names(which.max(values(model$logp1, keys=voc)))) # named score
  scores <- c(maxscore, nscore + bown)
  names(scores) <- nglast(names(scores))
  maxscore <- scores[which.max(scores)]
  return( maxscore ) 
}



#' Predict next word probability using a NGram language model
#' 
#' @param model : an NGramLM model
#' @param text : left context 
#' @param alts : character vector with next words to conpute probability  c("alt1", "alt2", ... )
#' @param NBest : number of predicted alternatives to compute
#' @param prefix : character string with prefix string of next word
#' @return N words as a named vector of scores with words as names
#' 

predict.NGramLM <- function(model, text, alts=NULL, Nbest=1, prefix="")
{ 
  cleanctrl <-list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                   content_transformer(tolower),
                   removePunctuation,
                   removeNumbers,
                   stripWhitespace)
  tokens <- RWeka::WordTokenizer(content(tm_reduce(PlainTextDocument(text), cleanctrl)))
  history <- tail(tokens,model$N-1)
  if(Nbest==1) return(LMbest.NGramLM (model, history, prefix=prefix))
  if(length(alts) == 0)  { 
    if (prefix != "") alts <- union(prefix, model$trie[[prefix]])
    else if(length(history) == 0) alts <- names(head(model$top20,Nbest)) # unigram no prefix
    else { # ngram
      last <- tail(history,1) # last word, what if last word is OOV? or history zero length 
      alts <- union(nglast(model$followers[[last]]), names(head(model$top20,Nbest))) # no prefix
    }  
  }  
  return(head(sort(LMScore(model, history, alts), decreasing = TRUE), Nbest))
}

