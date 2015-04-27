# source file for the NGramLM class

# see http://www.speech.sri.com/projects/srilm/manpages/ngram-discount.7.html

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

# good-turing probability estimate for number of OOV given 
GoodTuringOOV <- function (cnt,threshold) {
  tab <- transform(as.data.frame(table(cnt)),cnt=as.integer(cnt)) 
  fit <- lm(log(Freq) ~ log(cnt), data=tab)
  sum(exp(predict(fit, data.frame(cnt=1:(threshold+1)))))/sum(cnt[cnt > threshold]) 
}


# NGram LM R class constructor
trainNGramLM <- function(corpus, N=2, threshold=0, debug=FALSE) 
{
  model <- list( "N" = N )
  class(model) <- append(class(model),"NGramLM")
  train.NGramLM(model, corpus, list(N=N, threshold=threshold, debug=debug))
}

# load an LM model reading a ARPA format file
loadNGramLM <- function(mypath) {
  model <- readARPA(mypath)
  class(model) <- append(class(model),"NGramLM")
  return(model)
}  

# tm corpus subsample function

subsample.TextDocument <- function(doc,frac) 
  PlainTextDocument( sample(content(doc), frac * length(content(doc))), 
                     id = meta(doc,"id") )

# arpa ngram  format = http://www.speech.sri.com/projects/srilm/manpages/ngram-format.5.html

readARPA <- function(mypath) {
  
  con <- file(mypath, "r", blocking = FALSE)
  arpa <- list(ngrams=c())
  
  # regular expression used for parsing
  blank.rex <- "^[[:blank:]]*$"
  ngram.rex <- "^[[:blank:]]*ngram[[:blank:]]+(?<N>[[:digit:]]+)[[:blank:]]*=[[:blank:]]*(?<ngrams>[[:digit:]]+)"
  data.rex <- "(?<N>[[:digit:]]+)-grams:" 
  n.rex <- "[[:digit:]]+" 
  
  state = "begin"
  while(TRUE) {
    line <-readLines(con,n=1)
    
    if( state == "begin") 
      if( line != "\\data\\") next   # wait until we find a \data\ section
    else { state <- "counts"; next } 
    
    
    if(grepl(blank.rex,line,perl=TRUE)) next # skip blank lines
    
    if(line == "\\end\\") {
      state <- "end"
      break
    }
    
    if( state == "counts") {
      
      # get data from "ngram n=NNN" line
      if(grepl(ngram.rex,line,perl=TRUE)) { 
        result <- regexpr(ngram.rex, line,perl=TRUE)
        st <- attr(result, "capture.start")[1,]
        len <- attr(result, "capture.length")[1,]
        n <-substring(line,st[1],st[1]+len[1]-1)  # put this in a list
        ngrams <-substring(line,st[2],st[2]+len[2]-1)
        arpa[["ngrams"]] <- c(arpa[["ngrams"]], as.double(ngrams))
        names(arpa[["ngrams"]])[length(arpa[["ngrams"]])] <- n
        next
      }
      
      # get data from "\N-grams:" line
      if(grepl(data.rex,line,perl=TRUE)) {
        result <- regexpr(data.rex, line, perl=TRUE)
        st <- attr(result, "capture.start")[1,]
        len <- attr(result, "capture.length")[1,]
        n<-substring(line,st[1],st[1]+len[1]-1)  # put this in a list
        state <- n
        arpa[[paste0('logp',state)]] <- hash()
        arpa[[paste0('bow',state)]] <- hash()
        next
      }
    }
    
    # read data from ngram section
    if(grepl("[[:digit:]]+", state, perl=TRUE)) { # read ngram data
      
      n <- as.numeric(state)
      
      # "\N-grams:" line - end of present section, start new section
      if(grepl(data.rex,line,perl=TRUE)) {
        result <- regexpr(data.rex, line, perl=TRUE)
        st <- attr(result, "capture.start")[1,]
        len <- attr(result, "capture.length")[1,]
        n<-substring(line,st[1],st[1]+len[1]-1)  # put this in a list
        state <- n
        arpa[[paste0('logp',state)]] <- hash()
        arpa[[paste0('bow',state)]] <- hash()
        next
      }
      
      # read a data from "p w1 w2 ... [bow]" line, implemented only for ngrams without bow
      tokens <- unlist(strsplit(line," "))
      if(length(tokens) < 2) {
        print("Syntax error in ")
        print(line)
        break
      }
      ng <- paste(tokens[2:(2+n-1)],collapse=" ")
      arpa[[paste0('logp',state)]][[ng]] <- as.double(tokens[1])
      if(length(tokens) == n+2)
        arpa[[paste0('bow',state)]][[ng]] <- as.double(tokens[length(tokens)])
      next
    }
    
    print("Syntax error in:")
    print(line)
    break
  }
  close(con)
  
  return(arpa)
}

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

countfilename <- function(data,N=3,K=3) paste0("count.en_US.", data,".N",N,".K",K,".RData")

# traincontrol is list(N=,threshold=,debug=)
# works for corpora up to 10 million words of 8Gbyte MacPro
#' Create a Ngram Language mode
#' 
#' @param path     : path to the data directory where text corpus is 
#' @param data     : corpus size (small, medium, large, training)
#' @param N        : order N from NGram
#' @param metod    : default Katz
#' @param threshold : frequency thresholding, counts < threshold are discarded
#' @param debug    :  boolean flag, if TRUE keeps intermediate data (counts, discounts etc) 
#' @return an NGramLM S3 object 
#'  
NGramLM <- function(path=".", data, N=2, method="Katz", threshold=0, debug=FALSE) {
 
  model <- list(N = 0, control = list(N=N, data=data, method=method, threshold=threshold, debug=debug) )
  class(model) <- append(class(model),"NGramLM")
  
  # does model exits ? then load 
  # do counts exist ? then use them to train
  # otherwise count and train

  modelFile <- file.path(file.path(path,"models"), modelfilename(data,method,N,threshold,debug))
  countFile <- file.path(file.path(path,"counts"), countfilename(data,N,threshold))
  
  if(file.exists(modelFile)) load(file=modelFile) 
  else if(!file.exists(countFile)) {  
      corpus <- VCorpus(DirSource(file.path(file.path(file.path(path,"data"),data),"en_US")))
      
      cleanctrl <-list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                       content_transformer(tolower),
                       removePunctuation,
                       removeNumbers,
                       stripWhitespace)
      
      corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns= cleanctrl)
      
      # counting Ngrams, frequency filter with threshold and good turing discount calc
      for( n in 1:N ) {
        cat(paste0("Counting ",n,"-grams...\n"))
        tokctrl<- list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = n, max = n)), # count
                       wordLengths=c(1, Inf))
        cnt <- row_sums(TermDocumentMatrix(corpus, control=tokctrl))    
        if (threshold>0) cnt <- cnt[cnt > threshold]
        if(length(cnt)>0) { 
          model[[paste0(n,"-grams")]] <- hash(cnt) 
          model$N <- n
        }                    
      }
  } else cat("loading from counts not implemented yet\n")

  # construct word-trie in hash: all prefixes of word point to word 
  cat("Construct word trie for", getNumberNGrams(model,1), "words...\n")
  trie <- hash()
  for( word in keys(model[["1-grams"]])) {
     if( nchar(word) < 2 ) next
     prefixes <- sapply(2:nchar(word)-1, function(n) substr(word,1,n)) #all prefixes of word
     for( prefix in prefixes) trie[[prefix]] <- append(trie[[ prefix]], word)  
  }
  model[["trie"]] <- trie
  
  # Good-turing discounting
  POOV <- GoodTuringOOV(values(model[["1-grams"]]),threshold)
  for( n in 1:model$N ) 
     model[[paste0("disc",n)]] <- hash(GoodTuringDisc(values(model[[paste0(n,"-grams")]])))
  
  # Katz back-off probability calculation
  cat("Calculating", getNumberNGrams(model,1), "unigram probabilities...\n")
  V <- sum(values(model[["1-grams"]]))
  ngkeys <- keys(model[["1-grams"]])
  model[["p1"]] <- hash(Map(function (cnt,d) d * cnt / V, 
                              values(model[["1-grams"]],keys=ngkeys), 
                              values(model[["disc1"]],keys=ngkeys)))
  model[["p1"]][["<UNK>"]] <- POOV
  
  if(model$N>1) {
    # hash table with followers (n-1)gram + word -> ngram followers
    followers <- hash()
    
    # add (n-1)gram followers to hastable:  (n-1)gram + word --> ngram 
    for(n in 2:model$N) {
      cat(paste0("Calculating ",getNumberNGrams(model,n)," ",n,"-gram sets...\n")) 
      for( ng in keys(model[[paste0(n,"-grams")]])) {
        key <- ngprefix(ng)
        followers[[ key ]] <- append(followers[[ key ]], ng)  
      }
      
      cat(paste0("Calculating ",getNumberNGrams(model,n)," ",n,"-gram probabilities...\n"))
      ngkeys <- keys(model[[paste0(n,"-grams")]])
      model[[paste0("p",n)]] <- hash(Map(function (ng,cnt,d) d * cnt / model[[paste0(n-1,"-grams")]][[ ngprefix(ng) ]],
                                         ngkeys, 
                                         values(model[[paste0(n,"-grams")]],keys=ngkeys),
                                         values(model[[paste0("disc",n)]],keys=ngkeys))) 
      
      cat(paste0("Calculating ",getNumberNGrams(model,n-1)," ",n-1,"-gram back off weights...\n"))
      model[[paste0("alpha",n-1)]] <- hash(Map(function (start) ifelse(!has.key(start, followers), 
                                                                       1, # no following trigrams
                                                                       (1 - sum(values(model[[paste0("p",n)]], keys=followers[[start]]))) /
                                                                         (1 - sum(values(model[[paste0("p",n-1)]], keys=sapply(followers[[start]], ngsuffix))))),
                                               keys(model[[paste0(n-1,"-grams")]]) ))
      
      
    }
    model$followers <- followers # keep the followers to speed up prediction
  }

  # keep training summary
  model$stats<- data.frame(N = 1:model$N, 
                           NGrams = sapply(1:model$N,function(n) getNumberNGrams(model,n)),
                           Counts = sapply(1:model$N,function(n) getNGramCount(model,n)))
  
  # clear counts and discounts,  we keep them for debugging
  if(!debug) 
     for (h in grep("grams|disc",names(model),value=TRUE)) {
       hash::clear(model[[h]])
       model[[h]] <- NULL  
     }
  
  # calculate log probs
  for( n in 1:model$N ) {
    cat(paste0("Calculate log probs for ",n,"-grams and back off weights...\n"))
    model[[paste0("logp",n)]] <- hash(keys(model[[paste0("p",n)]]),log10(values(model[[paste0("p",n)]])))
    if(n<model$N) model[[paste0("bow",n)]] <- hash(keys(model[[paste0("alpha",n)]]),log10(values(model[[paste0("alpha",n)]])))
  }

  # keep top-20 unigrams (but not <UNK> TODO)
  model[["top20"]] <- head(sort(values(model[["logp1"]]),decreasing=TRUE),20)
  
# clear probs and alpha,  we keep them for debugging
  if(!debug) 
    for (h in grep("^p|alpha",names(model),value=TRUE)) {
      hash::clear(model[[h]])
      model[[h]] <- NULL  
    }
  return(model)
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

# get all the bigrams starting with a word, work on a list of words (obsolete)
#get2grams <- function(model,startlist) UseMethod("get2grams",model)
#get2grams.NGramLM <- function(model,startlist) 
#  lapply(startlist, function(start) grep(paste0("^",start,"[[:space:]]+"),
#                                         keys(model[["2-grams"]]),value=TRUE))

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

#' Perplexity of a token sequence with NGram model
#' 
#' @param model   : an NGramLM model
#' @param tokens  : character vector with words  c("w1", "w2", ... )
#' @return Perplexity of the word sequence ie. geometric mean of the 1/probabilities of the individual words in context

Perplexity <- function(model, tokens, N) UseMethod("Perplexity",model)
Perplexity.NGramLM <- function (model,tokens, N=model$N) 
  10^(-sum(mapply(function(w,history) LMScore.NGramLM(model, history, w),
                  tokens,
                  lapply(1:length(tokens),function(n) tail(head(tokens,n-1),N-1)) ))
      / length(tokens))

#' Perplexity on a Tokenized Corpus using a NGram model
#' 
#' @param model   : an NGramLM model
#' @param tokenCorp  : a tokenized tm Corpus, tokenized 
#' @param N      : Order to be used for LM 
#' @return Perplexity of the corpus on the model
#' 
#' 

CorpusPerplexity  <- function(model, corpus, N)  UseMethod("CorpusPerplexity",model)
CorpusPerplexity.NGramLM  <- function(model, tokenCorp, N)
  exp(weighted.mean(log(sapply(tokenCorp, function(tokens) Perplexity(model,tokens,N))), 
                    sapply(tokenCorp,length))) 

#' Perplexity on a Corpus using a NGram model
#' 
#' @param model   : an NGramLM model
#' @param tokens  : character vector with words  c("w1", "w2", ... )
#' @return Perplexity of all word sequences in a corpus 

CorpusPerplexity  <- function(models, corpus, N) {
  mergedcorp <- VCorpus(VectorSource(unlist(lapply(corpus,content))))  # merge corpus so that every line in all docs is seperate document
  cleanctrl <-list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                   content_transformer(tolower),
                   removePunctuation,
                   removeNumbers,
                   stripWhitespace)
  mergedcorp <- tm_map(mergedcorp, FUN = tm_reduce, tmFuns= cleanctrl)
  tokenCorp <- tm_map(mergedcorp, RWeka::WordTokenizer)                # tokenize the corpus: each doc is now a token vector
  tokenCorp <- tm_filter(tokenCorp, function(x) (length(x) > 0))       # remove zero length token strings
  if(inherits(models,"NGramLM")) CorpusPerplexity.NGramLM(models, tokenCorp, N) 
  else sapply(models, CorpusPerplexity.NGramLM, tokenCorp, N)
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



# LM training receipe with controls
LMTrainingReceipe <- function(control, path=".") {
  modelFile <- file.path(file.path(path,"models"),modelfilename(control$data,control$method,control$N,control$threshold,control$debug))
  if(!file.exists(modelFile)) {
    mod <- NGramLM(path, control$data, control$N, control$method, control$threshold, control$debug)
    save(mod,file=modelFile)
  } else load(file=modelFile)
  return(mod)
}

if(FALSE) {
#  testing LM functionality

setwd("~/Documents/Courses/DataScience/CapStone")

mod <- NGramLM(path=".", data="micro", N=1, method="Katz", threshold=0,debug=TRUE)
summary(mod)
LMbest.NGramLM(mod, character(0))
LMScore(mod, character(0), c("<UNK>"))
LMScore(mod, character(0), c("the"))
LMScore(mod, "",c("the"))
LMScore(mod, "is",c("the"))
LMScore(mod, "<UNK>",c("the"))
LMScore(mod, "",c("test","shape","<UNK>"))
head(sort(LMScore(mod, "",Voc(mod)),decreasing = TRUE))

head(predict(mod,"this is a"))
head(predict(mod,"this is a",prefix="t"))
head(predict(mod,"this is a",prefix="te"))
head(predict(mod,"this is a",prefix="tes"))
clear(mod);rm(mod)

mod <- NGramLM(data="micro", N=2, method="Katz", threshold=0,debug=TRUE)
summary(mod)
LMbest.NGramLM(mod, character(0))
LMbest.NGramLM(mod, c("a"))
LMbest.NGramLM(mod, c("a"), prefix="t")
LMbest.NGramLM(mod, c("a"), prefix="ta")
LMbest.NGramLM(mod, c("a"), prefix="taxghsdhfg")
LMScore(mod, character(0), c("<UNK>"))
LMScore(mod, character(0), c("the"))
LMbest.NGramLM(mod, character(0),prefix="t")
LMScore(mod, "",c("the"))
LMScore(mod, "",c("test","shape","<UNK>"))
head(sort(LMScore(mod, "",Voc(mod)),decreasing = TRUE),20)
#LMbest.NGramLM(mod, "is")
LMScore(mod, "is",c("the")) 
LMScore(mod, "<UNK>",c("the")) # bUG wrong word name
LMScore(mod, "<UNK>",c("the","is"))
LMScore(mod, "is", c("it","shape","phone","<UNK>")) 
head(sort(LMScore(mod,"is",Voc(mod)),decreasing = TRUE),20)
sort(LMScore(mod, "is", nglast(mod$followers[["is"]])),decreasing = TRUE)

head(predict(mod,"This is Vinton"))
head(predict(mod,"This is Vinton",prefix="c"))
clear(mod);rm(mod)

mod <- NGramLM(path="./", data="micro", N=3, method="Katz", threshold=0,debug=TRUE)
summary(mod)
LMbest.NGramLM(mod, character(0))
LMbest.NGramLM(mod, c("is","a"))
LMbest.NGramLM(mod, c("is","a"), prefix="t")
LMbest.NGramLM(mod, c("is","a"), prefix="ta")
LMbest.NGramLM(mod, c("is","a"), prefix="taxghsdhfg")
LMScore(mod, character(0), c("the"))
LMScore(mod, "",c("the"))
LMScore(mod, "",c("test","shape","<UNK>"))
head(sort(LMScore(mod, "",Voc(mod)),decreasing = TRUE),20)
LMScore(mod, "is",c("the"))
LMScore(mod, "<UNK>",c("the")) # bUG wrong word name
LMScore(mod, "<UNK>",c("the","is"))
LMScore(mod, "is", c("it","shape","phone","<UNK>"))
sort(LMScore(mod, "is", nglast(mod$followers[["is"]])),decreasing = TRUE)
head(sort(LMScore(mod,"is",Voc(mod)),decreasing = TRUE),20)
LMScore(mod, c("is","a"),c("shape","test","<UNK>"))
sort(LMScore(mod, c("in","a"), nglast(mod$followers[["in a"]])),decreasing = TRUE)
sort(LMScore(mod, c("a"), nglast(mod$followers[["in a"]])),decreasing = TRUE)
sort(LMScore(mod, "", nglast(mod$followers[["in a"]])),decreasing = TRUE)
head(sort(LMScore(mod,c("in","a"),Voc(mod)),decreasing = TRUE),20)
head(predict(mod,"this is vinton"))
head(predict(mod,"This is Vinton",prefix="c"))

# load evaluation corpus, and clean it and tokenize
corpus <- VCorpus(DirSource(file.path(file.path("data","eval"),"en_US")))
set.seed(123)
microeval <- tm_map(corpus, subsample.TextDocument, 1/1000) #subsample
Perplexity(mod,c("this", "is", "vinton"),1)
Perplexity(mod,c("this", "is", "vinton"),2)
Perplexity(mod,c("this", "is", "vinton"),3)
CorpusPerplexity(mod, microeval, 1)
CorpusPerplexity(mod, microeval, 2)
CorpusPerplexity(mod, microeval, 3)
clear(mod);rm(mod)
}
