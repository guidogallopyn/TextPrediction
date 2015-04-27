
# debugging counting NGramLM class



library(hash)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

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
        arpa[[paste0('p',state)]] <- hash()
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
        arpa[[paste0('p',state)]] <- hash()
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
      arpa[[paste0('p',state)]][[ng]] <- as.double(tokens[1])
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

# testing read an ARPA format LM
setwd("~/Documents/Courses/DataScience/CapStone")
mypath <- "python/smallBigram.LM"
start.time <- Sys.time()
small <- readARPA(mypath)
Sys.time() - start.time
str(small)
small$ngrams
length(small[['p1']])
length(small[['bow1']])
length(small[['p2']])
length(small[['bow2']])

source("NGramLM.R")
mypath <- "python/microUnigram.LM"
start.time <- Sys.time()
micro <- loadNGramLM(mypath)
Sys.time() - start.time
str(micro)
micro$ngrams
length(micro[['p1']])
length(micro[['bow1']])
length(micro[['p2']])

small <- VCorpus(DirSource("data/small/en_US/"))
small <- tm_map(small, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
small <- tm_map(small, content_transformer(tolower))
small <- tm_map(small, removePunctuation, preserve_intra_word_dashes = TRUE)
small <- tm_map(small, removeNumbers)
small <- tm_map(small, stripWhitespace)
mod2 <- trainNGramLM(small,N=2,threshold=0,debug=TRUE)
str(mod2)

