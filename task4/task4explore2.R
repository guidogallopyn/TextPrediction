# exploring storing, cleaning and basic tokenization and counting on very lorge corpora 

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
library(slam)
library(hash)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka

#options(mc.cores=1) # RWeka bug workaround 


#source("NGramLM.R")

# loading 100 million word corpus, cleaning and counting unigrams with tdm fails on Mac with 8 Mb (so do not run this code)
if(FALSE) {
start.time <- Sys.time()
corpus <- VCorpus(DirSource("data/final/en_US/"))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
tdm <- TermDocumentMatrix(corpus)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
}

#what about PCorpus? works, created a db file on hdisk
#install.packages("filehash")
corpus <- VCorpus(DirSource("data/micro/en_US/"))
inspect(corpus)
meta(corpus)
meta(corpus[[1]])
(tdm <- TermDocumentMatrix(corpus))
nTerms(tdm)

library(filehash)
corpus <- PCorpus(DirSource("data/micro/en_US/"),dbControl = list(dbName = "db/enUSmicro.db"))
inspect(corpus)
meta(corpus)
meta(corpus[[1]])
(tdm <- TermDocumentMatrix(corpus))
nTerms(tdm)

# loading, cleaning and counting small corpus with Pcorpus, compare with 
cleaning <- function(corpus) {
  corpus<-tm_map(corpus, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
  corpus<-tm_map(corpus, content_transformer(tolower))
  corpus<-tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  corpus<-tm_map(corpus, removeNumbers)
  tm_map(corpus, stripWhitespace)
}  
corpus <- VCorpus(DirSource("data/micro/en_US/"))
inspect(corpus)
meta(corpus)
meta(corpus[[1]])
clean<-cleaning(corpus)
inspect(corpus)
inspect(clean)
meta(clean)
meta(clean[[1]])
(tdm <- TermDocumentMatrix(clean))
nTerms(tdm)

# caveate is that tm_map chnages originalpCorpus, not VCorpus
corpus <- PCorpus(DirSource("data/micro/en_US/"),dbControl = list(dbName = "db/enUSmicro.db"))
inspect(corpus)
meta(corpus)
meta(corpus[[1]])
clean<-cleaning(corpus)
inspect(corpus)
inspect(clean)
meta(clean)
meta(clean[[1]])
(tdm <- TermDocumentMatrix(clean))
nTerms(tdm)



# maybe better loading, cleaning and counting small corpus with Pcorpus, compare with 
cleaning2 <- function(corpus) {
 tm_map(corpus, FUN = tm_reduce, tmFuns= list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                                              content_transformer(tolower),
                                              removePunctuation,
                                              removeNumbers,
                                              stripWhitespace))
} 

corpus <- PCorpus(DirSource("data/micro/en_US/"),dbControl = list(dbName = "db/enUSmicro.db"))
#corpus <- PCorpus(DirSource("data/large/en_US/"),dbControl = list(dbName = "db/enUSlarge.db"))
inspect(corpus)
meta(corpus)
meta(corpus[[1]])
clean<-cleaning2(corpus)
inspect(corpus)
inspect(clean)
meta(clean)
meta(clean[[1]])
meta(corpus)
meta(corpus[[1]])
(tdm <- TermDocumentMatrix(clean))
nTerms(tdm)


measure <- function (name, permanent=FALSE) {    
  start.time <- Sys.time()
  if (permanent) corpus <- PCorpus(DirSource(paste0("data/",name,"/en_US/")), dbControl = list(dbName = paste0("db/enUS",name,".db")))
  else corpus <- VCorpus(DirSource(paste0("data/",name,"/en_US/")))  
  loadtime <- Sys.time() - start.time
  start.time <- Sys.time()
  corpus <- cleaning2(corpus)
  cleaningtime<- Sys.time() - start.time
  start.time <- Sys.time()
  n<-sum(slam::row_sums(TermDocumentMatrix(corpus, control=list(tokenize = RWeka::WordTokenizer, wordLengths=c(1, Inf)))))
  countingtime <- Sys.time() - start.time
  data.frame(corpus=name,nWords=n, perm=permanent, LoadTime=loadtime, CleaningTime=cleaningtime,CountingTime=countingtime)
}
measure("micro",permanent=FALSE)
measure("micro",permanent=TRUE)

# loading, cleaning and counting small corpus with V corpus
(df<-measure("small", FALSE))
(df<-rbind(df, measure("small",TRUE)))
(df<-rbind(df, measure("medium",FALSE)))
(df<-rbind(df, measure("medium",TRUE)))
(df<-rbind(df, measure("large",FALSE)))
(df<-rbind(df, measure("large",TRUE)))
#(df<-rbind(df, measure("final",TRUE))) takes too long
df


if(FALSE) { # this takes very long
  start.time <- Sys.time()
  corpus <- VCorpus(DirSource(paste0("data/final/en_US/")))  
  loadtime <- Sys.time() - start.time
  df<-rbind(df,data.frame(corpus="final",nWords=df$nWords[7], perm=FALSE, LoadTime=10*3600, CleaningTime=NA,CountingTime=NA))
  df
}

save(df,file="task4/tab2.RData")

summary(lm(log(as.numeric(LoadTime)) ~ log(nWords) * factor(perm), data=df ))
summary(lm(CleaningTime ~ nWords * factor(perm), data=df ))
summary(lm(CountingTime ~ nWords * factor(perm), data=df ))

# new strategy: read, process and count line by line (no reading of entire corpus)
ctrl<- list(tolower=TRUE,
            removePunctuation=list(preserve_intra_word_dashes = TRUE),
            removeNumbers=TRUE,
            tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 3)), # count unigrams 
            wordLengths=c(1, Inf))

mypath <- "data/small/en_US/"

start.time <- Sys.time()

for (doc in list.files(mypath)) {
  con <- file(paste0(mypath,doc), "r")
  n<-1
  freq<-hash()
  while(TRUE) {
    line <- readLines(con,1)  # read 1 line
    if(length(line)==0) break                # EOF
    tf <- termFreq(PlainTextDocument(line), control=ctrl) # clean and count with tm
    if(length(tf)==0) next
    f <- tf[1:length(tf)]  
    si <- intersect(names(freq),names(f))   # words alreday seen in prev lines 
    sd <- setdiff(names(f),names(freq))     # new words
    if(length(si)>0) freq[si] <- values(freq[si]) + f[si]
    if(length(sd)>0) freq[sd] <- f[sd]
    n<-n+1
    cat(".")
  }   
  close(con)
  cat(paste(doc,"lines processed",n))  
  save(freq,file=paste0("counts/ngram",doc,".Rdata"))
}

(totalTime <- Sys.time() - start.time)

head(freq) 

unigrams<- sapply(strsplit(names(freq)," "),length) ==1
bigrams<- sapply(strsplit(names(freq)," "),length) ==2
head(sort(freq[unigrams],decreasing=TRUE),12)
head(sort(freq[bigrams],decreasing=TRUE),12)



