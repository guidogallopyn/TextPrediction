# Capstone Milestone report

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
#library(openNLP) interferes with RWeka

library(plyr)
library(dplyr)
library(xtable)
library(wordcloud)

#
# produce US English dataset summary table 
#
corpus <- VCorpus(DirSource("data/final/en_US/"))
#corpus <- VCorpus(DirSource("data/micro/en_US/")) for summary function development

Corpus.summary <- function(corpus) {
  d <- ldply(lapply(corpus, function(x) { ellen <- nchar(content(x))
                                          wordlist <- lapply(content(x), RWeka::WordTokenizer)
                                          tokens <- unlist(wordlist)
                                          data.frame(TextDocument=meta(x,"id"), NLines=length(content(x)),
                                                     NWords=length(tokens), 
                                                     NChars=sum(ellen), 
                                                     AvgLineLen=round(mean(ellen)),
                                                     AvgWordsLine=round(mean(sapply(wordlist,length)),1),
                                                     NVoc=length(unique(tokens)),
                                                     AvgWordLen=round(mean(nchar(tokens)),2)) }))
  big <-Reduce(c,Map(content,corpus))
  ellen <- nchar(big)
  wordlist <- lapply(big, RWeka::WordTokenizer)
  tokens <- unlist(wordlist)
  rbind(d,data.frame(TextDocument="Total Corpus", NLines=length(big),
                     NWords=length(tokens), NChars=sum(ellen), 
                     AvgLineLen=round(mean(ellen)),
                     AvgWordsLine=round(mean(sapply(wordlist,length)),1),
                     NVoc=length(unique(tokens)),
                     AvgWordLen=round(mean(nchar(tokens)),2)))
}


(tab<-Corpus.summary(corpus))
save(tab,file="MilestoneReport/tab1.RData")
rm(corpus,tab)



#
# produce US English dataset summary table 
#
corpus <- VCorpus(DirSource("data/small/en_US/"))
(tab<-Corpus.summary(corpus))
save(tab,file="MilestoneReport/tab2.RData")
rm(corpus,tab)


#
# Exploring tokenization on small subset for English
#
corpus <- VCorpus(DirSource("data/small/en_US/"))

gg_tokenizer <- function(f) lapply(f, function(s) { temp <- as.String(s); temp[wordpunct_tokenizer(temp)] })

gg_tokenizer2 <- function(f) {
  pipeline <- list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator() )
  lapply(f, function(s) { s.temp <- as.String(s); 
                          a.temp <- annotate(s.temp, pipeline)
                          s.temp[a.temp[a.temp$type == "word"]]})}

Tokenizers <- list("strsplit" = function(x) unlist(strsplit(x, "[[:space:]]+")), 
                   "tm::MC_tokenizer" = MC_tokenizer,
                   "RWeka::WordTokenizer" = RWeka::WordTokenizer,
                   "NLP::wordpunct_tokenizer"=function(x) unlist(gg_tokenizer(x)),
                   "openNLP::Maxent_Word_Token_Annotator" = function(x) unlist(gg_tokenizer2(x)))  # BUG

lapply(Tokenizers, function(f) f(content(corpus[[3]])[19]))

# benchmarking on 1 million words
small <- tm_map(corpus , content_transformer(function(x) x[sample(length(x),length(x)/10)])) 
tab <- data.frame( Name=names(Tokenizers), row.names = NULL,
                   Time=unlist(lapply(Tokenizers,function(f) system.time(tm_map(corpus, content_transformer(f)))["elapsed"])))
tab[5,2] <- 10* (system.time(gg_tokenizer2(content(small[[1]])))["elapsed"]+
                      system.time(gg_tokenizer2(content(small[[2]])))["elapsed"]+
                      system.time(gg_tokenizer2(content(small[[3]])))["elapsed"]) # workaround
print(tab)
save(tab,file="MilestoneReport/tab3.RData")
rm(corpus,small,tab)
detach("package:openNLP", unload=TRUE)












