# exploratory analysis script for Capstone task 1

# exploring tokenization on small subset for English


setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
library(rJava)
library(RWeka)
library(openNLP)

# read an english corpus
#eng <- VCorpus(DirSource("data/final/en_US/"))
eng <- VCorpus(DirSource("data/small/en_US/"))
#eng <- VCorpus(DirSource("data/micro/en_US/"))

Corpus.summary <- function(corpus) as.data.frame(t(sapply(corpus, 
                                                          function(x) { ellen <- nchar(content(x))
                                                                        wordlist <- words(x)  # from NLP package
                                                                        c(TextDocument=meta(x,"id"),
                                                                          NChars=sum(ellen), NElements=length(content(x)),
                                                                          MedianElement=median(ellen), MaxElement=max(ellen),
                                                                          NWords2=length(wordlist), NVoc=length(unique(wordlist)) 
                                                                         )} )))
# summary statistics
Corpus.summary(eng)

# a simple white space tokenizer
tokens <- tm_map(eng, content_transformer(function(x) unlist(strsplit(x, "[[:space:]]+"))))
head(content(tokens[[1]]),72)
strsplit(head(content(eng[[1]]),10),"[[:space:]]+")
# tons of issues: line bounderies are lost, punctuation/capitalization not dealth with etc, good to get rough idea of number of tokens

# tm build in tokenizer: "scan_tokenizer" ==> same as whitespace tokenizer
scan_tokenizer(content(eng[[1]])[1:3])
# essentially the same as the white space tokenizer
tokens2 <- tm_map(eng, content_transformer(function(x) scan_tokenizer(x)))
head(content(tokens2[[1]]),72)

# tm build in tokenizer: "MC_tokenizer"
MC_tokenizer(content(eng[[1]])[1:10])
tokens3 <- tm_map(eng, content_transformer(MC_tokenizer))
head(content(tokens3[[1]]),87)
head(MC_tokenizer(content(eng[[1]])[1:1000]),100)
# strange behavior, adds spurious tokens to beginning when text is too long

# wordpunct_tokenizer from package NLP. returns list of span objects used to retreive words ++. good trade off speed and accuracy
gg_tokenizer <- function(f) lapply(f, function(s) { temp <- as.String(s); temp[wordpunct_tokenizer(temp)] })
tokens <- gg_tokenizer(content(eng[[1]])) # returns token character vector
tokens[[1]]  # tokens for first entity
tokens[[2]]  # tokens for sencond entity
head(unlist(tokens),75) # all tokens vector
# now tokenize the small corpus
tokens4 <- tm_map(eng, content_transformer(function(x) unlist(gg_tokenizer(x))))
str(tokens4) # corpus with tokens
head(content(tokens4[[1]]),75)
head(content(tokens4[[2]]),75)
head(content(tokens4[[3]]),75)

# use the tokenizers from R/Weka package

AlphabeticTokenizer(eng[[1]])
WordTokenizer(eng[[1]])
NGramTokenizer(eng[[1]], Weka_control(min = 1, max = 1)) #same as wordTokenizer
NGramTokenizer(eng[[1]], Weka_control(min = 2, max = 2)) #bigrams

# use the tokenizers from Apache openNLP tools interface  package


# first annotate on all elements in the file separately ==> quite slow 
gg_tokenizer2 <- function(f) {
  pipeline <- list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator() )
  lapply(f, function(s) { s.temp <- as.String(s); 
                          a.temp <- annotate(s.temp, pipeline)
                          s.temp[a.temp[a.temp$type == "word"]]})}

tokens <- gg_tokenizer2(content(eng[[1]])) # returns token character vector
tokens[[1]]  # tokens for first entity
tokens[[2]]  # tokens for second entity
head(unlist(tokens),75) # all tokens vector
# now tokenize the small corpus ==> BUG!!!
#tokens5 <- tm_map(eng, content_transformer(function(x) unlist(gg_tokenizer2(x))))
lapply(eng, function(x) unlist(gg_tokenizer2(content(x))))
ptd <- PlainTextDocument(unlist(gg_tokenizer2(content(eng[[1]]))), description="tokens", id=meta(eng[[1]],"id"))
tokens5<- lapply(eng, function(x)  PlainTextDocument(unlist(gg_tokenizer2(content(x))),description="tokens"))

str(tokens5) # corpus with tokens
head(content(tokens5[[1]]),75)
head(content(tokens5[[2]]),75)
head(content(tokens5[[3]]),75)

# append all elements and then tokenize, take even longer
gg_tokenizer3 <- function(s) { s.temp <- as.String(s); 
                               a.temp <- annotate(s.temp,list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator(language = "en")))
                               s.temp[a.temp[a.temp$type == "word"]]}

tokens <- gg_tokenizer3(content(eng[[1]])) # returns token character vector
head(tokens,75) # all tokens vector

# compare tokenizers CPU vs quality
# 
eng <- VCorpus(DirSource("data/small/en_US/"))
Corpus.summary(eng) # roughly a bit more than million words

Func <- list("strsplit"= function(x) unlist(strsplit(x, "[[:space:]]+")), 
#             "tm::scan_tokenizer"=scan_tokenizer, 
             "tm::MC_tokenizer"=MC_tokenizer,
             "RWeka::WordTokenizer"=WordTokenizer,
             "NLP::wordpunct_tokenizer"=function(x) unlist(gg_tokenizer(x)),
             "OpenNLP::Maxent_Word_Token_Annotator"=function(x) unlist(gg_tokenizer2(x)))  ## BUG with this

lapply(Func, function(f) f("This is a test."))
lapply(Func, function(f) f(content(eng[[1]])[2]))
lapply(Func, function(f) f(content(eng[[1]])[5]))
lapply(Func, function(f) f(content(eng[[1]])[6]))
lapply(Func, function(f) f(content(eng[[1]])[7]))
lapply(Func, function(f) f(content(eng[[1]])[8]))

lapply(Func, function(f) f(content(eng[[1]])[1:8]))
lapply(Func, function(f) f(content(eng[[1]])))
lapply(Func,function(f) system.time(f(content(eng[[1]])))["elapsed"])

# benchmarking
tab <- data.frame( Name=names(Func),row.names = NULL,
                   Time=unlist(lapply(Func,function(f) system.time(tm_map(eng, content_transformer(f)))["elapsed"])))


print(tab)






