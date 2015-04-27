# exploring perplexity metric

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(ggplot2)
library(plyr)
library(dplyr)
library(slam)
library(hash)

options(mc.cores=1) # RWeka bug workaround 


source("NGramLM.R")

# training ngram model
corpus <- VCorpus(DirSource("data/micro/en_US/"))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

# only for unigrams
mod <- NGramLM(corpus, N=1, threshold=0,debug=TRUE)
str(mod)
CorpusPerplexity(mod, corpus)

# testing: correctness of formula confimed
mergedcorp <- VCorpus(VectorSource(unlist(lapply(corpus,content))))  # merge corpus so that every line in all docs is seperate document
tokenCorp <- tm_map(mergedcorp, RWeka::WordTokenizer)                # tokenize the corpus: each doc is now a token vector
tokenCorp <- tm_filter(tokenCorp, function(x) (length(x) > 0)) 

tokenCorp[[14]]
1/mod[["Pbo1"]][["q"]]
1/mod[["Pbo1"]][["are"]]
1/mod[["Pbo1"]][["there"]]
1/mod[["Pbo1"]][["new"]]
1/mod[["Pbo1"]][["terms"]]
1/mod[["Pbo1"]][["to"]]
1/mod[["Pbo1"]][["understand"]]

(mod[["Pbo1"]][["q"]] *
 mod[["Pbo1"]][["are"]] *
 mod[["Pbo1"]][["there"]] *
 mod[["Pbo1"]][["new"]] *
 mod[["Pbo1"]][["terms"]] *
 mod[["Pbo1"]][["to"]] *
 mod[["Pbo1"]][["understand"]])^(-1/7)
Perplexity(mod,tokenCorp[[14]])

#test for OOV
1/mod[["Pbo1"]][["<OOV>"]]
1/KatzPbo.NGramLM(mod, NULL, "Guido")
1/KatzPbo.NGramLM(mod, c("my","name","is"), "Guido")
#clear(mod)

#now for bigrams, how to efficiently construct history
mod <- NGramLM(corpus, N=2, threshold=0,debug=TRUE)
Perplexity(mod,tokenCorp[[14]]) # much lower perplexity
1/KatzPbo.NGramLM(mod, NULL, "q")
1/KatzPbo.NGramLM(mod, "q", "are")
1/KatzPbo.NGramLM(mod, "are", "there")
1/KatzPbo.NGramLM(mod, "there","new")
1/KatzPbo.NGramLM(mod, "new","terms")
1/KatzPbo.NGramLM(mod, "terms","to")
1/KatzPbo.NGramLM(mod, "to","understand")
mod[["2-grams"]][["to understand"]]
1/mod[["Pbo2"]][["to understand"]]
grep("^to ",keys(mod[["2-grams"]]),value=TRUE)

inspect(tokenCorp[1:3])
lapply(tokenCorp, function(tokens) Perplexity(mod,tokens))

CorpusPerplexity(mod, corpus)

#now for trigrams, how to efficiently construct history
mod <- NGramLM(corpus, N=3, threshold=0,debug=FALSE)
CorpusPerplexity(mod, corpus)# perplexity goes up from N=2, something not right


# now for the 1 million word corpus
corpus <- VCorpus(DirSource("data/small/en_US/"))
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_dashes = TRUE)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

mod <- NGramLM(corpus, N=1, threshold=0)
CorpusPerplexity(mod, corpus)

mod <- NGramLM(corpus, N=2, threshold=0)
CorpusPerplexity(mod, corpus)

mod <- NGramLM(corpus, N=3, threshold=0)
CorpusPerplexity(mod, corpus)

mod <- NGramLM(corpus, N=4, threshold=0)
CorpusPerplexity(mod, corpus)





