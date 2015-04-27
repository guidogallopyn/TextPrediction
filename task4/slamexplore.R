setwd("~/Documents/Courses/DataScience/CapStone")
options( java.parameters = "-Xmx4g" ) # more jave heap space for RWeka see http://www.bramschoenmakers.nl/en/node/726
options(mc.cores=1) # RWeka bug workaround 
library(rJava)
library(tm)


corpus <- VCorpus(DirSource("data/micro/en_US/"))

cleanctrl <-list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                 content_transformer(tolower),
                 removePunctuation,
                 removeNumbers,
                 stripWhitespace)

tokctrl<- list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 4)), # count unigrams 
               wordLengths=c(1, Inf))

corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns= cleanctrl)            # clean fold
tdm <- TermDocumentMatrix(corpus, control=tokctrl)               # ngram count fold

ngc <- row_sums(tdm[,1])

tdm <- tdm[,1]+tdm[,2]+tdm[,3]

# this is how to collapse a term document matrix
Reduce("+", Map(function(doc) tdm[,doc],Docs(tdm)))

row_sums(ngc2)
row_sums(ngc2) - row_sums(tdm[,1])


head(sort(ngramcounts,decreasing=TRUE),100)


