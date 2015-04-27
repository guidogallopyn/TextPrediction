
# split Capstone corpus in training, eval and test set and store 
# subsample the training set in smaller pieces
# 1% small
# 3% medium
# 10% large 


setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)

#corpus <- PCorpus(DirSource("data/final/en_US/"),dbControl = list(dbName = "db/enUSfinal.db"))
corpus <- VCorpus(DirSource("data/final/en_US/"))


sample.fraction <- function(len, frac)  sample(len,frac*len)

subsample.TextDocument <- function(doc,frac) 
  PlainTextDocument( sample(content(doc), frac * length(content(doc))), 
                     id = meta(doc,"id") )

split.Corpus <- function(corpus,frac) {
  corpus<- tm_map(corpus, function(x) { meta(x,"sample") <- sample.fraction(length(content(x)), frac); x })
  split<- list(tm_map(corpus, function(x) PlainTextDocument(content(x)[ meta(x,"sample") ], id=meta(x,"id"))),
               tm_map(corpus, function(x) PlainTextDocument(content(x)[ -meta(x,"sample") ], id=meta(x,"id") )))
  return(split)
}


set.seed(123)
split <- list()

# subsample test corpus
split[c("testing","rest")] <- split.Corpus(corpus,1/20)

# store the corpus
dir.create(file.path("data", "testing"), showWarnings = FALSE)
dir.create(file.path("data/testing", "en_US"), showWarnings = FALSE)
writeCorpus(split$testing, path = "data/testing/en_US/")

# subsample evaluation corpus
split[c("evaluation","training")] <- split.Corpus(split$rest,1/10)

dir.create(file.path("data", "eval"), showWarnings = FALSE)
dir.create(file.path("data/eval", "en_US"), showWarnings = FALSE)
writeCorpus(split$evaluation, path = "data/eval/en_US/")

dir.create(file.path("data", "training"), showWarnings = FALSE)
dir.create(file.path("data/training", "en_US"), showWarnings = FALSE)
writeCorpus(split$training, path = "data/training/en_US/")

rm(corpus)

# subsample training corpus

large   <- tm_map(split$training, subsample.TextDocument, 1/10)
dir.create(file.path("data", "large"), showWarnings = FALSE)
dir.create(file.path("data/large", "en_US"), showWarnings = FALSE)
writeCorpus(large, path = "data/large/en_US/")
rm(training)

medium <- tm_map(large, subsample.TextDocument, 1/3)
dir.create(file.path("data", "medium"), showWarnings = FALSE)
dir.create(file.path("data/medium", "en_US"), showWarnings = FALSE)
writeCorpus(medium, path = "data/medium/en_US/")
rm(large)

small <- tm_map(medium, subsample.TextDocument, 1/3)
dir.create(file.path("data", "small"), showWarnings = FALSE)
dir.create(file.path("data/small", "en_US"), showWarnings = FALSE)
writeCorpus(small, path = "data/small/en_US/")
rm(medium,small)
