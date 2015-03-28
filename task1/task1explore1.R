# exploratory analysis script for Capstone task 1

# downloading and creating a smaller subset for English


setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)

if (!file.exists("data/final")){
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", 
                destfile="data/Dataset.zip", method="curl")
  unzip("data/Dataset.zip")
} 

system("wc -lwc data/final/en_US/en_US.blogs.txt")
system("wc -lwc data/final/en_US/en_US.news.txt")
system("wc -lwc data/final/en_US/en_US.twitter.txt")

# read corpus
eng <- VCorpus(DirSource("data/final/en_US/"))

Corpus.summary <- function(corpus) as.data.frame(t(sapply(corpus, function(x) c(TextDocument=meta(x,"id"), 
                                                                                NLines=length(content(x)), 
                                                                                #NWords=length(unlist(strsplit(as.character(x), "[[:space:]]+"))),
                                                                                #NWords=sum(sapply(gregexpr("[[:alnum:]]+",x),length)),
                                                                                NWords=length(words(x)),
                                                                                NChars=sum(nchar(content(x)))) ) ))
# summary statistics
#Corpus.summary(eng)

# subsample the corpus 10%
set.seed(123)
large <- tm_map(eng, content_transformer(function(x) x[sample(length(x),length(x)/10)])) 
writeCorpus(large, path = "data/large/en_US")
Corpus.summary(large)

# subsample the corpus 3%
set.seed(123)
medium <- tm_map(large, content_transformer(function(x) x[sample(length(x),length(x)/3.33)])) 
writeCorpus(medium, path = "data/medium/en_US")
Corpus.summary(medium)

# subsample the corpus 1%
set.seed(123)
small <- tm_map(eng, content_transformer(function(x) x[sample(length(x),length(x)/100)])) 


# summary statistics
Corpus.summary(small)

# write smaller corpus
writeCorpus(small, path = "data/small/en_US/")

## now create an even smaller subset
set.seed(123)
micro <- tm_map(small, content_transformer(function(x) x[sample(length(x),length(x)/1000)])) 
writeCorpus(micro, path = "data/micro/en_US")

# summary statistics
Corpus.summary(eng)



