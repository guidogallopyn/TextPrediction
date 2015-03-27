# exploratory analysis script for Capstone task 0

# learning ngram package: seems very primitive

setwd("~/Documents/Courses/DataScience/CapStone")

library(ngram)


str <- "A B A C A B B"
ng <- ngram(str,n=3)
babble(ng, genlen=5, seed=1234)

get.ngrams(ng)
get.string(ng)
get.nextwords(ng)

print(ng, full=FALSE)
show(ng)

wordcount(ng)
