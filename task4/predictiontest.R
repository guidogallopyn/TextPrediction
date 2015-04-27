
setwd("~/Documents/Courses/DataScience/CapStone/shinyapp")


# training ngram model
mod <- train(control=list(data="small",N=1, method="Katz", threshold=2))


