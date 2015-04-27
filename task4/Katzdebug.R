# script to evaluate LM modele performance

# KPIs to evaluate
# OOV
# perplexity
# prediction accuracy
# prediction top-10 accuracy (if word appear in 10best it is counted corect)


setwd("~/Documents/Courses/DataScience/CapStone")
source("NGramLM.R")

load(file=file.path("models", modelfilename("small",debug=TRUE)))
summary(mod)


predict(mod,"Today")
mod$followers[["today"]]
sort(values(mod[["2-grams"]][ mod$followers[["today"]] ]),decreasing=TRUE)

predict(mod,"Today is")
mod$followers[["today is"]]
sort(values(mod[["3-grams"]][ mod$followers[["today is"]] ]),decreasing=TRUE)

mod$disc3[ mod$followers[["today is"]] ]
