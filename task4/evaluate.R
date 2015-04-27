# script to evaluate LM modele performance

# KPIs to evaluate
# ===> coverage (in this script)
# perplexity
# prediction accuracy
# prediction top-10 accuracy (if word appear in 10best it is counted corect)


setwd("~/Documents/Courses/DataScience/CapStone")
source("NGramLM.R")
library(tm)
library(dplyr)
library(ggplot2)

#what model files are available
(df <- LMparams(list.files("models")))


loadLM <- function(name,path="models"){
  load(file=file.path(path, name))
  return(mod)
}

# load subset of models to evaluate
for( name in filter(df, ((data=="small" | data=="medium") & N==4) | (data=="large" & N==3), debug==FALSE)$name)
#for( name in filter(df, data=="large",debug==FALSE)$name)
  assign(name,loadLM(name))
(models <- grep("^LM[.]",ls(),value=TRUE))

#summary(get(name))
#Voc <- keys(get(name)[["logp1"]])
#length(Voc)

# load evaluation corpus, and clean it and tokenize
corpus <- VCorpus(DirSource(file.path(file.path("data","eval"),"en_US")))

# subsample the eval corpus to much smaller eval set
set.seed(123)
microeval <- tm_map(corpus, subsample.TextDocument, 1/10) #subsample

# clean the microeval corpus
cleanctrl <-list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                 content_transformer(tolower),
                 removePunctuation,
                 removeNumbers,
                 stripWhitespace)
microeval<- tm_map(microeval, FUN = tm_reduce, tmFuns= cleanctrl)
#eval<- tm_map(corpus, FUN = tm_reduce, tmFuns= cleanctrl)

# tokenize and count
tokctrl <- list(tokenize = RWeka::WordTokenizer,  wordLengths=c(1, Inf))
tdm <- TermDocumentMatrix(microeval,control=tokctrl)
evalwords <- Terms(tdm) # are words in eval set

#Voc and corpus coverage analysis 
moddf <- filter(df, name %in% models)
moddf$Voc <- sapply(moddf$name, function(name) length(get(as.character(name))[["logp1"]]))
moddf$VocCover <- sapply(moddf$name, function(name) 100*length(intersect(evalwords,keys(get(as.character(name))[["logp1"]])))/length(evalwords))
moddf$Coverage <- sapply(moddf$name, function(name) 100*sum(tdm[intersect(evalwords,keys(get(as.character(name))[["logp1"]])),])/sum(tdm))
moddf <- arrange(moddf,K)
print(moddf)
save(moddf,file="Coverage.RData")

ggplot(aes(Voc,Coverage,group=data,colour=data), data=moddf) + 
  geom_line() + geom_point(size=9, shape=21) + geom_text(aes(label=K), size=6)

# observations large corpus gives slightly btter coverage/V then medium corp, K 7 or 10 seems reasonable, medium K=2 or 3

