# script to evaluate LM modele performance

# KPIs to evaluate
# coverage
# ==> in this script perplexity 
# prediction accuracy
# prediction top-10 accuracy (if word appear in 10best it is counted corect)


setwd("~/Documents/Courses/DataScience/CapStone")
source("NGramLM.R")
library(tm)
library(dplyr)
library(tidyr)
library(ggplot2)

#what model files are available
(df <- LMparams(list.files("models")))


loadLM <- function(name,path="models"){
  load(file=file.path(path, name))
  return(mod)
}

# load subset of models to evaluate
for( name in filter(df, (data=="small"  & N==4 & K %in% c(1,2)) | 
                        (data=="medium" & N==4 & K %in% c(2,3,5)) |
                        (data=="large"  & N==3 & K %in% c(7,10,20)), debug==FALSE)$name)
  assign(name,loadLM(name))
(models <- grep("^LM[.]",ls(),value=TRUE))

# load evaluation corpus, and clean it and tokenize
corpus <- VCorpus(DirSource(file.path(file.path("data","eval"),"en_US")))

# subsample the eval corpus to much smaller eval set
set.seed(123)
microeval <- tm_map(corpus, subsample.TextDocument, 1/1000) #subsample

# perplexity calculations  
#df %>%  filter(name %in% models) %>%
#        mutate(Voc = length(get(as.character(name))[["logp1"]])) %>%  # BUG
#        bind_rows(mutate(.,N=1),mutate(.,N=2))
#        mutate(PP = CorpusPerplexity(get(as.character(name)), microeval, N)

moddf <- filter(df, name %in% models)
moddf$Voc <- sapply(moddf$name, function(name) length(get(as.character(name))[["logp1"]]))
#moddf<- rbind(moddf,mutate(moddf,N=3),mutate(moddf,N=2),mutate(moddf,N=1))
moddf<- bind_rows(mutate(moddf,N=3), mutate(moddf,N=2), mutate(moddf,N=1)) # only for bigrams and unigrams now

moddf$PP <- unlist(Map(function(name,N) CorpusPerplexity(get(as.character(name)), microeval, N), 
                       moddf$name, 
                       moddf$N))

print(moddf)
save(moddf,file="perplexity.RData")

# display evaluation corpus perplexity in function of N and K ==> basically benefit from bigrams is huge, benefit trigrams is small

moddf %>% ggplot(aes(Voc,PP,group=data,colour=data)) + 
          geom_line() +
          geom_point(size=9, shape=21) + 
          geom_text(aes(label=K), size=6) +
          facet_grid(N ~ . )

moddf %>% ggplot(aes(Voc,PP,group=N,colour=as.factor(N))) + 
  geom_line() +
  geom_point(size=9, shape=21) + 
  geom_text(aes(label=K), size=6) +
  facet_grid(data ~ . )

# eval on large models only, with bigger eval set
for( name in filter(df,data=="large" & N==3, debug==FALSE)$name)
  assign(name,loadLM(name))
(models <- grep("^LM[.]",ls(),value=TRUE))

set.seed(123)
eval <- tm_map(corpus, subsample.TextDocument, 1/100) #subsample


moddf <- filter(df, name %in% models)
moddf$Voc <- sapply(moddf$name, function(name) length(get(as.character(name))[["logp1"]]))
moddf<- bind_rows(mutate(moddf,N=3), mutate(moddf,N=2), mutate(moddf,N=1)) 

moddf$PP <- unlist(Map(function(name,N) CorpusPerplexity(get(as.character(name)), eval, N), 
                       moddf$name, 
                       moddf$N))
print(moddf)
save(moddf,file="perplexity.large.RData")

moddf %>% ggplot(aes(Voc,PP,group=N,colour=as.factor(N))) + 
  geom_line() +
  geom_point(size=3, shape=21)

# summary table
moddf %>% spread(N,PP) %>% select(-data,-debug,-name)


#measurement per subset (Blogs, News,Tweets), take best model
pptab <- data.frame(Data=c("All","Blogs","News","Tweets")) 
pptab$PP <- c(CorpusPerplexity(LM.en_US.large.Katz.N3.K3.RData, eval, 3),
              CorpusPerplexity(LM.en_US.large.Katz.N3.K3.RData, eval[1], 3), 
              CorpusPerplexity(LM.en_US.large.Katz.N3.K3.RData, eval[2], 3), 
              CorpusPerplexity(LM.en_US.large.Katz.N3.K3.RData, eval[3], 3))
print(pptab)

save(moddf,pptab,file="perplexity.large.RData")
