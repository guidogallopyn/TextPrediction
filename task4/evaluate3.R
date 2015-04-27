# script to evaluate LM modele performance

# KPIs to evaluate
# coverage
# perplexity 
# ==> in this script prediction accuracy
# prediction top-10 accuracy (if word appear in 10best it is counted corect)


setwd("~/Documents/Courses/DataScience/CapStone")
source("NGramLM.R")
library(tm)
library(dplyr)
library(tidyr)
library(ggplot2)

# Expand so that each line in corpus becomes a seperate document.
ExpandCorpus <- function(corpus)  VCorpus(VectorSource(unlist(lapply(corpus,content))))


predictWord <- function(mod,history,prefix)
  names(unlist(Map(function(h,p) predict.NGramLM(mod, h, prefix=p, Nbest=1), history, prefix)))

#############################
# prepare evalution corpus
#############################

corpus <- VCorpus(DirSource(file.path(file.path("data","eval"),"en_US")))

# subsampeling, data transformation,  and tokenization
set.seed(123)

corpus %>%  tm_map(subsample.TextDocument, 1/100) %>%                  # subsample eval corpus (accuracy level are low, don't need large evaluation corpus)
            ExpandCorpus() %>%                                         # expand eval corpus so that every line in all docs is seperate document
            tm_map(RWeka::WordTokenizer)  %>%                          # tokenize the corpus doc per doc: each doc is now a token vector
            tm_filter(function(x) (length(x) > 0)) -> tokenCorp        # remove zero length token strings

#############################
# LM models to evaluate
#############################

# what model files are available
(df <- LMparams(list.files("models")))

# LM models to evaluate
(moddf <- filter(df,data=="large"  & N==4 & K %in% c(8,18,40), debug==FALSE))

# LOOP to evaluate
for( name in moddf$name) {  # loop over models
  cat("Evaluating model",name,"\n")
  load(file=file.path("models", name))
  summary(mod)
  
  for (N in mod$N) { # loop over N
    
    cat("Evaluating N ", N,"\n")

# now create predict ~ history + prefix set, here for unigram model  

start.time <- Sys.time()

data <- data.frame(reference=unlist(lapply(tokenCorp, function(tokens) sapply(1:length(tokens), function(n) tolower(tail(head(tokens,n),1))))),
                    history=unlist(lapply(tokenCorp, function(tokens) sapply(1:length(tokens), function(n) paste(tail(head(tokens,n-1),N-1),collapse=" ")))))

accuracy <- data.frame(N=integer(0),Voc=integer(0),PrefixLen=integer(0), Accuracy=numeric(0))

for (itt in 1:10) {
  cat("Bootstrap iteration",itt,"\n")

  data %>% sample_frac(1/200, replace = TRUE) %>% 
           mutate(L0=0,L1=1,L2=2,L3=3,L4=4) %>%
           gather(Label,PrefixLen, -reference, -history) %>%
           select(-Label) %>%
           mutate(prefix = substr(reference,1,PrefixLen)) %>%
           mutate(predict = predictWord(mod,history,prefix),
                  score   = (as.character(reference)==as.character(predict)))  %>%
           group_by(PrefixLen) %>%
           summarise(Accuracy=100*mean(score)) %>% 
           mutate(N=N,Voc=nVoc(mod)) %>%
           bind_rows(accuracy,.) -> accuracy
  
  save(accuracy,file=file.path("acc", paste0("acc",name)))
  print(accuracy)
}     
Sys.time() - start.time
  
  } # end N for loop

  clear(mod);rm(mod)
} # end model loop




