# script for Capstone Quiz 3

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)
#library(RWeka) # don't load it, see http://stackoverflow.com/questions/17703553/bigrams-instead-of-single-words-in-termdocument-matrix-using-r-and-rweka
library(plyr)
library(slam)
library(hash)

options(mc.cores=1) # RWeka bug workaround 

Corpus.summary <- function(corpus) 
  ldply(lapply(corpus, function(x) { ellen <- nchar(content(x))
                                     wordlist <- RWeka::WordTokenizer(x)  
                                     data.frame(TextDocument=meta(x,"id"),
                                                NChars=sum(ellen), NElements=length(content(x)),
                                                MedianElement=median(ellen), MaxElement=max(ellen),
                                                NWords=length(wordlist), NVoc=length(unique(wordlist)))} ), 
        data.frame)
#
# load the NGram class and helper functions 
#
source("NGramLM.R") 


#quiz sentences
quiz<- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
         "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
         "I'd give anything to see arctic monkeys this",
         "Talking to your mom has the same effect as a hug and helps reduce your",
         "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
         "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
         "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
         "Every inch of you is perfect from the bottom to the",
         "I’m thankful my childhood was filled with imagination and bruises from playing",
         "I like how the same people are in almost all of Adam Sandler's"
)

# list of alternatives
alternatives <- list(
  c("give", "eat","sleep","die"),                   # not give
  c("spiritual", "financial", "horticultural", "marital"),       # not spiritual ? likely marital
  c("morning","month","decade","weekend"),          # weekend LM says morning?
  c("sleepiness","hunger","happiness","stress"),    # stress LM says hapiness? 
  c("look","walk","minute","picture"),              # picture
  c("case","account","matter","incident"),          # not case
  c("finger","toe","hand","arm"),                   # hand LM says finger?
  c("side","center","middle","top"),                # top
  c("daily","inside","weekly","outside"),           # not dayly likely outside
  c("pictures","stories","novels","movies")         # movies LM says pictures
)

# training ngram model
#mod <- train(control=list(data="small",N=4, method="Katz", threshold=0))
modelFile <- paste0("models/LM.en_US.", "small",".Katz.",4,".",0,".d.RData")
load(file=modelFile)
names(mod)<-gsub("Pbo","p",names(mod)) # old model file, change prob labels
names(mod)

# generate quiz results
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 6/10 gg guess, LMsmallKatzN4k0 score 2/10....

Map(function(sent,alts) sapply(alts, function(alt) has.key(paste(nglast(sent,2),alt),mod[["3-grams"]])), 
    quiz, alternatives) # 3 gram coverage in quiz set tail+alt

Map(function(sent,alts) sapply(alts, function(alt) has.key(paste(nglast(sent,1),alt),mod[["2-grams"]])), 
    quiz, alternatives) # 2 gram coverage in quiz set tail+alt
