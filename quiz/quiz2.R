# script for Capstone Quiz2

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
#source("NGramLM.improg.R") #### attention this is in progress
source("NGramLM.R") #### this is stable


#quiz sentences
quiz<- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
         "You're the reason why I smile everyday. Can you follow me please? It would mean the",
         "Hey sunshine, can you follow me and make me the",
         "Very early observations on the Bills game: Offense still struggling but the",
         "Go on a romantic date at the",
         "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
         "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
         "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
         "Be grateful for the good times and keep the faith during the",
         "If this isn't the cutest thing you've ever seen, then you must be"
)

# list of alternatives
alternatives <- list(
  c("soda", "cheese","pretzels","beer"),        # beer
  c("best", "universe", "world", "most"),       # world
  c("happiest","smelliest","bluest","saddest"), # happiest
  c("referees","players","crowd","defense"),    # not crowd ==> players?
  c("mall","movies","beach","grocery"),         # beach
  c("motorcycle","way","horse","phone"),        # way
  c("years","thing","weeks","time"),            # time
  c("ears","toes","eyes","fingers"),            # not eyes ==> fingers
  c("hard","sad","worse","bad"),                # not hard ==> bad
  c("callous","asleep","insensitive","insane")  # not asleep ==> insance
)



# training ngram model
#mod <- train(control=list(data="small",N=1, method="Katz", threshold=2))
mod <- LMTrainingReceipe(control=list(data="small", N=2, method="Katz", threshold=2))

# generate quiz results
Map(function(x,y) predict(mod,x,y), quiz, alternatives) # 7/10


# error analysis: 3 errors is where no trigrams occur and bigram probs are a close call
# what is best approach? train on more data, or LM smoothing
sum(sapply(quiz, function(x) has.key(nglast(x,3),mod[["3-grams"]])))/10 # 3 gram coverage in quiz set
sum(sapply(quiz, function(x) has.key(nglast(x,2),mod[["2-grams"]])))/10 # 2 gram coverage in quiz set
sum(sapply(quiz, function(x) has.key(nglast(x,1),mod[["1-grams"]])))/10 # 1 gram coverage in quiz set

Map(function(sent,alts) sapply(alts, function(alt) has.key(paste(nglast(sent,2),alt),mod[["3-grams"]])), 
    quiz, alternatives) # 3 gram coverage in quiz set tail+alt
mod[["Pbo3"]][["at the mall"]]
mod[["Pbo3"]][["at the movies"]]
mod[["Pbo3"]][["at the beach"]]
mod[["Pbo3"]][["at the grocery"]]


Map(function(sent,alts) sapply(alts, function(alt) has.key(paste(nglast(sent,1),alt),mod[["2-grams"]])), 
    quiz, alternatives) # 2 gram coverage in quiz set tail+alt

mod[["Pbo2"]][["the referees"]]
mod[["Pbo2"]][["the players"]]
mod[["Pbo2"]][["the crowd"]]
mod[["Pbo2"]][["the defense"]]

mod[["Pbo2"]][["little eyes"]]
mod[["Pbo2"]][["little fingers"]]

mod[["Pbo2"]][["be asleep"]]
mod[["Pbo2"]][["be insane"]]

