#touch all models to add a word trie

setwd("~/Documents/Courses/DataScience/CapStone")

source("NGramLM.R")
library(dplyr)

#what model files are available
(df <- LMparams(list.files("models")))

for( name in df$name){
  load(file=file.path("models", name))
  cat("Construct word trie for", length(mod$logp1), "words in", name,"...\n")
  trie <- hash()
  for( word in keys(mod$logp1)) {
    if( nchar(word) < 2 ) next
    prefixes <- sapply(2:nchar(word)-1, function(n) substr(word,1,n)) #all prefixes of word
    for( prefix in prefixes) trie[[prefix]] <- append(trie[[ prefix]], word)  
  }
  mod[["trie"]] <- trie
  save(mod,file=name)
  hash::clear(trie)
  clear(mod)
}  
