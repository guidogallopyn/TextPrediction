# LM model training

setwd("~/Documents/Courses/DataScience/CapStone")
options( java.parameters = "-Xmx4g" ) # more jave heap space for RWeka see http://www.bramschoenmakers.nl/en/node/726
options(mc.cores=1) # RWeka bug workaround 
source("NGramLM.R")

# training ngram model for unigrams to quadrigrams on different subsampled corpora
for (s in c("large"))
  for (n in c(4)) 
    for (k in c(40,18,8)) {
      cat("training ;",s,", N=",n,", K=",k,"\n")
      mod <- LMTrainingReceipe(control=list(data=s, N=n, method="Katz", threshold=k, debug=FALSE))
      print(summary(mod))
      nbest <- predict(mod,"this is a")
      print(head(nbest,5))
      clear(mod)
  }
