
# count full Capstone training corpus  
#  step 1: split training corpus in K=10 folds (each doc is split in K subsets, so we get k *nDocs documents)
#  step 2: perform N gram counting with Rweka (1,n=5) ans save count in file
#  step 3: combine the counts of the chunks and save as 1 counts file



setwd("~/Documents/Courses/DataScience/CapStone")
options( java.parameters = "-Xmx4g" ) # more jave heap space for RWeka see http://www.bramschoenmakers.nl/en/node/726
options(mc.cores=1) # RWeka bug workaround 
library(rJava)
library(tm)

K <-10  # number of folds
N <- 4  # N from Ngrams

# step1: split training corpus in K=10 folds (each doc is split in K subsets, so we get k *nDocs documents)
# store in a folds directory

if(!file.exists("data/folds")) {
  
  # read training corpus
  start.time <- Sys.time()
  corpus <- VCorpus(DirSource("data/training/en_US"))
  (loadTime <- Sys.time() - start.time)
    
  start.time <- Sys.time()
  
  df <- data.frame(doc=rep(names(corpus),K),
                   len=rep(sapply(corpus,function(doc) length(content(doc))),K),
                   fold=as.vector(outer(names(corpus), 1:K, FUN = function(x,y) y)))
  
  df <- transform(df, fold_begin=as.integer((fold-1)*len/K+1), 
                  fold_end=as.integer(fold*len/K), 
                  id=paste("fold", gsub(".txt","",doc), fold, sep = "_"))
  
  
  dir.create("data/folds", showWarnings = FALSE)
  
  createfold <- function(doc, begin, end, fid) { 
    fold  <- tm_map(corpus[doc], content_transformer(function(x) x[ begin:end ]))
    dir.create(file.path("data/folds", fid), showWarnings = FALSE)
    writeCorpus(fold, path=file.path("data/folds",fid), filenames = paste0(fid,".txt"))
    rm(fold)
    return(as.character(fid))
  }
  with(df, mapply(createfold, doc, fold_begin, fold_end, id))
  rm(corpus) 
  (foldTime <- Sys.time() - start.time)

} else cat("using existing folds directory")

#  step 2: perform N=4 gram counting with Rweka and save count in file
cat("Ngram Counting folds")

cleanctrl <-list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                 content_transformer(tolower),
                 removePunctuation,
                 removeNumbers,
                 stripWhitespace)

tokctrl<- list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = N, max = N)), # count unigrams 
               wordLengths=c(1, Inf))

ngramCount <- function(f) { fold <- VCorpus(DirSource(file.path("data/folds",f)))               # read fold
                            fold <- tm_map(fold, FUN = tm_reduce, tmFuns= cleanctrl)            # clean fold
                            return(TermDocumentMatrix(fold, control=tokctrl))          # ngram count fold
                          }
start.time <- Sys.time()
#tdms <- Map(ngramCount,list.files("data/folds")) 
#tdm <- Reduce(c, tdms)  # combine fold counts in 1 big term doc matrix

dir.create(file.path(".", "counts"), showWarnings = FALSE)

for(f in list.files("data/folds")) {
  varname <- paste0("tdm",f)  # make tdm var for each tdm without using a list
  if(!file.exists(file.path("counts",paste0(f,".RData")))) {
    cat("c")
    assign(varname,ngramCount(f))           # count ngrams
    save(list=varname,file=file.path("counts",paste0(f,".RData")))
    rm(list=varname)
  } else cat("f") 
}
(countTime <- Sys.time() - start.time)

# step 3: combine the counts of the chunks and save as 1 counts file
# tdm's are too big to redl in all--> tree based appoach 
cat("Combining  blog, news and tweets for per K one Term Document Matrix")

# helper function for binary tree map-reduce
maketree<- function(elements) {
  if(length(elements)==1) return(list(node=elements))
  split <- floor((length(elements)+1)/2)
  return(list(left=maketree(elements[1:split]),
              right=maketree(elements[(split+1) : length(elements)])))
}

treeMapReduce <- function(tree,fMap,fReduce) {
  if("node" %in% names(tree)) return(fMap(tree$node))
  return(fReduce(treeMapReduce(tree$left,fMap,fReduce), treeMapReduce(tree$right,fMap,fReduce)))
}


start.time <- Sys.time()
for(k in 1:K) { # read tdms per fold for  blog, news and tweets
  varname <- paste0("tdm",k)
  if(!file.exists(file.path("counts",paste0(varname,".RData")))) {
    pattern <- paste0("^fold.+_",k,"[.]RData$")
    for(f in grep(pattern,list.files("counts"),value=TRUE)) {
      cat("r")
      load(file=file.path("counts",f))
    } 
    cat("c")
    pattern <- paste0("^tdmfold[[:print:]]+_",k,"$")
    vars    <- grep(pattern,ls(),value=TRUE,perl=TRUE)
#    assign(varname, Reduce("+", Map(get, vars)))  # add 3 fold counts in 1 term doc matrix
    tdm <- Reduce(c, Map(get, vars))  # combine 3 fold counts in 1 big term doc matrix
    assign(varname, Reduce("+", Map(function(doc) tdm[,doc],Docs(tdm))))    # collapse the tdm to 1 column: add all the collums
    rm(list=vars); rm(tdm)
    cat("s")
    save(list=varname,file=file.path("counts",paste0(varname,".RData")))
    rm(list=varname)
  } else cat("f") 
}
(nodecombineTime <- Sys.time() - start.time)

cat("\nCombining K one Term Document Matrice into 1")
pattern <- paste0("^tdm[[:digit:]]+[.]RData$")
tree <- maketree(grep(pattern,list.files("counts"),value=TRUE))

fReduce <- function(left,right) {
  cat("reading file",left,"...\n")
  load(file=file.path("counts",left))
  cat("reading file",right,"...\n")
  load(file=file.path("counts",right))
  vars <- grep("^tdm",ls(), value=TRUE) # there should be just two
  my_uuid <- paste0("tdm",system("uuidgen", intern=T))
  cat("Combine (",vars,") in one tdm\n")
  tdm <- Reduce(c, mget(vars))  # combinetwo
  rm(list=vars)
  cat("Sum(",vars,") =",my_uuid,"\n")
  assign(my_uuid, Reduce("+", Map(function(doc) tdm[,doc],Docs(tdm))))    # collapse the tdm to 1 column: add all the collums
  rm(tdm)
  cat("Saving file",paste0(my_uuid,".Rdata"),"\n")
  save(list=my_uuid,file=file.path("counts",paste0(my_uuid,".Rdata")))
  rm(list=my_uuid)
  return(paste0(my_uuid,".Rdata"))
}

start.time <- Sys.time()
tdmname<- treeMapReduce(tree, function(x) x, fReduce)
(nodecombineTime <- Sys.time() - start.time)


# combination last two
tree <- maketree(grep("tdm(37|5A)",list.files("counts"),value=TRUE))
start.time <- Sys.time()
tdmname<- treeMapReduce(tree, function(x) x, fReduce)
(nodecombineTime <- Sys.time() - start.time)


#save(tdm,file=file.path("counts","trainingtdm.RData"))

#inspect(tdm[1:10,1:2])

#ngram.counts <- slam::row_sums(tdm)
#rm(tdm)

#ngrams <- strsplit(names(ngram.counts)," ")

#unigram.counts <- ngram.counts[sapply(ngrams,length)==1]
#head(sort(unigram.counts,decreasing=TRUE),100)

#bigram.counts <- ngram.counts[sapply(ngrams,length)==2]
#head(sort(bigram.counts,decreasing=TRUE),100)


if(FALSE) {
  start.time <- Sys.time()
  corpus <- VCorpus(DirSource("data/training/en_US"))
  (loadTime <- Sys.time() - start.time)
  
  cleanctrl <-list(content_transformer(function(x) iconv(x, from="latin1", to="ASCII", sub="")),
                   content_transformer(tolower),
                   removePunctuation,
                   removeNumbers,
                   stripWhitespace)
  
  tokctrl<- list(tokenize = function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = N)), # count unigrams 
                 wordLengths=c(1, Inf))
  
  corpus <- tm_map(corpus, FUN = tm_reduce, tmFuns= cleanctrl)            # clean fold
  
  tmdfull<- tm::TermDocumentMatrix(fold, control=tokctrl)               # ngram count fold
  
  (oneshotTime <- Sys.time() - start.time)
}

