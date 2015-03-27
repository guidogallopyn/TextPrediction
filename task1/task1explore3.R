# exploratory analysis script for Capstone task 1

# exploring OpenNLP annotations on small subset for English

setwd("~/Documents/Courses/DataScience/CapStone")

library(tm)

# read corpus small english corpus
eng <- VCorpus(DirSource("data/small/en_US/"))

Corpus.summary <- function(corpus) as.data.frame(t(sapply(corpus, function(x) c(TextDocument=meta(x,"id"), 
                                                                                NLines=length(content(x)), 
                                                                                NWords=length(words(x)), # from NLP package
                                                                                NChars=sum(nchar(content(x)))) ) ))
# summary statistics
Corpus.summary(eng)

###### from http://rpubs.com/lmullen/34069
#install.packages("rJava")
library(rJava)
#install.packages(c("NLP", "openNLP", "RWeka", "qdap"))
#install.packages("openNLPmodels.en",
#                 repos = "http://datacube.wu.ac.at/",
#                 type = "source")

library(NLP)
library(openNLP)
library(RWeka)
library(magrittr)

blogs <- lapply(content(eng[[1]]), as.String)
head(blogs)

word_ann <- Maxent_Word_Token_Annotator() # from OpenNLP
sent_ann <- Maxent_Sent_Token_Annotator()

blog_annotations <- annotate(blogs[[4]], list(sent_ann, word_ann))
class(blog_annotations)
head(blog_annotations,10)

blog_doc <- AnnotatedPlainTextDocument(blogs[[4]], blog_annotations)
sents(blog_doc)
words(blog_doc)
sort(unique(words(blog_doc)))

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")

pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)

blog_doc <- AnnotatedPlainTextDocument(blogs[[4]], annotate(blogs[[4]], pipeline))
annotations(blog_doc)

# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(blog_doc, kind = "location")
entities(blog_doc, kind = "organization")

# now for the blogs 
str(blogs, max.level = 1)

annotate_entities <- function(doc, annotation_pipeline) {
  annotations <- annotate(doc, annotation_pipeline)
  AnnotatedPlainTextDocument(doc, annotations)
}

# entities: "date", "location", "money", "organization", "percentage", "person", "misc"

pipeline <- list(
  Maxent_Sent_Token_Annotator(),
  Maxent_Word_Token_Annotator(),
  Maxent_Entity_Annotator(kind = "person"),
  Maxent_Entity_Annotator(kind = "location"),
  Maxent_Entity_Annotator(kind = "organization")
)

texts_annotated <- blogs %>% lapply(annotate_entities, pipeline)

head(str(texts_annotated, max.level = 2),2)

places <- texts_annotated %>% 
  lapply(entities, kind = "location")

people <- texts_annotated %>%
  lapply(entities, kind = "person")

