# script to evaluate LM modele performance

# KPIs to evaluate
# coverage
# perplexity 
# ==> in this script prediction accuracy (combination of results)
# prediction top-10 accuracy (if word appear in 10best it is counted corect)


setwd("~/Documents/Courses/DataScience/CapStone")
source("NGramLM.R")
library(dplyr)
library(tidyr)
library(ggplot2)
library(Hmisc)

# load accuracy result data sets with same K

load(file=file.path("acc", "acc.en_US.large.Katz.N1.K7.RData"))
mutate(accuracy,N=1,K=7) -> accn1k7

load(file=file.path("acc", "acc.en_US.large.Katz.N2.K7.RData"))
accuracy %>% mutate(N=2,K=7) -> accn2k7

load(file=file.path("acc", "acc.en_US.large.Katz.N3.K7.RData"))
accuracy %>% mutate(N=3,K=7) -> accn3k7

load(file=file.path("acc", "accLM.en_US.large.Katz.N4.K8.RData"))
accuracy %>% mutate(N=4,K=8) %>% select(-Voc) -> accn4k8

load(file=file.path("acc", "accLM.en_US.large.Katz.N4.K18.RData"))
accuracy %>% mutate(N=4,K=18) %>% select(-Voc) -> accn4k18

load(file=file.path("acc", "acc.en_US.large.Katz.N3.K20.RData"))
accuracy %>% mutate(N=3,K=20) -> accn3k20

load(file=file.path("acc", "acc.en_US.large.Katz.N2.K40.RData"))
accuracy %>% mutate(N=2,K=40) -> accn2k40

load(file=file.path("acc", "accLM.en_US.large.Katz.N4.K40.RData"))
accuracy %>% mutate(N=4,K=40) %>% select(-Voc) -> accn4k40

accuracy <- bind_rows(accn1k7,accn2k7,accn3k7,accn4k8, accn4k18,accn3k20,accn2k40,accn4k40)

save(accuracy,file=file.path("acc", "acc.summary.RData"))
load(file=file.path("acc", "acc.summary.RData"))

accuracy %>% ggplot(aes(as.factor(PrefixLen), Accuracy,color=as.factor(N))) + 
             geom_boxplot( ) +
             facet_grid(K ~ N)

accuracy %>% group_by(N,K,PrefixLen) %>%
             summarise(mean=mean(Accuracy),se=sd(Accuracy)) %>%
             filter(PrefixLen==3)

# models in shiny
accuracy %>% filter((N==3 & K==7) | (N==3 & K==20) | (N==2 & K==40)) %>%
             ggplot(aes(factor(PrefixLen), Accuracy, group=factor(K),colour=factor(K))) + 
             stat_summary(fun.data="mean_cl_normal") +
             stat_summary(fun.y=mean, geom ="line")

# nice plot for performance summary
accuracy %>% filter((N==3 & K==7) | (N==3 & K==20) | (N==2 & K==40)) %>%
             ggplot(aes(factor(K),Accuracy,color=factor(K))) +   
             geom_boxplot() +
             facet_grid(~ PrefixLen)

# nice summary accuracy table 

modtab <-data.frame(Model    = c("small","medium","large"),
                    N        = c(2,3,3),
                    K        = c(40,20,7),
                    uniGrams = c(11784, 17896 ,32285),
                    biGrams  = c(19985, 40661, 110943),
                    triGrams = c(0,16757,65020),
                    Size.Mb  = c(0.95, 2.3, 5.9))

accuracy %>% filter((N==3 & K==7) | (N==3 & K==20) | (N==2 & K==40)) %>%
             group_by(K,PrefixLen) %>%
             summarise(acc=round(mean(Accuracy),1)) %>%
             spread(PrefixLen,acc) %>%
             full_join(modtab,.,by ="K") -> modtab 
save(modtab,file=file.path(".", "mod.summary.RData"))
rm(accuracy,modtab)
load(file=file.path(".", "mod.summary.RData"))
