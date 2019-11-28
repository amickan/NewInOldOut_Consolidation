#### Bayesian stats

library(brms)
require(reshape)
require(data.table)
require(here)
require(tidyr)
require(plyr)
require(ggplot2)
require(rstan)

# some settings
rstan::rstan_options(autowrite=TRUE)
# most modern computers have at least cores, so this should speed things up
options(mc.cores=2)
# set seed to ensure we get the same results every time we run this script
set.seed(42)

# initiate participants
A = c(701:722, 724:755)

### Read in all data and merge into one data file that can be read in at later stages 
data_list <- list()

for (i in 1:length(A)){
  pNumber = A[i]
  wd1 <-  paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING/English_Finaltest/", pNumber, "_Finaltest", sep="")
  wd2 <-  paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING/Spanish_Posttest_a/",pNumber, "_Posttest_a", sep="")
  wd3 <-  paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING/English_Pretest/",pNumber, "_Pretest_subset", sep="")
  wd4 <-  paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING/Spanish_Posttest_b/",pNumber, "_Posttest_b", sep="")
  infile1 <- paste(pNumber,"Finaltest_new.txt",sep="_")
  infile2 <- paste(pNumber,"Posttest_A.txt",sep="_")
  infile3 <- paste(pNumber,"Pretest_new.txt",sep="_")
  infile4 <- paste(pNumber,"Posttest_B.txt",sep="_")
  
  # read in Spanish postest file (only the second one for now)
  setwd(wd4)
  currentFile <- as.data.frame(read.delim(infile4, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  # set all partially correct answers to complete errors
  if (length(currentFile[currentFile$Error == 999,]$Error) > 0){
    currentFile[currentFile$Error == 999,]$Error<-1
  }
  
  # read in the English final test file 
  setwd(wd1)
  currentFile2 <- as.data.frame(read.delim(infile1, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  # read in the English pretest file 
  setwd(wd3)
  currentFile3 <- as.data.frame(read.delim(infile3, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  
  ## adding pretest RTs to final test file
  for (j in 1:nrow(currentFile2)) {
    pos <- which(tolower(as.character(currentFile3$English_Label)) == tolower(as.character(currentFile2$Item[j])))
    if (length(pos)==0) {} 
    else {
      currentFile2$RT_pre[j] <- currentFile3$RT_new[pos]
      currentFile2$ArticlesPre[j] <- currentFile3$ArticlesPre[pos]
    }}
  
  ## marking unlearned words (from Spanish posttest) as missing values in final test 
  for (j in 1:nrow(currentFile)) {
    pos <- which(tolower(as.character(currentFile2$Item)) == tolower(as.character(currentFile$Item[j])))
    if (currentFile$Error[j] == 1) {
      currentFile2$Error[pos] <- NA
      currentFile2$ErrorDetail[pos] <- NA
      currentFile2$VoiceOnset[pos] <- NA
      currentFile2$RT_new[pos] <- NA
      currentFile2$RT_pre[pos] <- NA
    }}
  
  # set trials with errors to NA for naming latencies 
  if (length(currentFile2[ifelse(is.na(currentFile2$Error),
                                 1,currentFile2$Error) == 999,]$Error) > 0) {
    currentFile2[ifelse(is.na(currentFile2$Error),
                        1,currentFile2$Error) == 999,]$Error<-1
  }
  
  if (length(currentFile2[ifelse(is.na(currentFile2$Error),
                                 1,currentFile2$Error) == 1,]$VoiceOnset) > 0) {
    currentFile2[ifelse(is.na(currentFile2$Error),
                        1,currentFile2$Error) == 1,]$VoiceOnset <- NA # this excludes words that were produced with errors after interference from RT analysis
  }
  
  if (length(currentFile2[ifelse(is.na(currentFile2$Error),
                                 1,currentFile2$Error) == 1,]$RT_new) > 0) {
    currentFile2[ifelse(is.na(currentFile2$Error),
                        1,currentFile2$Error) == 1,]$RT_new <- NA # this excludes words that were produced with errors after interference from RT analysis
  }
  
  data_list[[i]] <- currentFile2
  
  print(A[i])
  
}
post <- rbindlist(data_list)

# possibly need to rename first column 
colnames(post)[1] <- "Subject_nr"

# add a group variable
for (i in 1:nrow(post)){
  if (post$Subject_nr[i] < 726){
    post$ConsolidationGroup[i] <- "Consolidation"
  } else if (post$Subject_nr[i] > 725 && post$Subject_nr[i] < 751) {
    post$ConsolidationGroup[i] <- "NoConsolidation"
  } else if (post$Subject_nr[i] == 751 || post$Subject_nr[i] == 753 || post$Subject_nr[i] == 755) {
    post$ConsolidationGroup[i] <- "Consolidation"
  } else if (post$Subject_nr[i] == 752 || post$Subject_nr[i] == 754) {
    post$ConsolidationGroup[i] <- "NoConsolidation"}
}

# log-transform RTs
post$RT_new_log <- log(post$RT_new)

# difference scores for reaction times
post$RTdiff <- post$RT_pre - post$RT_new
post$Prelog <- log(post$RT_pre)
post$RTdifflog <- post$Prelog - post$RT_new_log

### deleting trials of words that were already known in Spanish before the learning phase
setwd("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING")
known <- read.delim("KnownUnknown.txt")
for (i in 1:nrow(known)){
  pNumber <- known$Participant[i]
  num <- which(tolower(post[post$Subject_nr == pNumber,]$Spanish_Label) == tolower(known$KnownSpa[i]))
  if (length(num)!= 0 ){
    post[post$Subject_nr == pNumber,]$RT_new[num] <- NA
    post[post$Subject_nr == pNumber,]$RT_pre[num] <- NA
    post[post$Subject_nr == pNumber,]$Prelog[num] <- NA
    post[post$Subject_nr == pNumber,]$RT_new_log[num] <- NA
    post[post$Subject_nr == pNumber,]$RTdiff[num] <- NA
    post[post$Subject_nr == pNumber,]$RTdifflog[num] <- NA
    post[post$Subject_nr == pNumber,]$VoiceOnset[num] <- NA
    post[post$Subject_nr == pNumber,]$Error[num] <- NA
  }
  num <- which(tolower(post[post$Subject_nr == pNumber,]$Item) == tolower(known$UnknownEn[i]))
  if (length(num)!= 0 ){
    post[post$Subject_nr == pNumber,]$RT_new[num] <- NA
    post[post$Subject_nr == pNumber,]$RT_pre[num] <- NA
    post[post$Subject_nr == pNumber,]$Prelog[num] <- NA
    post[post$Subject_nr == pNumber,]$RT_new_log[num] <- NA
    post[post$Subject_nr == pNumber,]$RTdiff[num] <- NA
    post[post$Subject_nr == pNumber,]$RTdifflog[num] <- NA
    post[post$Subject_nr == pNumber,]$VoiceOnset[num] <- NA
    post[post$Subject_nr == pNumber,]$Error[num] <- NA
  }
}

# setting variables
post$Subject_nr <- as.factor(post$Subject_nr)
post$Condition <- as.factor(post$Condition)
post$Errorfact <- as.factor(post$Error)
post$ConsolidationGroup <- as.factor(post$ConsolidationGroup)
post$Item <- as.factor(post$Item)

# exclude trials in which articles were used from RT analysis 
for (i in 1:nrow(post)){
  if (is.na(post$ArticlesPost[i]) == 1 && is.na(post$ArticlesPre[i]) == 1){
  } else if (is.na(post$ArticlesPost[i]) == 0 && is.na(post$ArticlesPre[i]) == 0) {
  } else if (is.na(post$ArticlesPost[i]) == 0 | is.na(post$ArticlesPre[i]) == 0){
    post$RT_new[i] <- NA
    post$RT_pre[i] <- NA
    post$Prelog[i] <- NA
    post$RT_new_log[i] <- NA
    post$RTdiff[i] <- NA
    post$RTdifflog[i] <- NA
    post$VoiceOnset[i] <- NA
  } 
}

# possibly exclude people that don't have enough data left 
postrt<-post[!(post$Subject_nr== 719 | post$Subject_nr == 736 | post$Subject_nr == 745),]
postrt$Subject_nr <- droplevels(postrt$Subject_nr)

### run model 
model <- brm(Error ~ Condition*ConsolidationGroup,
             data=post,
             family=bernoulli,
             algorithm="sampling", # other options are "meanfield" and "fullrank" for variational approximations
             sample_prior=TRUE,    # this is needed for plotting hypotheses
             save_all_pars=TRUE,   # this is needed for plotting hypotheses
             chains=4,
             iter=6e3)

summary(model)
plot(model)

pp = brms::pp_check(model)
pp + theme_bw()
marginal_effects(model)
