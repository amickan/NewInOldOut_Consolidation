### Consolidation project analysis ###
# last updated by Anne Mickan, April 2nd, 2019

# load required pacakges
require(reshape)
require(data.table)
require(here)
require(tidyr)
require(plyr)
require(ggplot2)
require(lme4)
require(lmerTest)
require(lmtest)

# initiate participants
A = c(701:722, 724:755)

# Transfer hand-coded RTs from Praat output to English posttest and pretest files
data_list <- list()

for (i in 1:length(A)){
  pNumber = A[i]
  setwd("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING")
  infile1 <- paste(pNumber,"Engfinal_logfile_manual.txt",sep="_")
  infile3 <- paste(pNumber,"Pretest_logfile_manual.txt",sep="_")
  pretest <- as.data.frame(read.delim("PretestArticles.txt", stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  posttest <- as.data.frame(read.delim("FinaltestArticles.txt", stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  
  setwd("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING/English_Finaltest_Coding")
  data <- read.delim(infile1, header = F)
  data <- separate(data = data, col = V4, into = c("Trial", "rand"), sep = "-")
  data <- separate(data = data, col = Trial, into = c("Trial", "rand2"), sep = "l")
  as.numeric(data$rand2)->data$rand2
  data <- data[order(data$rand2),]
  
  if (length(data$V1) > 46) {
    print(pNumber)
  }
  
  wd1 <-  paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING/English_Finaltest/", pNumber, "_Finaltest", sep="")
  setwd(wd1)
  infile2 <- paste(pNumber,"Finaltest.txt",sep="_")
  currentFile <- as.data.frame(read.delim(infile2, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  
  for (j in 1:nrow(currentFile)) {
    pos <- which(tolower(as.character(data$rand2)) == tolower(as.character(currentFile$Trial_nr[j])))
    currentFile$RT_new[j] <- data$V5[pos]
  }
  
  for (j in 1:nrow(currentFile)) {
    if(currentFile$RT_new[j]==0){
      currentFile$RT_new[j] <- NA
    }
  }
  
  currentFile$ArticlesPost <- NA
  if (any(posttest$Participant %in% currentFile$Subject_nr[1]) == T){
    for (m in 1:nrow(posttest[posttest$Participant==currentFile$Subject_nr[1],])){
      num <- which(tolower(as.character(currentFile$Trial_nr)) == tolower(as.character(posttest$Trial[m])))
      currentFile$ArticlesPost[num] <- 1
    }}
  
  setwd(wd1)
  write.table(currentFile, infile2, quote = F, row.names = F, col.names = T, sep = "\t")
  
  setwd("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING")
  data <- read.delim(infile3, header = F)
  data <- separate(data = data, col = V4, into = c("Trial", "rand"), sep = "-")
  data <- separate(data = data, col = Trial, into = c("Trial", "rand2"), sep = "l")
  as.numeric(data$rand2)->data$rand2
  data <- data[order(data$rand2),]
  
  if (length(data$V1) > 46) {
    print(pNumber)
  }
  
  wd3 <-  paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/CODING/English_Pretest/", pNumber, "_Pretest_subset", sep="")
  setwd(wd3)
  infile4 <- paste(pNumber,"Pretest.txt",sep="_")
  currentFile2 <- as.data.frame(read.delim(infile4, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  
  for (j in 1:nrow(data)) {
    pos <- which(tolower(as.character(currentFile2$Trial_nr)) == tolower(as.character(data$rand2[j])))
    currentFile2$RT_new[pos] <- data$V5[j]
  }
  
  for (j in 1:nrow(currentFile2)) {
    if(is.na(currentFile2$RT_new[j])==0 && currentFile2$RT_new[j] == 0){
      currentFile2$RT_new[j] <- NA
    }
  }
  
  # adding a column to the pretest that codes for article trials
  currentFile2$ArticlesPre <- NA
  if (any(pretest$Participant %in% currentFile2$Subject_nr[1]) == T){
    for (f in 1:nrow(pretest[pretest$Participant==currentFile2$Subject_nr[1],])){
      num <- which(tolower(as.character(currentFile2$Trial_nr)) == tolower(as.character(pretest$Trial[f])))
      currentFile2$ArticlesPre[num] <- 1
    }}
  
  
  setwd(wd3)
  write.table(currentFile2, infile4, quote = F, row.names = F, col.names = T, sep = "\t")
}

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

# check group assignment 
#table(post$Subject_nr, post$ConsolidationGroup)

# checking RTs
min(post$RT_new, na.rm=T)
hist(post$RT_new)
shapiro.test(post$RT_new) ## data are not normal 
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

# safe the full dataset as txt
#setwd("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya")
#write.table(post, "CompleteDatasetConsolidation.txt", quote = F, row.names = F, col.names = T, sep = "\t")


###### Analysis #####
# read in the dataframe 
#setwd("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya")
#post <- as.data.frame(read.delim("CompleteDatasetConsolidation.txt", stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))

# setting variables
post$Subject_nr <- as.factor(post$Subject_nr)
post$Condition <- as.factor(post$Condition)
post$Errorfact <- as.factor(post$Error)
post$ConsolidationGroup <- as.factor(post$ConsolidationGroup)
post$Item <- as.factor(post$Item)

# checking coding instances with error code "6"
#subset <- post[is.na(post$ErrorDetail)==0 && post$ErrorDetail == 6,] # only one case

###### How many people used articles on each test #####
#table(post$Subject_nr, post$ArticlesPre)

# exclude trials in which articles were used from RT analysis 
for (i in 1:nrow(post)){
  if (is.na(post$ArticlesPost[i]) == 0 && is.na(post$ArticlesPre[i]) == 0){
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

# check how many trials per person we have left
table(post[is.na(post$RTdiff)==0,]$Subject_nr)
table(post[is.na(post$RTdiff)==0,]$Subject_nr, post[is.na(post$RTdiff)==0,]$Condition) # per condition
# 719 and 729 and 738 are problematic with less than 15 trials in one or both conditions
# 716 and 718 and 736 and 737 are also problematic because the conditions are very unbalanced, 13 vs. 23

# percentage of article traisl per person per condition
article <- post[(is.na(post$ArticlesPre)==0 | is.na(post$ArticlesPost)==0),]
article1 <- (table(article$Subject_nr, article$Condition)/23)*100
article2 <- (table(article$Subject_nr)/46)*100

# possibly exclude people that don't have enough data left 
## THIS NEEDS TO BE DISCUSSED WITH JAMES AND KRISTIN
post<-post[!(post$Subject_nr== 719 | post$Subject_nr == 738 | post$Subject_nr == 716 | post$Subject_nr == 718 | post$Subject_nr == 736 | post$Subject_nr == 737),]
post$Subject_nr <- droplevels(post$Subject_nr)

########## Plots with GGplot ###########

### Error rates ###
# histogram of results 
#hist(post$Error)

ddply(post, .(Condition, Subject_nr, ConsolidationGroup), 
      summarise, N=length(Error), 
      mean   = mean(Error, na.rm = TRUE), 
      sem = sd(Error, na.rm = TRUE)/sqrt(N)) -> aggregatedError

aggregated_means_error <- ddply(post, .(Condition, ConsolidationGroup), 
                                summarise,
                                condition_mean = mean(Error,na.rm = T),
                                condition_sem = sd(Error,na.rm = T)/sqrt(length(Error[!is.na(Error)])))

aggregatedError <- merge(aggregatedError, aggregated_means_error, by = c("Condition", "ConsolidationGroup"))

aggregated_means_error$condition_mean <- aggregated_means_error$condition_mean*100
aggregated_means_error$condition_sem <- aggregated_means_error$condition_sem*100
aggregatedError$mean <- aggregatedError$mean*100
aggregatedError$sem <- aggregatedError$sem*100
aggregatedError$condition_mean <- aggregatedError$condition_mean*100
aggregatedError$condition_sem <- aggregatedError$condition_sem*100

#lineplot <- ggplot(aggregatedError, aes(y = mean, x = Condition, fill = Subject_nr, group = ConsolidationGroup))
#lineplot + geom_point(color="darkgrey") +
#  geom_line(color="darkgrey") +
#  geom_point(aes(y = condition_mean,
#                 color = Condition), color="black") +
#  geom_text(aes(label=Subject_nr)) +
#  geom_line(aes(y = condition_mean,color="red")) +
#  geom_errorbar(aes(ymin=condition_mean-condition_sem,
#                    ymax=condition_mean+condition_sem,
#                    color = "red",
#                    na.rm = T),
#                width = 0.5) +
#  facet_wrap(~ConsolidationGroup) +
#  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
#  scale_x_discrete(labels=c("Interference", "No interference"), breaks = 1:2, expand = c(0.1,0.1)) +
#  ylab("Percentage correctly recalled words in Spanish") +
#  theme_bw()

barplot <- ggplot(aggregated_means_error, aes(y = condition_mean, x = Condition, group = ConsolidationGroup))
barplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  coord_cartesian(ylim=c(0,4)) +
  facet_wrap(~ConsolidationGroup) +
  scale_x_discrete(labels=c("Interference", "No Interference"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Percentage incorrectly recalled words in English") +
  scale_fill_grey(labels=c("Interference","No Interference")) +
  theme_bw()

# Accuracy instead of error rates

post$Accuracy <- 1-post$Error
ddply(post, .(Condition, Subject_nr), 
      summarise, N=length(Accuracy), 
      mean   = mean(Accuracy, na.rm = TRUE), 
      sem = sd(Accuracy, na.rm = TRUE)/sqrt(N)) -> aggregatedAcc

aggregated_means_acc <- ddply(post, .(Condition), 
                              summarise,
                              condition_mean = mean(Accuracy,na.rm = T),
                              condition_sem = sd(Accuracy,na.rm = T)/sqrt(length(Accuracy[!is.na(Accuracy)])))

aggregatedAcc <- merge(aggregatedAcc, aggregated_means_acc, by = c("Condition"))

aggregated_means_acc$condition_mean <- aggregated_means_acc$condition_mean*100
aggregated_means_acc$condition_sem <- aggregated_means_acc$condition_sem*100
aggregatedAcc$mean <- aggregatedAcc$mean*100
aggregatedAcc$sem <- aggregatedAcc$sem*100
aggregatedAcc$condition_mean <- aggregatedAcc$condition_mean*100
aggregatedAcc$condition_sem <- aggregatedAcc$condition_sem*100


barplot <- ggplot(aggregated_means_acc, aes(y = condition_mean, x = Condition, fill = Condition))
barplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  coord_cartesian(ylim=c(90,100)) +
  scale_x_discrete(labels=c("Interference", "No Interference"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Percentage correctly recalled words in English") +
  scale_fill_grey(labels=c("Interference","No Interference")) +
  theme_bw()


#### Plot for RTs raw ###
ddply(post, .(Condition, Subject_nr, ConsolidationGroup), 
      summarise, N=length(RT_new), 
      mean   = mean(RT_new, na.rm = TRUE), 
      sem = sd(RT_new, na.rm = TRUE)/sqrt(N)) -> aggregatedrt

aggregated_means_rt<- ddply(post, .(Condition, ConsolidationGroup), 
                            summarise,
                            condition_mean = mean(RT_new,na.rm = T),
                            condition_sem = sd(RT_new,na.rm = T)/sqrt(length(RT_new[!is.na(RT_new)])))

aggregatedrt <- merge(aggregatedrt, aggregated_means_rt, by = c("Condition", "ConsolidationGroup"))


# lineplot <- ggplot(aggregatedrt, aes(y = mean, x = Condition, group = ConsolidationGroup))
# lineplot + geom_point(color="darkgrey") +
#   geom_line(color="darkgrey") +
#   geom_point(aes(y = condition_mean,
#                  color = Condition), color="black") +
#   geom_text(aes(label=Subject_nr)) +
#   geom_line(aes(y = condition_mean,color="red")) +
#   geom_errorbar(aes(ymin=condition_mean-condition_sem,
#                     ymax=condition_mean+condition_sem,
#                     color = "red",
#                     na.rm = T),
#                 width = 0.5) +
#   facet_wrap(~ConsolidationGroup) +
#   theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
#   scale_x_discrete(labels=c("Interference", "No interference"), breaks = 1:2, expand = c(0.1,0.1)) +
#   ylab("Naming latencies in ms") +
#   # scale_color_manual(guide=F, "Frequency Condition", values=c("dodgerblue4","firebrick"),labels=c("High","Low")) +
#   theme_bw()

barplot <- ggplot(aggregated_means_rt, aes(y = condition_mean, x = Condition, fill = Condition, group = ConsolidationGroup))
barplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  facet_wrap(~ConsolidationGroup) +
  scale_x_discrete(labels=c("Interference", "No Interference"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Naming latencies in ms") +
  scale_fill_grey(labels=c("Interference","No Interference")) +
  theme_bw()

#### Plot for RTs difference ###
ddply(post, .(Condition, Subject_nr, ConsolidationGroup), 
      summarise, N=length(RTdiff), 
      mean   = mean(RTdiff, na.rm = TRUE), 
      sem = sd(RTdiff, na.rm = TRUE)/sqrt(N)) -> aggregatedrtdiff

aggregated_means_rtdiff<- ddply(post, .(Condition, ConsolidationGroup), 
                                summarise,
                                condition_mean = mean(RTdiff,na.rm = T),
                                condition_sem = sd(RTdiff,na.rm = T)/sqrt(length(RTdiff[!is.na(RTdiff)])))

aggregatedrtdiff <- merge(aggregatedrtdiff, aggregated_means_rtdiff, by = c("Condition", "ConsolidationGroup"))


# lineplot <- ggplot(aggregatedrtdiff, aes(y = mean, x = Condition, group = Consolidationgroup))
# lineplot + geom_point(color="darkgrey") +
#   geom_line(color="darkgrey") +
#   geom_point(aes(y = condition_mean,
#                  color = Condition), color="black") +
#   geom_text(aes(label=Subject_nr)) +
#   geom_line(aes(y = condition_mean,color="red")) +
#   geom_errorbar(aes(ymin=condition_mean-condition_sem,
#                     ymax=condition_mean+condition_sem,
#                     color = "red",
#                     na.rm = T),
#                 width = 0.5) +
#   facet_wrap(~ConsolidationGroup) +
#   theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
#   scale_x_discrete(labels=c("Interference", "No interference"), breaks = 1:2, expand = c(0.1,0.1)) +
#   ylab("Naming latencies in ms") +
#   # scale_color_manual(guide=F, "Frequency Condition", values=c("dodgerblue4","firebrick"),labels=c("High","Low")) +
#   theme_bw()

barplot <- ggplot(aggregated_means_rtdiff, aes(y = condition_mean, x = Condition, fill = Condition, group = ConsolidationGroup))
barplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  facet_wrap(~ConsolidationGroup) +
  scale_x_discrete(labels=c("Interference", "No Interference"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Speed up in naming latencies from English pre- to posttest (in ms)") +
  scale_fill_grey(labels=c("Interference","No Interference")) +
  theme_bw()


###### Stats ######

# setting contrasts to the mean of each condition 
contrasts(post$Condition) <- c(-0.5,0.5)
contrasts(post$ConsolidationGroup) <- c(-0.5,0.5)
# turning my factors into numerical factors reflecting a dummy coding 
post$ConditionN <- (-(as.numeric(post$Condition)-2))-0.5
post$ConsolidationGroupN <- (-(as.numeric(post$ConsolidationGroup)-2))-0.5


###### Accuracy after interference #####

## Full model with maximal random effects structure
modelfull <- glmer(Error ~ ConditionN*ConsolidationGroupN + (1|Item) + (1+ConditionN|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = post)
summary(modelfull)
# the model converges with the maximal justifyable random effects structure, but the random slope is perfectly correlated with the intercept for subject, so we leave the slope out
modelfullfinal <- glmer(Error ~ ConditionN*ConsolidationGroupN + (1|Item) + (1|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = post)
summary(modelfullfinal)

# model comparisons to obtain chi-square p-values, note that these p-values are slightly different from the ones that the summary function returns, that's because they are calculated differently, but they usually lead to the same conclusions, and the method we use here is recommended for small sample sizes
modelcond <- glmer(Error ~ ConditionN*ConsolidationGroupN -ConditionN + (1|Item) + (1|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = post)
anova(modelfullfinal, modelcond)
modelconsol <- glmer(Error ~ ConditionN*ConsolidationGroupN -ConsolidationGroupN + (1|Item) + (1|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = post)
anova(modelfullfinal, modelconsol)
modelinteraction <- glmer(Error ~ ConditionN*ConsolidationGroupN -ConditionN:ConsolidationGroupN + (1|Item) + (1|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = post)
anova(modelfullfinal, modelinteraction)

## Simple Anova for accuracy
# aggregate data over subjects 
agg <- aggregate(post$Error, by = list(post$Subject_nr, post$Condition, post$ConsolidationGroup), FUN = mean, na.rm = T)
colnames(agg) <- c("Subject_nr", "Condition", "ConsolidationGroup", "Error")
## Arcsine transformed error rates
anova <- aov(asin(sqrt(agg$Error)) ~ Condition*ConsolidationGroup, data = agg)
summary(anova)


###### Modelling for RTs #####
# simple Anova for RTs (log-transformed)
# first aggregate data
aggrt <- aggregate(post$RT_new_log, by = list(post$Subject_nr, post$Condition, post$ConsolidationGroup), FUN = mean, na.rm = T)
colnames(aggrt) <- c("Subject_nr", "Condition", "ConsolidationGroup", "RT_new_log")
anova_rt <- aov(RT_new_log ~ Condition*ConsolidationGroup, data = aggrt)
summary(anova_rt)

## Full model on log transformed data 
# Full model with maximum random effects structure 
# We take the log of the reaction times because the distribution is very non-normal, and we subtract 2000ms because that's the lowest value there is currently (due to 2s delay), log transform works better if there are values close to 0 and between 0-1
modelRT2full <- lmer(RTdifflog ~ ConditionN*ConsolidationGroupN + (1|Item) + (1+ConditionN|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = post)
summary(modelRT2full)

## Model reporting - fullest model above
# First let's take out the main effect for Condition (-Condition below in the code)
modelRT2Condition <- lmer(RTdifflog ~ ConditionN*ConsolidationGroupN - ConditionN + (1|Item) + (1+ConditionN|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = post)
anova(modelRT2full, modelRT2Condition)
modelRT2Consol <- lmer(RTdifflog ~ ConditionN*ConsolidationGroupN - ConsolidationGroupN + (1|Item) + (1+ConditionN|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = post)
anova(modelRT2full, modelRT2Consol)
modelRT2Interaction <- lmer(RTdifflog ~ ConditionN*ConsolidationGroupN - ConditionN:ConsolidationGroupN + (1|Item) + (1+ConditionN|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = post)
anova(modelRT2full, modelRT2Interaction)


### Correlations with LexTale score, interference performance 
# LexTale
library(tidyr)
data_list <- list()
for (i in 1:length(A)){
  pNumber = A[i]
  wd <- paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/BACK-UP/", pNumber, "/", sep="")
  setwd(wd)
  infile1 <- paste(pNumber,"score_LexTale.txt",sep="_")
  
  currentFile <- as.data.frame(read.delim(infile1, sep = "\t", header = T, skipNul = TRUE))
  
  data_list[[i]] <- currentFile
  
  print(A[i])
}
lextale <- rbindlist(data_list)

lextalescore <- lextale[-seq(1, nrow(lextale), 2),]
lextalescore <- separate(data = lextalescore, col = Number.of.correctly.identified.words..19, into = c("text", "score"), sep = "\\: ")
lextalescore[,2] <- as.numeric(unlist(lextalescore[,2]))

Pnames <- A
lextalescore$text <- Pnames

#### Interference Phase: Spanish adaptive learning and posttest ####
#### Adaptive learning task ####
# count number of exposure and learning success after the frist two rounds of this test
#B <- c(601:603, 608, 610:620, 622:631)# no adaptive picture naming fuile available for pp 604
data_list <- list()
for (i in 1:length(A)){
  pNumber = A[i]
  #setwd(file.path("//cnas.ru.nl/Wrkgrp/L2-Attrition-Mickan/RESULTS_EXP1/", pNumber))
  wd <- paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/BACK-UP/", pNumber, "/", pNumber, "_PicNamingAdap", sep="")
  setwd(wd)
  infile1 <- paste(pNumber,"LearnPicNaming_A.txt",sep="_")
  
  currentFile <- as.data.frame(read.delim(infile1, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  
  if (length(currentFile[currentFile$Error == 999,]$Error) > 0){
    currentFile[currentFile$Error == 999,]$Error<-1
  }
  
  data_list[[i]] <- currentFile
  
  print(A[i])
}
adap <- rbindlist(data_list)
blocks <- data.frame(tapply(adap$Block_nr, adap$Subject_nr,max))

adaptive <- adap[adap$Block_nr <3,] # keep only the second block
#adaptive <- adaptive[adaptive$Block_nr >1,]
successAdap <- 1-tapply(adaptive$Error, adaptive$Subject_nr,mean)

#### Exposure per item/pp ####
exposures<-data.frame(table(adap$Item, adap$Subject_nr))
exposures <- exposures[exposures$Freq != 0,]
exposures$Freq <- exposures$Freq + 6
expavg <- data.frame(tapply(exposures$Freq, exposures$Var2, mean))

######## Forgetting score ########
# difference between error rates in interference and no interfernce condition
forgetting <- data.frame(tapply(post$Error, list(post$Subject_nr, post$Condition), mean, na.rm = T))
forgetting$Difference <- forgetting$X2 - forgetting$X1
forgetting2 <- data.frame(tapply(post$RTdiff, list(post$Subject_nr, post$Condition), mean, na.rm = T))
forgetting2$Difference <- forgetting2$X1 - forgetting2$X2
forgetting$ForgettingRT <- forgetting2$Difference
forgetting$Interference_RT <- forgetting2$X1
forgetting$NoInterference_RT <- forgetting2$X2
colnames(forgetting) <- c("Interference_Error", "NoInterference_Error","Difference_Error", "Difference_RT", "Interference_RT","NoInterference_RT")

### Mean learning success at posttest in Spanish
data_list <- list()
data_list2 <- list()
for (i in 1:length(A)){
  pNumber = A[i]
  #setwd(file.path("//cnas.ru.nl/Wrkgrp/L2-Attrition-Mickan/RESULTS_EXP1/", pNumber))
  wd <- paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/BACK-UP/", pNumber, "/", pNumber, "_Posttest_a", sep="")
  setwd(wd)
  infile1 <- paste(pNumber,"Posttest_A.txt",sep="_")
  
  currentFile <- as.data.frame(read.delim(infile1, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  
  if (length(currentFile[currentFile$Error == 999,]$Error) > 0){
    currentFile[currentFile$Error == 999,]$Error<-1
  }
  
  data_list[[i]] <- currentFile
  
  wd2 <- paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/BACK-UP/", pNumber, "/", pNumber, "_Posttest_b", sep="")
  setwd(wd2)
  infile2 <- paste(pNumber,"Posttest_B.txt",sep="_")
  
  currentFile2 <- as.data.frame(read.delim(infile2, stringsAsFactors=FALSE, sep = "\t", header = T, skipNul = TRUE))
  
  if (length(currentFile2[currentFile2$Error == 999,]$Error) > 0){
    currentFile2[currentFile2$Error == 999,]$Error<-1
  }
  
  data_list2[[i]] <- currentFile2
  
  print(A[i])
}

posttestSpanishA <- rbindlist(data_list)
posttestSpanishB <- rbindlist(data_list2)

#learning success posttest A
m1a <- 100-(tapply(posttestSpanishA$Error, posttestSpanishA$Subject_nr, mean)*100)
m2a <- tapply(posttestSpanishA$VoiceOnset, posttestSpanishA$Subject_nr, mean)
#learning success posttest B
m1b <- 100-(tapply(posttestSpanishB$Error, posttestSpanishB$Subject_nr, mean)*100)
m2b <- tapply(posttestSpanishB$VoiceOnset, posttestSpanishB$Subject_nr, mean)


### Correlations #### 
correlations <- matrix(nrow = length(A), ncol = 11)
for (i in 1:length(A)) {
  pNumber = A[i]
  correlations[i,1] <- pNumber
  correlations[i,2] <- forgetting[i,3]                                    # Forgetting score error rate
  correlations[i,3] <- forgetting[i,4]                                    # Forgetting score RT
  correlations[i,4] <- m1a[[i]]                                              # Learning success Spanish posttest A
  correlations[i,5] <- m1b[[i]]                                              # Learning success Spanish posttest b
  correlations[i,5] <- blocks[[i,1]]                                         # Number of blocks in adaptive learning
  correlations[i,7] <- successAdap[[i]]                                    # Percent learned after second adaptive round
  correlations[i,8] <- expavg[[i,1]]                                       # Average exposures to items (minimum 8)
  #num <- which(tolower(as.character(rownames(pretest)))== pNumber)
  #correlations[i,9] <- pretest[[num,1]]                                # Pretest percent known among first 101 words
  #correlations[i,9] <- LBQ$SRmean[i]                                      # Self-ratings average Spanish  
  #correlations[i,10] <- LBQ$SpanishExposureLengthMonth[i]                  # Spanish exposure in month
  #correlations[i,11] <- LBQ$FreqUseTotal[i]                                # Amount of time spent with spanish per week
  correlations[i,9] <- m2a[[i]]                                      
  correlations[i,10] <- m2b[[i]]                                      
  num <- which(tolower(as.character(rownames(lextalescore)))== pNumber)
  correlations[i,11] <- lextalescore[[i,2]]                                   # Lextale score
}
as.data.frame(correlations)->correlations
colnames(correlations) <- c("Pnumber","Forgetting_Error","Forgetting_RT","MeanLearnSpaA","MeanLearnSpaB", "BlocksinAdap","AdapSuccess", "AvgExp", "MeanLearnSpaA2", "MeanLearnSpaB2", "Lextale")

require(Hmisc)
corrtable <- round(rcorr(correlations[,2:11]),2)

cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}

pvalues <- cor.test.p(corrtable)

for (i in 2:length(correlations)) {
  for (j in 2:length(correlations)) {
    if (pvalues[i,j]<0.05){
    } else {
      pvalues[i,j] = NA
      corrtable[i,j]=NA
    }
  }}

pvalues <- pvalues[-1,-1]
corrtable <- corrtable[-1,-1]
corrtable[corrtable==1] <- NA
pvalues[pvalues==0]<- NA

### Language background questionnaire ###
LBQ <- read.delim(here("LBQ_Honours.txt"))
# subset to only those participants that are included in the analyses
LBQfin <- LBQ[(LBQ$Subj %in% A),]

# age 
mean(LBQfin$Age)
sd(LBQfin$Age)

# AoA
mean(LBQfin$EnglishAoA)

# LoE
mean(LBQfin$EnglishLengthExposure)

# Frequency of use 
mean(LBQfin$EnglishFreqSpeakingMin)
mean(LBQfin$EnglishFreqListeningMin)
mean(LBQfin$EnglishFreqReadingMin)
mean(LBQfin$EnglishFreqWritingMin)

# Proficiency 
mean(LBQfin$EnglishProficiencySpeaking)
mean(LBQfin$EnglishProficiencyListening)
mean(LBQfin$EnglishProficiencyReading)
mean(LBQfin$EnglishProficiencyWriting)

#### Pretest English performance ####
data_list <- list()
for (i in 1:length(A)){
  pNumber <- A[i]
  wd <- paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/BACK-UP/", pNumber,"/",pNumber,"_Pretest/", sep="")
  setwd(wd)
  infile1 <- paste(pNumber,"Pretest.txt",sep="_")
  pretest <- as.data.frame(read.delim(infile1, sep = "\t", header = T, skipNul = TRUE))
  
  data_list[[i]] <- pretest
  
  print(A[i])
}

pre <- rbindlist(data_list)

pre$Subject_nr <- as.factor(pre$Subject_nr)
pre$Condition <- as.factor(pre$Condition)
pre$Trial_nr <- as.factor(pre$Trial_nr)
pre$English_Label <- as.factor(pre$English_Label)

mean(tapply(pre$Unknown, pre$Subject_nr, mean))
pretest <- as.data.frame(tapply(pre$Unknown, pre$Subject_nr, mean))
cor.test(pretest$`tapply(pre$Unknown, pre$Subject_nr, mean)`, lextalescore$score)

for (i in 1:nrow(LBQ)){
  LBQ$MeanProfEng[i] <- sum(LBQ$EnglishProficiencyListening[i], LBQ$EnglishProficiencyReading[i], LBQ$EnglishProficiencySpeaking[i], LBQ$EnglishProficiencyWriting[i])/4}
LBQfin <- LBQ[(LBQ$Subj %in% A),]
cor.test(pretest$`tapply(pre$Unknown, pre$Subject_nr, mean)`, LBQfin$EnglishAoA)

