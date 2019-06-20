### Analysis with honours data: comparing the no consolidation group with the Honours group 

## if you've loaded in the consolidation data, subset the dataframe to only the no consolidation group 
post[post$ConsolidationGroup=="NoConsolidation",]->noconsol
noconsol[,-c(13)] -> noconsol
#noconsol<-noconsol[!(noconsol$Subject_nr== 729 | noconsol$Subject_nr== 719 | noconsol$Subject_nr == 738 | noconsol$Subject_nr == 716 | noconsol$Subject_nr == 718 | noconsol$Subject_nr == 736 | noconsol$Subject_nr == 737),]
postrt[postrt$ConsolidationGroup=="NoConsolidation",]->noconsolrt
noconsolrt[,-c(13)] -> noconsolrt

## read in Honours data and preprocess accordingly 
setwd("U:/PhD/EXPERIMENT 3 -Honours/Script/AnalysisScript")
post <- read.table("CompleteDataset_Honours.txt", header = T)

# new corrected for reaction time
post$RT_new_log <- log(post$RT_new)
post$RTdiff <- post$RT_pre - post$RT_new
post$Prelog <- log(post$RT_pre)
post$RTdifflog <- post$Prelog - post$RT_new_log

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
  } 
}

## deleting trials of words that were already known in Spanish before the learning phaser
known <- read.delim("KnownWords.txt")
for (i in 1:nrow(known)){
  pNumber <- known$PP[i]
  num <- which(tolower(post[post$Subject_nr == pNumber,]$Spanish_Label) == tolower(known$Word[i]))
  if (length(num)!= 0 ){
    post[post$Subject_nr == pNumber,]$RT_new[num] <- NA
    post[post$Subject_nr == pNumber,]$RT_pre[num] <- NA
    post[post$Subject_nr == pNumber,]$Prelog[num] <- NA
    post[post$Subject_nr == pNumber,]$RT_new_log[num] <- NA
    post[post$Subject_nr == pNumber,]$RTdiff[num] <- NA
    post[post$Subject_nr == pNumber,]$RTdifflog[num] <- NA
    post[post$Subject_nr == pNumber,]$Error[num] <- NA
  }
}

postrt<-post[!(post$Subject_nr == 604 | post$Subject_nr == 624),]
postrt <- droplevels(postrt)

### append dataframes 
#colorder <- colnames(post)
noconsol2 = noconsol[, c("Subject_nr", "Trial_nr", "Item", "Spanish_Label", "Condition", "VoiceOnset", "Error", "ErrorDetail","RT_new","RT_pre","ArticlesPre","ArticlesPost","RT_new_log","RTdiff","Prelog","RTdifflog")]
rbind(post, noconsol2) -> complete

noconsol2rt = noconsolrt[, c("Subject_nr", "Trial_nr", "Item", "Spanish_Label", "Condition", "VoiceOnset", "Error", "ErrorDetail","RT_new","RT_pre","ArticlesPre","ArticlesPost","RT_new_log","RTdiff","Prelog","RTdifflog")]
rbind(postrt, noconsol2rt) -> completert

# add a group names to the dataframe
complete$Subject_nr <- as.numeric(as.character(complete$Subject_nr))
for (i in 1:nrow(complete)){
  if (complete$Subject_nr[i] > 700 && complete$Subject_nr[i] < 726){
    complete$Group[i] <- "Consolidation"
  } else if (complete$Subject_nr[i] > 725 && complete$Subject_nr[i] < 751) {
    complete$Group[i] <- "NoConsolidation"
  } else if (complete$Subject_nr[i] == 751 || complete$Subject_nr[i] == 753 || complete$Subject_nr[i] == 755) {
    complete$Group[i] <- "Consolidation"
  } else if (complete$Subject_nr[i] == 752 || complete$Subject_nr[i] == 754) {
    complete$Group[i] <- "NoConsolidation"
  } else if (complete$Subject_nr[i] < 700){
    complete$Group[i] <- "Honours"}
}

completert$Subject_nr <- as.numeric(as.character(completert$Subject_nr))
for (i in 1:nrow(completert)){
  if (completert$Subject_nr[i] > 700 && completert$Subject_nr[i] < 726){
    completert$Group[i] <- "Consolidation"
  } else if (completert$Subject_nr[i] > 725 && completert$Subject_nr[i] < 751) {
    completert$Group[i] <- "NoConsolidation"
  } else if (completert$Subject_nr[i] == 751 || completert$Subject_nr[i] == 753 || completert$Subject_nr[i] == 755) {
    completert$Group[i] <- "Consolidation"
  } else if (completert$Subject_nr[i] == 752 || completert$Subject_nr[i] == 754) {
    completert$Group[i] <- "NoConsolidation"
  } else if (completert$Subject_nr[i] < 700){
    completert$Group[i] <- "Honours"}
}

# setting variables
complete$Subject_nr <- as.factor(complete$Subject_nr)
complete$Condition <- as.factor(complete$Condition)
complete$Errorfact <- as.factor(complete$Error)
complete$Group <- as.factor(complete$Group)
complete$Item <- as.factor(complete$Item)

completert$Subject_nr <- as.factor(completert$Subject_nr)
completert$Condition <- as.factor(completert$Condition)
completert$Errorfact <- as.factor(completert$Error)
completert$Group <- as.factor(completert$Group)
completert$Item <- as.factor(completert$Item)

#table(complete$Group, complete$Subject_nr)

## Plotting error rates
ddply(complete, .(Condition, Subject_nr, Group), 
      summarise, N=length(Error), 
      mean   = mean(Error, na.rm = TRUE), 
      sem = sd(Error, na.rm = TRUE)/sqrt(N)) -> aggregatedError

aggregated_means_error <- ddply(complete, .(Condition, Group), 
                                summarise,
                                condition_mean = mean(Error,na.rm = T),
                                condition_sem = sd(Error,na.rm = T)/sqrt(length(Error[!is.na(Error)])))

aggregatedError <- merge(aggregatedError, aggregated_means_error, by = c("Condition", "Group"))

aggregated_means_error$condition_mean <- aggregated_means_error$condition_mean*100
aggregated_means_error$condition_sem <- aggregated_means_error$condition_sem*100
aggregatedError$mean <- aggregatedError$mean*100
aggregatedError$sem <- aggregatedError$sem*100
aggregatedError$condition_mean <- aggregatedError$condition_mean*100
aggregatedError$condition_sem <- aggregatedError$condition_sem*100

barplot <- ggplot(aggregated_means_error, aes(y = condition_mean, x = Condition, group = Group))
barplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  coord_cartesian(ylim=c(0,4)) +
  facet_wrap(~Group) +
  scale_x_discrete(labels=c("Interference", "No Interference"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Percentage incorrectly recalled words in English") +
  scale_fill_grey(labels=c("Interference","No Interference")) +
  theme_bw()


#### Plot for RTs difference ###
ddply(completert, .(Condition, Subject_nr, Group), 
      summarise, N=length(RTdiff), 
      mean   = mean(RTdiff, na.rm = TRUE), 
      sem = sd(RTdiff, na.rm = TRUE)/sqrt(N)) -> aggregatedrtdiff

aggregated_means_rtdiff<- ddply(completert, .(Condition, Group), 
                                summarise,
                                condition_mean = mean(RTdiff,na.rm = T),
                                condition_sem = sd(RTdiff,na.rm = T)/sqrt(length(RTdiff[!is.na(RTdiff)])))

aggregatedrtdiff <- merge(aggregatedrtdiff, aggregated_means_rtdiff, by = c("Condition", "Group"))

barplot <- ggplot(aggregated_means_rtdiff, aes(y = condition_mean, x = Condition, fill = Condition, group = Group))
barplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  facet_wrap(~Group) +
  scale_x_discrete(labels=c("Interference", "No Interference"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Speed up in naming latencies from English pre- to posttest (in ms)") +
  scale_fill_grey(labels=c("Interference","No Interference")) +
  theme_bw()


###### Stats ######
options(contrasts = c("contr.sum", "contr.poly"))
# setting contrasts to the mean of each condition 
contrasts(complete$Condition)  <- contrasts(complete$Condition)  / 2
contrasts(complete$Group) <- contrasts(complete$Group)  / 3
#contrasts(complete$Condition) <- c(-0.5,0.5)
#contrasts(complete$Group) <- c(-0.5,0.5)

# turning my factors into numerical factors reflecting a dummy coding 
#complete$ConditionN <- (-(as.numeric(complete$Condition)-2))-0.5
#complete$GroupN <- (-(as.numeric(complete$Group)-2))-0.5


###### Accuracy after interference #####

## Full model with maximal random effects structure
modelfull <- glmer(Error ~ Condition*Group + (1|Item) + (1+Condition|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = complete)
summary(modelfull)

## Full model on log transformed data 
modelRT2full <- lmer(RTdifflog ~ ConditionN*GroupN + (1|Item) + (1+ConditionN|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = complete)
summary(modelRT2full)
