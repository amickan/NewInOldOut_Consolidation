### Analysis with honours data: comparing the no consolidation group with the Honours group 

## if you've loaded in the consolidation data, subset the dataframe to only the no consolidation group 
post[post$ConsolidationGroup=="NoConsolidation",]->noconsol
#noconsol[,-c(13)] -> noconsol
#noconsol<-noconsol[!(noconsol$Subject_nr== 729 | noconsol$Subject_nr== 719 | noconsol$Subject_nr == 738 | noconsol$Subject_nr == 716 | noconsol$Subject_nr == 718 | noconsol$Subject_nr == 736 | noconsol$Subject_nr == 737),]
postrt[postrt$ConsolidationGroup=="NoConsolidation",]->noconsolrt
#noconsolrt[,-c(13)] -> noconsolrt

# or for full analysis 
katya <- post
katyart <- postrt

## read in Honours data and preprocess accordingly 
setwd("U:/PhD/EXPERIMENT 3 -Honours/Script/AnalysisScript")
post <- read.table("CompleteDataset_Honours.txt", header = T)

# new corrected for reaction time
post$RT_new_log <- log(post$RT_new)
post$RTdiff <- post$RT_pre - post$RT_new
post$Prelog <- log(post$RT_pre)
post$RTdifflog <- post$Prelog - post$RT_new_log
post$ConsolidationGroup <- "Honours"

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
noconsol2 = noconsol[, c("Subject_nr", "Trial_nr", "Item", "Spanish_Label", "Condition", "VoiceOnset", "Error", "ErrorDetail","RT_new","RT_pre","ArticlesPre","ArticlesPost","RT_new_log","RTdiff","Prelog","RTdifflog", "ConsolidationGroup")]
rbind(post, noconsol2) -> complete

noconsol2rt = noconsolrt[, c("Subject_nr", "Trial_nr", "Item", "Spanish_Label", "Condition", "VoiceOnset", "Error", "ErrorDetail","RT_new","RT_pre","ArticlesPre","ArticlesPost","RT_new_log","RTdiff","Prelog","RTdifflog", "ConsolidationGroup")]
rbind(postrt, noconsol2rt) -> completert

# to look at the full dataset
katya = katya[, c("Subject_nr", "Trial_nr", "Item", "Spanish_Label", "Condition", "VoiceOnset", "Error", "ErrorDetail","RT_new","RT_pre","ArticlesPre","ArticlesPost","RT_new_log","RTdiff","Prelog","RTdifflog", "ConsolidationGroup")]
katyart = katyart[, c("Subject_nr", "Trial_nr", "Item", "Spanish_Label", "Condition", "VoiceOnset", "Error", "ErrorDetail","RT_new","RT_pre","ArticlesPre","ArticlesPost","RT_new_log","RTdiff","Prelog","RTdifflog", "ConsolidationGroup")]
rbind(post, katya) -> complete
rbind(post, katyart) -> completert


# setting variables
complete$Subject_nr <- as.factor(complete$Subject_nr)
complete$Condition <- as.factor(complete$Condition)
complete$Errorfact <- as.factor(complete$Error)
complete$ConsolidationGroup <- as.factor(complete$ConsolidationGroup)
complete$Item <- as.factor(complete$Item)

completert$Subject_nr <- as.factor(completert$Subject_nr)
completert$Condition <- as.factor(completert$Condition)
completert$Errorfact <- as.factor(completert$Error)
completert$ConsolidationGroup <- as.factor(completert$ConsolidationGroup)
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
complete$ConsolidationGroup <- droplevels((complete$ConsolidationGroup))
# setting contrasts to the mean of each condition 
#contrasts(complete$Condition)  <- contrasts(complete$Condition)  / 2
#contrasts(complete$Group) <- contrasts(complete$Group)  / 3
contrasts(complete$Condition) <- c(-0.5,0.5)
contrasts(complete$ConsolidationGroup) <- c(-0.5,0.5)

# turning my factors into numerical factors reflecting a dummy coding 
complete$ConditionN <- (-(as.numeric(complete$Condition)-2))-0.5
complete$ConsolidationGroupN <- (-(as.numeric(complete$ConsolidationGroup)-2))-0.5

###### Accuracy after interference #####

## Full model with maximal random effects structure
modelfull <- glmer(Error ~ Condition*ConsolidationGroup + (1|Item) + (1+Condition|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = complete)
summary(modelfull)

modelint <- glmer(Error ~ ConsolidationGroupN + (1|Item) + (1|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = complete[complete$Condition=="1",])
summary(modelnocon)

modelnoint <- glmer(Error ~ ConsolidationGroupN + (1|Item) + (1|Subject_nr), family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data = complete[complete$Condition=="2",])
summary(modelhon)

## Full model on log transformed data 
modelRT2full <- lmer(RTdifflog ~ ConditionN*ConsolidationGroupN + (1|Item) + (1+ConditionN|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = complete)
summary(modelRT2full)

modelRTnocon <- lmer(RTdifflog ~ ConditionN + (1|Item) + (1|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = complete[complete$Group=="NoConsolidation",])
summary(modelRTnocon)

modelRThon <- lmer(RTdifflog ~ ConditionN + (1|Item) + (1|Subject_nr), control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)),data = complete[complete$Group=="Honours",])
summary(modelRThon)


### Comparing groups 
setwd("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya")
consolidation <- read.delim("GroupMeasures_ConsolidationProject.txt")
setwd("U:/PhD/EXPERIMENT 3 -Honours")
honours <- read.delim("Honours_group_data.txt")

#merge dataframes
consolidation <- consolidation[,-c(3,12)]
colnames(consolidation)[2] <- "MeanLearnSpa1"
colnames(consolidation)[10] <- "MeanLearnSpa2"
colnames(consolidation)[12] <- "Group"
combined <- rbind(consolidation, honours)

m <- aov(MeanLearnSpa1 ~ Group, data=combined)
m <- aov(BlocksinAdap ~ Group, data=combined)
m <- aov(AvgExp ~ Group, data=combined)
m <- aov(PretestScore ~ Group, data=combined) ## sig.
m <- aov(EnglishAoA ~ Group, data=combined) 
m <- aov(SRPEnglish ~ Group, data=combined)
m <- aov(FreqEnglish ~ Group, data=combined)
m <- aov(Lextale ~ Group, data=combined)
summary(m)

tapply(combined$PretestScore, combined$Group, mean)

## Honours study forgetting effect
forgettingH <- data.frame(tapply(post$Error, list(post$Subject_nr, post$Condition), mean, na.rm = T))
forgettingH$Difference <- forgettingH$X1 - forgettingH$X2
forgettingH2 <- data.frame(tapply(postrt$RTdiff, list(postrt$Subject_nr, postrt$Condition), mean, na.rm = T))
forgettingH2$Difference <- forgettingH2$X2 - forgettingH2$X1
forgettingH$ForgettingRT <- NA
forgettingH$Interference_RT <- NA
forgettingH$NoInterference_RT <- NA

for (i in 1: nrow(forgettingH)){
  num <- which(rownames(forgettingH2) == rownames(forgettingH)[i])
  if (length(num) == 0){
    forgettingH$ForgettingRT[i] <- NA
    forgettingH$Interference_RT[i] <- NA
    forgettingH$NoInterference_RT[i] <- NA
  } else {
    forgettingH$ForgettingRT[i] <- forgettingH2$Difference[num]
    forgettingH$Interference_RT[i] <- forgettingH2$X1[num]
    forgettingH$NoInterference_RT[i] <- forgettingH2$X2[num]
  }
}
colnames(forgettingH) <- c("Interference_Error", "NoInterference_Error","Difference_Error", "Difference_RT", "Interference_RT","NoInterference_RT")

forgettingH$PP <- rownames(forgettingH)
comb <- rbind(forgetting, forgettingH)
cor.test(comb$Difference_Error, comb$Difference_RT)
cor.test(forgettingH$Difference_Error, forgettingH$Difference_RT)

### add individual differene predictors to the combined LBQ dataframe above 
for (i in 1:nrow(combined)){
  num <- which(comb$PP == combined$Pnumber[i])
  if (length(num)==0){} else {
    combined$Difference_Error[i] <- comb$Difference_Error[num]
    combined$Difference_RT[i] <- comb$Difference_RT[num]
  }
}

write.table(combined, "Full_groups_data.txt", sep = "\t", quote = F, row.names = F, col.names = T)

corrmat <- combined[,-c(1,12)]
corrdetail <- round(cor(corrmat, use = "complete.obs"), 1)
library(Hmisc)
corrdetail2 <- rcorr(as.matrix(corrmat))  ## no significant correlations with "forgetting" scores

library(ggcorrplot)
ggcorrplot(corrdetail)

# the closest correlation is for Frequency of English use with RT_forgetting
ggplot(aes(x=FreqEnglish, y=Difference_RT, color= Group), data = combined) +
  #geom_point() +
  geom_smooth(method="lm")
ggplot(aes(x=FreqEnglish, y=Difference_RT), data = combined) +
  geom_point() +
  geom_smooth(method="lm")
