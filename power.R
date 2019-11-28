## Power simulations based on results from Katya's master thesis
## How many people do we need to reliably detect an interaction effect?

library(lme4)
library(plyr)

## generate accuracy data
## to do so draw from a binomial distribution

## total observations per condition is si / 4

# d will change in steps of 0.5 % --> 0.005

simFunc<- function(s,d){
  d=d
  i=23       # i = items per condition, s = participants 
  si=s*i*2   # total nr of observations
  
  # draw random samples for each variable around effect from previous experiment  
  ## 4 conditions: Int-Con, Int-NoCon, NoInt-Con, NoInt-NoCon
  # make 4 vectors, with per person 23 items 
  ## get s*i samples per condition = s*23
  intcon <-rbinom(i*s, size = 1, prob = 0.955-d) # we're gonna turn down the size of this difference by d 
  intnocon <-rbinom(i*s, size = 1, prob = 0.969) 
  nointcon <-rbinom(i*s, size = 1, prob = 0.989)
  nointnocon <-rbinom(i*s, size = 1, prob = 0.984)
  
  ## set up vars for subs and items
  ss <- rep(rep(1:(s*2), each=i),2)                # participants
  ii <- rep(c(rep(1:i,s),rep((i+1):(i*2),s)),2)    # items

  ## FOR EXPERIMENT 1
  # create data frame with all vectors
  Error <- c(intcon,intnocon,nointcon,nointnocon)
  Consolidation <- rep(c(rep("Con",i*s),rep("NoCon",i*s)),2)
  Interference <- c(rep("I",(i*s*2)),rep("N",(i*s*2)))
  
  ds <- cbind(Error,Consolidation,Interference,ss,ii)
  ds <- as.data.frame(ds)
  ds$Error <- as.numeric(as.character(ds$Error))
  
  #set variables in dataframe to factors
  ds$Consolidation <- as.factor(ds$Consolidation)
  ds$Interference <- as.factor(ds$Interference)
  ds$ss <- as.factor(ds$ss)
  ds$ii <- as.factor(ds$ii)
  
  #setting contrasts
  contrasts(ds$Interference) <- c(-0.5,0.5)
  contrasts(ds$Consolidation) <- c(-0.5,0.5)
  # turning my factors into numerical factors reflecting a dummy coding 
  ds$InterferenceN <- (-(as.numeric(ds$Interference)-2))-0.5
  ds$ConsolidationN <- (-(as.numeric(ds$Consolidation)-2))-0.5
  
  
  ### FOR EXPERIMENT 1: fit models for interaction effect
  fit1 <- suppressMessages(glmer(Error ~ InterferenceN*ConsolidationN + (1|ss) + (1|ii),family = "binomial", control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), data=ds))
  fit2 <- suppressMessages(glmer(Error ~ InterferenceN*ConsolidationN - InterferenceN:ConsolidationN + (1|ss) + (1|ii), control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)), family = "binomial", data=ds))
  
  round(anova(fit2,fit1)[2,8],3)
}

## set a progress bar
pb <- txtProgressBar(max=1000)

### run multiple times (each time simulating a new dataset)
# for the number of subjects I have, can I observe a difference of the same size or smaller?
d=1
power=rep(0,8)

for(d in 1:8){
  setTxtProgressBar(pb,0)
  out2 <- replicate(1000, {setTxtProgressBar(pb,getTxtProgressBar(pb)+1); simFunc(23,(d-1)*0.005) })
  #save the outputs
  power[d] = mean(out2<0.05)
}

# how much power do I have to detect an effect of the size in Exp 1 or smaller 
power

# testing for different sample sizes
s = 1
d = 1
p2way = matrix(nrow=6,ncol=6)
#p2d=rep(0,10)

for(d in 1:6){
  for(s in 1:6){
    setTxtProgressBar(pb,0)
    print(paste("D = ", d, " , S = ", s))
    out2 <- replicate(1000, {setTxtProgressBar(pb,getTxtProgressBar(pb)+1); simFunc(30+s*2,(d-1)*0.005) })
    #save the outputs
    p2way[d,s] = mean(out2<0.05)
  }
  #p2d <- rbind(cbind(rep((d-1)*50,8),p2way),p2d)
}

colnames(p2way)=c(30,32,34,36,38,40)
d = c(1:6)
rownames(p2way)=c((d-1)*0.005)
p2way

#contour(y=c(22,24,26,28,30,32),x=c((d-1)*50),p2way,main="Power with 20 items per condition",xlab="Difference in ms from observed effect size",ylab="N Subjects")

###### Double check the simulations outcome #####
#### check averages from simulated data
tapply(ds$Error, list(ds$Consolidation, ds$Interference), mean) -> means
means[,2] - means[,1]

### plot the simulated data (to check whether the function does the right thing)
ddply(ds, .(Interference, ss, Consolidation), 
      summarise, N=length(Error), 
      mean   = mean(Error, na.rm = TRUE), 
      sem = sd(Error, na.rm = TRUE)/sqrt(N)) -> aggregatedError

aggregated_means_error <- ddply(ds, .(Interference, Consolidation), 
                                summarise,
                                condition_mean = mean(Error,na.rm = T),
                                condition_sem = sd(Error,na.rm = T)/sqrt(length(Error[!is.na(Error)])))

aggregatedError <- merge(aggregatedError, aggregated_means_error, by = c("Interference", "Consolidation"))

aggregated_means_error$condition_mean <- aggregated_means_error$condition_mean*100
aggregated_means_error$condition_sem <- aggregated_means_error$condition_sem*100
aggregatedError$mean <- aggregatedError$mean*100
aggregatedError$sem <- aggregatedError$sem*100
aggregatedError$condition_mean <- aggregatedError$condition_mean*100
aggregatedError$condition_sem <- aggregatedError$condition_sem*100


barplot <- ggplot(aggregated_means_error, aes(y = condition_mean, x = Interference, fill = Interference, group = Consolidation))
barplot + geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=condition_mean-condition_sem,
                    ymax=condition_mean+condition_sem),
                width = 0.5, position=position_dodge(0.9)) +
  theme(axis.text = element_text(size = 20), axis.title = element_text(size = 20)) + 
  coord_cartesian(ylim=c(90,100)) +
  facet_wrap(~Consolidation) +
  scale_x_discrete(labels=c("Interference", "No Interference"), breaks = 1:2, expand = c(0.1,0.1)) +
  ylab("Percentage incorrectly recalled words in English") +
  scale_fill_grey(labels=c("Interference","No Interference")) +
  theme_bw()
