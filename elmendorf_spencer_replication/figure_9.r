##########################################################################################

##  FIGURE 9
# Average Latino stereotype of self-identified white, Asian American,
# and African American respondents in the 2008 CCAP sample. Estimates
# created by disaggregation after applying survey weights. Error bars denote
# 95% confidence intervals.

##########################################################################################

## Library required packages
library(foreign)
library(ggplot2)
library(weights)

## Set working directory
setwd('~/Dropbox/Papers/Elmendorf-Spencer/Data/elmendorf_spencer_replication/')


## READ IN CCAP DATA
ccap <- read.csv('data/ccap_08.csv', stringsAsFactors=F)

## ANTI-LATINO STEREOTYPE
## Subset to Asian, Black, or White (N=18,125)
ccap.hisp <- ccap[ccap$race=='White' | ccap$race=='Asian' | ccap$race=='Black',]

## Subset to just Whites (for paper)
#ccap.hisp <- ccap[ccap$race=='White' ,]

## Create own group stereotype variables
ccap.hisp$lazy.own <- ifelse(ccap.hisp$race=='Asian', ccap.hisp$lazy.asian,
                        ifelse(ccap.hisp$race=='Black', ccap.hisp$lazy.black,
                               ifelse(ccap.hisp$race=='White', ccap.hisp$lazy.white, NA)))

ccap.hisp$intel.own <- ifelse(ccap.hisp$race=='Asian', ccap.hisp$intel.asian,
                         ifelse(ccap.hisp$race=='Black', ccap.hisp$intel.black,
                                ifelse(ccap.hisp$race=='White', ccap.hisp$intel.white, NA)))

## Calculate the difference between own group and Latino score
ccap.hisp$lazy <- ccap.hisp$lazy.own - ccap.hisp$lazy.hisp
ccap.hisp$intel <- ccap.hisp$intel.own - ccap.hisp$intel.hisp

## Aggregate the differences
ccap.hisp$prej.hisp <- ccap.hisp$lazy + ccap.hisp$intel

## Subset to complete cases
ccap.hisp <- ccap.hisp[complete.cases(ccap.hisp$prej.hisp),]

## Normalize measure
ccap.hisp$prej.hisp.stdz <- stdz(ccap.hisp$prej.hisp, weight=ccap.hisp$weight)

##########

## Aggregate by state

latino.mean <- aggregate(prej.hisp~state, data=ccap.hisp, FUN=mean)

## Section 5
latino.mean$section.5 <- 2
latino.mean$section.5[c(5,10,23,30,34,42)] <- 1 # partially covered states
latino.mean$section.5[c(1:3,11,19,25,41,44,47)] <- 0 # fully covered states

## Standard Errors
# SEs by state sigma
sample <- data.frame(table(ccap.hisp$state))
names(sample) <- c('state', 'sample.n')

ssname <- unique(ccap.hisp$state)

latino.mean <- merge(latino.mean, sample)
temp <- NA
for (i in ssname){
  temp2 <- sd(ccap.hisp$prej.hisp.stdz[ccap.hisp$state==i], na.rm=T)
  temp <- rbind(temp, temp2)
}
mean.se <- data.frame(state=ssname, state.sigma=temp[-1,])
latino.mean <- merge(latino.mean, mean.se)

latino.mean$state.low.68 <- latino.mean$prej.hisp-(latino.mean$state.sigma/sqrt(latino.mean$sample.n))
latino.mean$state.high.68 <- latino.mean$prej.hisp+(latino.mean$state.sigma/sqrt(latino.mean$sample.n))
latino.mean$state.low.95 <- latino.mean$prej.hisp-(2*latino.mean$state.sigma/sqrt(latino.mean$sample.n))
latino.mean$state.high.95 <- latino.mean$prej.hisp+(2*latino.mean$state.sigma/sqrt(latino.mean$sample.n))

#########################################################################################

## CREATE PLOT

latino.mean$state.order <- reorder(latino.mean$state, latino.mean$prej.hisp)

p.mean <- qplot(prej.hisp, state.order, data=latino.mean, shape=as.factor(section.5), col=as.factor(section.5)) + 
  xlim(c(-.5, 2)) + ggtitle('Anti-Latino stereotyping') + ylab("") + 
  xlab("Average difference between stereotype of \n one's own race and Latinos") +
  geom_vline(xintercept=mean(latino.mean$prej.hisp), linetype=2, colour="black") +
  scale_shape_manual("Section 4", values=c(19, 1, 19), labels=c("Covered", "Partially covered", "Not covered")) +
  scale_colour_manual("Section 4", values=c('firebrick2', 'black', 'black'), labels=c("Covered", "Partially covered", 'Not covered')) +
  theme(panel.background=element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y=element_text(size=7, colour="gray20"), axis.title.x=element_text(size=10)) + geom_point() +
  geom_errorbarh(aes(xmin=latino.mean$state.low.95, xmax=latino.mean$state.high.95), height=0, size=0.4)


## SAVE PLOT
pdf('figure_9.pdf', height=6.25, width=6.5)
print(p.mean)
dev.off()


## Clear environment
rm(list=ls())

##########################################################################################
## END OF FILE
##########################################################################################