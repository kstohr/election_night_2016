##########################################################################################

##  FIGURE 1

## Estimated density of anti-black stereotypes among non-black respondents
## to the 2008 NAES (N=19,325) and the 2008 CCAP (N=17,825) surveys.
## For ease of comparison, the measures have been normalized so that the
## mean value is 0 and the standard deviation is 1 for each distribution.
## The vertical line represents the median value of the pooled distribution,
## which is less than zero due to the positive skew of responses in both datasets.
## Larger (i.e. positive) numbers represent more negative stereotyping.

##########################################################################################

## Library required packages
library(foreign)
library(weights)

## Set working directory
setwd('')

##########################################################################################

## READ IN DATA

ccap <- read.csv('data/ccap_08.csv', stringsAsFactors=F)
naes <- read.csv('data/naes_online_08.csv', stringsAsFactors=F)

## STEREOTYPING VARIABLE

## CCAP

## Subset to Asian, Hispanic, or White (N=17,825)
ccap <- ccap[ccap$race=='White' | ccap$race=='Asian' | ccap$race=='Hispanic',]

## Create own group stereotype variables
ccap$lazy.own <- ifelse(ccap$race=='Asian', ccap$lazy.asian,
                        ifelse(ccap$race=='Hispanic', ccap$lazy.hisp,
                               ifelse(ccap$race=='White', ccap$lazy.white, NA)))

ccap$intel.own <- ifelse(ccap$race=='Asian', ccap$intel.asian,
                        ifelse(ccap$race=='Hispanic', ccap$intel.hisp,
                               ifelse(ccap$race=='White', ccap$intel.white, NA)))

## Calculate the difference between own group and black score
ccap$lazy <- ccap$lazy.own - ccap$lazy.black
ccap$intel <- ccap$intel.own - ccap$intel.black

## Aggregate the differences
ccap$prejudice <- ccap$lazy + ccap$intel

## Standardize prejudice measure
ccap$prej.norm <- stdz(ccap$prejudice, weight=ccap$weight)


## NAES

## Subset to non black (N=19,325)
naes <- naes[naes$race!=2 & complete.cases(naes$race),]

## Calculate the difference between own group and black score
naes$hw <- naes$work.oth - naes$work.own
naes$intel <- naes$intel.oth - naes$intel.own
naes$trust <- naes$trust.oth - naes$trust.own

## Aggregate the three dummies
naes$prejudice <- naes$hw + naes$intel + naes$trust

## Standardize prejudice measure
naes$prej.norm <- stdz(naes$prejudice, weight=naes$wt.345)

##########################################################################################

## DENSITY PLOTS

pdf('figure_1.pdf', width=6, height=6)
plot(density(naes$prej.norm, bw=0.25, na.rm=T), main='', xlab='Bandwidth = 0.25')
lines(density(ccap$prej.norm, bw=0.25, na.rm=T), lty=2)
abline(v=mean(c(median(naes$prej.norm), median(ccap$prej.norm, na.rm=T))))
abline(h=0)
legend(1, 0.7, legend=c('NAES (N=19,325)', 'CCAP (N=17,825)'), lty=c(1,2), cex=0.8, bty='n')
dev.off()


## Clear environment
rm(list=ls())

##########################################################################################
## END OF FILE
##########################################################################################