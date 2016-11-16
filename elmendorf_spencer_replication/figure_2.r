##########################################################################################

##  FIGURE 2

## Whites’ stereotypes of minorities. The histogram represents the difference
## between whites’ stereotypes of their own race and stereotypes of Asians, blacks
## and Latinos. The scale runs from -14 to 14 and captures the sum of respondents'
## placement of each race on a seven point scale for perceived intelligence and 
## work ethic. Data: White respondents to the 2008 CCAP.

## Code last updated July 27, 2013

##########################################################################################

## Library in required packages
library(foreign)
library(weights)
library(plyr)
library(ggplot2)

## Set working directory
setwd('')

## Read in ccap data
ccap <- read.csv('data/ccap_08.csv', stringsAsFactors=F)

##########################################################################################
## BLACK PREJUDICE
##########################################################################################

## Subset to White
ccap.black <- ccap[ccap$race=='White',]

## Calculate the difference between own group and black score
ccap.black$lazy <- ccap.black$lazy.white - ccap.black$lazy.black
ccap.black$intel <- ccap.black$intel.white - ccap.black$intel.black

## Aggregate the differences
ccap.black$prej.black <- ccap.black$lazy + ccap.black$intel

## Subset to complete cases
ccap.black <- ccap.black[complete.cases(ccap.black$prej.black),]

## Standardize measure (for KS test)
ccap.black$pweight <- ccap.black$weight/sum(ccap.black$weight)
ccap.black$prej.black.stdz <- stdz(ccap.black$prej.black, weight=ccap.black$weight)

##########################################################################################
## LATINO PREJUDICE
##########################################################################################

## Subset to White
ccap.hisp <- ccap[ccap$race=='White',]

## Calculate the difference between own group and Latino score
ccap.hisp$lazy <- ccap.hisp$lazy.white - ccap.hisp$lazy.hisp
ccap.hisp$intel <- ccap.hisp$intel.white - ccap.hisp$intel.hisp

## Aggregate the differences
ccap.hisp$prej.hisp <- ccap.hisp$lazy + ccap.hisp$intel

## Subset to complete cases
ccap.hisp <- ccap.hisp[complete.cases(ccap.hisp$prej.hisp),]

## Standardize measure (for KS test)
ccap.hisp$pweight <- ccap.hisp$weight/sum(ccap.hisp$weight)
ccap.hisp$prej.hisp.stdz <- stdz(ccap.hisp$prej.hisp, weight=ccap.hisp$weight)

##########################################################################################
## ASIAN PREJUDICE
##########################################################################################

## Subset to White
ccap.asian <- ccap[ccap$race=='White',]

## Calculate the difference between own group and Asian score
ccap.asian$lazy <- ccap.asian$lazy.white - ccap.asian$lazy.asian
ccap.asian$intel <- ccap.asian$intel.white - ccap.asian$intel.asian

## Aggregate the differences
ccap.asian$prej.asian <- ccap.asian$lazy + ccap.asian$intel

## Subset to complete cases
ccap.asian <- ccap.asian[complete.cases(ccap.asian$prej.asian),]

## Standardize measure (for KS test)
ccap.asian$pweight <- ccap.asian$weight/sum(ccap.asian$weight)
ccap.asian$prej.asian.stdz <- stdz(ccap.asian$prej.asian, weight=ccap.asian$weight)

##########################################################################################

## Combine prejudice measures into one object

x <- data.frame(prejudice=c(ccap.asian$prej.asian, ccap.black$prej.black, ccap.hisp$prej.hisp),
                race=c(rep('Asian', dim(ccap.asian)[1]), rep('Black', dim(ccap.black)[1]), rep('Latino', dim(ccap.hisp)[1])))
x$prejudice <- as.numeric(x$prejudice)

y <- ddply(x, .(race, prejudice), 'nrow', .drop=F)

y$race.sum <- NA
y$race.sum[y$race=='Asian'] = dim(ccap.asian)[1]
y$race.sum[y$race=='Black'] = dim(ccap.black)[1]
y$race.sum[y$race=='Latino'] = dim(ccap.hisp)[1]

y$prop <- y$nrow/y$race.sum

##########################################################################################

## CREATE PLOT

pdf('figure_2.pdf', height=5, width=6)
ggplot(y, aes(x=prejudice, y=prop, fill=race)) + geom_bar(stat='identity', position='dodge') +
  theme_bw() + scale_fill_manual('Race', values=c('black', 'gray40', 'lightgray')) +
  xlab("Difference between own race and minority groups \n (White respondents)") + ylab('Proportion')
dev.off()


## Clear environment
rm(list=ls())

##########################################################################################
## END OF FILE
##########################################################################################