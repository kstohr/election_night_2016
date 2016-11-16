####################################################################################################

##  FIGURE 4

# Coefficients from a linear probability models predicting agreement
# with the statement "illegal immigrants should be arrested and deported." All
# non-dichotomous independent variables (stereotype, age, education, and
# income) have been normalized to facilitate interpretation.

####################################################################################################

## Library in required packages
library(foreign)
library(ggplot2)
library(weights)
library(apsrtable)

## Set working directory
setwd('')

####################################################################################################

## FIRST ANTI-LATINO

## 2008 CCAP
ccap <- read.csv('data/ccap_08.csv', stringsAsFactors=F)

## Subset to non-Latino
ccap <- subset(ccap, ccap$race != 'Hispanic')

## Create 3 category race (White, Black, Asian)
# Race (1=White, 2=Black, 3=Asian)
ccap$race.3 <- NA
ccap$race.3[ccap$race=='White'] <- 1
ccap$race.3[ccap$race=='Black'] <- 2
ccap$race.3[ccap$race=='Asian'] <- 3

## Create female variable
ccap$female <- ifelse(ccap$sex=='Female', 1, 0)

## Normalize age measure
ccap$age.norm <- stdz(ccap$age, weight=ccap$weight)

## Normalize education measure
# Education (4 category)
ccap$educ[ccap$educ=='No HS'] <- 1
ccap$educ[ccap$educ=='High school graduate'] <- 2
ccap$educ[ccap$educ=='Some college'] <- 3
ccap$educ[ccap$educ=='2 year college'] <- 3
ccap$educ[ccap$educ=='College graduate'] <- 4
ccap$educ[ccap$educ=='Post-grad'] <- 5

ccap$educ.norm <- stdz(as.numeric(ccap$educ), weight=ccap$weight)

## Normalize income measure
ccap$finc[ccap$finc=='less than $10,000'] <- 1
ccap$finc[ccap$finc=='$10,000 - $14,999'] <- 2
ccap$finc[ccap$finc=='$15,000 - $19,999'] <- 3
ccap$finc[ccap$finc=='$20,000 - $24,999'] <- 4
ccap$finc[ccap$finc=='$25,000 - $29,999'] <- 5
ccap$finc[ccap$finc=='$30,000 - $39,999'] <- 6
ccap$finc[ccap$finc=='$40,000 - $49,999'] <- 7
ccap$finc[ccap$finc=='$50,000 - $59,999'] <- 8
ccap$finc[ccap$finc=='$60,000 - $69,999'] <- 9
ccap$finc[ccap$finc=='$70,000 - $79,999'] <- 10
ccap$finc[ccap$finc=='$80,000 - $99,999'] <- 11
ccap$finc[ccap$finc=='$100,000 - $119,999'] <- 12
ccap$finc[ccap$finc=='$120,000 - $149,999'] <- 13
ccap$finc[ccap$finc=='$150,000 or more'] <- 14
ccap$finc[ccap$finc=='Prefer not to say'] <- NA

ccap$finc.norm <- stdz(as.numeric(ccap$finc), weight=ccap$weight)

##########

## DEPENDENT VARIABLE -- IMMIGRATION SELF PLACEMENT

## Illegals should be deported dummy
ccap$deport.dum <- ifelse(ccap$immigration=='Arrested and deported', 1, 0)

## Categorical variable
ccap$imm.cat <- NA
ccap$imm.cat[ccap$immigration=='Arrested and deported'] <- 2
ccap$imm.cat[ccap$immigration=='Fined and allowed to become citizens'] <- 1
ccap$imm.cat[ccap$immigration=="I'm not sure"] <- 0

###################################################################################

## MERGE IN STATE-LEVEL DATA
state <- read.csv('data/state_level_predictors.csv', stringsAsFactors=F)
ccap <- merge(state, ccap, by.x='state.name', by.y='state', all.x=T, all.y=F)
rm(state)

###################################################################################

## CREATE ANTI-LATINO PREJUDICE VARIABLE

## Create own group stereotype variables
ccap$lazy.own <- ifelse(ccap$race=='Asian', ccap$lazy.asian,
                        ifelse(ccap$race=='Black', ccap$lazy.black,
                               ifelse(ccap$race=='White', ccap$lazy.white, NA)))

ccap$intel.own <- ifelse(ccap$race=='Asian', ccap$intel.asian,
                         ifelse(ccap$race=='Black', ccap$intel.black,
                                ifelse(ccap$race=='White', ccap$intel.white, NA)))

## Calculate the difference between own group and hisp score
ccap$lazy <- ccap$lazy.own - ccap$lazy.hisp
ccap$intel <- ccap$intel.own - ccap$intel.hisp

## Aggregate the differences
ccap$prejudice <- ccap$lazy + ccap$intel

## Standardize prejudice measure (optional)
ccap$prej.stdz <- stdz(ccap$prejudice, weight=ccap$weight)


###################################################################################

## MODEL & PLOT

model.1 <- lm(deport.dum ~ prej.stdz + age.norm + ideol + party.id + female + educ.norm + finc.norm + as.factor(region),
              data=ccap)

## CREATE PLOT

coef.vec <- c(model.1$coefficients[2],model.1$coefficients[4:8],model.1$coefficients[3],model.1$coefficients[9:11])
se.vec <- c(.01,.01,.005,.01,.01,.01,.01,.01,.01,.02)
var.names <- c('Anti-Latino stereotype', 'Conservativeness', 'Party ID',
              'Female', 'Education', 'Income', 'Age', 'Midwest',
              'South', 'West')
    
y.axis <- c(length(coef.vec):1)

pdf('figure_4.pdf', width=6, height=5.5)

par(mar=c(2, 9, 0, 0))
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, cex = 1, xlim = c(-0.1,0.3), xaxs = "r", main = "") 
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, y.axis, lwd =  1.5)
axis(1, at = seq(-0.1,0.3,by=.1), labels = NA, tick = T, cex.axis = 1, mgp = c(2,.7,0))
axis(1, at = seq(-0.1,0.3,by=.1), tick = T,cex.axis = 1, mgp = c(2,.7,0))
axis(2, at = y.axis, label = var.names, las = 1, tick = T, ,mgp = c(2,.6,0),cex.axis = 1) 
abline(v=0, lty=2)
abline(v=c(-.1, .1, .2, .3), lty='dotted', col='lightgray')

x.height <- .15
text(x.height, 7, expression(R^{2} == .35), adj = 0, cex = 1) #add text for R-squared
text(x.height, 6, expression(paste("Adjusted ", R^{2} == ".35", "")), adj = 0, cex = 1)
text(x.height, 5, "N = 7,780", adj = 0, cex = 1)

dev.off()


## Clear environment
rm(list=ls())

###################################################################################
## END OF FILE
###################################################################################