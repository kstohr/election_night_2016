##########################################################################################

##  FIGURE 3 AND TABLE 1
##  Code last updated: July 26, 2013

##########################################################################################

## TABLE 1

## Linear probability models predicting votes for or against Obama. All
## non-dichotomous independent variables (prejudice, age, education, and income)
## have been normalized to facilitate interpretation.

## Model (A) predicts the probability of voting for Obama in the 2008 general election.

## Model (B) predicts the probability of voting for Obama in the 2008 primary election and
## only includes primary voters.

## Model (C) predicts the probability that a person who voted for John Kerry in 2004 defected
## and did not vote for Obama in 2008. The model only includes people who voted for Kerry in 2004.

##########################################################################################

## Library in required packages
library(foreign)
library(ggplot2)
library(apsrtable)
library(weights)

## Set working directory where replication files are saved
setwd('')

##########################################################################################
## READ IN DATA
##########################################################################################

## 2008 NAES
naes <- read.csv('data/naes_online_08.csv', stringsAsFactors=F)

## Subset to non-black
naes <- subset(naes, naes$race != 2)
  
## Create 3 category race (White, Hispanic, Other)
naes$race.wha <- naes$race
naes$race.wha[naes$race==4] <- 2
naes$race.wha[naes$race==5] <- 3

## CREATE STEREOTYPING VARIABLE

## Calculate the difference between own group and black score
naes$hw <- naes$work.oth - naes$work.own
naes$intel <- naes$intel.oth - naes$intel.own
naes$trust <- naes$trust.oth - naes$trust.own

## Aggregate the three dummies
naes$prejudice <- naes$hw + naes$intel + naes$trust

## Normalize relevant variables for tables
naes$prej.norm <- stdz(naes$prejudice, weight=naes$wt.345)
naes$age.norm <- stdz(naes$age, weight=naes$wt.345)
naes$educ.norm <- stdz(naes$educ, weight=naes$wt.345)
naes$hhinc.norm <- stdz(naes$hhinc, weight=naes$wt.345)

##########################################################################################

## 2008 CCAP
ccap <- read.csv('data/ccap_08.csv', stringsAsFactors=F)

## Subset to non-black (N=18,500)
ccap <- subset(ccap, ccap$race != 'Black')

## Create 3 category race (White, Hispanic, Asian)
ccap$race.wha <- NA
ccap$race.wha[ccap$race=='White'] <- 1
ccap$race.wha[ccap$race=='Hispanic'] <- 2
ccap$race.wha[ccap$race=='Asian'] <- 3

## CREATE STEREOTYPING VARIABLE

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

##########################################################################################

## POOL NAES AND CCAP

## Recalibrate CCAP left-right measures to NAES scale

ccap$party.id <- ccap$party.id + 4
ccap$ideol <- ifelse(ccap$ideol==2, ccap$ideol + 5,
                ifelse(ccap$ideol==1, ccap$ideol + 4,
                  ifelse(ccap$ideol==-1, ccap$ideol + 4,
                    ifelse(ccap$ideol==-2, ccap$ideol + 3, NA))))
          

c2 <- ccap[intersect(names(ccap), names(naes))]
n2 <- naes[intersect(names(ccap), names(naes))]

x <- rbind(c2, n2)

##########################################################################################

## MERGE IN STATE-LEVEL PREDICTORS
state <- read.csv('data/state_level_predictors.csv', stringsAsFactors=F)
x <- merge(state, x, by.x='state.fips', by.y='fips', all.x=T, all.y=F)
rm(state)

##########################################################################################

## MODELS FOR TABLE 1

model.1b <- lm(obama.vote.gen ~ prej.norm + age.norm + ideol + party.id + female + as.factor(race.wha) + educ.norm + hhinc.norm + as.factor(region),
                  data=x)

model.2b <- lm(obama.vote.prim ~ prej.norm + age.norm + ideol + party.id + female + as.factor(race.wha) + educ.norm + hhinc.norm + as.factor(region),
                  data=x)

model.3b <- lm(obama.kerry.dum2 ~ prej.norm + age.norm + ideol + party.id + female + as.factor(race.wha) + educ.norm + hhinc.norm + as.factor(region),
                  data=x)

apsrtable(model.1b, model.2b, model.3b, stars="default", digits=2, order="longest",
        coef.names=c("(Intercept)", "Negative stereotype", "Age", "Conservativeness", "Party ID (7 point)", 
                "Female", "R is Hispanic", "R is ``other'' race", "Education", "Income",
                "Midwest", "South", "West"))

##########################################################################################

##  FIGURE 3

# Probability of vote choice conditional on varying levels of racial
# stereotyping (intervals of 0.5 standard deviation). Probabilities are estimated
# using linear probability models that control for ideology, party identification,
# age, sex, race, education, income and region (see Table 1). Reported
# probabilities are relative to the national median. Vertical bars represent 95%
# confidence intervals and points represent values that are statistically
# significantly different from zero.

##########################################################################################

x$prej.bin <- NA

## Reference category = -0.25sd to +0.25sd
x$prej.bin[x$prej.norm >= (median(x$prej.norm, na.rm=T) - 0.25*sd(x$prej.norm, na.rm=T)) & 
              x$prej.norm <= (median(x$prej.norm, na.rm=T) + 0.25*sd(x$prej.norm, na.rm=T))] <- 5

## Four categories of negative (i.e. non-prejudice) scores
x$prej.bin[x$prej.norm >= (median(x$prej.norm, na.rm=T) - 0.75*sd(x$prej.norm, na.rm=T)) & 
              x$prej.norm < (median(x$prej.norm, na.rm=T) - 0.25*sd(x$prej.norm, na.rm=T))] <- 4

x$prej.bin[x$prej.norm >= (median(x$prej.norm, na.rm=T) - 1.25*sd(x$prej.norm, na.rm=T)) & 
              x$prej.norm < (median(x$prej.norm, na.rm=T) - 0.75*sd(x$prej.norm, na.rm=T))] <- 3

x$prej.bin[x$prej.norm >= (median(x$prej.norm, na.rm=T) - 1.75*sd(x$prej.norm, na.rm=T)) & 
              x$prej.norm < (median(x$prej.norm, na.rm=T) - 1.25*sd(x$prej.norm, na.rm=T))] <- 2

x$prej.bin[x$prej.norm < (median(x$prej.norm, na.rm=T) - 1.75*sd(x$prej.norm, na.rm=T))] <- 1

## Four categories of positive (i.e. prejudice) scores
x$prej.bin[x$prej.norm <= (median(x$prej.norm, na.rm=T) + 0.75*sd(x$prej.norm, na.rm=T)) & 
              x$prej.norm > (median(x$prej.norm, na.rm=T) + 0.25*sd(x$prej.norm, na.rm=T))] <- 6

x$prej.bin[x$prej.norm <= (median(x$prej.norm, na.rm=T) + 1.25*sd(x$prej.norm, na.rm=T)) & 
              x$prej.norm > (median(x$prej.norm, na.rm=T) + 0.75*sd(x$prej.norm, na.rm=T))] <- 7

x$prej.bin[x$prej.norm <= (median(x$prej.norm, na.rm=T) + 1.75*sd(x$prej.norm, na.rm=T)) & 
              x$prej.norm > (median(x$prej.norm, na.rm=T) + 1.25*sd(x$prej.norm, na.rm=T))] <- 8

x$prej.bin[x$prej.norm > (median(x$prej.norm, na.rm=T) + 1.75*sd(x$prej.norm, na.rm=T))] <- 9


# Choose the median quantile as reference category:
x <- within(x, prej.dec <- relevel(as.factor(prej.bin), ref=5))

decile.1 <- lm(obama.vote.gen ~ prej.dec + age.norm + ideol + party.id + 
                female + as.factor(race) + educ.norm + hhinc.norm + as.factor(region) - 1,
              data=x)

decile.2 <- lm(obama.vote.prim ~ prej.dec + age.norm + ideol + party.id + 
                female + as.factor(race) + educ.norm + hhinc.norm + as.factor(region) - 1,
              data=x)

decile.3 <- lm(obama.kerry.dum2 ~ prej.dec + age.norm + ideol + party.id + 
                female + as.factor(race) + educ.norm + hhinc.norm + as.factor(region) - 1,
              data=x)

### With intercept
decile.11 <- lm(obama.vote.gen ~ prej.dec + age.norm + ideol + party.id + 
                female + as.factor(race) + educ.norm + hhinc.norm + as.factor(region),
              data=x) # Note intercept = 0.5820976

decile.21 <- lm(obama.vote.prim ~ prej.dec + age.norm + ideol + party.id + 
                female + as.factor(race) + educ.norm + hhinc.norm + as.factor(region),
              data=x) # Intercept = 0.409085

decile.31 <- lm(obama.kerry.dum2 ~ prej.dec + age.norm + ideol + party.id + 
                female + as.factor(race) + educ.norm + hhinc.norm + as.factor(region),
              data=x) # Intercept = 0.1569678

n1 <- table(x$prej.dec)[2]; p1 <- format((prop.table(table(x$prej.dec))[2]*100), digits=2)
n2 <- table(x$prej.dec)[3]; p2 <- format((prop.table(table(x$prej.dec))[3]*100), digits=2)
n3 <- table(x$prej.dec)[4]; p3 <- format((prop.table(table(x$prej.dec))[4]*100), digits=2)
n4 <- table(x$prej.dec)[5]; p4 <- format((prop.table(table(x$prej.dec))[5]*100), digits=2)
n5 <- table(x$prej.dec)[1]; p5 <- format((prop.table(table(x$prej.dec))[1]*100), digits=2)
n6 <- table(x$prej.dec)[6]; p6 <- format((prop.table(table(x$prej.dec))[6]*100), digits=2)
n7 <- table(x$prej.dec)[7]; p7 <- format((prop.table(table(x$prej.dec))[7]*100), digits=2)
n8 <- table(x$prej.dec)[8]; p8 <- format((prop.table(table(x$prej.dec))[8]*100), digits=2)
n9 <- table(x$prej.dec)[9]; p9 <- format((prop.table(table(x$prej.dec))[9]*100), digits=2)


## PLOT IN BASE GRAPHICS

gen.pt <- decile.1$coefficients[c(2:5,1,6:9)]-decile.1$coefficients[1]
prim.pt <- decile.2$coefficients[c(2:5,1,6:9)]-decile.2$coefficients[1]
def.pt <- decile.3$coefficients[c(2:5,1,6:9)]-decile.3$coefficients[1]

pdf('figure_3.pdf', height=5.5, width=7.5)

plot(c(1:9), gen.pt, type='l', lwd=2, ylim=c(-.24,.2), xlim=c(1,9.6), axes=F, 
    xlab='Distribution of prejudice score', ylab='Probability of vote choice')
axis(2, at=c(-0.2, -0.1, 0, 0.1, 0.2))
axis(1, at=c(1, 5, 9), labels=c('Most favorable\nstereotype', 'Median', 'Most negative\nstereotype'), mgp=c(3,1.5,0), cex.axis=0.8)
abline(h=c(seq(-0.15, 0.25, 0.05)), lty='dotted', col='lightgray')
abline(h=0)
lines(c(1:9), prim.pt, lty=2, lwd=2)
lines(c(1:9), def.pt, lty=3, lwd=2)
legend(4, 0.18, legend=c('Obama in `08 general', 'Obama in `08 primary', 'Kerry defections'), bty='n',
  lty=c(1,2,3), lwd=c(1,1,1), cex=0.7)
text(1.1, -0.23, paste(n1,'\n(',p1,'%)', sep=''), cex=0.7)
text(2, -0.23, paste(n2,'\n(',p2,'%)', sep=''), cex=0.7)
text(3, -0.23, paste(n3,'\n(',p3,'%)', sep=''), cex=0.7)
text(4, -0.23, paste(n4,'\n(',p4,'%)', sep=''), cex=0.7)
text(5, -0.23, paste(n5,'\n(',p5,'%)', sep=''), cex=0.7)
text(6, -0.23, paste(n6,'\n(',p6,'%)', sep=''), cex=0.7)
text(7, -0.23, paste(n7,'\n(',p7,'%)', sep=''), cex=0.7)
text(8, -0.23, paste(n8,'\n(',p8,'%)', sep=''), cex=0.7)
text(9, -0.23, paste(n9,'\n(',p9,'%)', sep=''), cex=0.7)

## Add error bars for just the significant points
segments(x0=0.96, x1=0.96, y0=gen.pt[1]-1.96*summary(decile.11)$coef[1,2], y1=gen.pt[1]+1.96*summary(decile.11)$coef[1,2])
segments(x0=1.96, x1=1.96, y0=gen.pt[2]-1.96*summary(decile.11)$coef[2,2], y1=gen.pt[2]+1.96*summary(decile.11)$coef[2,2])
segments(x0=2.96, x1=2.96, y0=gen.pt[3]-1.96*summary(decile.11)$coef[3,2], y1=gen.pt[3]+1.96*summary(decile.11)$coef[3,2])
segments(x0=3.96, x1=3.96, y0=gen.pt[4]-1.96*summary(decile.11)$coef[4,2], y1=gen.pt[4]+1.96*summary(decile.11)$coef[4,2])
segments(x0=5.96, x1=5.96, y0=gen.pt[6]-1.96*summary(decile.11)$coef[6,2], y1=gen.pt[6]+1.96*summary(decile.11)$coef[6,2])
segments(x0=6.96, x1=6.96, y0=gen.pt[7]-1.96*summary(decile.11)$coef[7,2], y1=gen.pt[7]+1.96*summary(decile.11)$coef[7,2])
segments(x0=7.96, x1=7.96, y0=gen.pt[8]-1.96*summary(decile.11)$coef[8,2], y1=gen.pt[8]+1.96*summary(decile.11)$coef[8,2])
segments(x0=8.96, x1=8.96, y0=gen.pt[9]-1.96*summary(decile.11)$coef[9,2], y1=gen.pt[9]+1.96*summary(decile.11)$coef[9,2])

segments(x0=1.04, x1=1.04, y0=prim.pt[1]-1.96*summary(decile.21)$coef[1,2], y1=prim.pt[1]+1.96*summary(decile.21)$coef[1,2])
segments(x0=2.04, x1=2.04, y0=prim.pt[2]-1.96*summary(decile.21)$coef[2,2], y1=prim.pt[2]+1.96*summary(decile.21)$coef[2,2])
segments(x0=3.04, x1=3.04, y0=prim.pt[3]-1.96*summary(decile.21)$coef[3,2], y1=prim.pt[3]+1.96*summary(decile.21)$coef[3,2])
segments(x0=4.04, x1=4.04, y0=prim.pt[4]-1.96*summary(decile.21)$coef[4,2], y1=prim.pt[4]+1.96*summary(decile.21)$coef[4,2])
segments(x0=6.04, x1=6.04, y0=prim.pt[6]-1.96*summary(decile.21)$coef[6,2], y1=prim.pt[6]+1.96*summary(decile.21)$coef[6,2])
segments(x0=7.04, x1=7.04, y0=prim.pt[7]-1.96*summary(decile.21)$coef[7,2], y1=prim.pt[7]+1.96*summary(decile.21)$coef[7,2])
segments(x0=8.04, x1=8.04, y0=prim.pt[8]-1.96*summary(decile.21)$coef[8,2], y1=prim.pt[8]+1.96*summary(decile.21)$coef[8,2])
segments(x0=9.04, x1=9.04, y0=prim.pt[9]-1.96*summary(decile.21)$coef[9,2], y1=prim.pt[9]+1.96*summary(decile.21)$coef[9,2])

segments(x0=1, x1=1, y0=def.pt[1]-1.96*summary(decile.31)$coef[1,2], y1=def.pt[1]+1.96*summary(decile.31)$coef[1,2])
segments(x0=2, x1=2, y0=def.pt[2]-1.96*summary(decile.31)$coef[2,2], y1=def.pt[2]+1.96*summary(decile.31)$coef[2,2])
segments(x0=3, x1=3, y0=def.pt[3]-1.96*summary(decile.31)$coef[3,2], y1=def.pt[3]+1.96*summary(decile.31)$coef[3,2])
segments(x0=4, x1=4, y0=def.pt[4]-1.96*summary(decile.31)$coef[4,2], y1=def.pt[4]+1.96*summary(decile.31)$coef[4,2])
segments(x0=6, x1=6, y0=def.pt[6]-1.96*summary(decile.31)$coef[6,2], y1=def.pt[6]+1.96*summary(decile.31)$coef[6,2])
segments(x0=7, x1=7, y0=def.pt[7]-1.96*summary(decile.31)$coef[7,2], y1=def.pt[7]+1.96*summary(decile.31)$coef[7,2])
segments(x0=8, x1=8, y0=def.pt[8]-1.96*summary(decile.31)$coef[8,2], y1=def.pt[8]+1.96*summary(decile.31)$coef[8,2])
segments(x0=9, x1=9, y0=def.pt[9]-1.96*summary(decile.31)$coef[9,2], y1=def.pt[9]+1.96*summary(decile.31)$coef[9,2])

## Add POINTS for stat sig values
points(x=0.96, y=gen.pt[1], pch=19); points(x=0.96, y=gen.pt[1], cex=0.8, pch=1, col='white')
points(x=5.96, y=gen.pt[6], pch=19); points(x=5.96, y=gen.pt[6], cex=0.8, pch=1, col='white')
points(x=6.96, y=gen.pt[7], pch=19); points(x=6.96, y=gen.pt[7], cex=0.8, pch=1, col='white')
points(x=7.96, y=gen.pt[8], pch=19); points(x=7.96, y=gen.pt[8], cex=0.8, pch=1, col='white')
points(x=8.96, y=gen.pt[9], pch=19); points(x=8.96, y=gen.pt[9], cex=0.8, pch=1, col='white')

points(x=6.04, y=prim.pt[6], pch=19); points(x=6.04, y=prim.pt[6], cex=0.8, pch=1, col='white')
points(x=7.04, y=prim.pt[7], pch=19); points(x=7.04, y=prim.pt[7], cex=0.8, pch=1, col='white')
points(x=8.04, y=prim.pt[8], pch=19); points(x=8.04, y=prim.pt[8], cex=0.8, pch=1, col='white')
points(x=9.04, y=prim.pt[9], pch=19); points(x=9.04, y=prim.pt[9], cex=0.8, pch=1, col='white')

points(x=7, y=def.pt[7], pch=19); points(x=7, y=def.pt[7], cex=0.8, pch=1, col='white')
points(x=8, y=def.pt[8], pch=19); points(x=8, y=def.pt[8], cex=0.8, pch=1, col='white')
points(x=9, y=def.pt[9], pch=19); points(x=9, y=def.pt[9], cex=0.8, pch=1, col='white')

## And label the final value for each model
gen <- format(decile.1$coefficients[9]-decile.1$coefficients[1], digits=2)
prim <- format(decile.2$coefficients[9]-decile.2$coefficients[1], digits=3)
def <- format(decile.3$coefficients[9]-decile.3$coefficients[1], digits=3)
text(9.5, decile.1$coefficients[9]-decile.1$coefficients[1], gen, cex=0.8)
text(9.5, decile.2$coefficients[9]-decile.2$coefficients[1], prim, cex=0.8)
text(9.5, decile.3$coefficients[9]-decile.3$coefficients[1], def, cex=0.8)


dev.off()


## Clear the environment
rm(list=ls())

##########################################################################################
## END OF FILE
##########################################################################################