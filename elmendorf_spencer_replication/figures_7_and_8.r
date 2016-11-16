####################################################################################################

##  FIGURE 7

# State rankings based on
#   (A) negative stereotyping of blacks by nonblacks,
#   (B) proportion of the population that is black, and
#   (C) racially polarized voting.
# Negative stereotypes are estimated using the 2008 NAES and 2008 CCAP. Black population
# size and proportion are reported in the 2008 American Community Survey. Our meausre
# of racially polarized voting is the absolute difference between votes for Obama among
# black and white voters as reported by respondents to the 2008 NAES and CCAP surveys.
# We estimate black support for Obama using MRP because the sample of black respondents
# is very small in some states. Solid horizontal lines represent 95% confidence intervals
# for estimates in Panels 1 and 3. The horizontal dashed lines denote the top quartile
# of states (12) in each panel.

####################################################################################################

## Library in required packages
library(foreign)
library(ggplot2)
library(apsrtable)
library(arm)
library(weights)

## Set working directory
setwd('')


###################################################################################
###################################################################################
## BLACK POPULATION
###################################################################################
###################################################################################

state <- read.csv('data/state_level_predictors.csv', stringsAsFactors=F)
black.pop <- state[,c(1,5)]


##########################################################################################
##########################################################################################
## RACIALLY POLARIZED VOTING
##########################################################################################
##########################################################################################

## READ IN DATA

###################################################################################
## ENTER NAES DATA
###################################################################################

## 2008 NAES
naes <- read.csv('data/naes_online_08.csv', stringsAsFactors=F)

naes$race.name <- NA
naes$race.name[naes$race==1] <- 'White'
naes$race.name[naes$race==2] <- 'Hispanic'
naes$race.name[naes$race==3] <- 'Other'

## Age (4 category)
naes$age.4 <- 4
naes$age.4[naes$age <= 29] <- 1
naes$age.4[naes$age > 29 & naes$age <= 45] <- 2
naes$age.4[naes$age > 45 & naes$age > 60] <- 3

## Education (4 category)
naes$educ.4 <- 4
naes$educ.4[naes$educ <= 3] <- 1
naes$educ.4[naes$educ > 3 & naes$educ <= 6] <- 2
naes$educ.4[naes$educ == 6] <- 3

## Income (4 category)
naes$inc.4 <- 4
naes$inc.4[naes$hhinc > 0 & naes$hhinc <= 9] <- 1
naes$inc.4[naes$hhinc > 9 & naes$hhinc <= 12] <- 2
naes$inc.4[naes$hhinc > 12 & naes$hhinc <= 15] <- 3


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

ccap$race.name <- ccap$race

# Education (4 category)
ccap$educ.4[ccap$educ=='No HS'] <- 1
ccap$educ.4[ccap$educ=='High school graduate'] <- 1
ccap$educ.4[ccap$educ=='Some college'] <- 2
ccap$educ.4[ccap$educ=='2 year college'] <- 2
ccap$educ.4[ccap$educ=='College graduate'] <- 3
ccap$educ.4[ccap$educ=='Post-grad'] <- 4

## Income (4 category)
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

ccap$finc <- as.numeric(ccap$finc)
ccap$inc.4 <- NA
ccap$inc.4[ccap$finc > 0 & ccap$finc <= 6] <- 1
ccap$inc.4[ccap$finc > 6 & ccap$finc <= 8] <- 2
ccap$inc.4[ccap$finc > 8 & ccap$finc <= 11] <- 3
ccap$inc.4[ccap$finc > 11 & ccap$finc <= 14] <- 4

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

x$race.name[x$race.name=='Asian' | x$race.name=='Middle Eastern' | x$race.name=='Mixed' | x$race.name=='Native American'] <- 'Other'

##########################################################################################

## MERGE IN STATE-LEVEL PREDICTORS
state <- read.csv('data/state_level_predictors.csv', stringsAsFactors=F)
x <- merge(state, x, by.x='state.fips', by.y='fips', all.x=T, all.y=F)
rm(state)

##########################################################################################

## MRP FOR BLACK SHARE OF VOTE FOR OBAMA

## Read in Census file for poststratification
census <- read.csv('data/poststratification_PUMS_state_black.csv', stringsAsFactors=F)

## Run MRP on N bootstrap replications for black share of vote for Obama

N <- 1000 ## Set number of bootstrap runs

ssname <- unique(x$state.name)
rpv.boot <- rep(NA, 51)

for (i in 1:N){
    
    ## Create bootstrap sample
    boot.data <- x[sample(1:nrow(x), size=dim(x)[1], replace=TRUE),]
    
    # MULTI-LEVEL MODEL FOR BLACK VOTES FOR OBAMA BY STATE
  
    glm.out <- lmer(obama.vote.gen ~ (1 | female) + (1 | educ.4) + (1 | inc.4) + (1 | state.name) + (1 | region)
                 + black.pct.08, data=boot.data[boot.data$race.name=='Black',])
  
    # Create vector of state ranefs and then fill in missing ones
    state.ranefs <- array(NA,c(51,1))
    dimnames(state.ranefs) <- list(c(ssname),"effect")
    for(j in ssname){
        state.ranefs[j,1] <- ranef(glm.out)$state.name[j,1]
    }
    state.ranefs[,1][is.na(state.ranefs[,1])] <- 0

    # POSTSTRATIFICATION
    ## Create a prediction for each cell in Census data
    cells <- (fixef(glm.out)['(Intercept)']
              + ranef(glm.out)$female[census$SEX,1]
              + ranef(glm.out)$educ.4[census$EDUC.CAT,1]
              + ranef(glm.out)$inc.4[census$INC.CAT,1]
              + state.ranefs[census$state.name,1]
              + ranef(glm.out)$region[census$region,1] 
              + (fixef(glm.out)['black.pct.08']*census$black.pct.08)
    )
    
    ## Weight the cell predictions by the cell frequency
    weight.mean <- cells * census$cell.percent
    weight.mean <- ifelse(is.na(weight.mean), 0, weight.mean)
    
    ## Calculate state score and state proportions (i.e., weighted average of responses)
    black.vote <- data.frame(state.name=sort(unique(census$state.name)),
                          mrp=as.numeric(tapply(weight.mean,census$state.name,sum)))
    
    ## DISAGGREGATED VOTE SHARE FOR WHITES
    white.vote <- aggregate(obama.vote.gen~state.name, data=boot.data[boot.data$race.name=='White',], FUN=mean)
    
    ## RPV -- ABSOLUTE DIFFERENCE BETWEEN BLACK AND WHITE VOTES
    rpv <- merge(black.vote, white.vote, by='state.name')
    rpv$rpv <- abs(rpv$mrp-rpv$obama.vote.gen)
    rpv.boot <- cbind(rpv.boot, rpv$rpv)

    print(i)
}

rpv <- data.frame(state.name=ssname,
                  rpv.boot.median=(apply(rpv.boot, 1, median, na.rm=T)),
                  rpv.boot.mean=(apply(rpv.boot, 1, mean, na.rm=T)),
                  rpv.low.95=(apply(rpv.boot, 1, quantile, 0.025, na.rm=T)),
                  rpv.high.95=(apply(rpv.boot, 1, quantile, 0.975, na.rm=T))
                  )



##########################################################################################
##########################################################################################
## NEGATIVE STEREOTYPING
##########################################################################################
##########################################################################################

## READ IN DATA

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

## PROPORTION ABOVE MEDIAN, TOP QUARTILE, TOP TEN PERCENT

x$above.median <- ifelse(x$prej.norm >= median(x$prej.norm, na.rm=T), 1, 0)
y.above.median <- aggregate(above.median~state.name, data=x, FUN=mean)

## CALCULATE STANDARD ERRORS

# SEs by state sigma
sample <- data.frame(table(x$state.name))
names(sample) <- c('state.name', 'sample.n')

y.above.median <- merge(y.above.median, sample)

ssname <- unique(x$state.name)

temp <- NA
for (i in ssname){
  temp2 <- sd(x$above.median[x$state.name==i], na.rm=T)
  temp <- rbind(temp, temp2)
}
se.above.median <- data.frame(state.name=ssname, state.sigma=temp[-1,])

y.above.median <- merge(y.above.median, se.above.median)

## Now for SEs
y.above.median$low.95 <- y.above.median$above.median-(2*y.above.median$state.sigma/sqrt(y.above.median$sample.n))
y.above.median$high.95 <- y.above.median$above.median+(2*y.above.median$state.sigma/sqrt(y.above.median$sample.n))

stereotype <- y.above.median[,c(1,2,5,6)];
names(stereotype) <- c('state.name', 'stereotype', 'st.low.95', 'st.high.95')
stereotype$state <- stereotype$state.name

##########################################################################################
##########################################################################################
##########################################################################################

## COMBINE ALL THREE MEASURES AND PLOT

df.1 <- merge(rpv, stereotype)
df <- merge(df.1, black.pop)

df$section.5 <- 2
df$section.5[c(5,10,23,30,34,42)] <- 1 # partially covered states
df$section.5[c(1:3,11,19,25,41,44,47)] <- 0 # fully covered states

## Check correlations between three measures (vary slightly based on number of bootstrap reps)
cor(df$rpv.boot.median, df$black.pct.08, use='complete.obs') # 0.517
cor(df$rpv.boot.median, df$stereotype, use='complete.obs') # 0.775
cor(df$black.pct.08, df$stereotype, use='complete.obs') # 0.420

df$rpv.rank <- reorder(as.character(df$state), df$rpv.boot.mean)
df$black.rank <- reorder(as.character(df$state), df$black.pct.08)
df$stereotype.rank <- reorder(as.character(df$state), df$stereotype)

## Use 'multiplot' function from the Cookbook for R
## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/

multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                       # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
}


p.rpv <- qplot(rpv.boot.mean, rpv.rank, data=df, shape=as.factor(section.5), col=as.factor(section.5)) + 
  xlim(c(0,1)) + ggtitle('Racially polarized voting') + ylab('') + 
  xlab('% Black vote  - % White vote for Obama \n (Absolute value)') +
  geom_hline(yintercept=39.5, linetype=2) +
  scale_shape_manual("Section 4", values=c(19, 1, 19), labels=c("Covered", "Partially covered", "Not covered"), guide='none') +
  scale_colour_manual("Section 4", values=c('firebrick2', 'black', 'black'), labels=c("Covered", "Partially covered", 'Not covered'), guide='none') +  
  theme(panel.background=element_rect(fill = "white", colour = "black")) +
  geom_errorbarh(aes(xmin=df$rpv.low.95, xmax=df$rpv.high.95), height=0, size=0.4) +
  theme(axis.text.y=element_text(size=7, colour="gray20"), axis.title.x=element_text(size=8)) + geom_point()
  
p.black <- qplot(black.pct.08, black.rank, data=df, shape=as.factor(section.5), col=as.factor(section.5)) + 
  xlim(c(0,.51)) + ggtitle('Black population') + ylab('') + 
  xlab('Percent of population that is black \n (2008 ACS)') +
  geom_hline(yintercept=39.5, linetype=2) +
  scale_shape_manual("Section 4", values=c(19, 1, 19), labels=c("Covered", "Partially covered", "Not covered")) +
  scale_colour_manual("Section 4", values=c('firebrick2', 'black', 'black'), labels=c("Covered", "Partially covered", 'Not covered')) +  
  theme(panel.background=element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y=element_text(size=7, colour="gray20"), axis.title.x=element_text(size=8)) + geom_point() +
  theme(legend.justification=c(1,0.5), legend.position=c(1,0.5))

p.stereotype <- qplot(stereotype, stereotype.rank, data=df, shape=as.factor(section.5), col=as.factor(section.5)) + 
  xlim(c(0.2,0.8)) + ggtitle('Negative stereotype of blacks') + ylab('') + 
  xlab('Proportion of residents that stereotype more \n negatively than the national median') +
  geom_hline(yintercept=39.5, linetype=2) +
  scale_shape_manual("Section 4", values=c(19, 1, 19), labels=c("Covered", "Partially covered", "Not covered"), guide='none') +
  scale_colour_manual("Section 4", values=c('firebrick2', 'black', 'black'), labels=c("Covered", "Partially covered", 'Not covered'), guide='none') +
  geom_errorbarh(aes(xmin=df$st.low.95, xmax=df$st.high.95), height=0, size=0.4) +
  theme(panel.background=element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y=element_text(size=7, colour="gray20"), axis.title.x=element_text(size=8)) + geom_point()

pdf('figure_7.pdf', height=6, width=13)
multiplot(p.stereotype, p.black, p.rpv, cols=3)
dev.off()

####################################################################################################

## FIGURE 8

# States that would be covered based on various thresholds of three
# measures: (1) racial stereotyping (2) Black population and (3) racially polarized
# voting. Red borders represent states that were covered using the formula in
# Section 4 that was invalidated in Shelby County.

####################################################################################################

library(maps)

df$prej.median <- ifelse(df$stereotype > quantile(df$stereotype, 0.5), 1, 0)
df$prej.quart <- ifelse(df$stereotype > quantile(df$stereotype, 0.75), 1, 0)
df$prej.10 <- ifelse(df$stereotype > quantile(df$stereotype, 0.9), 1, 0)

df$bp.median <- ifelse(df$black.pct.08 > quantile(df$black.pct.08, 0.5), 1, 0)
df$bp.quart <- ifelse(df$black.pct.08 > quantile(df$black.pct.08, 0.75), 1, 0)
df$bp.10 <- ifelse(df$black.pct.08 > quantile(df$black.pct.08, 0.9), 1, 0)

df$rpv.median <- ifelse(df$rpv.boot.median > quantile(df$rpv.boot.median, 0.5), 1, 0)
df$rpv.quart <- ifelse(df$rpv.boot.median > quantile(df$rpv.boot.median, 0.75), 1, 0)
df$rpv.10 <- ifelse(df$rpv.boot.median > quantile(df$rpv.boot.median, 0.9), 1, 0)

df$all.median <- ifelse(df$prej.median==1 & df$bp.median==1 & df$rpv.median==1, 1, 0)
df$all.quart <- ifelse(df$prej.quart==1 & df$bp.quart==1 & df$rpv.quart==1, 1, 0)
df$all.10 <- ifelse(df$prej.10==1 & df$bp.10==1 & df$rpv.10==1, 1, 0)

## PLOT

# Draw map of lower 48
proj.type <- "albers"
proj.stdlats <- c(29.5, 45.5)
proj.orient <- c(90,-100,0)

pdf('figure_8.pdf', width=7, height=5)

par(mfrow=c(3,5), mar=c(0,0,0,0))

### TOP
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="white", mar=rep(0, 4), myborder=0)
text(.28, -1.2, 'Top\n90%', cex=1.2)

# Prej top
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$prej.10==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)
title('\n\n Stereotyping')

# Black pop top
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$bp.10==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)
title('\n\n Black population')

# RPV top
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$rpv.10==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)
title('\n\n Polarized voting')

# ALL top
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$all.10==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)
title('\n\n ALL MEASURES')


### QUARTILE ###
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="white", mar=rep(0, 4), myborder=0)
text(.28, -1.2, 'Top\n75%', cex=1.2)

# Prej quart
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$prej.quart==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)

# Black pop quart
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$bp.quart==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)

# RPV quart
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$rpv.quart==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)

# ALL quart
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$all.quart==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)


### MEDIAN ###
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="white", mar=rep(0, 4), myborder=0)
text(.28, -1.2, 'Top\nhalf', cex=1.2)

# Prej median
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$prej.median==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)

# Black pop median
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$bp.median==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)

# RPV median
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$rpv.median==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)

# RPV median
map('state', proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, bound=T, col="lightgray", mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$all.median==1], col="gray", fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.05, mar=rep(0, 4), myborder=0)
map('state', df$state.name[df$section.5==0], col="red", fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, wrap=F, add=T, lwd=0.01, mar=rep(0, 4), myborder=0)


dev.off()


## Clear the environment
rm(list=ls())

####################################################################################################
## END OF FILE
####################################################################################################