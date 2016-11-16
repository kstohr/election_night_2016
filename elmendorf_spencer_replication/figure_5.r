##########################################################################################

## FIGURE 5

# State-level estimates of stereotyping by nonblacks. States are ranked by the 
# proportion of nonblack residents who stereotype blacks more negatively than
#   (A) the national median,
#   (B) 75% of all respondents, and
#   (C) 90% of all respondents.
# Horizontal lines are 95% confidence intervals.
# Vertical lines represent the “average” state.

##########################################################################################

## Library required packages
library(foreign)
library(weights)
library(ggplot2)

## Set working directory
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

## PROPORTION ABOVE MEDIAN, TOP QUARTILE, TOP TEN PERCENT

x$above.median <- ifelse(x$prej.norm >= median(x$prej.norm, na.rm=T), 1, 0)
x$high <- ifelse(x$prej.norm >= quantile(x$prej.norm, probs=0.75, na.rm=T), 1, 0)
x$very.high <- ifelse(x$prej.norm >= (median(x$prej.norm, na.rm=T) + 1.75*sd(x$prej.norm, na.rm=T)), 1, 0)

y.above.median <- aggregate(above.median~state.name, data=x, FUN=mean)
y.high <- aggregate(high~state.name, data=x, FUN=mean)
y.very.high <- aggregate(very.high~state.name, data=x, FUN=mean)

##########################################################################################

## ADD SECTION 5

y.above.median$section.5 <- 2
y.above.median$section.5[c(5,10,23,30,34,42)] <- 1 # partially covered states
y.above.median$section.5[c(1:3,11,19,25,41,44,47)] <- 0 # fully covered states

y.high$section.5 <- y.above.median$section.5
y.very.high$section.5 <- y.above.median$section.5

##########################################################################################

## CALCULATE STANDARD ERRORS

# SEs by state sigma
sample <- data.frame(table(x$state.name))
names(sample) <- c('state.name', 'sample.n')

y.above.median <- merge(y.above.median, sample)
y.high <- merge(y.high, sample)
y.very.high <- merge(y.very.high, sample)

ssname <- unique(x$state.name)

temp <- NA
for (i in ssname){
  temp2 <- sd(x$above.median[x$state.name==i], na.rm=T)
  temp <- rbind(temp, temp2)
}
se.above.median <- data.frame(state.name=ssname, state.sigma=temp[-1,])

temp <- NA
for (i in ssname){
  temp2 <- sd(x$high[x$state.name==i], na.rm=T)
  temp <- rbind(temp, temp2)
}
se.high <- data.frame(state.name=ssname, state.sigma=temp[-1,])

temp <- NA
for (i in ssname){
  temp2 <- sd(x$very.high[x$state.name==i], na.rm=T)
  temp <- rbind(temp, temp2)
}
se.very.high <- data.frame(state.name=ssname, state.sigma=temp[-1,])


y.above.median <- merge(y.above.median, se.above.median)
y.high <- merge(y.high, se.high)
y.very.high <- merge(y.very.high, se.very.high)

## Now for SEs
y.above.median$low.68 <- y.above.median$above.median-(y.above.median$state.sigma/sqrt(y.above.median$sample.n))
y.above.median$high.68 <- y.above.median$above.median+(y.above.median$state.sigma/sqrt(y.above.median$sample.n))
y.above.median$low.95 <- y.above.median$above.median-(2*y.above.median$state.sigma/sqrt(y.above.median$sample.n))
y.above.median$high.95 <- y.above.median$above.median+(2*y.above.median$state.sigma/sqrt(y.above.median$sample.n))

y.high$low.68 <- y.high$high-(y.high$state.sigma/sqrt(y.high$sample.n))
y.high$high.68 <- y.high$high+(y.high$state.sigma/sqrt(y.high$sample.n))
y.high$low.95 <- y.high$high-(2*y.high$state.sigma/sqrt(y.high$sample.n))
y.high$high.95 <- y.high$high+(2*y.high$state.sigma/sqrt(y.high$sample.n))

y.very.high$low.68 <- y.very.high$very.high-(y.very.high$state.sigma/sqrt(y.very.high$sample.n))
y.very.high$high.68 <- y.very.high$very.high+(y.very.high$state.sigma/sqrt(y.very.high$sample.n))
y.very.high$low.95 <- y.very.high$very.high-(2*y.very.high$state.sigma/sqrt(y.very.high$sample.n))
y.very.high$high.95 <- y.very.high$very.high+(2*y.very.high$state.sigma/sqrt(y.very.high$sample.n))

y.very.high$low.95 <- ifelse(y.very.high$low.95 < 0, 0, y.very.high$low.95)


##########################################################################################

## PLOT

## Reorder state variables by proportion
y.above.median$state <- reorder(as.character(y.above.median$state.name), y.above.median$above.median)
y.high$state <- reorder(as.character(y.high$state.name), y.high$high)
y.very.high$state <- reorder(as.character(y.very.high$state.name), y.very.high$very.high)

p.above.median <- qplot(above.median, state, data=y.above.median, shape=as.factor(section.5), col=as.factor(section.5)) + 
  xlim(c(0,0.8)) + ggtitle('Proportion above median') + ylab('') + 
  xlab('Proportion of residents that stereotype blacks more \n negatively than the national median') +
  geom_vline(xintercept=mean(y.above.median$above.median), linetype=2, colour="black") +
  scale_shape_manual("VRA Section 4", values=c(19, 1, 19), labels=c("Covered", "Partially covered", "Not covered"), guide='none') +
  scale_colour_manual("VRA Section 4", values=c('firebrick2', 'black', 'black'), labels=c("Covered", "Partially covered", 'Not covered'), guide='none') +
  geom_errorbarh(aes(xmin=y.above.median$low.95, xmax=y.above.median$high.95), height=0, size=0.4) +
  theme(panel.background=element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y=element_text(size=7, colour="gray20"), axis.title.x=element_text(size=9)) + geom_point() 

p.high <- qplot(high, state, data=y.high, shape=as.factor(section.5), col=as.factor(section.5)) + 
  xlim(c(0,0.6)) + ggtitle('Proportion in the top quartile') + ylab('') + 
  xlab('Proportion of residents that stereotype more \n negatively than 75% of all respondents') +
  geom_vline(xintercept=mean(y.high$high), linetype=2, colour="black") +
  scale_shape_manual("Section 4", values=c(19, 1, 19), labels=c("Covered", "Partially covered", "Not covered"), guide='none') +
  scale_colour_manual("Section 4", values=c('firebrick2', 'black', 'black'), labels=c("Covered", "Partially covered", 'Not covered'), guide='none') +
  geom_errorbarh(aes(xmin=y.high$low.95, xmax=y.high$high.95), height=0, size=0.4) +
  theme(panel.background=element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y=element_text(size=7, colour="gray20"), axis.title.x=element_text(size=9)) + geom_point()

p.very.high <- qplot(very.high, state, data=y.very.high, shape=as.factor(section.5), col=as.factor(section.5)) + 
  xlim(c(0,0.3)) + ggtitle('Proportion in the top 10%') + ylab('') + 
  xlab('Proportion of residents that stereotype more \n negatively than 90% of all respondents') +
  geom_vline(xintercept=mean(y.very.high$very.high), linetype=2, colour="black") +
  scale_shape_manual("Section 4", values=c(19, 1, 19), labels=c("Covered", "Partially covered", "Not covered")) +
  scale_colour_manual("Section 4", values=c('firebrick2', 'black', 'black'), labels=c("Covered", "Partially covered", 'Not covered')) +
  geom_errorbarh(aes(xmin=y.very.high$low.95, xmax=y.very.high$high.95), height=0, size=0.4) +
  theme(panel.background=element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y=element_text(size=7, colour="gray20"), axis.title.x=element_text(size=9)) + geom_point() +
  theme(legend.justification=c(1,0.25), legend.position=c(1,0.25), legend.text=element_text(size=8), legend.title=element_text(size=8))

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

pdf('figure_5.pdf', width=13, height=6)
multiplot(p.above.median, p.high, p.very.high, cols=3)
dev.off()


## Clear environment
rm(list=ls())

##########################################################################################
## END OF FILE
##########################################################################################