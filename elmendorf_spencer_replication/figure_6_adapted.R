##########################################################################################

##  FIGURE 6

## County-level estimates of anti-black stereotyping using two different
## MRP models. In the top map, anti-black stereotypes are estimated as a function
## of non-black respondents’ race, sex, and education within each county
## independently. The bottom map reflects stereotypes estimated using the same
## model with one additional variable that controls for within-in state variation.
## Shading reflects the proportion of non-black residents in each county that
## stereotype blacks more negatively than 75% of the nation.

##  Code last updated: July 27, 2013

##########################################################################################

## Library in required packages
library(foreign)
library(arm)
library(weights)
library(zipcode)
library(maps)
library(RColorBrewer)

## Load packages 
packages <- c("foreign", "arm", "weights", "zipcode", "maps", "RColorBrewer")
sapply(packages, require, character.only = TRUE, quietly = TRUE, warn = FALSE)
## Set working directory
dir = getwd()
setwd(dir)

##########################################################################################

## CCAP data
ccap <- read.csv('data/ccap_08.csv', stringsAsFactors=F)

## Census data for poststratification
census <- read.csv('data/poststratification_AFF_county.csv', stringsAsFactors=F)

##########################################################################################

## MERGE IN COUNTY DATA TO CCAP VIA ZIP CODE AREAS

## Zip-to-County data available via Missouri Census Data Center
mcdc <- read.csv('data/zip_to_county_equivalency.csv', stringsAsFactors=F)

mcdc$zipcode <- clean.zipcodes(mcdc$zcta5)
mcdc <- mcdc[!duplicated(mcdc$zipcode),] # Remove multiple entries

ccap$zipcode <- clean.zipcodes(ccap$zip)

ccap <- merge(ccap, mcdc, by='zipcode', all=F)

ccap$county.fips <- as.numeric(ccap$county02)
census.names <- census[!duplicated(census$county.fips), -c(3:8)]

## Change county names
ccap$polyname <- NULL # to avoid double-merge

ccap <- merge(ccap, census.names[,-c(4,6,8)], by='county.fips', all=F)

##########################################################################################

## CREATE DEPENDENT VARIABLE

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

## Normalize prejudice measure
ccap$prej.norm <- stdz(ccap$prejudice, weight=ccap$weight)

##########################################################################################

## INDEPENDENT VARIABLES

# Female dummy
ccap$sex <- ifelse(ccap$sex=='Female', 1, 0)

# Race (1=White, 2=Hispanic, 3=Asian)
ccap$race.wha <- 1
ccap$race.wha[ccap$race=='Hispanic'] <- 2
ccap$race.wha[ccap$race=='Asian'] <- 3

# Education (4 category)
ccap$educ.4 <- NA
ccap$educ.4[ccap$educ=='No HS'] <- 1
ccap$educ.4[ccap$educ=='High school graduate'] <- 2
ccap$educ.4[ccap$educ=='Some college'] <- 3
ccap$educ.4[ccap$educ=='2 year college'] <- 3
ccap$educ.4[ccap$educ=='College graduate'] <- 4
ccap$educ.4[ccap$educ=='Post-grad'] <- 4

##########################################################################################

## INTERACTION VARIABLES FOR MRP

# CCAP
ccap$race.female <- (ccap$female*3) + ccap$race.wha # from '1' for White males to '6' for 'Other' females

# CENSUS
census$race.female <- (census$sex*3) + census$race.wha # from '1' for White males to '6' for 'Other' females

##########################################################################################

## MULTI-LEVEL MODELS

lm.out.1 <- lmer(prej.norm ~ (1 | race.female) + (1 | educ.4) + (1 | polyname) +
              black.pct, data=ccap)

lm.out.2 <- lmer(prej.norm ~ (1 | race.female) + (1 | educ.4) + (1 | polyname) + (1 | state.name) +
              black.pct, data=ccap)

# Create vector of county ranefs and then fill in missing ones
county.name <- unique(as.character(census$polyname))

county.ranefs.1 <- array(NA,c(length(county.name),1)); dimnames(county.ranefs.1) <- list(c(county.name),"effect")
county.ranefs.2 <- array(NA,c(length(county.name),1)); dimnames(county.ranefs.2) <- list(c(county.name),"effect")
    
for(j in county.name){
    county.ranefs.1[j,1] <- ranef(lm.out.1)$polyname[j,1]
    county.ranefs.2[j,1] <- ranef(lm.out.2)$polyname[j,1]
}

county.ranefs.1[,1][is.na(county.ranefs.1[,1])] <- 0
county.ranefs.2[,1][is.na(county.ranefs.2[,1])] <- 0

## POSTSTRATIFICATION

## Create a prediction for each cell in Census data
cells.1 <- (fixef(lm.out.1)['(Intercept)']
          + ranef(lm.out.1)$race.female[census$race.female,1]
          + ranef(lm.out.1)$educ.4[census$educ.cat,1]
          + county.ranefs.1[as.character(census$polyname),1]
          + (fixef(lm.out.1)['black.pct']*census$black.pct))

cells.2 <- (fixef(lm.out.2)['(Intercept)']
          + ranef(lm.out.2)$race.female[census$race.female,1]
          + ranef(lm.out.2)$educ.4[census$educ.cat,1]
          + county.ranefs.2[as.character(census$polyname),1]
          + (fixef(lm.out.2)['black.pct']*census$black.pct)
          + ranef(lm.out.2)$state.name[census$state.name,1])

## Generate indicator if cell is above 75% quantile
quart.1 <- ifelse(cells.1 > quantile(ccap$prej.norm, 0.75, na.rm=T), 1, 0)
quart.2 <- ifelse(cells.2 > quantile(ccap$prej.norm, 0.75, na.rm=T), 1, 0)

## Weight the cell predictions by the cell frequency
weight.quart.1 <- quart.1 * census$cell.percent
weight.quart.2 <- quart.2 * census$cell.percent

## Calculate state score and state proportions (i.e., weighted average of responses)
df <- data.frame(county.code=county.name,
                  quart.1=as.numeric(tapply(weight.quart.1, census$polyname, sum)),
                  quart.2=as.numeric(tapply(weight.quart.2, census$polyname, sum)))

##########################################################################################

## CREATE MAPS

data(county.fips)
county.fips <- county.fips[!duplicated(county.fips$fips),]
county.fips <- county.fips[-(grep('south dakota,x', county.fips$polyname)),]

## Define colors (gray scale for printing)
colors <- c('gray80', 'gray65', 'gray50', 'gray40', 'gray30', 'black')

## Create color buckets
df$colorBuckets.1 <- cut(df$quart.1, breaks=c(0, .1, .2, .3, .4, .5, 1.5), include.lowest=T)
df$colorBuckets.2 <- cut(df$quart.2, breaks=c(0, .1, .2, .3, .4, .5, 1.5), include.lowest=T)

df.map <- merge(county.fips, df, by.x='polyname', by.y='county.code', all=T, sort=F)

## Set mapping projections
proj.type <- "albers"
proj.stdlats <- c(29.5, 45.5)
proj.orient <- c(90,-100,0)

## Map of model 1 (demographic + county)
pdf('figure_6a.pdf', width=10, height=8)
par(mar=c(0,0,0,1))
map('county', col=colors[df.map$colorBuckets.1], fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, lty=0, wrap=F, bound=T)
map('state', col='white', fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, lty=1, lwd=1.5, add=T)
#leg.txt <- c('< 10%', '10-20%', '20-30%', '30-40%', '40-50%', '> 50%')
#legend(0.1, -1.46, legend=leg.txt, fill=colors, border="gray90", bty="n", cex=0.75, title='Proportion in top quartile')
dev.off()

## Map of model 2 (demographic + county + state)
pdf('figure_6b.pdf', width=10, height=8)
par(mar=c(0,0,0,1))
map('county', col=colors[df.map$colorBuckets.2], fill=T, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, lty=0, wrap=F, bound=T)
map('state', col='white', fill=F, proj=proj.type, par=proj.stdlats, orient=proj.orient, resolution=0, lty=1, lwd=1.5, add=T)
leg.txt <- c('< 10%', '10-20%', '20-30%', '30-40%', '40-50%', '> 50%')
legend(0.1, -1.46, legend=leg.txt, fill=colors, border="gray90", bty="n", cex=0.75, title='Proportion in top quartile')
dev.off()

## Clear environment
rm(list=ls())

##########################################################################################
#  END OF FILE
##########################################################################################