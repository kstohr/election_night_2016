--------------------------------------------------------
The Geography of Racial Stereotyping:
Evidence and Implications for VRA "Preclearance" After Shelby County
   by Christoper S. Elmendorf and Douglas M. Spencer
--------------------------------------------------------

Replication code created using R v2.15.2 ("Trick or Treat")
on a MacBook running OSX 10.5.8

Required R packages (and dependencies):
   apsrtable
   arm 
   foreign
   ggplot2
   plyr
   weights


--------------------------------------------------------
DATA SOURCES
--------------------------------------------------------

SURVEYS:

2008 National Annenberg Election Survey Online Edition
https://services.annenbergpublicpolicycenter.org/naes08/online/data/index.html
   *  NOTE: "ALL-WAVES COMPACT" data file

2008 Cooperative Campaign Analysis Project
Generously provided by Lynn Vavreck (vavreck@mac.com) and Simon Jackman (jackman@stanford.edu)


CENSUS:

State-level post-stratification file based on 5% IPUMS sample:
https://usa.ipums.org/usa/

County-level post-stratification file based on 5% ACS sample:
http://factfinder2.census.gov/faces/nav/jsf/pages/searchresults.xhtml?refresh=t

Zip code to County equivalency file generated via
Missouri Census Data Center Geographic Correspondence Engine:
http://mcdc2.missouri.edu/websas/geocorr2k.html


STATE-LEVEL PREDICTORS

* Black percent of population available at the Census FactFinder:
  http://factfinder2.census.gov/faces/nav/jsf/pages/searchresults.xhtml

* State GINI coefficients available at the Census FactFinder:  
  http://factfinder2.census.gov/faces/tableservices/jsf/pages/productview.xhtml?pid=ACS_08_3YR_B19083&prodType=table

* Blacks percent in poverty from IPUMS 2008 3yr sample (cross-tab 'race' and 'poverty'):
  https://usa.ipums.org/usa-action/variables/group

* Dissimilarity Indices generously provided to authors by John De Witt
  'jpdewitt@umich.edu' at Univ. of Michigan's Institute for Social Research

--------------------------------------------------------
END OF FILE
--------------------------------------------------------