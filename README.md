# 2016_election_night
Election night scripts and data
AP Election Night voting results data scraped from here:   
http://static.fusion.net/2016-election-results/index.html

- get_us_population_by_race_gender_county.ipynb
https://github.com/kstohr/election_night_2016/blob/master/get_us_population_by_race_gender_county.ipynb
Munges Census data to allow 'Hispanic' to be treated in the same way as Race. Merges with FIPS, congressional districts

- election_night_diverse_counties.ipynb	 
https://github.com/kstohr/election_night_2016/blob/master/election_night_diverse_counties.ipynb
Process file from above script, runs code to select counties, and creates 1) story tables and 2) layer of map with election results for just 38 selected counties 

- election_night_all_counties.ipynb
https://github.com/kstohr/election_night_2016/blob/master/election_night_all_counties.ipynb
Outputs the base layer of the map which shows election results for all counties. NOTE: *may* contain redundant code at top for processing data, etc. was done on the fly night-of. Run from top and you'll get a clean output; some redundant variables may be dependent. 

Story is here: 
http://fusion.net/?p=366271?p=366271&preview=true

Map is here: 
https://kstohr.carto.com/viz/5d5e38ee-a300-11e6-8274-0e233c30368f/map

