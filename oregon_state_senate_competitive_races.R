# Created by Sabi, 2020-08-30, updated 2020-09-25 with September data
# Which Oregon State Senate Districts are the most competitive,
# and which may be even more competitive in 2020 due to trends
# with voter registration?

# How
# The function choose_district() has the input of a senate district (1-30)
# The output shows the registered voters by party in 2016, 2020-03, and 2020-08

library(tidyverse)

### DATA ###

# map of the Oregon State Senate Districts
# https://sos.oregon.gov/blue-book/Pages/state/legislative/district-maps.aspx

# data source (formerly) https://sos.oregon.gov/elections/Pages/electionsstatistics.aspx
# Registered Voters data source (now) https://catalog.data.gov/dataset/voter-registration-data
import_voters <- read_csv('Voter_Registration_Data.csv') %>%
  mutate(snapshot_date = as.Date(SYSDATE, '%m/%d/%Y')) %>%
  arrange(desc(snapshot_date), HD_NAME, PARTY)
dates_voter_reg <- unique(import_voters$snapshot_date)

# Current Legislators data source: https://en.wikipedia.org/wiki/80th_Oregon_Legislative_Assembly 
import_sen <- read_csv('oregon_state_senators_2020.csv')

#############

### WRANGLE DATA FOR THE FUNCTION ###

# use the data needed from the import
voters <- import_voters %>%
  mutate(house_districts = as.integer(str_sub(HD_NAME, -2))) %>%
  group_by(house_districts, PARTY, snapshot_date) %>%
  tally(`COUNT(V.ID)`) %>%
  arrange(desc(snapshot_date), house_districts, PARTY)

# most recent snapshot date of voter registration data (2020)
voters_2020 <- voters %>% filter(snapshot_date == '2020-09-03') 
results1 <- merge(voters_2020, import_sen, by = 'house_districts') %>%
  group_by(District, Party, Senator, PARTY) %>%
  tally(n)

# 2018 voter registration before the election that year
voters_2018 <- voters %>% filter(snapshot_date == '2018-10-03') 
results2 <- merge(voters_2018, import_sen, by = 'house_districts') %>%
  group_by(District, Party, Senator, PARTY) %>%
  tally(n)

# 2016 voter registration before the election that year
voters_2016 <- voters %>% filter(snapshot_date == '2016-11-01') 
results3 <- merge(voters_2016, import_sen, by = 'house_districts') %>%
  group_by(District, Party, Senator, PARTY) %>%
  tally(n)

### OUTPUT CURRENT VOTER REGISTRATION NUMBERS FOR 2020 ELECTION

vote2016 <- results3 %>% 
  mutate(n_2016 = n) %>%
  group_by(PARTY) %>% 
  select(District, Incumbent_Party = Party, Senator, PARTY, n_2016)
vote2018 <- results2 %>%  
  mutate(n_2018 = n) %>%
  group_by(PARTY) %>% 
  select(District, Incumbent_Party = Party, Senator, PARTY, n_2018)
vote2020 <- results1 %>%  
  mutate(n_2020 = n) %>%
  group_by(PARTY) %>% 
  select(District, Incumbent_Party = Party, Senator, PARTY, n_2020)
vote1 <- merge(vote2016, vote2018)
vote2 <- merge(vote1, vote2020) %>%
  arrange(District, desc(n_2020))

electons_2020 <- vote2 %>%
  filter(District %in% c(1,2,5,9,10,12,14,18,21,22,23,25,27,28,29,30))

# View elections for 2020 
view(electons_2020)

# District 10 (South of Salem) is competitive, and the incumbent party is 3rd in voter registrations
# District 27 (Bend) is competitive, and the incumbent party is 3rd in voter registrations
