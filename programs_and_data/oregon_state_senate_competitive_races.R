# Created by Sabi, 2020-08-30, updated 2020-09-25 with September data
# Which Oregon State Senate Districts are the most competitive,
# and which may be even more competitive in 2020 due to trends
# with voter registration?

# How
# The function choose_district() has the input of a senate district (1-30)
# The output shows the registered voters by party in 2016, 2020-03, and 2020-08

library(tidyverse)
library(cowplot)

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
  mutate('2016' = n) %>%
  group_by(PARTY) %>% 
  select(District, Incumbent_Party = Party, Senator, PARTY, '2016')
vote2018 <- results2 %>%  
  mutate('2018' = n) %>%
  group_by(PARTY) %>% 
  select(District, Incumbent_Party = Party, Senator, PARTY, '2018')
vote2020 <- results1 %>%  
  mutate('2020' = n) %>%
  group_by(PARTY) %>% 
  select(District, Incumbent_Party = Party, Senator, PARTY, '2020')
vote1 <- merge(vote2016, vote2018)
vote2 <- merge(vote1, vote2020) %>%
  arrange(District, desc('2020'))

elections_2020 <- vote2 %>%
  filter(District %in% c(1,2,5,9,10,12,14,18,21,22,23,25,27,28,29,30))

# View elections for 2020 
view(elections_2020)

# District 10 (South of Salem) is competitive, and the incumbent party is 3rd in voter registrations
# District 27 (Bend) is competitive, and the incumbent party is 3rd in voter registrations

district_chart <- function(district){
  filter_district <- elections_2020 %>% filter(District == district) 
  d <- filter_district %>% gather(Year,Voters, 5:7)
  p <- ggplot(d, aes(x=Year,
                y=Voters,
                color = PARTY,
                group = PARTY)) + 
  geom_point() +
  geom_line()  +
  theme_classic() + 
  ggtitle(paste0("Voter Registration for Senate District ",district))
  p
}
p1 <- district_chart(10)
p2 <- district_chart(27)
p3 <- district_chart(5)
p4 <- district_chart(12)

plot_grid(p1,p2,p3,p4)

# For charts where other parties are grouped together
Democrat_Nonaffiliated_Republican <- elections_2020 %>%
  filter(PARTY == 'Democrat' |
           PARTY == 'Nonaffiliated' |
           PARTY == 'Republican')

non_Democrat_Nonaffiliated_Republican <- elections_2020 %>%
  filter(PARTY != 'Democrat' & 
           PARTY != 'Nonaffiliated' &
           PARTY != 'Republican') %>%
  group_by(District, Incumbent_Party, Senator) %>%
  summarise('2016' = sum(`2016`), 
            '2018' = sum(`2018`),
            '2020' = sum(`2020`)) %>%
  mutate(PARTY = 'Other')

elections_2020_other <- rbind(Democrat_Nonaffiliated_Republican,
                              non_Democrat_Nonaffiliated_Republican) %>%
  arrange(District,PARTY)

district_chart_other <- function(district){
  filter_district <- elections_2020_other %>% filter(District == district) 
  d <- filter_district %>% gather(Year,Voters, 5:7)
  p <- ggplot(d, aes(x=Year,
                     y=Voters,
                     color = PARTY,
                     group = PARTY)) + 
    geom_point() +
    geom_line()  +
    theme_classic() + 
    ggtitle(paste0("Voter Registration for Senate District ",district))
  p
}
o1 <- district_chart_other(10)
o2 <- district_chart_other(27)
o3 <- district_chart_other(5)
o4 <- district_chart_other(12)

plot_grid(o1,o2,o3,o4)


