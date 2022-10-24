# This script is to link different types of election data from NC
# Data sets:
# (1) Voter Registration
# (2) Voter History
# (3) Polling Places
# (4) Precinct Geometry


## 1. Voter Registration
# Import state voter registration file
nc_voterreg <- read.delim("./raw_data/ncvoter_Statewide.txt")

# Clean the data
nc_voterreg_for_joining <- nc_voterreg %>%
  mutate(res_street_address = trimws(res_street_address), 
         res_city_desc = trimws(res_city_desc), 
         state_cd = trimws(state_cd), 
         zip_code = trimws(zip_code)) %>%
  select(county_id, county_desc, voter_reg_num, status_cd, voter_status_desc,
         last_name, first_name, middle_name, 
         res_street_address, res_city_desc, state_cd, zip_code, 
         race_code, ethnic_code, party_cd, gender_code, birth_year, birth_state, drivers_lic,
         registr_dt, precinct_abbrv, precinct_desc, municipality_abbrv, municipality_desc, 
         ncid,
         # voter tabulation district abbreviation
         vtd_abbrv, vtd_desc) %>%
  mutate(full_res_address = paste(res_street_address, res_city_desc, state_cd, zip_code, sep = ", "))

## 2. Voter History
# Import NC voter history file
nc_voterhistory <- read.delim("./raw_data/ncvhis_Statewide.txt")

# Filter those who voted for the 2020 presidential election only
voters_2020_pres <- nc_voterhistory %>%
  filter(election_desc == "11/03/2020 GENERAL") %>%
  distinct()

nc_voterhistory %>%
  filter(election_desc %in% c("11/03/2020 GENERAL", "03/03/2020 PRIMARY", "06/23/2020 SECOND PRIMARY", "06/23/2020 PRIMARY")) %>%
  spread(election_desc, )
  
  

# Select columns of interest only
voters_2020_pres_for_joining <- voters_2020_pres %>% 
  select(voter_reg_num, voting_method, voted_party_cd, 
         pct_label, pct_description, ncid, vtd_label, vtd_description)

## 3. Polling Places
# Import state polling place file
pp_2020_pres <- read_csv("./raw_data/polling_place_20201103.csv") %>%
  mutate(full_pp_address = paste(paste(house_num, street_name, sep = " "), city, state, zip, sep = ", "))

pp_2020_pres_for_joining <- pp_2020_pres %>%
  select(county_name, precinct_name, full_pp_address)

## 4. Precinct Geometry
# Import state precinct shp file
pct_boundary <- st_read("./raw_data/SBE_PRECINCTS_20201018/SBE_PRECINCTS_20201018.shp")

# Join subsetted voters_2020_pres with nc_voterreg
voter_reg_his_joined <- voters_2020_pres_for_joining %>%
  left_join(nc_voterreg_for_joining, by = c("voter_reg_num", "ncid"))

pct_matching <- voter_reg_his_joined %>%
  select(county_desc, precinct_abbrv, precinct_desc, pct_label, pct_description) %>%
  mutate(pct_abbrv_same = if_else(precinct_abbrv == pct_label, 1, 0),
         pct_desc_same = if_else(precinct_desc == pct_description, 1, 0))

pct_matching %>%
  #filter(pct_abbrv_same == 1 && pct_desc_same == 1) %>% #5543899 rows
  filter(pct_abbrv_same == 0 | pct_desc_same == 0) %>% #451450 rows
  nrow()
# found that only 7.5% of voters have unmatched pct b/w voter reg and history datasets

pct_matching %>%
  left_join(pp_2020_pres, by = c("county_desc" = "county_name", "pct_description" = "precinct_name")) %>%
  # filter(is.na(full_pp_address)) %>% # 6% of voters pct address unmatched
  nrow()

unique_pp_pct_names <- sort(unique(pp_2020_pres$precinct_name))
unique_his_pct_names <- sort(unique(voter_reg_his_joined$pct_description))

setdiff(unique_pp_pct_names, unique_his_pct_names)
setdiff(unique_his_pct_names, unique_pp_pct_names)

# First go only with 94% of voters with matching
all_three_joined <- voter_reg_his_joined %>% 
  left_join(pp_2020_pres_for_joining, by = c("county_desc" = "county_name", "pct_description" = "precinct_name"))

save(all_three_joined, 
     file = "./cleaned_data/01.RData")

nc_voterhistory %>% 
  filter(str_detect(election_desc, "PRIMARY") & str_detect(election_desc, "2020")) %>%
  pull(election_desc) %>% unique() %>% sort()