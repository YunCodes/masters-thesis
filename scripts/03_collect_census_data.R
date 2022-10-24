# This script is to import census block group-level socioeconomic data

data(fips_codes)

nc_fips <- fips_codes %>%
  filter(state == "NC")

census_variables <- load_variables(2020, "acs5", cache = TRUE)

# block group-level is the smallest geographical unit available in the ACS
data_blockgroup <- get_acs(geography = "block group",
                  state = "NC",
                  year = 2020,
                  geometry = TRUE,
                  variables = c(
                    # household income - invalid
                    "med_household_income" = "B25099_001",
                    # median income - invalid
                    "med_income" = "B19326_001",
                    # housing value - valid
                    "med_housing_value" = "B25077_001",
                    "per_capita_income" = "B19301_001",
                    # household type - invalid
                    "married_own_fam" = "B25115_004",
                    "married_rent_fam" = "B25115_017",
                    "total_fam" = "B25115_001",
                    # marital status - invalid
                    "never_mar" = "B06008_001",
                    "now_mar" = "B06008_003",
                    "divorced_mar" = "B06008_004",
                    "separated_mar" = "06008_005",
                    "widowed_mar" = "06008_006",
                    "total_mar" = "B06008_001",
                    # marital status 2 - valid
                    "married_hou" = "B11001_003",
                    "total_hou" = "B11001_001",
                    # education - valid
                    "below_high_1_edu" = "B23006_002",
                    "below_high_2_edu" = "B23006_003",
                    "high_edu" = "B29002_004",
                    "college_edu" = "B29002_005",
                    "associate_edu" = "B29002_006",
                    "bachelor_edu" = "B29002_007",
                    "graduate_edu" = "B29002_008",
                    "total_edu" = "B29002_001",
                    # means of transportation - valid
                    "car_trans" = "B08301_002",
                    "public_trans" = "B08301_010",
                    "total_trans" = "B08301_001")) %>%
  select(-moe) %>%
  spread(variable, estimate)

# Filter columns with NA values
data_blockgroup <- data_blockgroup %>%
  select(GEOID, NAME,
         per_capita_income, med_housing_value, ends_with("_edu"), ends_with("hou"), ends_with("trans"),
         geometry) %>%
  mutate(pct_below_high_edu = 1 - ((high_edu + college_edu + associate_edu + bachelor_edu + graduate_edu)/total_edu),
         pct_above_bach_edu = (bachelor_edu + graduate_edu)/total_edu,
         pct_married_hou = married_hou / total_hou,
         pct_car_trans = car_trans / total_trans,
         pct_public_trans = public_trans / total_trans) %>%
  arrange(desc(pct_public_trans)) %>%
  select(GEOID, NAME, per_capita_income, med_housing_value, starts_with("pct_"), geometry) 


# tract-level all variables are valid
data_tract <- get_acs(geography = "tract",
                state = "NC",
                year = 2020,
                geometry = TRUE,
                variables = c(
                  # household income 
                  "med_household_income" = "B25099_001",
                  # housing value
                  "med_housing_value" = "B25077_001",
                  # marital status
                  "m_never_m_mar" = "B12006_003",
                  "f_never_m_mar" = "B12006_008", 
                  "m_now_m_mar" = "B12006_013",
                  "f_now_m_mar" = "B12006_019",
                  "m_separated_mar" = "B12006_025",
                  "f_separated_mar" = "B12006_030",
                  "m_widowed_mar" = "B12006_036",
                  "f_widowed_mar" = "B12006_041",
                  "m_divorced_mar" = "B12006_047",
                  "f_divorced_mar" = "B12006_052",
                  # education 
                  "m_below_high_edu" = "C15002B_003",
                  "m_high_edu" = "C15002B_004",
                  "m_college_edu" = "C15002B_005",
                  "m_uni_edu" = "C15002B_006",
                  "m_total_edu" = "C15002B_002",
                  "f_below_high_edu" = "C15002B_008",
                  "f_high_edu" = "C15002B_009",
                  "f_college_edu" = "C15002B_010",
                  "f_uni_edu" = "C15002B_011",
                  "f_total_edu" = "C15002B_007",
                  # means of transportation - valid
                  "car_trans" = "B08301_002",
                  "public_trans" = "B08301_010",
                  "total_trans" = "B08301_001")) %>%
  select(-moe) %>%
  spread(variable, estimate) %>% 
  glimpse()

save(data_blockgroup,
     file = "./cleaned_data/02.RData")