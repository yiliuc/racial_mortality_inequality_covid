# This is the file to clean all the mortalitydata
source("scripts/import_data.R")
source("scripts/data_cleaning_acs.R")


# Import all the packages
library(tidyverse)
library(dplyr)


# Clean the state-level mortality data
## COVID
state_covid_clean <- state_covid %>% 
  filter(! (State == "United States" | State == "Alaska")) %>% 
  mutate(state = get_state_abbreviation(State),
         age_group = Age.group,
         age_group = ifelse(age_group == "85 years and over", 
                            "85+ years", age_group),
         age_group = ifelse(age_group == "Under 1 year", "< 1 year", age_group),
         race = Race.and.Hispanic.Origin.Group,
         race = ifelse(race == "Non-Hispanic White", "White",
                       ifelse(race == "Non-Hispanic Black", "Black or African American",
                              race)),
         covid_deaths = COVID.19.Deaths,
         total_deaths_pandemic = Total.Deaths,
         annual_death_pandemic = round(total_deaths_pandemic * 3/11, 1)) %>%
  filter(race == "White" | race == "Black or African American") %>% 
  dplyr::select(state, age_group, race, covid_deaths, total_deaths_pandemic, annual_death_pandemic)

## 2019
state_mortality_clean <- state_mortality %>% 
  filter(! (State == "Alaska"),
         ! (Deaths == "Suppressed"),
         ! (Population == "Not Applicable"),
         Year == 2019,
         Ten.Year.Age.Groups.Code != "NS") %>% 
  mutate(state = get_state_abbreviation(State),
         population_2019 = as.numeric(Population),
         age_group = Ten.Year.Age.Groups,
         race = Single.Race.6,
         death_2019 = as.numeric(Deaths),
         across(state, 
                ~replace(., is.na(.), "D.C."))) %>% 
  filter(race == "White" | race == "Black or African American") %>% 
  dplyr::select(state, age_group, race, death_2019, population_2019)

## Merge the state data
merged_data_state <- state_mortality_clean %>%
  left_join(state_covid_clean, by = c("state", "age_group", "race")) %>% 
  inner_join(DP02_state_clean, by = "state") %>% 
  inner_join(DP03_state_clean, by = c("state", "fips")) %>%
  # filter(state != "D.C.") %>% 
  filter(state != "HI") %>% 
  mutate(population_2019 = as.numeric(population_2019),
         mortrate_2019 = death_2019/population_2019 * 100000,
         mortrate_covid = annual_death_pandemic/population_2019 * 100000,
         mortrate_change = mortrate_covid - mortrate_2019) %>% 
  filter(! is.na(mortrate_change))

income_pct <- merged_data_state %>% 
  group_by(state) %>% 
  summarise(income = mean(mean_household_income),
            education = mean(prop_higher_education)) %>% 
  mutate(income_decile = ntile(income, 10),
         education_decile = ntile(education, 10))

merged_data_state <- merged_data_state %>% 
  inner_join(income_pct, by = "state")

write.csv(merged_data_state, "outputs/data/merged_data_state.csv", row.names = FALSE)

# Clean the county-level mortality data
## COVID
county_covid_clean <- county_covid %>% 
  filter(Single.Race.6 %in% c("White", "Black or African American")) %>% 
  mutate(deaths = Deaths,
         fips = Residence.County.Code,
         race = Single.Race.6) %>% 
  left_join(s2701_clean[,c("state", "county", "fips", "race", "population_covid")], 
            by = c("fips", "race")) %>% 
  dplyr::select(state, county, fips, race, deaths, population_covid)

county_covid_clean$deaths <- as.numeric(replace(county_covid_clean$deaths, 
                                                county_covid_clean$deaths == "Suppressed", NA))
covid_mortrate <- county_covid_clean %>%
  filter(!is.na(deaths)) %>%
  group_by(state, race) %>%
  summarise(mean_covid_mortrate = mean(deaths/population_covid, na.rm = TRUE),
            .groups = "drop")

county_covid_clean <- county_covid_clean %>%
  left_join(covid_mortrate, by = c("state", "race")) %>% 
  mutate(total_deaths_pandemic = ifelse(is.na(deaths), 
                                        mean_covid_mortrate * population_covid, deaths),
         annual_death_pandemic = round(total_deaths_pandemic * 3/10, 1)) %>% 
  dplyr::select(-deaths, -mean_covid_mortrate)

## 2019
county_mortality_clean <- mortality_2019 %>% 
  filter(Single.Race.6 %in% c("White", "Black or African American"),
         Population != "Suppressed") %>% 
  mutate(fips = County.Code,
         race = Single.Race.6,
         deaths_2019 = Deaths,
         population_2019 = as.numeric(Population)) %>% 
  left_join(s2701_clean[,c("state", "county", "fips", "race")], 
            by = c("fips", "race")) %>% 
  dplyr::select(state, county, fips, race, deaths_2019, population_2019)

county_mortality_clean$deaths_2019 <- as.numeric(replace(county_mortality_clean$deaths_2019, 
                                                         county_mortality_clean$deaths_2019 == "Suppressed", NA))

mortrate_2019 <- county_mortality_clean %>%
  filter(!is.na(deaths_2019)) %>%
  group_by(state, race) %>%
  summarise(mean_mortrate_2019 = mean(deaths_2019/population_2019, na.rm = TRUE),
            mean_population_2019 = mean(population_2019, na.rm = TRUE),
            .groups = "drop")

county_mortality_clean <- county_mortality_clean %>%
  left_join(mortrate_2019, by = c("state", "race")) %>% 
  mutate(population_2019 = ifelse(is.na(population_2019), 
                                  mean_population_2019, population_2019),
         deaths_2019 = ifelse(is.na(deaths_2019), 
                              mean_mortrate_2019 * population_2019, deaths_2019)) %>% 
  dplyr::select(-mean_mortrate_2019, -mean_population_2019)

## Merge the county data
merged_data_county <- county_mortality_clean %>% 
  full_join(county_covid_clean, by = c("fips", "race")) %>% 
  full_join(DP02_clean, by = "fips") %>% 
  full_join(DP03_clean, by = "fips") %>% 
  # Need to refine the column in s2701
  full_join(s2701_clean, by = c("fips", "race")) %>% 
  dplyr::select(state, county, fips, everything(), -county.x, -county.y, -county.x.x,
                -county.y.y, -state.y, -state.x, -population_covid.x, -state.x.x,
                -state.y.y) %>% 
  mutate(county = ifelse(state == "LA", gsub(" Parish", "", county), county),
         population_covid = population_covid.y,
         mortrate_2019 = deaths_2019/population_2019 * 100000,
         mortrate_2019 = ifelse(mortrate_2019 == "NaN", 0, mortrate_2019),
         mortrate_covid = annual_death_pandemic/population_covid * 100000,
         mortrate_covid = ifelse(mortrate_covid == "NaN", 0, mortrate_covid),
         mortrate_change = mortrate_covid - mortrate_2019,
         income_pctile = ntile(mean_household_income, 100),
         education_pctile = ntile(prop_higher_education, 100)) %>% 
  filter(mortrate_change < 100000) %>% 
  dplyr::select(-population_covid.y)

write.csv(merged_data_county, "outputs/data/merged_data_county.csv", row.names = FALSE)
