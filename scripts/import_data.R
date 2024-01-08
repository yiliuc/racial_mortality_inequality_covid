# This is the file to import all the raw data

## Import all the ACS data
DP02 <- read.csv("inputs/data/ACS_DP02_county_2021.csv", stringsAsFactors = FALSE)
DP03 <- read.csv("inputs/data/ACS_DP03_county_2021.csv", stringsAsFactors = FALSE)
s2701 <- read.csv("inputs/data/ACS_S2701_county_2021.csv", stringsAsFactors = FALSE)

DP02_state <- read.csv("inputs/data/ACS_DP02_state_2021.csv", stringsAsFactors = FALSE)
DP03_state <- read.csv("inputs/data/ACS_DP03_state_2021.csv", stringsAsFactors = FALSE)

# Import the data
county_covid <- read.csv("inputs/data/county_mortality_covid.csv")
state_covid <- read.csv("inputs/data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin_and_Age.csv")
state_mortality <- read.csv("inputs/data/state_mortality_2018_to_2021.csv")
mortality_2019 <- read.csv("inputs/data/county_mortality_2019.csv")