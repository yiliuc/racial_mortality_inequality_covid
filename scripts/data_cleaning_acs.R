# This is the file to clean all the ACS data
source("codes/import_data.R")


# Import all the packages
library(tidyverse)
library(dplyr)


# Create helper functions
## Convert full state name to its abbreviation.
get_state_abbreviation <- function(state_names) {
  sapply(state_names, function(state_name) {
    # Check for the special case of District of Columbia
    if (state_name == "District of Columbia") {
      return("D.C.")
    } else {
      # Proceed with existing logic for other states
      state_data <- state.abb[match(state_name, state.name)]
      if (!is.na(state_data)) {
        return(state_data)
      } else {
        return(NA)  # Keep NA for consistency in vector
      }
    }
  })
}

## Extract the county name from a 'County, State' formatted string.
extract_county <- function(name) {
  if (!is.na(name) && str_detect(name, ",")) {
    return(str_replace(str_split(name, ",")[[1]][1], " County", ""))
  } 
  else if (!is.na(name)) {
    return(str_replace(str_split(name, " ")[[1]][1], " County", ""))
  }
  else {
    return(NA)
  }
}

## Extract the state abbreviation from a 'County, State' formatted string.
extract_state <- function(name) {
  if (!is.na(name) && str_detect(name, ",")) {
    return(get_state_abbreviation(str_trim(str_split(name, ",")[[1]][2])))
  } else {
    return(NA)
  }
}

## Clean and rename columns in a DataFrame.
data_cleaning_county_file <- function(df, rename_dict) {
  df$county <- sapply(df$NAME, extract_county)
  df$state <- sapply(df$NAME, extract_state)
  columns_to_select <- c("county", "state", names(rename_dict))
  selected_df <- df[, columns_to_select, drop = FALSE]
  colnames(selected_df) <- c("county", "state", rename_dict)
  selected_df$fips <- as.numeric(substr(df$GEO_ID, nchar(df$GEO_ID) - 4, nchar(df$GEO_ID)))
  selected_df <- na.omit(selected_df)
  # selected_df$state <- unlist(selected_df$state)
  return(selected_df %>% arrange(state))
}

## Merge all the helper functions
data_cleaning_state_file <- function(df, rename_dict) {
  df$state <- get_state_abbreviation(df$NAME)
  columns_to_select <- c("state", names(rename_dict))
  selected_df <- df[, columns_to_select, drop = FALSE]
  colnames(selected_df) <- c("state", rename_dict)
  selected_df$fips <- as.numeric(substr(df$GEO_ID, nchar(df$GEO_ID) - 1, nchar(df$GEO_ID)))
  selected_df <- na.omit(selected_df)
  # selected_df$state <- unlist(selected_df$state)
  return(selected_df %>% arrange(state))
}


# Clean all the ACS data
dict_02 <- c("DP02_0068PE" = "prop_higher_education")
dict_03 <- c("DP03_0063E" = "mean_household_income",
             "DP03_0097PE" = "private_insurance",
             "DP03_0098PE" = "public_insurance",
             "DP03_0099PE" = "no_insurance")
dict_2701 <- c("S2701_C01_016E" = "total_white",
               "S2701_C03_016E" = "percent_insured_white",
               "S2701_C05_016E" = "percent_uninsured_white",
               "S2701_C01_017E" = "total_black",
               "S2701_C03_017E" = "percent_insured_black",
               "S2701_C05_017E" = "percent_uninsured_black")

DP02_clean <- data_cleaning_county_file(DP02, dict_02) %>% 
  mutate(prop_higher_education = as.numeric(prop_higher_education))
DP02_state_clean <- data_cleaning_state_file(DP02_state, dict_02) %>% 
  mutate(prop_higher_education = as.numeric(prop_higher_education))
DP03_clean <- data_cleaning_county_file(DP03, dict_03)
DP03_state_clean <- data_cleaning_state_file(DP03_state, dict_03)
s2701_clean1 <- data_cleaning_county_file(s2701, dict_2701)

s2701_clean <- s2701_clean1 %>% 
  pivot_longer(cols = c(total_white, total_black, percent_insured_white, 
                        percent_insured_black, percent_uninsured_white, 
                        percent_uninsured_black),
               names_to = c(".value", "race"),
               names_pattern = "(.+)_(white|black)") %>% 
  mutate(race = ifelse(race == "white", "White", "Black or African American"),
         percent_insured = ifelse(percent_insured < 0, 0, percent_insured),
         percent_uninsured = ifelse(percent_uninsured < 0, 0, percent_uninsured),
         population_covid = total) %>% 
  dplyr::select(-total)