# This file is to perform data analysis based on all the cleanded data
source("codes/data_cleaning_mortality.R")


# Import all the packages
library(tidyverse)
library(tools)
library(ggplot2)
#install.packages("maps")
library(maps)
#install.packages("mapdata")
library(mapdata)
library(gridExtra)
#install.packages("broom")
library(broom)
library(kableExtra)
#install.packages("cowplot")
library(cowplot)
#install.packages("binsreg")
library(binsreg)
#install.packages("cowplot")
library(cowplot)


## -------------------------------------------------------------------------------------------------------
county_data <- merged_data_county
state_data <- merged_data_state
write.csv(merged_data_county, "merged_data_county.csv")
write.csv(merged_data_state, "merged_data_state.csv")

## -------------------------------------------------------------------------------------------------------
summary <- county_data %>%
  group_by(race) %>%
  summarise(
    min = round(min(mortrate_change, na.rm = TRUE), 0),
    q1 = round(quantile(mortrate_change, probs = 0.25, na.rm = TRUE), 0),
    median = round(median(mortrate_change, na.rm = TRUE), 0),
    mean = round(mean(mortrate_change, na.rm = TRUE), 0),
    q3 = round(quantile(mortrate_change, probs = 0.75, na.rm = TRUE), 0),
    max = round(max(mortrate_change, na.rm = TRUE), 0)) %>%
  rename(`Race` = race,
         `Minimum` = min,
         `Q1` = q1,
         `Median` = median,
         `Mean` = mean,
         `Q3` = q3,
         `Maximum` = max)



## -------------------------------------------------------------------------------------------------------
get_state_abbreviation <- function(state_names) {
  sapply(state_names, function(state_name) {
    state_data <- state.abb[match(state_name, state.name)]
    if (!is.na(state_data)) {
      return(state_data)
    } else {
      return(NA)
    }
  })
}

county <- map_data("county")
state <- map_data("state")
county_clean <- county %>% 
  mutate(state = region,
         county = subregion,
         state = get_state_abbreviation(toTitleCase(state)),
         county = toTitleCase(county)) %>% 
  filter(region != "AK" & region != "HI") %>% 
  dplyr::select(-region, -subregion)
white_data <- county_data %>% filter(race == "White")
black_data <- county_data %>% filter(race == "Black or African American")
merged_white_data <- county_clean %>% 
  full_join(white_data, by = c("state", "county"))
merged_black_data <- county_clean %>% 
  full_join(black_data, by = c("state", "county"))


## -------------------------------------------------------------------------------------------------------
p1 <- ggplot(data = merged_white_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = mortrate_change), color = "black") +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey",
                      limits = c(0, 3000), oob = scales::squish) +
  labs(title = "The change of mortality per 100k for White people",
       fill = 'Change of Mortality per 100k') +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_path(data = state, aes(x = long, y = lat, group = group), color = "black", linewidth = 0.95)

# Second plot
p2 <- ggplot(data = merged_black_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = mortrate_change), color = "black") +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey",
                      limits = c(0, 4000), oob = scales::squish) +
  labs(title = "The change of mortality per 100k for Black people",
       fill = 'Change of Mortality per 100k') +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_path(data = state, aes(x = long, y = lat, group = group), color = "black", linewidth = 0.95)

maps_1 <- arrangeGrob(p1, p2, ncol = 1)

## -------------------------------------------------------------------------------------------------------
p3 <- ggplot(data = merged_white_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = percent_uninsured), color = "black") +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey",
                      limits = c(0, 100), oob = scales::squish) +
  labs(title = "The Percentage of White People Without Insurance",
       fill = "Percent of people without insurance (%)") +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_path(data = state, aes(x = long, y = lat, group = group), color = "black", linewidth = 0.95)

# Second plot
p4 <- ggplot(data = merged_black_data, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = percent_uninsured), color = "black") +
  scale_fill_gradient(low = "white", high = "darkblue", na.value = "grey",
                      limits = c(0, 100), oob = scales::squish) +
  labs(title = "The Percentage of Black People Without Insurance",
       fill = "Percent of people without insurance (%)") +
  coord_quickmap() +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_path(data = state, aes(x = long, y = lat, group = group), color = "black", linewidth = 0.95)

maps_2 <- arrangeGrob(p3, p4, ncol = 1)

## -------------------------------------------------------------------------------------------------------
data_processed <- state_data %>%
  mutate(age_group = ifelse(age_group %in% c("1-4 years", "5-14 years"), 
                            "1-14 years", age_group),
         age_group = ifelse(age_group %in% c("15-24 years", "25-34 years"), 
                            "15-34 years", age_group)) %>% 
  group_by(age_group, race, income_decile) %>%
  summarise(avgmortchange = mean(mortrate_change), .groups = "drop")

p5 <- ggplot(data_processed, aes(x = income_decile, y = avgmortchange, color = race)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~age_group) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(x = "Income Deciles",
       y = "Mortality Rate Change per 100k",
       color = "Race")



## -------------------------------------------------------------------------------------------------------
data_processed <- state_data %>%
  mutate(age_group = ifelse(age_group %in% c("1-4 years", "5-14 years"), 
                            "1-14 years", age_group),
         age_group = ifelse(age_group %in% c("15-24 years", "25-34 years"), 
                            "15-34 years", age_group)) %>% 
  group_by(age_group, race, education_decile) %>%
  summarise(avgmortchange = mean(mortrate_change), .groups = "drop")

p6 <- ggplot(data_processed, aes(x = education_decile, y = avgmortchange, color = race)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~age_group) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  labs(x = "High-education attainment deciles",
       y = "Mortality Rate Change per 100k",
       color = "Race")

## -------------------------------------------------------------------------------------------------------
model <- lm(mortrate_change ~ race*income_pctile + race*percent_uninsured, data = county_data)
tidy_model <- tidy(model)

tidy_model <- tidy_model %>%
  mutate(across(where(is.numeric), ~ round(., 3)))

## -------------------------------------------------------------------------------------------------------
county_data$race <- as.factor(county_data$race)
implement_binscatter <- function(data, y_var, x_var, by_var = "race", w_var = NULL, nbins = 10) {
  if (!is.null(w_var)) {
    graph <- binsreg(y = data[[y_var]], 
                     x = data[[x_var]], 
                     w = data[[w_var]], 
                     by = data[[by_var]], 
                     nbins = nbins)
  } 
  else {
    graph <- binsreg(y = data[[y_var]], 
                     x = data[[x_var]], 
                     by = data[[by_var]], 
                     nbins = nbins)
  }
  return(graph)
}
graph1 <- implement_binscatter(county_data, "mortrate_change", "income_pctile", 
                               w_var = "percent_uninsured")
graph2 <- implement_binscatter(county_data, "mortrate_change", "income_pctile")
graph3 <- implement_binscatter(county_data, "mortrate_change", "percent_uninsured", 
                               w_var = "income_pctile")
graph4 <- implement_binscatter(county_data, "mortrate_change", "percent_uninsured")


## -------------------------------------------------------------------------------------------------------
plot_binscatter <- function(graph, title, xlab, ylab) {
  data_black <- graph$data.plot$`Group Black or African American`$data.dots
  data_white <- graph$data.plot$`Group White`$data.dots
  combined_data <- rbind(data_black, data_white)
  plot <- ggplot(combined_data, aes(x = x, y = fit, color = group)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = title, x = xlab, y = ylab, color = "Race") +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_y_continuous(limits = c(150, 1250))
  return(plot)
}
plot1 <- plot_binscatter(graph1, "Income with Controlling Insurance",
                         "Income Percentile", "Mortality Change")
plot2 <- plot_binscatter(graph2, "Income without Controlling Insurance",
                         "Income Percentile", "Mortality Change")
plot3 <- plot_binscatter(graph3, "Insurance with Controlling Income",
                         "Uninsured Percentage", "Mortality Change")
plot4 <- plot_binscatter(graph4, "Insurance without Controlling Income",
                         "Uninsured Percentage", "Mortality Change")
legend <- get_legend(plot1 + theme(legend.position = "bottom"))
combined_plot <- plot_grid(plot1, plot2, plot3, plot4, nrow = 2)
binscatter <- plot_grid(combined_plot, legend, ncol = 1, rel_heights = c(1, 0.1))
