################################################################################
#
# Load libraries
#
################################################################################

library(gnr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggiraph)



################################################################################
#
#'
#' Download GNR datasets, read, and process
#'
#
################################################################################

## Download and read -----------------------------------------------------------

if (!file.exists("data/2021_Global_Nutrition_Report_Dataset.xlsx")) {
  gnr2021_info <- gnr::download_gnr_data(.year = "2021", path = "data")
  write.csv(gnr2021_info, file = "data/gnr2021_info.csv", row.names = FALSE)
} else {
  gnr2021_info <- read.csv("data/gnr2021_info.csv")
}
  
gnr2021 <- Map(
  f = readxl::read_xlsx,
  path = gnr2021_info$path,
  sheet = readxl::excel_sheets(path = gnr2021_info$path)
)

names(gnr2021) <- readxl::excel_sheets(path = gnr2021_info$path)

## Process datasets needed -----------------------------------------------------

# Convert child nutrition dataset to long tidy format

region_child_nutrition <- gnr2021[["Region child nutrition"]] |>
  tidyr::pivot_longer(
    cols = stunting_2000:lbw_2015, 
    names_to = c("indicator", "year"),
    names_sep = "\\_",
    values_to = "value"
  )

country_child_nutrition <- gnr2021[["Country child nutrition"]] |>
  dplyr::mutate(
    stunting_2000 = as.numeric(stunting_2000),
    overweight_2014 = as.numeric(overweight_2014),
    overweight_2020 = as.numeric(overweight_2020)
  ) |>
  tidyr::pivot_longer(
    cols = stunting_2000:lbw_2015, 
    names_to = c("indicator", "year"),
    names_sep = "\\_",
    values_to = "value"
  )

## Plot ------------------------------------------------------------------------

## World
region_child_nutrition |>
  dplyr::filter(
    region == "World", indicator != "lbw", disaggregation == "sex"
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = indicator)) +
  geom_line(mapping = aes(colour = indicator), size = 1) +
  geom_point(mapping = aes(colour = indicator), size = 3) +
  labs(
    title = "Prevalence of stunting, wasting and overweight in children under 5 years of age",
    x = "",
    y = "%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 14)
  )

## Countries

countries <- c(
  "Nepal", "Nigeria", "Sudan", "Kenya", "India", "Indonesia", "South Africa", 
  "Malaysia", "Cameroon", "Viet Nam", "Eritrea"
)

## Double burden: stunting and overweight

country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "coexistence",
    disaggregation == "all",
    disagg.value == "Stunting and overweight",
    !is.na(value)
  ) |>
  ggplot(mapping = aes(x = year, y = value)) +
  geom_point(
    mapping = aes(colour = country),
    size = 4
  ) +
  labs(
    title = "Double burden of malnutrition",
    subtitle = "Prevalence of coexisting stunting and overweight in children under 5 years of age",
    x = "Year",
    y = "%"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

## Double burden: wasting and stunting

country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "coexistence",
    disaggregation == "all",
    disagg.value == "Wasting and stunting",
    !is.na(value)
  ) |>
  ggplot(mapping = aes(x = year, y = value)) +
  geom_point(
    mapping = aes(colour = country),
    size = 4
  ) +
  labs(
    title = "Double burden of malnutrition",
    subtitle = "Prevalence of coexisting wasting and stunting in children under 5 years of age",
    x = "Year",
    y = "%"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank())

## Inequalities - stunting

country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "stunting",
    disaggregation == "wealth",
    !is.na(value)
  ) |>
  dplyr::mutate(
    disagg.value = factor(
      x = disagg.value, 
      levels = c("Highest", "Second highest", 
                 "Middle", "Second lowest", "Lowest")
    )
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = disagg.value)) +
  geom_line(
    mapping = aes(colour = disagg.value),
    size = 1.5
  ) +
  labs(
    title = "Inequalities in child stunting",
    subtitle = "Prevalence of stunting in children under 5 years of age by wealth quintiles",
    x = "Year",
    y = "%"
  ) +
  facet_wrap(. ~ country, nrow = 2) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(size = 8, angle = 90)
  )

## Inequalities - wasting

country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "wasting",
    disaggregation == "wealth",
    !is.na(value)
  ) |>
  dplyr::mutate(
    disagg.value = factor(
      x = disagg.value, 
      levels = c("Highest", "Second highest", 
                 "Middle", "Second lowest", "Lowest")
    )
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = disagg.value)) +
  geom_line(
    mapping = aes(colour = disagg.value),
    size = 1.5
  ) +
  labs(
    title = "Inequalities in child wasting",
    subtitle = "Prevalence of wasting in children under 5 years of age by wealth quintiles",
    x = "Year",
    y = "%"
  ) +
  facet_wrap(. ~ country, nrow = 2) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(size = 8, angle = 90)
  )

## Inequalities - overweight

country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "overweight",
    disaggregation == "wealth",
    !is.na(value)
  ) |>
  dplyr::mutate(
    disagg.value = factor(
      x = disagg.value, 
      levels = c("Highest", "Second highest", 
                 "Middle", "Second lowest", "Lowest")
    )
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = disagg.value)) +
  geom_line(
    mapping = aes(colour = disagg.value),
    size = 1.5
  ) +
  labs(
    title = "Inequalities in child overweight",
    subtitle = "Prevalence of overweight in children under 5 years of age by wealth quintiles",
    x = "Year",
    y = "%"
  ) +
  facet_wrap(. ~ country, nrow = 2) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    axis.text.x = element_text(size = 8, angle = 90)
  )
