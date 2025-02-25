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

## Download and read GNR data --------------------------------------------------

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



################################################################################
#
#'
#' Create plots
#'
#
################################################################################

## Plot global stunting, wasting, and overweight -------------------------------
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

global_plot <- region_child_nutrition |>
  dplyr::filter(
    region == "World", indicator != "lbw", disaggregation == "sex"
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = indicator)) +
  geom_line_interactive(
    mapping = aes(colour = indicator, tooltip = value), linewidth = 1
  ) +
  geom_point_interactive(
    mapping = aes(colour = indicator, tooltip = value), size = 3
  ) +
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

girafe(
  ggobj = global_plot, 
  options = list(
    opts_sizing(rescale = TRUE, width = 0.7)
  )
)


## Plot country level metrics --------------------------------------------------

## Select countries to plot
countries <- c(
  "Nigeria", "Sudan", "South Sudan", "Sierra Leone", "Ghana", "Lesotho", 
  "Kenya", "Indonesia", "Liberia", "Zimbabwe", "Japan", "Egypt", "Mozambique",
  "State of Palestine", "Sri Lanka"
)

## Plot double burden: stunting and overweight
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
    title = "Prevalence of coexisting stunting and overweight in children under 5 years of age",
    x = "",
    y = "%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.text.x = element_text(size = 14),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 14),
  )

## Plot double burden: wasting and stunting
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
    title = "Prevalence of coexisting wasting and stunting in children under 5 years of age",
    x = "",
    y = "%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

## Plot inequalities - stunting
country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "stunting",
    disaggregation == "wealth",
    disagg.value %in% c("Highest", "Middle", "Lowest"),
    !is.na(value)
  ) |>
  dplyr::mutate(
    disagg.value = factor(
      x = disagg.value, 
      #levels = c("Highest", "Second highest", 
      #           "Middle", "Second lowest", "Lowest")
      levels = c("Highest", "Middle", "Lowest")
    ),
    year = as.integer(year)
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = disagg.value)) +
  geom_line(
    mapping = aes(colour = disagg.value),
    linewidth = 1.5
  ) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015)) +
  labs(
    title = "Prevalence of stunting in children under 5 years of age by wealth quintiles",
    x = "",
    y = "%"
  ) +
  facet_wrap(. ~ country, nrow = 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

## Plot inequalities - wasting
country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "wasting",
    disaggregation == "wealth",
    disagg.value %in% c("Highest", "Middle", "Lowest"),
    !is.na(value)
  ) |>
  dplyr::mutate(
    disagg.value = factor(
      x = disagg.value, 
      #levels = c("Highest", "Second highest", 
      #           "Middle", "Second lowest", "Lowest")
      levels = c("Highest", "Middle", "Lowest")
    ),
    year = as.integer(year)
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = disagg.value)) +
  geom_line(
    mapping = aes(colour = disagg.value),
    linewidth = 1.5
  ) +
  scale_x_continuous(breaks = c(2000, 2005, 2010, 2015)) +
  labs(
    title = "Prevalence of wasting in children under 5 years of age by wealth quintiles",
    x = "",
    y = "%"
  ) +
  facet_wrap(. ~ country, nrow = 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 16)
  )

## Plot inequalities - overweight
country_child_nutrition |>
  dplyr::filter(
    country %in% countries,
    indicator == "overweight",
    disaggregation == "wealth",
    disagg.value %in% c("Highest", "Middle", "Lowest"),
    !is.na(value)
  ) |>
  dplyr::mutate(
    disagg.value = factor(
      x = disagg.value, 
      #levels = c("Highest", "Second highest", 
      #           "Middle", "Second lowest", "Lowest")
      levels = c("Highest", "Middle", "Lowest")
    ),
    year = as.integer(year)
  ) |>
  ggplot(mapping = aes(x = year, y = value, group = disagg.value)) +
  geom_line(
    mapping = aes(colour = disagg.value),
    linewidth = 1.5
  ) +
  labs(
    title = "Prevalence of overweight in children under 5 years of age by wealth quintiles",
    x = "",
    y = "%"
  ) +
  facet_wrap(. ~ country, nrow = 2) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 20),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 20),
    axis.text.y = element_text(size = 14),
    strip.text = element_text(size = 16)
  )


