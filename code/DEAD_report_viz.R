library(tidyverse)
library(readr)
library(here)
library(ggplot2)

# D.E.A.D. report visualizations

# read in data
model_data <- read_csv(here("data", "dead_model_data.csv")) |>
  mutate(across(month_2:month_7, as.integer))

# Alcohol Category -------------------------------------------------------------

## prepare data
p1 <- model_data |>
  # create one column containing alcohol categories and another column containing the percentages
  pivot_longer(cols = c("vodka_ptc", "whiskey_ptc", "tequila_ptc", "rum_ptc", "other_ptc"),
               names_to = "type",
               values_to = "type_ptc") |>
  # average % of each alcohol category for each county
  group_by(county_1, type) |>
  summarise(pct = mean(type_ptc),
            rev = mean(revenue)) |>
  # rename category values for plot
  mutate(type = case_when(
    type == "vodka_ptc" ~ "Vodka",
    type == "whiskey_ptc" ~ "Whiskey",
    type == "tequila_ptc" ~ "Tequila",
    type == "rum_ptc" ~ "Rum",
    type == "other_ptc" ~ "Other"
    
  ))

## create plot with category on x-axis and prop. on y-axis
p1 |>
  ggplot(aes(x=reorder(type, -pct), y=pct, fill=type)) +
  # bar plot
  geom_bar(position = "dodge", stat='identity') +
  # select manual colors
  scale_fill_manual(values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "darkgreen"),
                    name="Type") +
  # title and axis labels
  labs(x="Category",
       y="",
       title = "Proportion of alcohol categories sold") +
  theme_minimal()

# Median Age -------------------------------------------------------------------

model_data |>
  # take avg of median age value and alcohol sales for each county over the two years
  group_by(county) |>
  summarise(avg = mean(median_age),
            avg_rev = mean(revenue)) |>
  # x-axis = median age, y-axis = log of sales to show patterns more clearly and combat outliers in sales
  ggplot(aes(x=avg, y=log(avg_rev))) +
  # scatter plot
  geom_jitter() +
  # title and axis labels
  labs(x="Age (years)",
       y="Alcohol Sales ($, log scale)",
       title = "County Median Age by Average Alcohol Sales (2023–2024)") +
  theme_bw() 

# Education Level --------------------------------------------------------------

model_data |>
  # avg sales and % of pop. education below high school for each county and year
  group_by(county_1, year) |>
  summarise(avg_rev = mean(revenue),
            avg_edu = mean(pct_high_school_lower)) |>
  # plot with x-axis = % of pop. education below high school for each county and year 
  # and y-axis = log(sales) (log to deal with outliers so we can see patterns in data more clearly)
  ggplot(aes(x=avg_edu, y=log(avg_rev))) +
  # scatter plot
  geom_point() +
  # titlle and axis labels
  labs(x="% of Population",
       y="Alcohol Sales ($, log scale)",
       title = "Relationship Between % of County Population with Education Level \nBelow High School and Alcohol Sales") +
  theme_minimal()

# Ethnicity --------------------------------------------------------------------

## prepare data
p2 <- model_data |>
  # create new column containing ethnicities and another column of proportions
  pivot_longer(cols = c("pct_hispanic", "pct_black", "pct_api", "pct_aian"),
               names_to = "ethnicity",
               values_to = "proportion",
               # remove 'pct_' prefix in column values
               names_prefix = "pct_") |>
  # take avg of ethnicity proportions and sales for each county and ethnicity
  group_by(county_1, ethnicity) |>
  summarise(pct = mean(proportion),
            rev = mean(revenue)) |>
  # rename ethnicity values so it appears cleaner in the plot
  mutate(ethnicity = case_when(
    ethnicity == "hispanic" ~ "Hispanic",
    ethnicity == "foreign_born" ~ "Foreign born",
    ethnicity == "black" ~ "Black",
    ethnicity == "aian" ~ "American Indian/Alaska Native",
    ethnicity == "api" ~ "Asian, Native Hawaiian, or Pacific Islander"
  )) 

# create the plot!
p2 |>
  # x-axis = % of pop. for each ethnicity, y-axis = log(sales) (log since there are outliers in sales)
  # color points by ethnicity
  ggplot(aes(x=pct, y=log(rev), color = ethnicity)) +
  # scatter plot
  geom_jitter() +
  # manually choose colors for the graph
  scale_color_manual(name="Ethnicity", values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")) +
  # create an individual scatterplot for each ethnicity
  facet_wrap(~ethnicity) +
  theme_minimal() +
  # title and axis labels
  labs(x="% of Population",
       y = "Alcohol Sales ($, log scale)",
       title="Relationship between County Demographics and Alcohol Sales for \nMinority Ethnicities")
