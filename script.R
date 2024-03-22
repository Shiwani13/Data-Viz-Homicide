library(tidyverse)
library(lubridate)
library(plotly)
library(viridis)
library(sf)
library(ggtext)
library(ggmap)

data <- read_csv("data.csv")
names(data) <- str_replace_all(names(data), c(" " = ".", "," = ""))  # to change row names
data$Age.Group <- str_replace_all(data$Age.Group, c(" to " = "-")) #defining unique age range
summary(data)

unique(data$Age.Group)

data <- data %>% filter(Sex != "Unrecorded Gender") %>% filter(Officer.Observed.Ethnicity != "Unrecorded/Unknown")


# Grouping by Age, Gender, and Ethnicity
d <- data %>%
  group_by(Sex, Officer.Observed.Ethnicity, Age.Group) %>%
  summarise(count = n())

# Define the fill_color variable
d$Officer.Observed.Ethnicity <- factor(d$Officer.Observed.Ethnicity, levels = c("Other", "Asian", "Black", "White"))

#Defining color Palette
viridis_palette <- viridis(4)

# Bar Plot for Sex vs. Count of Victims
By_gender <- data %>%
  group_by(Sex) %>%
  summarise(count = n()) %>%
 
  ggplot(aes(x = Sex, y = count, fill = Sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = viridis_palette) +
  labs(title = "Distribution of Victims by Sex",
       x = "Sex",
       y = "Count of Victims") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

By_gender

# Bar plot for method of killing vs. count
bar_method_of_killing <- data %>%
  group_by(Method.of.Killing) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Method.of.Killing, y = count, fill = Method.of.Killing)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Victims by Method of Killing",
       x = "Method of Killing",
       y = "Count of Victims") +
  theme_minimal() + coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

bar_method_of_killing

# Time series graph to show crime trend over the year
data_time <- data %>%
  group_by(Recorded.Date) %>%
  summarise(count = sum(Count.of.Victims))


time_series <- ggplot(data_time, aes(x = Recorded.Date, y = count, group = 1)) +
  geom_point() +
  geom_line() +
  geom_smooth(se = FALSE, color = "blue", linetype = "dashed") +
  labs(title = "Time Series Line Plot",
       x = "Date",
       y = "Count of Victims") +
  theme_minimal()

time_series

#bar graph for plotting distribution of victims by Race for Different genders and
viridis_palette <- viridis(4)

bar_count_of_victims <- d %>%
  ggplot(aes(x = Age.Group, y = count, fill = Officer.Observed.Ethnicity)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = viridis_palette) +  
  labs(title = "Distribution of Victims by Race and faceted by Gender",
       x = "Age Group",
       y = "Count of Victims") +
  theme_minimal() + coord_flip() +
  facet_grid(rows = vars(Sex))


bar_count_of_victims

# GeoJSON file containing the boundaries of UK boroughs
boroughs <- st_read("london_boroughs.json")

# Setting Google Maps API key here
api_key <- "----------------"
register_google(key = api_key)

d <- data %>%
  group_by(Borough, Sex, Officer.Observed.Ethnicity, Age.Group) %>%
  summarise(count = n())

# Check the column names of boroughs and data
cat("Column names in boroughs:", names(boroughs), "\n")
cat("Column names in data:", names(data), "\n")

# Merge crime data with boroughs
boroughs_data <- left_join(boroughs, d, by = c("name" = "Borough"))
boroughs_data <- boroughs_data %>% filter(Sex %in% c("Male", "Female"))

# Plot the choropleth with tooltips
p <- ggplot() +
  geom_sf(
    data = boroughs_data,
    aes(
      fill = count,
      text = paste("<br>Borough: ", name, "<br>Age Group: ", Age.Group)
    )
  ) +
  geom_sf_label(aes(label = name), data = boroughs_data, size = 2) +
  scale_fill_gradient(name = "Log Count of Victims", low = "lightblue", high = "darkred") +
  theme_minimal() +
  ggtitle("Distribution of Victims by Borough Faceted by Gender") +
  facet_wrap(~Sex)

p <- ggplotly(p)

p
htmlwidgets::saveWidget(p, "Visualisation_2.html")