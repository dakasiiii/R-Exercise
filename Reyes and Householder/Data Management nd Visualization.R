# Data Management and Visualization
# Needed packages: tidyverse, politicalds, ggplot2, skmir, ggrepel

# Our Data Set is from a study by Reyes-Householder which argues that
# women presidents in Latin Ameica suffer steeper falls in comparison
#to their male counterparts in face of similar corruption scandals

library(tidyverse)
# The package politicalds contains our data set
library(politicalds)
library(ggplot2)
library(skimr)
library(ggrepel)

# The name of the data set is approval
data("approval")
# Let's look at a quick summary
skimr::skim(approval)

# Exercise: Select the two columns that register the president's gender
# in the data set and filter it to contain only observations of the year 2000

approval %>%
  select(president, president_gender, year) %>%
  filter(year == 2000)

# Exercise: Create a new data frame, that is sort from the country-year-quarter
# with the least presidential approval to the one with the highest. Retain only
# women as presidents

approval %>%
  filter(president_gender == "Female") %>%
  arrange(-quarter) %>%
  arrange(-year) %>%
  arrange(desc(country))

# Exercise: Calculate the mean executive's corruption and GDP by country.

approval %>% 
  group_by(country) %>%
  summarize(corruption_mean = mean(corruption))

# Exercise: sort them countries in the dataset from the one obtained the highest
# GDP per capita in the 2010-2014 period to the lowest

# since there is no GDP per capita variable in the dataset, we have to create one

approval %>%
  mutate(gdp_pc = gdp / population) %>%
  arrange(-gdp_pc) %>%
  filter(year %in% c(2010:2014))

  
# Exercise: Which Country-year-quarter, between the ones governed by women 
# presidents, had the highest executive's corruption, and the highest net approval

approval %>%
  filter(president_gender == "Female") %>%
  arrange(-corruption)

# Brazil, 2014, Q1 has the highest corruption index among those governed by women

approval %>%
  filter(president_gender == "Female") %>%
  arrange(-net_approval)

# Brazil, 2013, q1 has the highest net approval rating among those governed by women

# Exercise: Convert Gender into a dummy variable

approval %>%
  mutate(woman_pres = if_else(president == "Female", 1, 0)) %>%
  select(president_gender, woman_pres, everything())

#we put the select function for easy checking

# Exercise: Create a dummy variable, economic crisis. Show if Argentina would be in a 
# crisis in 2001 and 2010

approval %>%
  mutate(econ_crisis = if_else(gdp_growth < 0 | unemployment > 20, 1, 0)) %>%
  filter(country == "Argentina" & year %in% c(2001, 2010)) %>%
  select(country, year, econ_crisis, everything())

# Exercise: Categorize the countries in to Southern Cone, Central America, Rest of La

unique(approval$country)

approval %>%
  mutate(country_group = case_when(
    country %in% c("Argentina", "Chile", "Uruguay") ~ "Southern Cone",
    country %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras", 
                   "Nicaragua", "Panama") ~ "Central America",
    TRUE ~ "Rest of LA"
  )) %>%
  select(country, country_group)

# We make a variable called country group for later use
approval$country_group <- case_when(
  approval$country %in% c("Argentina", "Chile", "Uruguay") ~ "Southern Cone",
  approval$country %in% c("Costa Rica", "El Salvador", "Guatemala", "Honduras", 
                 "Nicaragua", "Panama") ~ "Central America",
  TRUE ~ "Rest of LA")

# Graph corruption over year across countries

plot_a <- ggplot(data = approval, 
       mapping = aes(x = year, y = corruption))
 
plot_a +
  geom_line(mapping = aes(group = country)) +
  facet_wrap(~country_group, nrow = 3)

# Show the mean corruption for these graphs

means <- approval %>% 
  group_by(country_group) %>%
  summarize(mean = mean(corruption))

plot_a +
  geom_line(color = "lightgray", mapping = aes(group = country)) +
  geom_hline(aes(yintercept = mean), data = means, color = "black") +
  scale_x_discrete(expand = c(0,0)) +
  facet_wrap(~country_group, nrow = 3) +
  labs(title = "Corruption in Latin America years (2000 - 2014)",
       y = "Corruption Index",
       x = "Years") +
  theme(panel.spacing = unit(3, "lines")) 
