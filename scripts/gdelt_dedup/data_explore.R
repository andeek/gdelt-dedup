# libraries
library(tidyverse)

# load data
gdelt <- read_csv("data/gdelt/westafrica_conflict_evts_1979-2019_gdelt.csv") %>%
  filter(Year != "Year")

dim(gdelt)

head(gdelt)

# potential blocking
gdelt %>%
  group_by(Year, ActionGeo_CountryCode) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  ggplot() +
  geom_histogram(aes(count))

gdelt %>%
  group_by(Year, ActionGeo_CountryCode) %>%
  summarise(count = n()) %>%
  group_by(count) %>%
  filter(count > 10000)

gdelt %>%
  group_by(GLOBALEVENTID) %>%
  mutate(count = n()) %>%
  filter(count > 1)
