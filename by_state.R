library('tidyverse')
library('janitor')

# equine census by state

census.states <- read_csv('states_census.csv') %>% clean_names() %>% 
  mutate(state_fips = str_pad(state_ansi, 2, pad = 0))

census.states.totals <- census.states %>% 
  group_by(state, state_fips) %>% 
  summarise(total = sum(value))

# https://www.census.gov/quickfacts/fact/note/US/LND110210

land.area <- read_csv('landarea.csv', skip = 1) %>% clean_names() %>% 
  select(target_geo_id2, geographic_area_1, area_in_square_miles_land_area) %>% 
  mutate(state_fips = str_pad(target_geo_id2, 2, pad = 0))

# equine animals per square mile

census.states.totals.merged <- merge(census.states.totals, 
                                     land.area,
                                     by = 'state_fips') %>% 
  mutate(equine_animals_per_sq_mi = total/area_in_square_miles_land_area)

# ranked in terms of equine animals per square mile
census.states.totals.merged %>% select(state, 
                                       equine_animals_per_sq_mi) %>% arrange(desc(equine_animals_per_sq_mi)) %>% View()

# ranked in terms of equine animal population
census.states.totals.merged %>% select(state, 
                                       total) %>% arrange(desc(total)) %>% View()




