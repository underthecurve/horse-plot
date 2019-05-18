library('tidyverse')
library('janitor')
library('tidycensus')
library('emoGG')

# inspiration: 

## https://twitter.com/christinezhang/status/1129777985593200641 --> Maryland Horse Industry Board website incorrectly states: "Fact - Maryland has more horses per square mile than any other state..." maybe this was true in 2010, but now it's NJ:
## https://www.nj.com/data/2018/02/there_are_way_more_horses_in_new_jersey_than_youd.html

# horse emoji id
emoji_search("horse")

# median household income data
md.income <- get_acs(geography = "county", 
              variables = c(med.income = "B19013_001",
                            med.income.pc = "B19301_001",
                            pop = "B01003_001"), 
              state = "MD", output = "wide") %>%
  mutate(cty_fips = str_sub(GEOID, -3, -1))

# horse census data
horses.mules <- read_csv('census.csv') %>% clean_names() %>% 
  mutate(cty_fips = str_pad(county_ansi, 
                            3, pad = 0))

horses.mules.totals <- horses.mules %>% 
  group_by(name, land_area, cty_fips) %>% 
  summarise(total = sum(value)) %>% 
  mutate(per_sq_mi = total/land_area,
         round.psm = round(per_sq_mi))

# merge with income data
horses.mules.merged <- merge(horses.mules.totals, 
                             md.income %>% select(cty_fips, 
                                                  med.incomeE, 
                                                  med.income.pcE,
                                                  popE), by = 'cty_fips')

# plot
ggplot(horses.mules.merged, 
       aes(x = med.incomeE/1000, 
           y = per_sq_mi)) +
  geom_emoji(emoji="1f434", size = .07) +
  scale_y_continuous(limits = c(0,8))+
  scale_x_continuous(breaks = seq(40,120,20),
                     limits = c(39, 120),
                     labels = c("40k", "60k", "80k", "100k", "120k")) +
  labs(x = "\nMedian household income (000's)", 
       y = "Horses* per sq. mile\n",
       title = "Equine-nomics",
       subtitle = 'a very serious scatterplot\n') +
  theme(text=element_text(size=18),
        plot.title = element_text(face = 'bold', size = 20)) 

ggsave('chart.pdf', width = 8, height = 8, device = 'pdf') # format in illustrator




