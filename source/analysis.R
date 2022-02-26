library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
# king county data
incarcination_file <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv",stringsAsFactors = FALSE)
king_county_data <- incarcination_file %>%
  filter(county_name == "King County") %>%
  filter(state == "WA")
# total black population in king county
black_pop_king <- king_county_data %>%
  filter(year == max(year)) %>%
  pull(black_pop_15to64)
#total white population in king county aged 15 to 64
white_pop_king <- king_county_data %>%
  filter(year == max(year)) %>%
  pull(white_pop_15to64)
#total Asian population in king county aged 15 to 64
asian_pop_king <- king_county_data %>%
  filter(year == max(year)) %>%
  pull(aapi_pop_15to64)
#percentage of black americans in king county aged 15to64
percentage_blk_king <-round(black_pop_king/(white_pop_king + asian_pop_king + black_pop_king) * 100)
# percentage of black american in king county in jail as of 2018
per_blkjail_king <- king_county_data %>%
  filter(year == max(year)) %>%
  select(black_jail_pop,total_jail_pop)
per_blkjail_king <- per_blkjail_king %>%
  mutate(percent_blk_jail = round(black_jail_pop/total_jail_pop * 100)) %>%
  pull(percent_blk_jail)
# percentage of white americans in jail in king county 
per_wtjail_king <- king_county_data %>%
  filter(year == max(year)) %>%
  select(white_jail_pop,total_jail_pop)
per_wtjail_king <- per_wtjail_king %>%
  mutate(percent_wt_jail = round(white_jail_pop/total_jail_pop * 100)) %>%
  pull(percent_wt_jail)

# percentage of Asian Americans in jail in King county 
per_asianjail_king <- king_county_data %>%
  filter(year == max(year)) %>%
  select(aapi_jail_pop,total_jail_pop)
per_asianjail_king <- per_asianjail_king %>%
  mutate(percent_asian_jail = round(aapi_jail_pop/total_jail_pop * 100)) %>%
  pull(percent_asian_jail)

# average jail pre trial in King County 
ave_jail_king <- king_county_data %>%
  select(total_jail_pretrial,year)
ave_jail_king <- ave_jail_king[16:49,]
ave_jail_king <- ave_jail_king %>%
  select(total_jail_pretrial) %>%
  sum()/34
ave_jail_king <- round(ave_jail_king)
#gets data for trends over time in king county.
trend_over_time_king <- king_county_data %>%
  select(year,black_jail_pop,total_jail_pretrial)
trend_over_time_king <- trend_over_time_king[16:49,]
trend_over_time_king <- ggplot(data = trend_over_time_king, mapping = aes(x = year)) +
  geom_line(mapping = aes(y = black_jail_pop, color = "Black Jail population")) +
  geom_line(mapping = aes(y = total_jail_pretrial, color = "Total jail Pretrial population")) +
  labs(x = "year", y = "Population",
       title = "Comparison between pretrial jail populations and black populations in jail",
       colour = ""
  )

# kitsap county data
kitsap_county_data <- incarcination_file %>%
  filter(county_name == "Kitsap County") %>%
  filter(state == "WA")        
# total black population in king county
black_pop_kitsap <- kitsap_county_data %>%
  filter(year == max(year)) %>%
  pull(black_pop_15to64)
#total white population in king county aged 15 to 64
white_pop_kitsap <- kitsap_county_data %>%
  filter(year == max(year)) %>%
  pull(white_pop_15to64)
#total Asian population in kitsap county aged 15 to 64
asian_pop_kitsap <- kitsap_county_data %>%
  filter(year == max(year)) %>%
  pull(aapi_pop_15to64)
#percentage of black americans in king county aged 15to64
percentage_blk_kitsap <-round(black_pop_kitsap/(white_pop_kitsap + asian_pop_kitsap + black_pop_kitsap) * 100)
# percentage of black american in king county in jail as of 2018
per_blkjail_kitsap <- kitsap_county_data %>%
  filter(year == max(year)) %>%
  select(black_jail_pop,total_jail_pop)
per_blkjail_kitsap <- per_blkjail_kitsap %>%
  mutate(percent_blk_jail = round(black_jail_pop/total_jail_pop * 100)) %>%
  pull(percent_blk_jail)
# percentage of white americans in jail in king county 
per_wtjail_kitsap <- kitsap_county_data %>%
  filter(year == max(year)) %>%
  select(white_jail_pop,total_jail_pop)
per_wtjail_kitsap <- per_wtjail_kitsap %>%
  mutate(percent_wt_jail = round(white_jail_pop/total_jail_pop * 100)) %>%
  pull(percent_wt_jail)

# percentage of Asian Americans in jail in King county 
per_asianjail_kitsap <- kitsap_county_data %>%
  filter(year == max(year)) %>%
  select(aapi_jail_pop,total_jail_pop)
per_asianjail_kitsap <- per_asianjail_kitsap %>%
  mutate(percent_asian_jail = round(aapi_jail_pop/total_jail_pop * 100)) %>%
  pull(percent_asian_jail)

# average jail pre trial in Kitsap County 
ave_jail_kitsap <- kitsap_county_data %>%
  select(total_jail_pretrial,year)
ave_jail_kitsap <- ave_jail_kitsap[16:49,]
ave_jail_kitsap <- ave_jail_kitsap %>%
  select(total_jail_pretrial) %>%
  sum()/34
ave_jail_kitsap <- round(ave_jail_kitsap)

## variable comparison chart 
add_kitsap <- kitsap_county_data %>%
              filter(year == max(year)) %>%
              select(county_name,black_jail_pop,white_jail_pop,aapi_jail_pop)
variable_com_king <- king_county_data %>%
                    filter(year == max(year)) %>%
                    select(county_name,black_jail_pop,white_jail_pop,aapi_jail_pop) %>%
                    add_row(county_name = "Kitsap County",black_jail_pop = 64.0,white_jail_pop = 353,aapi_jail_pop = 22) %>%
                    gather(key = Race, value = population, - county_name)

variable_com_king2 <- ggplot(variable_com_king) +
                     geom_col(mapping = aes(x = county_name, y = population, fill = Race)) + 
                      labs (
                      title= "The proportion of races in jail for King and Kitsap county (2018)"
                      )
