library(tidyverse)
library(lubridate)
library(magrittr)
library(dplyr)
library(broom)


# Exercise 2
fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")

# part a)
fed_rd.new <- fed_rd %>%
  mutate(rd_budget_frac = rd_budget/total_outlays)

fit <- lm(rd_budget_frac ~ department + year, fed_rd.new)


conf.int <- augment(fit) %>%
  mutate(fit = .fitted) %>%
  mutate(upper = .fitted + 1.96 * .se.fit) %>%
  mutate(lower = .fitted - 1.96 * .se.fit) %>%
  select(rd_budget_frac, department, year, fit, upper, lower)

fed_rd.new <- fed_rd.new %>%
  left_join(.,conf.int)

?mutate

# part b)
ploting <- function(department){
  
  coefs.plot=fed_rd.new[which(fed_rd.new$department==department),]
  
  ggplot(coefs.plot, aes(year, rd_budget_frac))+
    geom_point()+
    geom_line(aes(y=lower), color = "red", linetype = "dashed")+
    geom_line(aes(y=upper), color = "red", linetype = "dashed")+
    geom_smooth()
}

paste("The department wise plots are as shown below")
ploting("NASA")
ploting("NSF")
ploting("DHS")
ploting("DOD")



# exercise 3
# part a)
getwd()

untidy_migrant <- readxl::read_excel("UN_MigrantStockByOriginAndDestination_2015.xlsx", sheet = "Table 16", skip = 15)

head(untidy_migrant)


untidy_migrant <- untidy_migrant %>%
  rename(to_country = ...2)

untidy_migrant <- untidy_migrant %>%
  rename(country_code = ...4)

untidy_migrant <- filter(untidy_migrant,country_code < 900)


migrate_to_canada <- untidy_migrant[which(untidy_migrant$to_country == "Canada"),]
migrate_to_canada <- migrate_to_canada %>% gather(`Afghanistan`:`Zimbabwe`, key = "from_country", value = "count")


migrate_to_canada_cleaned <- select(migrate_to_canada,c(to_country,
                           from_country,count))

migrate_to_canada_cleaned %>%
  arrange(desc(count)) %>%
  head(5)


# part b)


# I exclude the country codes from the agrupations, that is those higher or equal than 900
migrate_from_canada <- untidy_migrant %>%filter(country_code < 900)
head(arrange(migrate_from_canada, desc(Canada)), n = 5)$to_country


# part c)
migration_pairs <- untidy_migrant %>%
  gather(`Afghanistan`:`Zimbabwe`, key = "from_country", value = "count") %>% arrange(desc(count))

#Excluding the unnecessary columns
drop_columns <- c("Total","Other North", "Other South", "Country_code", "..1", "..3", "..5")
pairs_top_10 <- migration_pairs[ , !(names(migration_pairs) %in% drop_columns)]

pairs_top_10 %>%
  select(to_country, from_country) %>%
  head(10)
