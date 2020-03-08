
install.packages("Lahman")
library(Lahman)
library(tidyverse)
library(lubridate)



playernames <- Master %>% group_by(birthYear, nameFirst) %>% summarize(count=n())
names(playernames) = c("year","name","n")
#2a
playernames


total_records <- count(Master)
common_years <- table(Master$birthYear==year(Master$birthDate))
different_years_count <-total_records -common_years 
#2b
#Out of total rows of data , for different_years_count instances they are not equal
different_years_count



new_df <- Master %>% select(playerID,nameFirst,nameLast,nameGiven)
new_df

updated_df <- new_df %>% left_join(Fielding %>% select(playerID,G), by = c("playerID"))
names(updated_df) <- c('playerID','first_name','last_name','given_name','G')

updated_dff <- updated_df %>% group_by(playerID,first_name,last_name,given_name) %>% summarise(carrer_total =sum(G))

names(updated_dff) <- c('playerID','first_name','last_name','given_name','carrer_total')

#2c Final
updated_dff

#2d
mutate(updated_dff, Full_Name = str_c(first_name,last_name,sep=" "))  


#2e
updated_dfff <- updated_dff %>% filter(carrer_total >=500)

#updated_dfff <- tbl_df(updated_dfff) %>% group_by(playerID) %>% tally(carrer_total) %>% top_n(5)
#updated_dfff

df_500 <- subset(updated_dfff, select = -c(last_name,given_name,carrer_total))
Master_year <- Master %>% select(playerID, birthYear)
joined_data <- left_join(df_500, Master_year, by = "playerID")

joined_data_firstname_count <- joined_data %>% group_by(first_name) %>% summarize(count=n())
joined_data_firstname_count <- joined_data_firstname_count[order(-joined_data_firstname_count$count),]


head(joined_data_firstname_count,5)
list_of_values <- c("Bill","Mike","Joe","John","Jim")

joined_data_updated <- joined_data %>% filter(first_name %in% list_of_values)
joined_data_updated <- joined_data_updated %>% group_by(birthYear, first_name) %>% summarize(count=n())
joined_data_updated

ggplot(joined_data_updated, aes(x = birthYear, y = count)) + 
    geom_line(aes(color = first_name, linetype = first_name)) + 
    scale_color_manual(values = c("darkred", "steelblue","black","violet","dark blue")) + 
    ggtitle("popularity of names over the year") + 
    xlab("year") + 
    ylab("count")





library(stringr)
restaurants <- read_csv("restaurant.csv")

rodent_count <- str_detect(restaurants$`VIOLATION DESCRIPTION`,"rodent")
sum(rodent_count,na.rm = TRUE)
#2a zero count for rodent

mice_count <- str_detect(restaurants$`VIOLATION DESCRIPTION`,"mice")


rat_count <- str_detect(restaurants$`VIOLATION DESCRIPTION`,"rat")

sum(mice_count,na.rm = TRUE)
#2a count for mice
sum(rat_count,na.rm = TRUE)
#2a count for rat

#2a more number of rats than mice


sum(mice_count & rat_count,na.rm = TRUE)
#2a zero count for mice and rat together


#str_replace(head(susanos$`VIOLATION DESCRIPTION`,1),"vermin","mice")
#str_replace(head(susanos$`VIOLATION DESCRIPTION`,1),"conditions","rat")


roach_count <- str_detect(restaurants$`VIOLATION DESCRIPTION`,"roach")
sum(roach_count,na.rm = TRUE)

flies_count <- str_detect(restaurants$`VIOLATION DESCRIPTION`,"flies")
sum(flies_count,na.rm = TRUE)
#3b more number of flies than roaches 

insect_count <- str_detect(restaurants$`VIOLATION DESCRIPTION`,"roach|flies")
sum(insect_count,na.rm = TRUE)

rodents_count <- str_detect(restaurants$`VIOLATION DESCRIPTION`,"rat|mice")
sum(rodents_count,na.rm = TRUE)

sum(roach_count & flies_count,na.rm = TRUE)
#3b there are no cases of restaurants that are cited for both roaches and flies


sum(insect_count & rodents_count,na.rm = TRUE)
#3c no restaurants have been cited for both rodents and insects



unique(restaurants$BORO,incomparables = FALSE)
distinct(restaurants,BORO)


Manhattan_count <- str_detect(filter(restaurants, BORO == "Manhattan")$`VIOLATION DESCRIPTION`,"rat|mice|roach|flies")
sum(Manhattan_count,na.rm = TRUE)

Queens_count <- str_detect(filter(restaurants, BORO == "Queens")$`VIOLATION DESCRIPTION`,"rat|mice|roach|flies")
sum(Queens_count,na.rm = TRUE)

Staten_count <- str_detect(filter(restaurants, BORO == "Staten Island")$`VIOLATION DESCRIPTION`,"rat|mice|roach|flies")
sum(Staten_count,na.rm = TRUE)

Bronx_count <- str_detect(filter(restaurants, BORO == "Bronx")$`VIOLATION DESCRIPTION`,"rat|mice|roach|flies")
sum(Bronx_count,na.rm = TRUE)

brooklyn_count <- str_detect(filter(restaurants, BORO == "Brooklyn")$`VIOLATION DESCRIPTION`,"rat|mice|roach|flies")
sum(brooklyn_count,na.rm = TRUE)

ranking <- c(sum(Manhattan_count,na.rm = TRUE),sum(Queens_count,na.rm = TRUE),sum(Staten_count,na.rm = TRUE),
             sum(Bronx_count,na.rm = TRUE),sum(brooklyn_count,na.rm = TRUE))
names(ranking) <- c('manhatten','queens','Staten Island','Bronx','Brooklyn')

ranking <- ranking[order(ranking,decreasing = TRUE)]

#3d
barplot(ranking, main="Number of rodents
and/or insect violations distribution across Boroughs",
        xlab="Boroughs",ylab="number of violations")

