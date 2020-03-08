phd_by_field <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

head(phd_by_field)

?boxplot


phd_by_field %>% split(.$major_field) %>% 
  map(~ lm(n_phds ~ year + field, data = .)) %>% 
  map(summary) %>% 
  map_dbl("r.squared")







phd_by_majorfield_rsquare_value <- phd_by_field %>% split(.$major_field) %>% 
  map(~lm(n_phds ~ year + field, data = .)) %>% 
  map(summary) %>% map("r.squared") 



phd_by_field %>% split(.$major_field) %>% 
  map(~ lm(n_phds ~ year + field, data = .)) %>% 
  map(summary) %>% 
  map_dbl("r.squared") %>% 
  tibble(major_field = names(.), slope = .) %>%
  left_join(.,phd_by_field) %>%
  select(broad_field,slope) %>%
  ggplot(aes(broad_field, slope)) + geom_boxplot(color = 'dodgerblue4', fill="deepskyblue3")



typeof(phd_by_majorfield_rsquare_value)


df1 <- do.call(rbind, lapply(phd_by_majorfield_rsquare_value, data.frame, stringsAsFactors=FALSE))

names(df1) <- c("major_field","R")

df <- phd_by_field %>% select(broad_field,major_field,n_phds) %>% group_by(broad_field,major_field) %>%
  summarise(count=sum(n_phds,na.rm = TRUE)) 

head(df,20)



for (value in phd_by_majorfield_rsquare_value)
{
value
  }
  

############

phd_by_field %>% select(field,year,n_phds) %>%
  mutate(G_rank = min_rank(desc(n_phds))) %>%
  arrange(year)

#########


phd_by_field_agg_year <- group_by(phd_by_field,major_field,field) %>% 
summarise(agg_phds = sum(n_phds,na.rm=TRUE))
  
#sum(phd_by_field_agg_year$agg_phds)
phd_by_field_agg_year %>% 
  mutate(Rank = min_rank(desc(agg_phds))) %>%
  arrange(major_field,Rank)

#########

phd_by_field_agg_field <- group_by(phd_by_field,broad_field,year) %>% 
  summarise(agg_phds = sum(n_phds,na.rm=TRUE))

#sum(phd_by_field_agg_field$agg_phds)
phd_by_field_agg_field %>% 
  mutate(Rank = min_rank(desc(agg_phds))) %>%
  arrange(broad_field,Rank)




##########################


#assigning zero value to NA fields

phd_by_field$n_phds[is.na(phd_by_field$n_phds)] = 0

SPLIT_DF <- phd_by_field %>% select(field,year,n_phds) %>% arrange(field) %>% split(.$field)


number_of_fields <- length(unique(sort(phd_by_field$field)))
print(SPLIT_DF[1]$'Accounting'$n_phds)


value<-"Accounting"
mutate(SPLIT_DF[1]$value, prev_year_phds = lag(SPLIT_DF[1]$value$n_phds))



i <- 1
for (value in  unique(sort(phd_by_field$field))){
  print("inside for loop" )
  print(value)
  if(i<number_of_fields){
    #print(SPLIT_DF[i])
    #typeof(SPLIT_DF[i]$value)
    print("inside if loop")
    print(i)
    print(mutate(SPLIT_DF[1]$"Accounting", prev_year_phds = lag(SPLIT_DF[1]$"Accounting"$n_phds)))
    
    
    mutate(SPLIT_DF[i]$value, prev_year_phds = lag(SPLIT_DF[i]$value$n_phds))
    i=i+1
  }
}


###############
phd_by_field %>% select(field,year,n_phds) %>% arrange(field) %>%
  filter( n_phds > lag(n_phds))

##############

phd_by_field %>% select(broad_field,n_phds) %>% arrange(broad_field) %>% split(.$broad_field) %>%
map(~quantile(.$n_phds,na.rm = TRUE))

#############
signal_to_noise <- function(broad_name){
  
  if(broad_name %in% unique(phd_by_field$broad_field)){
    updated_dataframe <- phd_by_field %>% filter(phd_by_field$broad_field == broad_name)
    sd_value <- sd(updated_dataframe$n_phds,na.rm = TRUE)
    mean_value <- mean(updated_dataframe$n_phds,na.rm = TRUE)
    
    return (mean_value/sd_value)
  }
  
  else
    print("error message: Please enter a valid entry")
  
}

signal_to_noise("Life sciences")

############

updated_dataframe <- phd_by_field %>% filter(phd_by_field$broad_field == "Life sciences")
sd(updated_dataframe$n_phds,na.rm = TRUE)
mean(updated_dataframe$n_phds,na.rm = TRUE)
