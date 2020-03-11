library(tidytext)
library(wordcloud)
library(gutenbergr)
library(lubridate)
library(rvest)
library(stringr)
library(kableExtra)
library(magrittr)
library(tidyverse)


books <- gutenberg_download(gutenberg_id = c(11, 1400),meta_fields = "title")


# (a) Find the 10 most common non-stop-words in Great Expectations. Create a world cloud of them.
books %>% distinct(title)

great_expectations <- books %>%
  filter(title == 'Great Expectations') 

great_expectations.tbl <- 
  tibble(text = great_expectations$text) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, "(?i)^chapter [0-9ivxlc]")))

tidy_great_expectations <- great_expectations.tbl %>%
  unnest_tokens(word, text)

tidy_great_expectations %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

tidy_great_expectations %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, colors=brewer.pal(8, "Dark2")))


# (b) Find the 10 most common bigrams in Great Expectations that do not include stop words.
tidy_great_expectations_bigram <- great_expectations.tbl %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

great_expectations_bigrams_sep <- tidy_great_expectations_bigram %>%
  separate(bigram, c("word1", "word2"), sep = " ")

great_expectations_bigrams_filtered <- great_expectations_bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

na.omit(great_expectations_bigrams_filtered %>% 
          mutate(bigram = str_c(word1, ' ', word2)) %>%
          count(bigram, sort = TRUE)) %>%
  head(10)


# (c) Plot the sentiment for the two books.

tidy_books <- books %>%
  group_by(title) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()  %>%
  unnest_tokens(word, text)

books_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(books_sentiment, aes(index, sentiment, fill = title)) +
  geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
  facet_wrap(~title, ncol = 3, scales = "free_x") + 
  theme(legend.position = "none") +
  geom_smooth(span = .15)


##############################


cleaner <- function(x){
  str_replace_all(x, " \\([0-9]+\\)|(\n)", "")
}

cleaner <- function(x){
  str_replace_all(x, "(^[ \t]+|[ \t]+$)|(\n)", "")
}


weather <- "https://weather.com/weather/hourbyhour/l/USNJ0524:1:US"
# browseURL(weather)

forecast <- weather %>%
  read_html() %>% 
  html_table(fill = TRUE)

length(forecast)
class(forecast)

class(forecast[[1]])

weather.df <- forecast[[1]]

# The column names are misplaced

# Drop the first column
weather.df <- weather.df[,-1]

length(weather.df)

names(weather.df)[7] <- "Wind_"

weather.df <- weather.df %>%
  rename(Time = Description) %>%
  rename(Description = Temp) %>%
  rename(Temp = Feels) %>%
  rename(Feels = Precip) %>%
  rename(Precip = Humidity) %>%
  rename(Humidity = Wind) %>%
  rename(Wind = Wind_)


weather.df$Time <- weather.df$Time %>% 
  str_replace_all("am", "am - ")  %>% 
  str_replace_all("pm", "pm - ")

weather.df <- weather.df %>% separate(Time, c("Time", "Day"), sep = "-") %>% map_df(cleaner)

# Fixing the time, precipitation and humidity percentage. In Precipitation case, I calculate it as a numerical value 
# (from 0 to 1), so when using it on the plot, it keeps it in order. I put Humidity in a percentage format (0 to 100),
# so when using it on the plot is correctly represented.
weather.clean <- weather.df %>% 
  mutate(Time = parse_time(Time, '%I:%M %p')) %>%
  mutate(Precip = as.numeric(sub("%", "", Precip))/100) %>%
  mutate(Humidity = as.numeric(sub("%", "", Humidity)))

weather.clean$Temp <- weather.clean$Temp %>% 
  str_replace_all("°", "") %>%
  as.numeric()


# Because I am checking the data today, if the day is not the same as today's, then it means that it corresponds to tomorrow
weather.clean1 <- weather.clean %>% 
  filter(wday(today(tzone = "EST"), label=TRUE) == weather.clean$Day) %>%
  mutate("Datetime" = as_datetime(paste(today(tzone = "EST"), Time, sep= ' '), tz = "EST"))


weather.clean2 <- weather.clean %>% 
  filter(wday(today(tzone = "EST"), label=TRUE) != weather.clean$Day) %>%
  mutate("Datetime" = as_datetime(paste(today(tzone = "EST") + 1, Time, sep= ' '), tz = "EST"))

weather.clean <- rbind(weather.clean1,weather.clean2)

weather.clean$Datetime <- format(weather.clean$Datetime,format='%Y-%m-%d %H:%M')

# Tidyng wind
weather.clean <- weather.clean %>% separate(Wind, c("Wind_Direction", "Wind_Speed", "Wind_measurement"), sep = " ") %>% map_df(cleaner)
weather.clean <- weather.clean %>% mutate(Wind_Speed = as.numeric(Wind_Speed))

weather.plot <- gather(weather.clean, key="measure", value="value", c("Temp",  "Humidity",  "Wind_Speed"))
weather.plot <- weather.plot %>% 
  mutate(value = as.numeric(value))

# time of day on the x-axis against temperature, humidity, and windspeed
ggplot(weather.plot, aes(Datetime, value, group = 1)) +
  geom_line(color = "dodgerblue4") + geom_point(aes(color=Precip)) + 
  ggtitle("Humidity, Temperature and Wind vs Time of the day") + 
  facet_wrap(~ measure, ncol = 1, scales = "free", strip.position = "left", 
             labeller = labeller( measure = c(Temp = "Temperature (ºF)", Humidity = "Humidity (%)", "Wind_Speed" = "Wind (mph)"))) +
  ylab(NULL) +
  xlab("Time of the day") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  scale_color_brewer(palette = "Dark2")


# Exercise 3



youtube <- "https://www.youtube.com/feed/trending" %>%
  read_html() %>% html_nodes("div") %>% 
  html_text()

youtube.clean <- youtube[2] %>% str_extract_all("(?!= Duration).*(?=views)") %>% unlist()

youtube.clean <- tibble(text = youtube.clean)

youtube.clean <- youtube.clean %>% separate(text, c("title", "rest"), sep = "- Duration: ") %>%
  subset(!is.na(rest)) %>%
  mutate(title = str_trim(title))

youtube.clean <- youtube.clean %>% separate(rest, c("minutes", "rest"), sep = ":")

youtube.clean <- youtube.clean %>% separate(rest, c("seconds", "rest"), sep = "\\.", extra = "merge")

youtube.clean <-  youtube.clean %>% separate(rest, c("rest", "views"), sep = " ago") %>%
  subset(!is.na(views))

youtube.clean <-  youtube.clean %>%
  mutate(rank = 1:dim(youtube.clean)[1])


youtube.clean$time_unit <- word(youtube.clean$rest, -1)

youtube.clean$rest <- 
  youtube.clean$rest %>% 
  str_replace_all("(day)s?", "") %>%
  str_replace_all("(week)s?", "") %>%
  str_replace_all("(hour)s?", "") 

youtube.clean <- youtube.clean %>%
  mutate(rest = str_trim(rest))

youtube.clean$time <-
  youtube.clean$rest %>% str_extract_all("[0-9]+$") 

head(youtube.clean) %>%
  kable() %>%
  kable_styling()

# part a
youtube.week <- youtube.clean %>%
  filter (time_unit == "week" | time_unit == "weeks") %>%
  mutate(post_date = (as.numeric(time)*7))

youtube.hour <- youtube.clean %>%
  filter (time_unit == "hour" | time_unit == "hours") %>%
  mutate(post_date = (as.numeric(time)/24))

youtube.day <- youtube.clean %>%
  filter (time_unit == "day" | time_unit == "days") %>%
  mutate(post_date = (as.numeric(time)))


youtube.clean <- rbind(youtube.hour, youtube.day, youtube.week) %>%
  arrange(rank)

head(youtube.clean) %>%
  kable() %>%
  kable_styling()

# part b
youtube.clean <- youtube.clean %>% 
  mutate(length = as.numeric(minutes) + as.numeric(seconds)/60)

head(youtube.clean) %>%
  kable() %>%
  kable_styling()

# part c
youtube.clean <- youtube.clean %>% 
  mutate(popularity = views/post_date)

head(youtube.clean) %>%
  kable() %>%
  kable_styling()

ggplot(youtube.clean, aes(rank, popularity/1000000, label=title)) + 
  geom_point() + 
  geom_smooth() +
  geom_label(aes(label=ifelse(popularity/1000000>5,as.character(title),NA)),hjust=0, vjust=0) +
  ggtitle("Popularity vs Rank") +
  xlab("Rank") +
  ylab("Popularity (M)")


