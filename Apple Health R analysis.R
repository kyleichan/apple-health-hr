library(XML)
library(tidyverse)
library(lubridate)
library(scales)
library(ggthemes)

#------------------------------Extract Data---------------------------------------------
xml <- xmlParse('export.xml')

df_record <- XML:::xmlAttrsToDataFrame(xml["//Record"])

df <- df_record %>%
  mutate(device = gsub(".*(name:)|,.*", "",device),
         value = as.numeric(as.character(value)),
         type = str_remove(type, "HKQuantityTypeIdentifier")
  ) # This code comes from: https://taraskaduk.com/posts/2019-03-23-apple-health/

#------------------------------Get heart rate data---------------------------------------------
hr <- df %>%
  filter(type == "HeartRate") %>%
  filter(device == "Apple Watch") %>%
  mutate(datetime = as_datetime(startDate)) %>% # datetime is combined date and time
  mutate(date = as.Date(datetime)) %>% # date is just date from datetime
  mutate(time = format(datetime, "%H:%M:%S")) %>% # time is just time from datetime
  mutate(value = as.integer(value)) %>% # value is the actual heart rate reading
  select(c(datetime, date, time, value))


# For each day, get the count of HR values below a certain threshold and the lowest HR value for that day
hr_day <- hr %>%
  mutate(low = if_else(value < 45, 1, 0),
         low45 = if_else(value <= 45, 1, 0),
         low46 = if_else(value <= 46, 1, 0),
         low47 = if_else(value <= 47, 1, 0),
         low48 = if_else(value <= 48, 1, 0),
         low49 = if_else(value <= 49, 1, 0),
         low50 = if_else(value <= 50, 1, 0)) %>%  # create new binary value: 1 = below HR threshold, 0 = above HR threshold
  group_by(date) %>%
  summarise(min = min(value), count = sum(low),
            count45 = sum(low45),
            count46 = sum(low46),
            count47 = sum(low47),
            count48 = sum(low48),
            count49 = sum(low49),
            count50 = sum(low50)) # "count" = the total number of HR values below threshold per day, "min" = lowest HR value for each day


# For each day, for each HR value, count the number of readings at that HR value
hr_spread <- hr %>%
  group_by(date, value) %>%
  tally() %>%
  filter(value <= 50)

windows()
hr_spread %>%
  ggplot(aes(y=n, x=date)) + 
  geom_bar(position="stack", stat="identity", aes(fill=value)) +
  scale_y_continuous(limits=c(0,100), expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 month") +
  theme_bw() +
  scale_fill_gradientn(colours = rainbow(3))



    # Old code: spread the HR table
    hr_spread <- hr %>%
      group_by(date, value) %>%
      tally() %>%
      spread(value, n)







# Look at data within a date range
hr_day <- hr_day %>%
  mutate(good_day = if_else(date >= "2020-09-14", 1, 0))

# Plot count of HR values below threshold by day
windows()
hr_day %>%
  ggplot(aes(x=date, y=count49, group=good_day, color=good_day)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(breaks=seq(0, max(hr_day$count), 1))

# Plot minimum daily HR value for each day
windows()
hr_day %>%
  ggplot(aes(x=date, y=min, group=good_day, color=good_day)) +
  geom_point() +
  scale_x_date(date_breaks = "1 month")

hr_range <- hr %>%
  filter(date >= "2020-08-14") %>%
  mutate(good_day = if_else(date >= "2020-09-14", 1, 0))

windows()
hr_range %>%
  ggplot(aes(x=datetime, y=value, group=good_day, color=good_day)) +
  geom_point() +
  theme_light() +
  scale_y_continuous(limits=c(40,50), breaks=seq(40, 50, 1)) +
  scale_x_datetime(labels = date_format("%Y-%m-%d %H"), date_breaks = "24 hours")



# Create CSV files
write.csv(hr_day, "Apple HR data.csv")
write.csv(hr, "Apple HR data.csv")



#------------------------------Other---------------------------------
hr_today <- hr %>%
  filter(day == "2020-04-13")

hr_lowest <- hr %>%
  arrange(value) %>%
  slice(1:100)

hr_lowest <- hr %>%
  filter(value <= 45)

windows()
hr_lowest %>%
  ggplot(aes(x=as.Date(date), y=value)) +
  geom_point() +
  theme_classic(base_size = 14) +
  theme(panel.grid.major = element_line(size = 0.5), plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m/%d")


#------------------------------Regression---------------------------------
fit <- lm(count ~ min, data=hr_day)
summary(fit)

windows()
hr_day %>%
  ggplot(aes(x=min, y=count)) +
  geom_point() +
  geom_smooth(method=lm)
#------------------------------------------------------------------------
