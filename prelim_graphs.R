library("data.table"); library("plyr"); library("dplyr"); library("lubridate")

# read clean data into R
data_to_use <- "./clean-data-files/lastweek47.csv"
data47 <- read.csv(file = data_to_use)

# compare survey date to true date
with(data47,  plot(x = as.POSIXct(SURVEY_DATE), 
                  y = day(as.POSIXct(TRUE_DATE_TIME))))

# passengers on by true date
with(data47, plot(y = PASSENGERS_ON, x = as.POSIXct(TRUE_DATE_TIME)))

# total passengers on by true date
days47 <- data47 %>% group_by(day = day(as.POSIXct(TRUE_DATE_TIME))) %>%
        summarize(passengers = sum(PASSENGERS_ON))
with(days47,  plot(x = day, y = passengers))

# total passengers on by trip
trips47 <- data47 %>% group_by(trip = SERIAL_NUMBER) %>%
        summarize(passengers = sum(PASSENGERS_ON))
with(trips47,  plot(x = trip, y = passengers))
with(trips47, boxplot(passengers ~ trip))

# total passengers on by stop - east
east47 <- data47 %>% filter(DIRECTION_NAME == "East") %>%
        group_by(Stop = MAIN_CROSS_STREET) %>%
        summarize(passengers = sum(PASSENGERS_ON))
with(east47,  plot(x = Stop, y = passengers))

# total passengers on by stop - west
west47 <- data47 %>% filter(DIRECTION_NAME == "West") %>%
        group_by(Stop = MAIN_CROSS_STREET) %>%
        summarize(passengers = sum(PASSENGERS_ON))
with(west47,  plot(x = Stop, y = passengers))
