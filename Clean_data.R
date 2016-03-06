library("data.table"); library("plyr"); library("dplyr"); library("lubridate")


# Download the file if necessary
zipped_file <- "./raw-data-files/foia_apc_6_8_j14_26_28_47_55.zip" ## the name of the zipped file

if (!file.exists(zipped_file)) {
        fileurl <- "http://www.moomers.org/cta-passenger-counts-data/
                        foia_apc_6_8_j14_26_28_47_55.zip"
        download.file(fileurl, zipped_file, method = "curl")
}

raw_data_directory <- "./raw-data-files/FOIA_apc" ## location of the unzipped data

# unzip the file if necessary
if (!dir.exists(raw_data_directory)) {
        unzip(zipped_file)
}

# read the route data of choice into R as a data.table if necessary
file_to_use <- "./raw-data-files/FOIA_apc/apc_47&55.csv"

if (!exists("data4755")) {
        data4755 <- data.table(read.csv(file_to_use))
        data4755 <- mutate(data4755, SURVEY_DATE = mdy(SURVEY_DATE))
}

# subset the dataset to a chosen route over the latest week and group by ride
route_to_use <- 47 #set this to the route you want
weeks_to_use <- 1 #set this to the number of weeks you want

first_day <- max(data4755$SURVEY_DATE) - weeks(weeks_to_use)
lw47 <- data4755 %>% filter(SURVEY_DATE >= first_day) %>% 
        filter(ROUTE_NUMBER == route_to_use) %>%
        mutate(TIME_ACTUAL_ARRIVE = mdy_hms(TIME_ACTUAL_ARRIVE, 
                                            tz = "America/Chicago")) %>%
        group_by(SERIAL_NUMBER) # group by ride

# assign the actual date and time for all stops
first_hour <- 0
threshold <- 5 #arbitrarily high to ensure true two day routes
last_hour <- 23
        # If some stops on the route have are in the last_hour of the day and 
        # some stops are in the first_hour of the day, then add 1 day to the
        # SURVEY_DATE for the stops in the first_hour of the day

        # add a variable to identify rides that span two days
lw47 <- lw47 %>% mutate(twoday = last_hour %in% hour(TIME_ACTUAL_ARRIVE) & 
                                  first_hour %in% hour(TIME_ACTUAL_ARRIVE)) %>%
        # add a variable for the actual date by incrementing SURVEY_DATE by 1
        # when actual time is before 5am for rides that span two days
        mutate(ACTUAL_DATE = as.POSIXct(ifelse(
                                 hour(TIME_ACTUAL_ARRIVE) < threshold & twoday,
                                 SURVEY_DATE + days(1),
                                 SURVEY_DATE),
                        origin = "1970-01-01 05:00.00 UTC")) %>%
        # add a variable that combines the actual date and actual time
        mutate(TRUE_DATE_TIME = ACTUAL_DATE +
                       hours(hour(TIME_ACTUAL_ARRIVE)) +
                       minutes(minute(TIME_ACTUAL_ARRIVE)) +
                       second(TIME_ACTUAL_ARRIVE)) %>%
        # order the dataset by ride and then by true date/time (equivalent of
        # putting the stops in order)
        arrange(SERIAL_NUMBER, TRUE_DATE_TIME)

write.csv(lw47, file = "./clean-data-files/lastweek47.csv")