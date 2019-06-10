setwd("C:/Users/Owen.Tolson/Desktop/R Practice/Exploratory Data Analysis")

library(ggplot2)
library(dplyr)
library(lubridate)

data <- data.table::fread(input = "household_power_consumption.txt", na.strings = "?")

###Data Manipulating

#Changing date characters to date and filtering to time specified 
data <- data %>% 
     dplyr::mutate(Date = dmy(Date)) %>% 
     dplyr::filter(Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))


#Plot 1----
data %>% 
     ggplot(aes(x = Global_active_power)) +
     geom_histogram(binwidth = 0.7, color = "black", fill = "red") +
     xlab("Global Active Power (kilowatts)") +
     ylab("Frequency") +
     ggtitle("Global Active Power") + 
     theme_classic() +
     theme(plot.title = element_text(hjust = 0.5)) +
     scale_x_continuous(breaks = c(0, 2, 4, 6)) +
     scale_y_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1200))

ggsave("Plot1.png")

#Plot 2----
plot2 <- data %>% 
     mutate(weekday = wday(Date, label = TRUE), DTG = paste(Date, Time)) %>% 
     mutate(DTG = ymd_hms(DTG)) %>% 
     select(Date, Time, Global_active_power, DTG, weekday) %>% 
     filter(wday(Date) %in% c(5, 6, 7))

plot2 %>% 
     ggplot(aes(x = DTG, y = Global_active_power)) +
     geom_line() +
     ylab("Global Active Power (kilowatts)") +
     xlab("") +
     theme_classic() +
     scale_x_datetime(date_breaks = "1 day", date_labels = "%a")

ggsave("Plot2.png")

#Plot 3----
plot3 <- data %>% 
     mutate(DTG = paste(Date, Time), DTG = ymd_hms(DTG)) %>% 
     select(DTG, Sub_metering_1, Sub_metering_2, Sub_metering_3)

plot3 %>% 
     ggplot() +
     geom_line(aes(x = DTG, y = Sub_metering_1), color = "black") +
     geom_line(aes(x = DTG, y = Sub_metering_2), color = "red") +
     geom_line(aes(x = DTG, y = Sub_metering_3), color = "blue") +
     xlab("") + 
     scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
     ylab("Energy sub metering") +
     theme_classic()

ggsave("Plot3.png")

#Plot 4----

p1 <- data %>% 
     mutate(DTG = paste(Date, Time), DTG = ymd_hms(DTG)) %>% 
     select(DTG, Global_active_power)
p1a <- p1 %>% 
     ggplot(aes(x = DTG, y = Global_active_power)) +
     geom_line() +
     ylab("Global Active Power") +
     xlab("") +
     theme_classic() +
     scale_x_datetime(date_breaks = "1 day", date_labels = "%a")
     
p2 <- data %>% 
     mutate(DTG = paste(Date, Time), DTG = ymd_hms(DTG)) %>% 
     select(DTG, Voltage)
p2a <- p2 %>% 
     ggplot(aes(x = DTG, y = Voltage)) +
     geom_line() +
     ylab("Voltage") +
     xlab("datetime") +
     theme_classic() +
     scale_x_datetime(date_breaks = "1 day", date_labels = "%a")
     
p3 <- data %>% 
     mutate(DTG = paste(Date, Time), DTG = ymd_hms(DTG)) %>% 
     select(DTG, Sub_metering_1, Sub_metering_2, Sub_metering_3)
p3a <- p3 %>% 
     ggplot() +
     geom_line(aes(x = DTG, y = Sub_metering_1), color = "black") +
     geom_line(aes(x = DTG, y = Sub_metering_2), color = "red") +
     geom_line(aes(x = DTG, y = Sub_metering_3), color = "blue") +
     xlab("") + 
     scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
     ylab("Energy sub metering") +
     theme_classic()
     
p4 <- data %>% 
     mutate(DTG = paste(Date, Time), DTG = ymd_hms(DTG)) %>% 
     select(DTG, Global_reactive_power)
p4a <- p4 %>% 
     ggplot(aes(x = DTG, y = Global_reactive_power)) +
     geom_line() +
     ylab("Global_reactive_power") +
     xlab("datetime") +
     theme_classic() +
     scale_x_datetime(date_breaks = "1 day", date_labels = "%a")

gridExtra::grid.arrange(p1a, p2a, p3a, p4a, nrow = 2)

