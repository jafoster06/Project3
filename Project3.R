---
title: "Project 3"
author: "Saif Farooq"
date: "2025-11-15"
---

# Packages
library(tidyverse)
library(dpylr)
library(ggplot2)
library(lubridate)
library(kableExtra)

# Dataset
metrobike <- read_delim("metrobike.csv")

# Filter by year
metrobike <- filter(metrobike, Year >= 2017 & Year <= 2023)

# Filter by necessary variables
metrobike <- select(metrobike, "Membership or Pass Type", "Trip Duration Minutes", "Checkout Date", "Year", "Month", "Checkout Time", "Checkout Kiosk", "Return Kiosk")
names(metrobike)[names(metrobike) == "Membership or Pass Type"] <- "Membership-Pass"

# Grouping Categories of Membership/Pass Type
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "U.T. Student Membership"] <- "Student Membership"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "UT Student Membership"] <- "Student Membership"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "UT Student Membership"] <- "Student Membership"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Local365"] <- "Local"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Local31"] <- "Local"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Local30"] <- "Local"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Local365- 1/2 off Anniversary Special"] <- "Local"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Local365 Youth (age 13-17 riders)"] <- "Local"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Local365+Guest Pass- 1/2 off Anniversary Special"] <- "Local"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Local365 Youth with helmet (age 13-17 riders)"] <- "Local"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Walk Up"] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Pay-as-you-ride"] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "24 Hour Walk Up Pass"] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Single Trip (Pay-as-you-ride)"] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Single Trip "] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Single Trip"] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "Single Trip Ride"] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "$1 Pay by Trip Winter Special"] <- "Per Trip"
metrobike$`Membership-Pass`[metrobike$`Membership-Pass` == "3-Day Explorer "] <- "Explorer"

# Filter to only include 4 membership types
metrobike <- filter(metrobike, `Membership-Pass` == "Student Membership" | `Membership-Pass` == "Local" | `Membership-Pass` == "Per Trip" | `Membership-Pass` == "Explorer")

# Change Months to Seasons
metrobike$Season <- NA
metrobike$Season[metrobike$Month == 12 | metrobike$Month == 1 | metrobike$Month == 2] <- "Winter"
metrobike$Season[metrobike$Month == 3 | metrobike$Month == 4 | metrobike$Month == 5] <- "Spring"
metrobike$Season[metrobike$Month == 6 | metrobike$Month == 7 | metrobike$Month == 8] <- "Summer"
metrobike$Season[metrobike$Month == 9 | metrobike$Month == 10 | metrobike$Month == 11] <- "Fall"

# Change Date to Weekend/Weekday
metrobike$EndDay <- NA
metrobike$EndDay <- mdy(metrobike$`Checkout Date`)
metrobike$EndDay <- wday(metrobike$EndDay)
metrobike$EndDay <- ifelse(metrobike$EndDay == 1 | metrobike$EndDay == 7, "Weekend", "Weekday")

# Seed
set.seed(83)

# Sampling
stdt <- filter(metrobike, `Membership-Pass` == "Student Membership")
stdt_sample <- sample_n(stdt, 5000)
local <- filter(metrobike, `Membership-Pass` == "Local")
local_sample <- sample_n(local, 5000)
pertrip <- filter(metrobike, `Membership-Pass` == "Per Trip")
pertrip_sample <- sample_n(pertrip, 5000)
explorer <- filter(metrobike, `Membership-Pass` == "Explorer")
explorer_sample <- sample_n(explorer, 5000)

# Final
metrobikeF <- rbind(stdt_sample, local_sample, pertrip_sample, explorer_sample)

# Question 1: 
  # How is trip duration affected by the seasons?

# Without Outliers
ggplot(metrobikeF, aes(x = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), y = `Trip Duration Minutes`, fill = Season)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 60)) +
  labs(title = "Trip Duration by Season (Up to 1 hour)",
       x = "Season",
       y = "Trip Duration (Minutes)") +
  theme_bw() +
  scale_fill_manual(values = c("#E38A5E", "#529B4F", "#F2CE7E", "#72C2A9")) +
  theme(legend.position = "none")

# With Outliers
ggplot(metrobikeF, aes(x = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")), y = `Trip Duration Minutes`, fill = Season)) +
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 300)) +
  labs(title = "Trip Duration by Season (Up to 5 hours)",
       x = "Season",
       y = "Trip Duration (Minutes)") +
  theme_bw() +
  scale_fill_manual(values = c("#E38A5E", "#529B4F", "#F2CE7E", "#72C2A9")) +
  theme(legend.position = "none")

# Statistics
Q1stats <- data.frame(
          Season = c("Winter", "Spring", "Summer", "Fall"),
          Median = c(
            median(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Winter"]),
            median(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Spring"]),
            median(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Summer"]),
            median(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Fall"])),
          IQR = c(
            IQR(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Winter"]),
            IQR(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Spring"]),
            IQR(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Summer"]),
            IQR(metrobikeF$`Trip Duration Minutes`[metrobikeF$Season == "Fall"]))
)

# Better Table
Q1stats %>%
  kbl() %>%
  kable_styling()

# Question 2: 
  # How does trip volume differ between membership/pass types on weekdays versus weekends?

ggplot(metrobikeF, aes(x = EndDay, fill = `Membership-Pass`)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Trip Volume by Membership Type: Weekday vs Weekend",
       x = "Day Type",
       y = "Number of Trips",
       fill = "Membership Type") +
  theme_minimal() +
  scale_fill_manual(values = c("#dd5128", "#0d7ba3", "#42b280", "#fbb257"))

# Statistics

Q2stats <- data.frame(
    Membership_Type = c("Student Membership", "Local", "Per Trip", "Explorer"),
    Weekday = c(
      sum(metrobikeF$`Membership-Pass` == "Student Membership" & metrobikeF$EndDay == "Weekday"),
      sum(metrobikeF$`Membership-Pass` == "Local" & metrobikeF$EndDay == "Weekday"),
      sum(metrobikeF$`Membership-Pass` == "Per Trip" & metrobikeF$EndDay == "Weekday"),
      sum(metrobikeF$`Membership-Pass` == "Explorer" & metrobikeF$EndDay == "Weekday")),
    Weekend = c(
      sum(metrobikeF$`Membership-Pass` == "Student Membership" & metrobikeF$EndDay == "Weekend"),
      sum(metrobikeF$`Membership-Pass` == "Local" & metrobikeF$EndDay == "Weekend"),
      sum(metrobikeF$`Membership-Pass` == "Per Trip" & metrobikeF$EndDay == "Weekend"),
      sum(metrobikeF$`Membership-Pass` == "Explorer" & metrobikeF$EndDay == "Weekend"))
)

# Better Table
Q2stats %>%
  kbl() %>%
  kable_styling()







#Question 4
  #What time has the greatest amount of checkouts and returns at PCL?
checkoutPCL <- filter(metrobikeF, `Checkout Kiosk` == "21st/Speedway @ PCL" | `Checkout Kiosk` == "21st & Speedway @PCL")
returnPCL <- filter(metrobikeF, `Return Kiosk` == "21st/Speedway @ PCL" | `Return Kiosk` == "21st & Speedway @PCL")
ggplot(checkoutPCL, aes(x = `Checkout Time`)) + geom_histogram() + theme_classic()
ggplot(returnPCL, aes(x = `Checkout Time`)) + geom_histogram() + theme_classic()
CheckoutTimePCl <- checkoutPCL$`Checkout Time`
CheckoutTimePeriodPCl <- hms(CheckoutTimePCl)
numeric_hoursCheckoutPCL <- hour(CheckoutTimePeriodPCl) + minute(CheckoutTimePeriodPCl) / 60 + second(CheckoutTimePeriodPCl) / 3600
mean(numeric_hoursCheckoutPCL)



