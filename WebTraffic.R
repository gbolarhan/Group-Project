######################################################
## Predicting Web Traffic Attack Patterns           ##
## Online Store Transactions                        ##
######################################################
######################################################
## Author: Omogbolahan Alli & Tatjana Nill          ##
## Email: oalli@student.hult.edu                    ##
##  Hult International Business School              ##
######################################################
######################################################
#preliminary
getwd()

#below copy the path of your folder where you have the 2 datasets 
setwd("/Users/gbolahanalli/Library/Mobile Documents/com~apple~CloudDocs/Documents/Gbolahan /Hult/Classes/3 Summer One/Introduction to R/GroupAss")

install.packages("readr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("scales")
install.packages("tidyr")
install.packages("gridExtra")
library(readr)
library(lubridate)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(gridExtra)
web_traffic <- read.csv("./CloudWatch_Traffic_Web_Attack.csv")
#View(traffic)
head(web_traffic)
str(web_traffic)

x <- F
x <- TRUE
x <- F
x <- F

is.na(my_value)

?pi
# Copy dataframe before modifications
traffic <- web_traffic

# Begining of EDA & Data Massaging & feature engineering
is.Date(traffic$creation_time) #Check if Creation Time is a Date
is.Date(traffic$end_time) #Check if End Time is a Date
is.Date(traffic$time) #Check if  Time is a Date

?mutate()
# Convert creation_time and end_time to POSIXct
traffic <- traffic %>%
  mutate(
    creation_time = as.POSIXct(creation_time, format = 
                                 "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    end_time = as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    duration = end_time -  creation_time
  )

table(traffic$time) # what insights can we draw from time
table(traffic$creation_time) # what insights can we draw from
# start time of attack
table(traffic$end_time) # what insights can we draw from end time of attack
# 
# # Find duration of each attack
traffic$attack_duration <- traffic$end_time - traffic$creation_time
View(traffic)
table(traffic$attack_duration) # All the attacks are 10 minutes long
table(traffic$src_ip) # Looks like the attacks are coming from a handful
# of source IPs, About 28
table(traffic$bytes_in) # No real pattern to Bytes In
table(traffic$bytes_out) # No real pattern to bytes out
# 
# Looks like the data isn't clean.
# We have US showing up alongside NL, IL(Illinios), CA (California)/Canada
table(traffic$src_ip_country_code)
# 
us_attacks <- traffic %>% filter(src_ip_country_code == "US")
# Filter out traffic coming from the US
# View(us_attacks)
summary(us_attacks)
cor(traffic[, 1:17])
?ggplot()
# 
summary(traffic)
head(traffic)
dim(traffic)

# Describe the Data using graphs
# 
# Data frame named 'traffic' (assuming it's already loaded) 
# Ensure `time`, `creation_time` and `end_time` 
# columns are in POSIXct datetime format

# Aggregate by time for line charts
traffic_agg <- traffic %>%
  group_by(time) %>%
  summarise(total_bytes_in = sum(bytes_in), 
            total_bytes_out = sum(bytes_out))

# Aggregate by source IP
us_attacks_agg <- us_attacks %>%
  group_by(src_ip) %>%
  summarise(total_bytes_in = sum(bytes_in), 
            total_bytes_out = sum(bytes_out),
            num_attacks = n())

# Sort by number of attacks
us_attacks_agg_sorted <- us_attacks_agg %>%
  arrange(desc(num_attacks))

# Get top 10 attackers
top_10_attackers <- us_attacks_agg_sorted %>%
  head(10)

# Merge to get traffic details for top 10 attackers
top_10_attackers_traffic <- top_10_attackers %>%
  left_join(us_attacks, by = "src_ip")
# 1. Scatter Plot: Bytes In vs. Bytes Out by Country
scatter_plot <- ggplot(traffic, aes(x = bytes_in, y = bytes_out, color = 
                                      src_ip_country_code)) +
  geom_point() +
  labs(title = "Bytes In vs. Bytes Out by Country",
       x = "Bytes In",
       y = "Bytes Out",
       color = "Country Code") +
  theme_minimal() +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)

# 2. Histogram: Distribution of Attack Durations
# Calculate attack duration in seconds
traffic$attack_duration <- as.numeric(difftime(
  traffic$end_time, traffic$creation_time, units = "secs"))

duration_hist <- ggplot(traffic, aes(x = attack_duration)) +
  geom_histogram(binwidth = 60, fill = "skyblue", color 
                 = "black") + # Bin width of 60 seconds (1 minute)
  labs(title = "Distribution of Attack Durations",
       x = "Attack Duration (seconds)",
       y = "Frequency") +
  theme_minimal()

# 3. Line Chart: Total Bytes In Over Time
bytes_in_line <- ggplot(traffic_agg, aes(x = time, y = total_bytes_in)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Total Bytes In Over Time",
       x = "Time",
       y = "Total Bytes In") +
  theme_minimal() +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = 
                     date_format("%H:%M")) +  # Adjust breaks as needed
  scale_y_continuous(labels = comma)


# 4. Line Chart: Total Bytes Out Over Time
bytes_out_line <- ggplot(traffic_agg, aes(x = time, y = total_bytes_out)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Total Bytes Out Over Time",
       x = "Start Time",
       y = "Total Bytes Out") +
  theme_minimal() +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = 
                     date_format("%H:%M")) +  # Adjust breaks as needed
  scale_y_continuous(labels = comma)

# 5. Histogram: Bytes In
bytes_in_hist <- ggplot(traffic, aes(x = bytes_in)) +
  geom_histogram(binwidth = 500000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Bytes In",
       x = "Bytes In",
       y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(labels = comma)

# 6.  Scatter plot Total Bytes In/Out
total_bytes_inout <- ggplot(traffic_agg, aes(x = time)) +
  geom_line(aes(y = total_bytes_in, color = "Bytes In")) +
  geom_point(aes(y = total_bytes_in, color = "Bytes In")) +
  geom_line(aes(y = total_bytes_out, color = "Bytes Out")) +
  geom_point(aes(y = total_bytes_out, color = "Bytes Out")) +
  scale_x_datetime(breaks = date_breaks("60 min"), labels = 
                     date_format("%H:%M")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Total Bytes In/Out Over Time",
       x = "Start Time",
       y = "Total Bytes",
       color = "Legend") +
  theme_minimal()

# 7. Bar chart of number of attacks by top 10 attackers
num_of_us_attacks <- ggplot(top_10_attackers, aes(x = reorder(src_ip, -num_attacks), y = num_attacks)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Attackers from US (by Number of Attacks)",
       x = "Source IP Address",
       y = "Number of Attacks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# 8. Bar chart of total bytes sent and received by top 10 attackers
top_10_attackers_long <- top_10_attackers %>%
  pivot_longer(cols = c(total_bytes_in, total_bytes_out), names_to = "Bytes Type", values_to = "Total Bytes")

bytes_by_us_ip <- ggplot(top_10_attackers_long, aes(x = reorder(src_ip, -`Total Bytes`), y = `Total Bytes`, fill = `Bytes Type`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 10 Attackers from US (Total Bytes Sent/Received)",
       x = "Source IP Address",
       y = "Total Bytes",
       fill = "Bytes Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)  # Add commas to y-axis labels

# Display the plots (arranged in a grid)
grid.arrange(scatter_plot, duration_hist, bytes_in_line, bytes_out_line 
             , total_bytes_inout, bytes_in_hist, num_of_us_attacks, 
             bytes_by_us_ip, ncol = 2)