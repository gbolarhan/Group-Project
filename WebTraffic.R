######################################################
## Predicting Web Traffic Attack Patterns           ##
## Online Store Transactions                        ##
######################################################
######################################################
## Author: Omogbolahan Alli                         ##
## Email: Omogbolahan Alli                          ##
##  Hult International Business School              ##
######################################################

library(readr)
library(lubridate)
library(ggplot2)
web_traffic <- read_csv("/Users/gbolahanalli/Downloads/CloudWatch_Traffic_Web_Attack 2.csv")
#View(web_traffic)

#Beggining of data Massaging & feature engineering
is.Date(web_traffic$creation_time) #Check if Creation Time is a Data
is.Date(web_traffic$end_time) #Check if End Time is a Data
is.Date(web_traffic$time) #Check if  Time is a Data

table(web_traffic$time) # what insights can we draw from time
table(web_traffic$creation_time) # what insights can we draw from start time of attack
table(web_traffic$end_time) # what insights can we draw from end time of attack

# Find duration of each attack
web_traffic$attack_duration <- web_traffic$end_time - web_traffic$creation_time
View(web_traffic)
table(web_traffic$attack_duration) # All the attacks are 10 minutes long
table(web_traffic$src_ip) # Looks like the attacks are coming from a handful of IPs
# Looks like the data isn't clean. 
# We have US showing up alongside NL, IL(Illinios), CA (California)
table(web_traffic$src_ip_country_code) 

#Clean the data set

table(web_traffic$bytes_in) # No real pattern to Bytes In
table(web_traffic$bytes_out) # No real pattern to bytes out

?ggplot()
summary(web_traffic)