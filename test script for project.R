library(tidyverse)
library(ggplot2)
library(shiny)
library(dplyr)
#load the read library for functions
library(readr)
library(fBasics)
library(grid)
library(gridExtra)
library(datasets)

#Set my working directory
path <-setwd("/Users/gbolahanalli/Library/Mobile Documents/com~apple~CloudDocs/Documents/Gbolahan /Hult/Classes/3 Summer One/Introduction to R/GroupAss")

#Define CloudWatch Variable to read the CSV File
CloudWatch <- read_csv("CloudWatch_Traffic_Web_Attack.csv")

#View the CSV File to see the actual data
data()
View(CloudWatch)
glimpse(CloudWatch)
#Give me the list of all columns/variables/Headers with the datatype classification of the CSV file
spec(CloudWatch)

#define variable (country = X, count of country codes = y)
(Country <- CloudWatch$src_ip_country_code)

ggplot(CloudWatch, aes(), )

