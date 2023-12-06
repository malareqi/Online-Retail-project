# Title: "PH125.9x - Capstone: Online Retail Project"
# Author: "Mohammed Al-Areqi"
# Date: "2023-12-01"

## Overview & Preparation ##

# Install all needed packages for the project if not installed

if(!require(dplyr)) 
  install.packages("dplyr", repos = "http://cran.us.r-project.org") 
if(!require(readr))
  install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org") 
if(!require(kableExtra)) 
  install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(hablar)) 
  install.packages("hablar", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) 
  install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(forcats))
  install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(stringr))
  install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))
  install.packages("ggfortify", repos = "http://cran.us.r-project.org")
if(!require(scales))
  install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) 
  install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(factoextra))
  install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readxl)) 
  install.packages("readxl", repos = "http://cran.us.r-project.org")

# Load all needed libraries for the project.

library(dplyr)
library(readr)
library(tidyverse)
library(kableExtra)
library(hablar)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(ggfortify)
library(scales)
library(lubridate)
library(factoextra)
library(caret)
library(readxl)

#Online Retail data set
#Extract the data from an excel format at UC Irvine Machine learning Repository below
# https://archive.ics.uci.edu/dataset/352/online+retail
# https://archive.ics.uci.edu/static/public/352/online+retail.zip

url <- "https://archive.ics.uci.edu/static/public/352/online+retail.zip"
temp_d <- tempdir()
temp_f <- tempfile(tmpdir = temp_d, fileext = ".zip")
if(!file.exists(temp_f))
  download.file(url, temp_f)

fname <- unzip(temp_f, list = TRUE)$Name[1]
if(!file.exists(fname))
  unzip(temp_f, files = fname, exdir = temp_d, overwrite = TRUE)
fpath <- file.path(temp_d, fname)
retail <- read_excel(fpath)

## Data Structure ##

# A quick display of the data
head(retail)

# Summary and structure of the data
str(retail, vec.len = 2)

## Data Cleaning ##

# Convert the data into data frame
retail <- as.data.frame(retail)

# Covert the InvoiceDate to only a Date format
retail$InvoiceDate <- as.Date(retail$InvoiceDate)

# Check for NA cells
colSums(is.na(retail)) %>% kable(format = "pipe")

# Remove the rows that contain N/A 
retail <- retail %>% drop_na()

# Check for NA cells if removed
colSums(is.na(retail)) %>% kable(format = "pipe")

# Summary and structure of the data
str(retail, vec.len = 2)

## Analysis ##

# Adding the total amount of purchase per row
retail <- retail %>% mutate(Total = Quantity*UnitPrice)
head(retail)

# Number of unique values for InvoiceNo, StockCode, CustomerID, and Country in retail data
retail %>% summarize(Invoices = n_distinct(InvoiceNo),
                     StockCodes = n_distinct(StockCode),
                     Customers = n_distinct(CustomerID),
                     Countries = n_distinct(Country)) %>% 
        kable( digits = 4, format = "pipe")

### Product Analysis ###

# Value of the top 10 products per unit
options(digits=3)
retail %>% 
  group_by(Description) %>%
  filter (!(Description %in%
              c("Manual", "POSTAGE", "DOTCOM POSTAGE",
                "CRUK Commission", "Discount" ))) %>% #Removing unrelated transactions
  summarize(UnitPrice =  mean(UnitPrice)) %>%
  arrange(desc(UnitPrice)) %>% 
  slice(1:10) %>% 
  kable(format = "pipe")

# Value of the 10 cheapest products per unit
retail %>% 
  group_by(Description) %>% 
  filter (!(Description %in%
              c("Manual", "POSTAGE", "DOTCOM POSTAGE",
                "CRUK Commission", "Discount" ))) %>% #Removing unrelated transactions
  summarize(UnitPrice =  mean(UnitPrice)) %>%
  arrange(UnitPrice) %>%
  slice(1:10) %>% 
  kable(format = "pipe")

# Distribution of the top 15 Highest sold products per unit
retail %>% group_by(Description) %>% 
  summarize(count =n()) %>%
  arrange(desc(count)) %>% 
  slice(1:15) %>% 
  ggplot(aes(fct_reorder(Description, desc(count)), count))+ 
  geom_col()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title = "Stock Type sold", x = "")

# Display of the top 15 Highest sold products per unit
retail %>% group_by(Description) %>% 
  summarize(count =n(), PriceUnit = mean(UnitPrice)) %>%
  arrange(desc(count)) %>% 
  slice(1:15) %>% 
  kable(format = "pipe")

### Country Analysis ###

# Distribution of the amount of purchases for the top 15 countries
retail %>% group_by(Country) %>%
  summarize(amount = sum(Total)) %>%
  arrange(desc(amount)) %>%
  slice(1:15) %>% 
  ggplot(aes(fct_reorder(Country, desc(amount)), amount))+
  geom_col()+ 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(title = "Amount of purchases per Country", x = "Country", y = "Total £") +
  scale_y_continuous(label=comma)

# Display of the amount of purchases for the top 15 countries including the percentage
retail %>% group_by(Country) %>%
  summarize(amount = sum(Total)) %>%
  mutate(percent = percent(amount/sum(amount), accuracy = 0.01)) %>% 
  arrange(desc(amount)) %>% 
  kable(format = "pipe")

### Time Analysis ###

# Distribution of the total amount of purchases per month
retail %>% 
  mutate(month = format_ISO8601(retail$InvoiceDate, precision = "ym")) %>%
  group_by(month) %>%
  summarize(Total = sum(Total), products = sum(Quantity)) %>% 
  ggplot(aes(x = reorder(month, -Total) , Total)) +
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90)) +
  scale_y_continuous(label=comma) +
  ggtitle("Total amount of purchases per Month") +
  xlab("Month") +
  ylab("Total (£)")

### Customer Analysis ###

# Display of the top 15 customers with the highest purchases
retail %>% group_by(CustomerID) %>% 
  summarize(invoices = n_distinct(InvoiceNo), Country = Country[1],Total = sum(Total)) %>% 
  arrange(desc(Total)) %>%
  slice(1:15)

## RFM Analysis ##

# Assign a_date variable a date value of first of January 2012. 
a_date <- as.Date("2012-01-01")

# Calculate and create RFM table using the retail data
retail_RFM <- retail %>% 
  group_by(CustomerID) %>% 
  summarize(Recency = as.numeric(a_date - max(as.Date(InvoiceDate))),
            Frequency = n_distinct(InvoiceNo), 
            Monetary = sum(Total))
head(retail_RFM) %>% kable(format = "pipe")

## K-means Cluster ##

# Removing the CustomerID column from the retail_RFM table
RFM <- retail_RFM[2:4]
head(RFM) %>% kable(format = "pipe")

# Scale the RFM data
RFM_scale <- scale(RFM)
head(RFM_scale) %>% kable(format = "pipe")

# Display the elbow plot method for the scaled RFM data to find optimal K clusters
fviz_nbclust(RFM_scale, kmeans, method = "wss") 

# Display the silhouette plot method for the scaled RFM data to find optimal K clusters
fviz_nbclust(RFM_scale, kmeans, method = "silhouette")

# set the seed and run K-means model
set.seed(14, sample.kind="Rounding")
clusters <- kmeans(RFM_scale, centers = 4, iter.max = 100, nstart = 100)

#graphing the clusters
fviz_cluster(clusters, data = retail_RFM) 

# Cluster plot evaluation for the model
autoplot(clusters, RFM_scale, frame = TRUE)

# Cluster centers evaluation for the model
clusters$centers

## Results ##

# Set the seed and add the cluster model results column to the Customer data
set.seed(14, sample.kind="Rounding")
retail_RFM<- retail_RFM %>% mutate(cluster = clusters$cluster)
head(retail_RFM) %>% kable(format = "pipe")

# Display the cluster points on Monetary vs Frequency plot
retail_RFM %>% 
  ggplot(aes(x = Frequency, y = Monetary, col = as.factor(cluster))) +
  geom_point() +
  scale_y_continuous(label=comma)

# Display the cluster points on Monetary vs Recency plot
retail_RFM %>%
  ggplot(aes(x = Recency, y = Monetary, col = as.factor(cluster))) +
  geom_point() +
  scale_y_continuous(label=comma)

# Group the clusters in to a table with Recency, Frequency, and Monetary averages.
retail_RFM %>% 
  group_by(cluster) %>% 
  summarize( count = n(),
             Recency_avg = mean(Recency),
             Frequency_avg = mean(Frequency),
             Monetary_avg = mean(Monetary)) %>% 
  kable(format = "pipe")