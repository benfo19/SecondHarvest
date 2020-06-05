library(readr)
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
Donor_Report <- read_csv("F:/Practicum/Donor Report.csv", 
                         col_types = cols(`Gift Amount` = col_number(), 
                                          `Gift Date` = col_date(format = "%m/%d/%Y")))

## FORECASTING
donors <- Donor_Report %>% mutate(year = year(`Gift Date`), month = month(`Gift Date`)) %>%
  group_by(year, month) %>% filter(`Source of Donation` == "Individual") %>% filter(`Source of Donation` != "Capital Individual") %>%
  summarise(total_donations = sum(`Gift Amount`))

ts <- ts(donors, start = c(2004, 7), frequency = 12)  

autoplot(ts[,"total_donations"])

ggAcf(ts[,"total_donations"])

ggseasonplot(ts[,"total_donations"], year.labels = TRUE)


forecast <- auto.arima(ts[, 'total_donations'])
autoplot(forecast(forecast)) + xlab("Year") + ylab("Donations in USD")

forecastdata <- forecast(forecast)
forecasttable <- (as.data.frame(forecastdata))

## RFM

donorrfm <- Donor_Report %>% filter(`Source of Donation` == "Individual") %>% filter(`Source of Donation` != "Capital Individual")
  
library(rfm)

analysis_date <- as_date("2019-12-31", tz = "EST")

rfm <- rfm_table_order(donorrfm, customer_id = `Unique Donor ID`, order_date = `Gift Date`, revenue = `Gift Amount`, analysis_date = analysis_date)
str(rfm)

rfm_heatmap(rfm)
rfm_bar_chart(rfm)
rfm_histograms(rfm)
rfm_heatmap_data(rfm)
rfm_data <- as.data.frame(rfm_barchart_data(rfm))

# rfm segments
segment_names <- c("Champions", "Loyal Customers", "Potential Loyalist",
                   "New Customers", "Promising", "Need Attention", "About To Sleep",
                   "At Risk", "Can't Lose Them", "Lost")
recency_lower <- c(4, 2, 3, 4, 3, 2, 2, 1, 1, 1)
recency_upper <- c(5, 5, 5, 5, 4, 3, 3, 2, 1, 2)
frequency_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
frequency_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
monetary_lower <- c(4, 3, 1, 1, 1, 2, 1, 2, 4, 1)
monetary_upper <- c(5, 5, 3, 1, 1, 3, 2, 5, 5, 2)
rfm_segments <- rfm_segment(rfm, segment_names, recency_lower, recency_upper,
            frequency_lower, frequency_upper, monetary_lower, monetary_upper)

rfm_segments %>%
  count(segment) %>%
  arrange(desc(n)) %>%
  rename(Segment = segment, Count = n)

# median recency graph
data <- 
  rfm_segments %>%
  group_by(segment) %>%
  select(segment, recency_days) %>%
  summarize(median(recency_days)) %>%
  rename(segment = segment, med_recency = `median(recency_days)`) %>%
  arrange(med_recency) 

n_fill <- nrow(data)

ggplot(data, aes(segment, med_recency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n=n_fill, name = "Paired")) +
  xlab("Segment") + ylab("Median Recency") +
  ggtitle("Median Recency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# median frequency graph

data <- 
  rfm_segments %>%
  group_by(segment) %>%
  select(segment, transaction_count) %>%
  summarize(median(transaction_count)) %>%
  rename(segment = segment, med_frequency = `median(transaction_count)`) %>%
  arrange(med_frequency) 

n_fill <- nrow(data)

ggplot(data, aes(segment, med_frequency)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Paired")) +
  xlab("Segment") + ylab("Median Frequency") +
  ggtitle("Median Frequency by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# median monetary graph

data <- 
  rfm_segments %>%
  group_by(segment) %>%
  select(segment, amount) %>%
  summarize(median(amount)) %>%
  rename(segment = segment, med_monetary = `median(amount)`) %>%
  arrange(med_monetary) 

n_fill <- nrow(data)

ggplot(data, aes(segment, med_monetary)) +
  geom_bar(stat = "identity", fill = brewer.pal(n = n_fill, name = "Paired")) +
  xlab("Segment") + ylab("Median Monetary Value") +
  ggtitle("Median Monetary Value by Segment") +
  coord_flip() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )