#### Loading required libraries
library(data.table)
library(ggplot2)
library(ggmosaic)
library(readr)
library(abind)
library(tidyverse)
library(stringi)
library(tm)
library(knitr)
library(rlang)
library(magrittr)
library(tidyr)
library(dplyr)
library(dbplyr)
library(lubridate)
library(tidyselect)



#### Loading required files
transaction_data= read.csv(file.choose())
customer_data = read.csv(file.choose())

######################################################################
## exploring transaction_data ##
str(transaction_data)
summary(transaction_data)

## examine transaction_data
original_transaction_data = transaction_data

attach(transaction_data)
transaction_data$DATE =
  as.Date(DATE, origin = "1899-12-30")  #correcting date format
summary(transaction_data)

summary(transaction_data$PROD_NAME)
str(transaction_data$PROD_NAME)
head(transaction_data$PROD_NAME)

unique_product_name = data.frame(unique(PROD_NAME))

## filtering data sets ##
(filter(transaction_data, PROD_QTY > 10))
summary(transaction_data)

transaction_data = subset(transaction_data, PROD_QTY!= 200)

original_transaction_data = subset(original_transaction_data, PROD_QTY!= 200)

## Removing Special Characters and Removing digits
transaction_data$PROD_NAME = gsub("[[:punct:]]", " ",
                                  transaction_data$PROD_NAME)

transaction_data$PROD_NAME = removeNumbers(transaction_data$PROD_NAME)

# examine the words in PROD_NAME
product_words = data.table(unlist(
  strsplit(unique(transaction_data$PROD_NAME), " " ))) 

#counting the number of words repetition
word_count = data.frame(table(unlist(
  strsplit(tolower(transaction_data$PROD_NAME), " "))))

# Product_name count
prod_name_count = original_transaction_data %>%
  separate_rows(PROD_NAME, sep = '            ') %>%
  group_by(PROD_NAME = tolower(PROD_NAME)) %>%
  summarise(Count = n()) 
summary(transaction_data)

#product quantity count
prod_quantity_count = transaction_data %>%
  separate_rows(PROD_QTY, sep = '     ') %>%
  group_by(PROD_QTY = tolower(PROD_QTY)) %>%
  summarise(Count = n()) 

class(prod_quantity_count)

ggplot(prod_quantity_count, aes(x= PROD_QTY,y= log(Count))) + geom_col()

# transaction count by date
transactions_by_date = transaction_data %>%
  separate_rows(DATE, sep = ' ') %>%
  group_by(DATE = tolower(DATE)) %>%
  summarise(Count = n()) 


class(transactions_by_date$DATE)

transactions_by_date$DATE = as.Date(transactions_by_date$DATE)
class(transactions_by_date$DATE)

summary(transactions_by_date)

# plotting transactions over time
ggplot(transactions_by_date, aes(DATE,Count)) +
  geom_line() +  labs(title = "Transactions over time") +
  scale_x_date(breaks = "1 month")

ggplot(transactions_by_date, aes(DATE,Count)) +
  geom_line() +  labs(title = "Transactions over time") +
  scale_x_date(breaks = "2 month")

dec_month = transactions_by_date %>% 
  filter(between(DATE, ymd("2018-12-01"), ymd("2018-12-31")))
dec_month

ggplot(dec_month, aes(DATE,Count )) +
  geom_line() +  labs(title = "Transactions over December")

## Pack Size of Chips ##
summary(unique_product_name)
pack_size= unique_product_name
pack_size = mutate_all(pack_size,parse_number)

class(pack_size)

unique_pack_size = data.frame(unique(pack_size$unique.PROD_NAME.))


max(unique_pack_size)
min(unique_pack_size)

unique_pack_size =  as.numeric(unlist(unique_pack_size))
class(unique_pack_size)
hist(unique_pack_size)

# transaction count by prod_size
prod_size= prod_name_count
prod_size$PROD_NAME = gsub("[a-zA-Z ]", "", prod_size$PROD_NAME)


prod_size$PROD_NAME = gsub("[[:punct:]]","", prod_size$PROD_NAME)

class(prod_size)

ggplot(prod_size, aes(x= PROD_NAME, y= Count)) + geom_col() +
  labs(x = "Packet_Size", y="No_of_Transactions",
       tittle = "Transactios as per Packet Size")

### Different Brand Names ##
brands = unique_product_name$unique.PROD_NAME.
brands = data.frame(removeNumbers(brands))

summary(brands)
brands$removeNumbers.brands. = substr(brands$removeNumbers.brands.,
                                      1,nchar(brands$removeNumbers.brands.)-1)


########################################################################
## Examining Customer Data ##
str(customer_data)
summary(customer_data)

## merging the transaction data & the customer data sets ##

transaction_data$PROD_NAME = substr(transaction_data$PROD_NAME,
                                      1,nchar(transaction_data$PROD_NAME)-1)
data = merge(transaction_data, customer_data, all.x = TRUE)

summary(data)

write.csv(data,"C:\\Users\\ASUS\\Desktop\\R Practice\\Quantium\\QVI_data.csv",
          row.names = FALSE)

###################################################################

## plotting  total_sales w.r.t life stage and Premium Customer ##
total_sales_plot = ggplot(data, aes(x=LIFESTAGE , y = log(TOT_SALES))) +  
  geom_col()  + facet_grid(col = vars(PREMIUM_CUSTOMER)) +  coord_flip()
total_sales_plot

## plotting  Customer Count w.r.t life stage and Premium Customer##
Customer_count_plot = ggplot(data, aes(x=LIFESTAGE)) +  geom_bar()  + 
  labs(x= "lifestage", y=" No_of_Customers", 
       title = "Coustomer_Count wrt Lifestage & Premium_Customer") +
  facet_grid(col = vars(PREMIUM_CUSTOMER)) +  coord_flip()
Customer_count_plot

## plotting No_of_units per Customer w.r.t life stage and Premium Customer##
avg_quantity_plot = ggplot(data, aes(x =LIFESTAGE, y=  mean(PROD_QTY) )) +
  geom_col()  + facet_grid(col = vars(PREMIUM_CUSTOMER)) +  coord_flip()
avg_quantity_plot

## plotting avg_price per Customer w.r.t life stage and Premium Customer##
data$TOT_SALES = as.numeric(data$TOT_SALES)
data$PROD_QTY = as.numeric(data$PROD_QTY)
class(data$PROD_QTY)

avg_sales_price = data$TOT_SALES/data$PROD_QTY
avg_sales_price = round(avg_sales_price, digits = 3)

unique_sales_price = data.frame(unique(avg_sales_price))

avg_price_plot = ggplot(data, aes(x =LIFESTAGE, y=avg_sales_price ))+
  stat_summary(fun="mean", geom="col") + 
  facet_grid(col = vars(PREMIUM_CUSTOMER)) +  coord_flip()
avg_price_plot

## t-test between Mainstream & budget - YOUNG SINGLES/COUPLES##
t.test(data$PREMIUM_CUSTOMER == "Mainstream" &
         data$LIFESTAGE == "YOUNG SINGLES/COUPLES",
       data$PREMIUM_CUSTOMER =="Budget"&
         data$LIFESTAGE == "YOUNG SINGLES/COUPLES")

## t-test between Mainstream & Premium - YOUNG SINGLES/COUPLES##
t.test(data$PREMIUM_CUSTOMER == "Mainstream" &
         data$LIFESTAGE == "YOUNG SINGLES/COUPLES",
       data$PREMIUM_CUSTOMER =="Premium"&
         data$LIFESTAGE == "YOUNG SINGLES/COUPLES")

## t-test between Mainstream & budget - Midage SINGLES/COUPLES##
t.test(data$PREMIUM_CUSTOMER == "Mainstream" &
         data$LIFESTAGE == "MIDAGE SINGLES/COUPLES",
       data$PREMIUM_CUSTOMER =="Budget"&
         data$LIFESTAGE == "MIDAGE SINGLES/COUPLES")

## t-test between Mainstream & PREMIUM - Midage SINGLES/COUPLES##
t.test(data$PREMIUM_CUSTOMER == "Mainstream" &
         data$LIFESTAGE == "MIDAGE SINGLES/COUPLES",
       data$PREMIUM_CUSTOMER =="Premium"&
         data$LIFESTAGE == "MIDAGE SINGLES/COUPLES")

#####################################################################

