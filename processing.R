# Load libraries
library(xlsx)
library(dplyr)
library(lubridate)

# P.S. The first row "Note: The data and information in this document is ..." was deleted. 

# Load data
custDemo <- read.xlsx("./Data/KPMG_VI_New_raw_data_update_final.xlsx", sheetName = "CustomerDemographic")
custAdd <- read.xlsx("./Data/KPMG_VI_New_raw_data_update_final.xlsx", sheetName = "CustomerAddress")
txn <- read.xlsx("./Data/KPMG_VI_New_raw_data_update_final.xlsx", sheetName = "Transactions")
new <- read.xlsx("./Data/KPMG_VI_New_raw_data_update_final.xlsx", sheetName = "NewCustomerList")

# Check for unique customer_ids in the different data frames
length(unique(custDemo$customer_id))
length(unique(custAdd$customer_id))
length(unique(txn$customer_id))

custDemo$customer_id[!custDemo$customer_id %in% custAdd$customer_id]
custAdd$customer_id[!custAdd$customer_id %in% custDemo$customer_id]

# Customer demographic
str(custDemo)
levels(factor(custDemo$gender))
custDemo$gender <- recode(custDemo$gender, 
                          Femal = "F",
                          Female = "F",
                          Male = "M")

## Convert date to date format
custDemo$DOB <- as.Date(as.numeric(as.character(custDemo$DOB)), origin = "1899-12-30")
custDemo$default <- NULL

# Customer address
levels(factor(custAdd$state))
custAdd$state <- recode(custAdd$state, 
                        "New South Wales" = "NSW",
                        "Victoria" = "VIC")

# Transaction
txn[,14:26] <- NULL
txn$product_first_sold_date <- as.Date(as.numeric(as.character(txn$product_first_sold_date),na.rm=TRUE), origin = "1899-12-30")

# New customer list
## Convert date
new$DOB_temp <- as.Date(as.character(new$DOB), origin = "1899-12-30")
new$DOB <- as.Date(as.numeric(as.character(new$DOB)), origin = "1899-12-30")
new$DOB[is.na(new$DOB)] <- new$DOB_temp[is.na(new$DOB)]

levels(factor(new$gender))
new$gender <- recode(new$gender,
                     Female = "F",
                     Male = "M")

new <- new %>% mutate(age = 2020 - year(DOB))
new$age_bin <- cut(new$age,
                   breaks = c(0,20,30,40,50,60,70,80,90), 
                   labels = c("Below 20","21-30","31-40","41-50","51-60","61-70","71-80","Above 80"),
                   ordered_result = TRUE)

## Combine CustomerDemographic with CustomerAddress by customer ID
custDemoAdd <-merge(custDemo, custAdd, by="customer_id", all=TRUE)
## Note that there are additional three new customer ID in custAdd

## cust.txn; Put all old customer information into transaction dataset
cust.txn <-merge(txn, custDemoAdd, by.x= "customer_id", all.x =TRUE)
cust.txn <- cust.txn %>% mutate(age = 2020 - year(DOB),
                                profit = list_price - standard_cost)
cust.txn$age_bin <- cut(cust.txn$age,
                        breaks = c(0,20,30,40,50,60,70,80,90), 
                        labels = c("Below 20","21-30","31-40","41-50","51-60","61-70","71-80","Above 80"),
                        ordered_result = TRUE)
## Filter for valid transactons and alive customers
cust.txn <- cust.txn %>% filter(order_status =="Approved",
                                     deceased_indicator =="N")



