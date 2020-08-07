# Required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)
library(scales)

# Data Exploration
## Age distribution
### Old customers
g1 <- cust.txn %>% filter(!is.na(age_bin)) %>% group_by(age_bin) %>% ggplot(aes(x=age_bin))
g1 <- g1 + geom_bar(aes(fill = age_bin))
g1 <- g1 + xlab("Age distribution")
g1 <- g1 + ylab("Number of customers")
g1 <- g1 + geom_label(aes(label= ..count..), stat = "count")
g1 <- g1 + ggtitle("Old Customer Age Distribution")
g1 <- g1 + theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              plot.title = element_text(hjust=0.5),
              legend.title = element_blank())
g1

### New customers
g2 <- new %>% filter(!is.na(age_bin)) %>% group_by(age_bin) %>% ggplot(aes(x=age_bin))
g2 <- g2 + geom_bar(aes(fill = age_bin))
g2 <- g2 + xlab("Age distribution")
g2 <- g2 + ylab("Number of customers")
g2 <- g2 + geom_label(aes(label= ..count..), stat = "count")
g2 <- g2 + ggtitle("New Customer Age Distribution")
g2 <- g2 + theme(axis.text.x = element_blank(),
                 axis.ticks.x = element_blank(),
                 plot.title = element_text(hjust=0.5),
                 legend.title = element_blank())
g2
grid.arrange(g1,g2)

## Wealth segment
### Old
g3 <- cust.txn %>% filter(!is.na(age_bin)) %>% group_by(age_bin) %>% ggplot(aes(x=age_bin))
g3 <- g3 + geom_bar(aes(fill = wealth_segment))
g3 <- g3 + scale_fill_brewer(type="qual", palette="Set1")
g3 <- g3 + xlab("Age distribution")
g3 <- g3 + ylab("Number of customers")
g3 <- g3 + ggtitle("Old Customer Wealth Segment by Age")
g3 <- g3 + theme(plot.title = element_text(hjust=0.5))
g3 <- g3 + guides(fill=guide_legend("Wealth Segment")) 
g3

### New
g4 <- new %>% filter(!is.na(age_bin)) %>% group_by(age_bin) %>% ggplot(aes(x=age_bin))
g4 <- g4 + geom_bar(aes(fill = wealth_segment))
g4 <- g4 + scale_fill_brewer(type="qual", palette="Set1")
g4 <- g4 + xlab("Age distribution")
g4 <- g4 + ylab("Number of customers")
g4 <- g4 + ggtitle("New Customer Wealth Segment by Age")
g4 <- g4 + theme(plot.title = element_text(hjust=0.5))
g4 <- g4 + guides(fill=guide_legend("Wealth Segment"))
g4

## Job Industry
### Old
cust.txn$job_industry_category <- recode(cust.txn$job_industry_category, Argiculture = "Agriculture")
g5 <- cust.txn %>% group_by(job_industry_category) %>% ggplot(aes(x=job_industry_category,fill=job_industry_category))
g5 <- g5 + geom_bar(aes(y = (..count..)/sum(..count..)))
g5 <- g5 + geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                         y= ((..count..)/sum(..count..))),
                     vjust = -.25, stat = "count")
g5 <- g5 + scale_y_continuous(labels=scales::percent)
g5 <- g5 + scale_fill_brewer(type="qual", palette="Set3")
g5 <- g5 + xlab("Job industry")
g5 <- g5 + ylab("Proportion of customers")
g5 <- g5 + ggtitle("Old Job Industry Distribution")
g5 <- g5 + theme(plot.title = element_text(hjust=0.5),
                 axis.text.x = element_blank(), 
                 axis.ticks.x = element_blank(),
                 legend.title = element_blank())
g5

### New
new$job_industry_category <- recode(new$job_industry_category, Argiculture = "Agriculture")
g6 <- new %>% group_by(job_industry_category) %>% ggplot(aes(x=job_industry_category,fill=job_industry_category))
g6 <- g6 + geom_bar(aes(y = (..count..)/sum(..count..)))
g6 <- g6 + geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                         y= ((..count..)/sum(..count..))),
                     vjust = -.25, stat = "count")
g6 <- g6 + scale_y_continuous(labels=scales::percent)
g6 <- g6 + scale_fill_brewer(type="qual", palette="Set3")
g6 <- g6 + xlab("Job industry")
g6 <- g6 + ylab("Proportion of customers")
g6 <- g6 + ggtitle("New Job Industry Distribution")
g6 <- g6 + theme(plot.title = element_text(hjust=0.5),
                 axis.text.x = element_blank(), 
                 axis.ticks.x = element_blank(),
                 legend.title = element_blank())
g6

## Past 3 years bike related purchases (New)
### Percentage
new$past_3_years_bike_related_purchases <- as.numeric(as.character(new$past_3_years_bike_related_purchases))
g7 <- new %>% select(gender,past_3_years_bike_related_purchases) %>% group_by(gender) %>% 
        summarise(total_purchases = sum(past_3_years_bike_related_purchases)) %>% 
        ggplot(aes(x=gender,fill=gender))
g7 <- g7 + geom_col(aes(y=total_purchases/sum(total_purchases)))
g7 <- g7 + geom_text(aes(label = scales::percent((total_purchases)/sum(total_purchases)),
              y= ((total_purchases)/sum(total_purchases))),
          vjust = -.25)
g7 <- g7 + scale_y_continuous(labels=scales::percent)
g7 <- g7 + scale_fill_brewer(type="qual", palette="Accent")
g7 <- g7 + xlab("Gender")
g7 <- g7 + ylab("Percentage of bike related purchases")
g7 <- g7 + ggtitle("Bike Related Purchases for the Past 3 Years by Gender")
g7 <- g7 + theme(plot.title = element_text(hjust=0.5),
                 axis.text.x = element_blank(), 
                 axis.ticks.x = element_blank(),
                 legend.title = element_blank())
g7

### Number
g8 <- new %>% select(gender,past_3_years_bike_related_purchases) %>% group_by(gender) %>% 
        summarise(total_purchases = sum(past_3_years_bike_related_purchases)) %>% 
        ggplot(aes(x=gender,fill=gender))
g8 <- g8 + geom_col(aes(y=total_purchases))
g8 <- g8 + geom_text(aes(label =total_purchases, y=total_purchases),
                     vjust = -.25)
g8 <- g8 + scale_fill_brewer(type="qual", palette="Accent")
g8 <- g8 + xlab("Gender")
g8 <- g8 + ylab("Number of bike related purchases")
g8 <- g8 + ggtitle("Bike Related Purchases for the Past 3 Years by Gender")
g8 <- g8 + theme(plot.title = element_text(hjust=0.5),
                 axis.text.x = element_blank(), 
                 axis.ticks.x = element_blank(),
                 legend.title = element_blank())
g8

## Car owners
g9 <- new %>% select(first_name,owns_car,state) %>% group_by(owns_car) %>% 
        ggplot(aes(x=owns_car,fill=owns_car))
g9 <- g9 + geom_bar(aes(y=..count..))
g9 <- g9 + geom_text(aes(label =..count..),
                     vjust = -.25, stat = "count")
g9 <- g9 + facet_grid(.~state)
g9 <- g9 + scale_fill_brewer(type="qual", palette="Accent")
g9 <- g9 + xlab("State")
g9 <- g9 + ylab("Number of cars owned and not owned")
g9 <- g9 + ggtitle("Customer Car Ownership in each State")
g9 <- g9 + theme(plot.title = element_text(hjust=0.5),
                 axis.text.x = element_blank(), 
                 axis.ticks.x = element_blank(),
                 legend.title = element_blank())
g9


## Output figs
if (!file.exists("figure")) {
        dir.create("figure")
}

png("./figure/plot1.png")
grid.arrange(g2,g1)
dev.off()

png("./figure/plot2.png")
grid.arrange(g7,g8)
dev.off()

png("./figure/plot3.png")
grid.arrange(g6,g5)
dev.off()

png("./figure/plot4.png")
grid.arrange(g4,g3)
dev.off()

png("./figure/plot5.png")
print(g9)
dev.off()