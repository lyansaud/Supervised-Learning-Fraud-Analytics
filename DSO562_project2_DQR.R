
library(dplyr)
library(ggplot2)
library(stringr)
library(lubridate)

setwd("~/Desktop/DSO 562/Data")


applications <- read.csv("applications.csv")


str(applications)
unique_values <- sapply(applications, function(x) length(unique(x)))


length(unique(applications$date))
#transforming the date type into ymd

applications$date = mdy(applications$date)

applications$month = month(applications$date, label = TRUE)

applications$month <- as.factor(applications$month)
applications$day = day(applications$date)

str(applications)
applications$day = as.factor(applications$day)

ggplot(data = applications, aes(month)) +
  geom_bar(fill = "lightblue") +
  ggtitle("Monthly Distribution of Applications")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))


ggplot(data = applications, aes(day)) +
  geom_bar(fill = "lightblue") +
  ggtitle("Distribution of Applications by Days")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 15),
        axis.title = element_text(size = 18))



#ssn

length(unique(applications$ssn))

applications$ssn[2]

snn_top10 <- applications %>%
  select(ssn) %>%
  group_by(ssn) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)


snn_top12 <- applications %>%
  select(ssn) %>%
  group_by(ssn) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(2:11)


ggplot(data = snn_top12, aes(x = reorder(as.factor(ssn),-total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of ssn")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("ssn")

#firstname


firstname_top10 <- applications %>%
  select(firstname) %>%
  group_by(firstname) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)

ggplot(data = firstname_top10, aes(x = reorder(as.factor(firstname),-total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of firstname")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 18))+
  xlab("firstname")

#now by number of letter

firstname1_top10 <- applications %>%
  mutate(lettercount = str_count(applications$firstname)) %>%
  select(lettercount) %>%
  group_by(lettercount) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)
  


summary(str_count(applications$firstname))

ggplot(data = firstname1_top10, aes(x = reorder(as.factor(lettercount),-total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of firstname by Number of Letters")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("Letter count of firstname")+
  geom_text(aes(x = as.factor(lettercount), label = total))


#last name

lastname_top10 <- applications %>%
  select(lastname) %>%
  group_by(lastname) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)


summary(str_count(applications$lastname))

lastname1_top10 <- applications %>%
  mutate(lettercount = str_count(applications$lastname)) %>%
  select(lettercount) %>%
  group_by(lettercount) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)


ggplot(data = lastname_top10, aes(x = reorder(as.factor(lastname),-total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of lastname")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("lastname")+
  geom_text(aes(x = as.factor(lettercount), label = total))


ggplot(data = lastname1_top10, aes(x = reorder(as.factor(lettercount),-total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of lastname by number of letters")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("Letter count of lastname")+
  geom_text(aes(x = as.factor(lettercount), label = total))



#address

address_top10 <- applications %>%
  select(address) %>%
  group_by(address) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)


ggplot(data = address_top10, aes(x = reorder(as.factor(address),-total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of address")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("address")+
  geom_text(aes(x = as.factor(address), label = total))+
  coord_flip()



#zip5

zip5_top10 <- applications %>%
  select(zip5) %>%
  group_by(zip5) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)

ggplot(data = zip5_top10, aes(x = reorder(as.factor(zip5),-total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of zip5")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("zip5")+
  geom_text(aes(x = as.factor(zip5), label = total))+
  coord_flip()

#dob


dob_top10 <- applications %>%
  select(dob) %>%
  group_by(dob) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)


ggplot(data = dob_top10, aes(x = reorder(as.factor(dob),total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of dob")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("dob")+
  geom_text(aes(x = as.factor(dob), label = total))+
  coord_flip()



applications$dob1 = mdy(applications$dob)

applications$month1 = month(applications$dob1, label = TRUE)

applications$month1 <- as.factor(applications$month1)
applications$day1 = day(applications$dob1)
applications$year1 <- year(applications$dob1)

str(applications)
applications$day1 = as.factor(applications$day1)



dob_year_top10 <- applications %>%
  select(year1) %>%
  group_by(year1) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)


dob_year_top10 <- applications %>%
  select(year1) %>%
  group_by(year1) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)

ggplot(data = applications, aes(x = reorder(as.factor(month1),total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of dob by Month")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("Month of dob")+
  geom_text(aes(x = as.factor(year1), label = total))+
  coord_flip()


ggplot(data = applications, aes(month1)) +
  geom_bar(fill = "lightblue") +
  ggtitle("Monthly Distribution of dob")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 18))+
  xlab("Month of dob")

#homephone

homephone_top10 <- applications %>%
  select(homephone, fraud) %>%
  group_by(homephone, fraud) %>%
  summarize(total = n()) %>%
  arrange(desc(total)) %>%
  slice(1:10)


ggplot(data = homephone_top10, aes(x = reorder(as.factor(homephone),total), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of homephone")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("homephone")+
  geom_text(aes(x = as.factor(homephone), label = total))+
  coord_flip()

sum(is.na(applications$fraud))



  
  
fraud_top <- applications %>%
  select(fraud) %>%
  group_by(fraud) %>%
  summarize(total = n())


ggplot(data = fraud_top, aes(x = as.factor(fraud), y = total)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of fraud")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("fraud")+
  geom_text(aes(x = as.factor(fraud), label = total))



ggplot(data = homephone_top10, aes(x = reorder(as.factor(homephone),total), y = total, fill = fraud )) +
  geom_bar(stat = "identity", fill = "lightblue") +
  ggtitle("Distribution of homephone")+
  theme_classic()+
  theme(plot.title =  element_text(hjust = 0.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 18))+
  xlab("homephone")+
  geom_text(aes(x = as.factor(homephone), label = total))+
  coord_flip()
