

library(ggplot2)
library(dplyr)
library(Rlab)
library(tidyr)
library(tidyverse)
library(modelr)
library(lubridate)
library(stats)
library(MASS)
library(pscl)

#################################################################################
#prapare the dataset
#################################################################################

load("descriptive/breaches.Rda")

#################################################################################
#add monthly lags
#################################################################################
days <- tibble(date = seq(as.Date('2005-01-01'), as.Date('2018-12-31'),
                          by = "1 day"))
daily <- US.breaches %>% 
  mutate(date= mdy(Date.Made.Public))%>%
  mutate(month=month(date), year=year(date), day = day(date)) %>%
  group_by(day, month, year) %>%
  summarise(total= mean(Total.Records), mnumber = n())%>%
  mutate(date=make_date(year,month,day))

daily_dist <- days %>% left_join(daily, by ="date") 
daily_dist$mnumber[is.na(daily_dist$mnumber)] =0
daily_dist$total[is.na(daily_dist$total)] =0

daily_dist = daily_dist %>% mutate(month=month(date), year=year(date), day = day(date))

monthly = daily_dist %>% group_by(month, year) %>%
  summarise(month_size=mean(total),month_number=sum(mnumber),.groups = 'drop') %>%
  arrange(desc(year), desc(month)) %>% ungroup()


monthly <- tibble::rowid_to_column(monthly, "ID")

monthly_lag1 <- monthly %>% filter(ID!=1) %>% 
  mutate(ID=ID-1, month_lag1=month_number, size_lag1=month_size)%>%
  dplyr::select(ID, month_lag1, size_lag1)



monthly_lag2 <- monthly %>% filter(ID!=1 & ID !=2) %>% 
  mutate(ID=ID-2, month_lag2=month_number, size_lag2=month_size) %>%
  dplyr::select(ID, month_lag2, size_lag2)


monthly <- monthly %>% left_join (monthly_lag1, "ID"="ID") %>%
  left_join(monthly_lag2, "ID"="ID")

daily_dist <- daily_dist %>% left_join(monthly, by =c("month", "year"))


daily_dist =daily_dist %>%
  mutate(l1 = lag(mnumber,1), l2= lag(mnumber, 2), l3= lag(mnumber, 3),
                                  l4= lag(mnumber, 4), l5= lag(mnumber, 5),
         sl1= lag(total,1), sl2=lag(total,2), sl3= lag(total,3),
         sl4=lag(total,4), sl5= lag(total,5)) 

#################################################################################
#predict
#################################################################################


#################################################################################
#Training and Test datasets
#################################################################################

train = daily_dist %>% filter(year < 2018)
test = daily_dist %>% filter(year > 2017)

#################################################################################
#Poisson Model
#################################################################################

m1 = glm(mnumber ~ month+l1+l2+l3+l4+l5+month_lag1+month_lag2,
         data = train, family=poisson)

summary(m1)

train$m1_pred = predict(m1, train, type = "response")
train = train %>% group_by(month, year)%>%
  mutate(median = mean(mnumber), m1_pred_median = mean(m1_pred))
test$m1_pred = predict(m1, test, type = "response")
test = test %>% group_by(month, year)%>%
  mutate(median = mean(mnumber), m1_pred_median = mean(m1_pred))

test %>% ggplot (aes(x= date))+
  geom_smooth(aes(y=m1_pred), color="green")+
  geom_smooth(aes(y=mnumber), color="red")+
  ylab("Number of Breaches")+
  xlab("Date")
c= m1$fitted.values

ggsave("model/m1-1.png")

test %>% ggplot (aes(x= date))+
  geom_line(aes(y=m1_pred_median), color="green")+
  geom_line(aes(y=median), color="red")+
  ylab("Number of Breaches")+
  xlab("Date")

ggsave("model/m1-2.png")

#################################################################################
#Negative Binomial
#################################################################################

m2 = glm.nb(mnumber ~ day+month+l1+l2+l3+l4+l5+month_lag1+month_lag2,
            data = train)

summary(m2)

train$m2_pred = predict(m2, train, type = "response")
train = train %>% group_by(month, year)%>%
  mutate(median = sum(mnumber), m2_pred_median = sum(m2_pred))
test$m2_pred = predict(m2, test, type = "response")
test = test %>% group_by(month, year)%>%
  mutate(median = sum(mnumber), m2_pred_median = sum(m2_pred))

test %>% ggplot (aes(x= date))+
  geom_smooth(aes(y=m2_pred), color="green")+
  geom_smooth(aes(y=mnumber), color="red")+
  ylab("Number of Breaches")+
  xlab("Date")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("model/m2-1.png")

test %>% ggplot (aes(x= date))+
  geom_line(aes(y=m2_pred_median), color="green")+
  geom_line(aes(y=median), color="red")+
  ylab("Number of Breaches")+
  xlab("Date")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("model/m2-2.png")


#################################################################################
#ZERO-INFLATION
#################################################################################


m3 = zeroinfl(mnumber ~ day+month+l1+l2+l3+l4+l5+month_lag1+month_lag2,
            data = train)

summary(m3)

train$m3_pred = predict(m3, train, type = "response")
train = train %>% group_by(month, year)%>%
  mutate(median = sum(mnumber), m3_pred_median = sum(m3_pred))
test$m3_pred = predict(m3, test, type = "response")
test = test %>% group_by(month, year)%>%
  mutate(median = sum(mnumber), m3_pred_median = sum(m3_pred))

test %>% ggplot (aes(x= date))+
  geom_smooth(aes(y=m3_pred), color="green")+
  geom_smooth(aes(y=mnumber), color="red")+
  ylab("Number of Breaches")+
  xlab("Date")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("model/m3-1.png")

test %>% ggplot (aes(x= date))+
  geom_line(aes(y=m3_pred_median), color="green")+
  geom_line(aes(y=median), color="red")+
  ylab("Number of Breaches")+
  xlab("Date")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))

ggsave("model/m3-2.png")




#################################################################################
#Poissin Model-1 for prediction of the Size of the breaches
#################################################################################


m4 = glm(total ~ month+l1+l2+l3+l4+l5+month_lag1+month_lag2,
         data = train, family=poisson())

summary(m4)

train$m4_pred = predict(m4, train, type = "response")
train = train %>% group_by(month, year)%>%
  mutate(median = mean(total), m4_pred_median = mean(m4_pred))
test$m4_pred = predict(m4, test, type = "response")
test = test %>% group_by(month, year)%>%
  mutate(median = mean(total), m4_pred_median = mean(m4_pred))

test %>% ggplot (aes(x= date))+
  geom_smooth(aes(y=m4_pred), color="green")+
  geom_smooth(aes(y=total), color="red")+
  ylab("Size of Breaches")+
  xlab("Date")
c= m4$fitted.values

ggsave("model/m4-1.png")

test %>% ggplot (aes(x= date))+
  geom_line(aes(y=m4_pred_median), color="green")+
  geom_line(aes(y=median), color="red")+
  ylab("Size of Breaches")+
  xlab("Date")

ggsave("model/m4-2.png")



#################################################################################
#Poissin Model-2 for prediction of the Size of the breaches
#################################################################################



m5 = glm(month_size ~ day+month+sl1+sl2+sl3+sl4+sl5+size_lag1+size_lag2,
         data = train, family=gaussian)

summary(m5)

train$m5_pred = predict(m5, train, type = "response")
train = train %>% group_by(month, year)%>%
  mutate(median = mean(total), m5_pred_median = mean(m5_pred))
test$m5_pred = predict(m5, test, type = "response")
test = test %>% group_by(month, year)%>%
  mutate(median = mean(total), m5_pred_median = mean(m5_pred))

test %>% ggplot (aes(x= date))+
  geom_smooth(aes(y=m5_pred_median), color="green")+
  geom_smooth(aes(y=month_size), color="red")+
  ylab("Size of Breaches")+
  xlab("Date")+
  theme_classic()

ggsave("model/m5-1.png")

test %>% ggplot (aes(x= date))+
  geom_line(aes(y=m5_pred_median), color="green")+
  geom_line(aes(y=median), color="red")+
  ylab("Size of Breaches")+
  xlab("Date")+
  theme_classic()

ggsave("model/m5-2.png")



