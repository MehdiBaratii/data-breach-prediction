
library(ggplot2)
library(dplyr)
library(Rlab)
library(tidyr)
library(tidyverse)
library(modelr)
library(lubridate)
library(stats)
library(MASS)


breaches <- tibble(read.csv("https://privacyrights.org/sites/default/files/2020-01/PRC%20Data%20Breach%20Chronology%20-%201.13.20.csv"))
breaches$Total.Records <- as.numeric(gsub(",", "", breaches$Total.Records))
breaches %>% group_by(State) %>% summarise()


states <- as.factor(state.name)

st_crosswalk <- tibble(State = state.name) %>%
  bind_cols(tibble(abb = state.abb))




breaches$state_abb <- factor(breaches$State,
                         levels=c('Alabama','Alaska','Arizona','Arkansas',
                                  
                                  'California','Colorado','Connecticut','Delaware',
                                  
                                  'District of Columbia','Florida','Georgia','Hawaii',
                                  
                                  'Idaho','Illinois','Indiana','Iowa','Kansas',
                                  
                                  'Kentucky','Louisiana','Maine','Maryland','Massachusetts',
                                  
                                  'Michigan','Minnesota','Mississippi','Missouri','Montana',
                                  
                                  'Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
                                  
                                  'New York','North Carolina','North Dakota','Ohio','Oklahoma',
                                  
                                  'Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota',
                                  
                                  'Tennessee','Texas','Utah','Vermont','Virginia',
                                  
                                  'Washington','West Virginia','Wisconsin','Wyoming',
                                  
                                  'AL','AK','AZ','AR',
                                  
                                  'CA','CO','CT','DE',
                                  
                                  'DC','FL','GA','HI',
                                  
                                  'ID','IL','IN','IA','KY',
                                  
                                  'KY','LA','ME','MD','MA',
                                  
                                  'MI','MN','MS','MO','MT',
                                  
                                  'NE','NV','NH','NJ','NM',
                                  
                                  'NY','NC','ND','OH','OK',
                                  
                                  'OR','PA','RI','SC','SD',
                                  
                                  'TN','TX','UT','VT','VA',
                                  
                                  'WA','WV','WI','WY'),

      labels=c('AL','AK','AZ','AR',
         
         'CA','CO','CT','DE',
         
         'DC','FL','GA','HI',
         
         'ID','IL','IN','IA','KY',
         
         'KY','LA','ME','MD','MA',
         
         'MI','MN','MS','MO','MT',
         
         'NE','NV','NH','NJ','NM',
         
         'NY','NC','ND','OH','OK',
         
         'OR','PA','RI','SC','SD',
         
         'TN','TX','UT','VT','VA',
         
         'WA','WV','WI','WY',
         
         'AL','AK','AZ','AR',
         
         'CA','CO','CT','DE',
         
         'DC','FL','GA','HI',
         
         'ID','IL','IN','IA','KY',
         
         'KY','LA','ME','MD','MA',
         
         'MI','MN','MS','MO','MT',
         
         'NE','NV','NH','NJ','NM',
         
         'NY','NC','ND','OH','OK',
         
         'OR','PA','RI','SC','SD',
         
         'TN','TX','UT','VT','VA',
         
         'WA','WV','WI','WY'))


breaches$State <- factor(breaches$State,
                             levels=c('Alabama','Alaska','Arizona','Arkansas',
                                      
                                      'California','Colorado','Connecticut','Delaware',
                                      
                                      'District of Columbia','Florida','Georgia','Hawaii',
                                      
                                      'Idaho','Illinois','Indiana','Iowa','Kansas',
                                      
                                      'Kentucky','Louisiana','Maine','Maryland','Massachusetts',
                                      
                                      'Michigan','Minnesota','Mississippi','Missouri','Montana',
                                      
                                      'Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
                                      
                                      'New York','North Carolina','North Dakota','Ohio','Oklahoma',
                                      
                                      'Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota',
                                      
                                      'Tennessee','Texas','Utah','Vermont','Virginia',
                                      
                                      'Washington','West Virginia','Wisconsin','Wyoming',
                                      
                                      'AL','AK','AZ','AR',
                                      
                                      'CA','CO','CT','DE',
                                      
                                      'DC','FL','GA','HI',
                                      
                                      'ID','IL','IN','IA','KY',
                                      
                                      'KY','LA','ME','MD','MA',
                                      
                                      'MI','MN','MS','MO','MT',
                                      
                                      'NE','NV','NH','NJ','NM',
                                      
                                      'NY','NC','ND','OH','OK',
                                      
                                      'OR','PA','RI','SC','SD',
                                      
                                      'TN','TX','UT','VT','VA',
                                      
                                      'WA','WV','WI','WY'),
                             
                             labels=c('Alabama','Alaska','Arizona','Arkansas',
                                      
                                      'California','Colorado','Connecticut','Delaware',
                                      
                                      'District of Columbia','Florida','Georgia','Hawaii',
                                      
                                      'Idaho','Illinois','Indiana','Iowa','Kansas',
                                      
                                      'Kentucky','Louisiana','Maine','Maryland','Massachusetts',
                                      
                                      'Michigan','Minnesota','Mississippi','Missouri','Montana',
                                      
                                      'Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
                                      
                                      'New York','North Carolina','North Dakota','Ohio','Oklahoma',
                                      
                                      'Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota',
                                      
                                      'Tennessee','Texas','Utah','Vermont','Virginia',
                                      
                                      'Washington','West Virginia','Wisconsin','Wyoming',
                                      'Alabama','Alaska','Arizona','Arkansas',
                                      
                                      'California','Colorado','Connecticut','Delaware',
                                      
                                      'District of Columbia','Florida','Georgia','Hawaii',
                                      
                                      'Idaho','Illinois','Indiana','Iowa','Kansas',
                                      
                                      'Kentucky','Louisiana','Maine','Maryland','Massachusetts',
                                      
                                      'Michigan','Minnesota','Mississippi','Missouri','Montana',
                                      
                                      'Nebraska','Nevada','New Hampshire','New Jersey','New Mexico',
                                      
                                      'New York','North Carolina','North Dakota','Ohio','Oklahoma',
                                      
                                      'Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota',
                                      
                                      'Tennessee','Texas','Utah','Vermont','Virginia',
                                      
                                      'Washington','West Virginia','Wisconsin','Wyoming'))



US.breaches <- subset(breaches, breaches$state_abb %in% state.abb) 

US.breaches <- US.breaches %>% mutate(LTotal.Records = log10(Total.Records+1),
                                      ETotal.Records = log(Total.Records+1))

save(US.breaches,file="descriptive/breaches.Rda")





c= US.breaches %>% drop_na(Total.Records) %>%
  dplyr::select(State, Total.Records, Type.of.breach, Year.of.Breach )%>%
  group_by(Year.of.Breach)%>%
  summarize(State=State, Total.Records=sum(Total.Records), Total.Breaches = n())

options(scipen=2)
ggplot(c, aes(Year.of.Breach, Total.Breaches))+
  geom_line()+
  ylab("Number of Breach Incidents")+
  xlab("Year")+
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank())
ggsave("descriptive/incidents.png")


d = c %>% group_by(State)%>% summarise(total=n())


options(scipen=2)
ggplot(c, aes(Year.of.Breach, Total.Records))+
  geom_line()+
  ylab("Number of Total Records")+
  xlab("Year")+
  theme(
    panel.background = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_blank())
ggsave("descriptive/records.png")



US.breaches %>% ggplot()+ geom_histogram(aes(x=LTotal.Records), bins = 30)+
  xlab("Log of Number of Records")+
  ylab("Frequency")
ggsave("descriptive/total.png")

total = US.breaches %>% drop_na(Total.Records) %>%
  group_by(Year.of.Breach)%>% summarise(total =sum(Total.Records))





US.breaches %>% filter(Total.Records< 1000000000)%>%
  group_by(Year.of.Breach,Type.of.breach) %>%
  summarise(Total.Records= sum(Total.Records)) %>%
  ggplot()+
  geom_bar(aes(x=Year.of.Breach, y= Total.Records, fill =Type.of.breach),
           stat = "identity", position = "fill")


US.breaches %>% filter(Total.Records< 1000000000)%>%
  ggplot()+
  geom_bar(aes(x=Year.of.Breach, fill =Type.of.organization))


US.breaches %>% filter(Total.Records< 100000000000000)%>%
  ggplot(aes(LTotal.Records))+
  geom_histogram()


#**************************************************************************
#Medical Sector figures
#**************************************************************************

US.breaches %>% filter(Type.of.organization== "MED") %>%
  ggplot()+
  geom_bar(aes(x=Year.of.Breach, fill =Type.of.breach))


US.breaches %>% #filter(Year.of.Breach < 2013)%>%
  filter(Type.of.organization== "MED") %>%
  mutate(size=cut(Total.Records,
                breaks= c(-Inf, 1000, 10000, 50000, 100000, Inf),
                labels=c("1000","10000","50000","100000", "+1000000")))%>%
  ggplot(aes(x=Year.of.Breach, fill =size))+
  geom_bar()


US.breaches %>% filter(Type.of.organization== "MED", Year.of.Breach<2013)%>%
  ggplot()+ geom_bar(aes(x=Year.of.Breach, 
                         fill =Type.of.breach), position = "fill")



#**************************************************************************
#Finance Sector figures
#**************************************************************************

US.breaches %>% filter(Type.of.organization== "BSF") %>%
  ggplot()+
  geom_bar(aes(x=Year.of.Breach, fill =Type.of.breach))+
  labs(title = "Business and Finance sector")


US.breaches %>% filter(Type.of.organization== "BSF")%>%
  ggplot()+ geom_bar(aes(x=Year.of.Breach, 
                         fill =Type.of.breach), position = "fill")+
  labs(title = "Business and Finance sector")



US.breaches %>% #filter(Year.of.Breach < 2013)%>%
  filter(Type.of.organization== "BSF") %>%
  mutate(size=cut(Total.Records,
                  breaks= c(-Inf, 1000, 10000, 50000, 100000, Inf),
                  labels=c("1000","10000","50000","100000", "+1000000")))%>%
  ggplot(aes(x=Year.of.Breach, fill =size))+
  geom_bar()+
  labs(title = "Frequency of data breaches in Business and Finance sector")



US.breaches %>% #filter(Year.of.Breach < 2013)%>%
  filter(Type.of.organization== "BSF") %>%
  mutate(size=cut(Total.Records,
                  breaks= c(-Inf, 1000, 10000, 50000, 100000, Inf),
                  labels=c("1000","10000","50000","100000", "+1000000")))%>%
  ggplot(aes(x=Year.of.Breach, fill =size))+
  geom_bar(position = "fill")+
  labs(title = "Frequency of data breaches in Business and Finance sector")



#**************************************************************************
#Government Sector figures
#**************************************************************************

US.breaches %>% filter(Type.of.organization== "GOV") %>%
  ggplot()+
  geom_bar(aes(x=Year.of.Breach, fill =Type.of.breach))+
  labs(title = "Government sector")+
  geom_line(data= US.breaches %>% filter(Type.of.organization== "GOV") %>%
              group_by(Year.of.Breach) %>% summarise(count=n()),
            aes(x= Year.of.Breach, y= count ))


US.breaches %>% filter(Type.of.organization== "GOV")%>%
  #filter(Year.of.Breach < 2013) %>%
  ggplot()+ geom_bar(aes(x=Year.of.Breach, 
                         fill =Type.of.breach), position = "fill")+
  labs(title = "Government sector")+
  geom_line(data= US.breaches %>% filter(Type.of.organization== "GOV") %>%
              group_by(Year.of.Breach) %>% summarise(count=n()),
            aes(x= Year.of.Breach, y= count/100 ))+
  scale_y_continuous(sec.axis = ~.*100)





US.breaches %>% #filter(Year.of.Breach < 2013)%>%
  filter(Type.of.organization== "BSF") %>%
  mutate(size=cut(Total.Records,
                  breaks= c(-Inf, 1000, 10000, 50000, 100000, Inf),
                  labels=c("1000","10000","50000","100000", "+1000000")))%>%
  ggplot(aes(x=Year.of.Breach, fill =size))+
  geom_bar()+
  labs(title = "Frequency of data breaches in Business and Finance sector")



US.breaches %>% #filter(Year.of.Breach < 2013)%>%
  filter(Type.of.organization== "BSF") %>%
  mutate(size=cut(Total.Records,
                  breaks= c(-Inf, 1000, 10000, 50000, 100000, Inf),
                  labels=c("1000","10000","50000","100000", "+1000000")))%>%
  ggplot(aes(x=Year.of.Breach, fill =size))+
  geom_bar(position = "fill")+
  labs(title = "Frequency of data breaches in Business and Finance sector")





d <- c %>% rename(state=State, year=Year.of.Breach) %>%full_join(fraud)%>%
  filter(year>2016) 


d %>% filter(Total.Records<3000000, Total.Records>1)%>%
  ggplot(aes(Total.Records, number_of_reports)) +
  geom_point(aes(color=as.factor(year)))


US.breaches.agg <- US.breaches %>% 
  group_by(State=as.factor(State),
           Type.of.breach=as.factor(Type.of.breach),
           Year.of.Breach=as.integer(Year.of.Breach)) %>%
  summarise(Total.Records=sum(Total.Records))

options(scipen=1)
ggplot(US.breaches.agg)+
  geom_line(aes(y=Total.Records,x=Year.of.Breach, color=Type.of.breach))+
  geom_point(aes(y=Total.Records,x=Year.of.Breach, color=Type.of.breach),size=0.5)+
  facet_wrap(~State)+
  theme(
    axis.title.y=element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal"
  )+
  guides(colour = guide_legend(nrow = 1))+
  scale_y_log10(breaks=c(1000,1000000, 1000000000 ))




#Data breach sizes (records exposed) over a ten-year period. Data taken from
sector_trend <- function(sector, title=""){
  mon <- US.breaches %>%filter(Total.Records > 0, Type.of.organization==sector)%>%
    mutate(date= mdy(Date.Made.Public))%>%
    mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
    summarise(total= mean(Total.Records), mnumber = n())
  yea <- mon %>% group_by(year)%>%
    summarise(ytotal=mean(total), ynumber = sum(mnumber)) %>% mutate(fyear=factor(year))
  
  
  US.breaches %>% filter(Total.Records > 0,  Type.of.organization==sector) %>%
    mutate(date= mdy(Date.Made.Public))%>%
    ggplot()+
    geom_jitter(aes(x= date, y= (Total.Records)), size= 0.5, width=0.4,alpha=0.4)+
    geom_line(data= mon, aes(x= make_date(year, month, "15"),
                             y= (total)),color="green")+
    geom_line(data = yea, aes(x= make_date(year, "6", "30"),
                              y=(ytotal)), color="blue")+
    theme(
      axis.title.y = element_blank(),
      plot.title = element_text(color="red", size=10, face="bold.italic"),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      axis.text.y = element_text(angle = 45),
      axis.text.x = element_text(angle=45)
    )+
    scale_y_log10(breaks=c(10, 100,10000, 1000000, 100000000),
                  labels = scales::comma)+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ggtitle(title)
  
}

sector_trend("MED", "Data breach sizes of Medical Industry")
sector_trend("GOV", "Data breach sizes of Government Sector")
sector_trend("BSF", "Data breach sizes of Financial Institutions")
sector_trend("BSR", "Data breach sizes of Retail Sector")
sector_trend("EDU", "Data breach sizes of Education Sector")



#Data breach sizes (records exposed) over a ten-year period. Data taken from
type_trend <- function(type, title=""){
  mon <- US.breaches %>%filter(Total.Records > 0, Type.of.breach==type)%>%
    mutate(date= mdy(Date.Made.Public))%>%
    mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
    summarise(total= mean(Total.Records), mnumber = n())
  yea <- mon %>% group_by(year)%>%
    summarise(ytotal=mean(total), ynumber = sum(mnumber)) %>% mutate(fyear=factor(year))
  
  
  US.breaches %>% filter(Total.Records > 0, Type.of.breach==type) %>%
    mutate(date= mdy(Date.Made.Public))%>%
    ggplot()+
    geom_jitter(aes(x= date, y= (Total.Records), size=), size= 0.5, width=0.4,alpha=0.4)+
    geom_line(data= mon, aes(x= make_date(year, month, "15"),
                             y= (total)),color="green")+
    geom_line(data = yea, aes(x= make_date(year, "6", "30"),
                              y=(ytotal)), color="blue")+
    theme(
      axis.title.y = element_blank(),
      plot.title = element_text(color="red", size=10, face="bold.italic"),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      axis.text.y = element_text(angle = 45),
      axis.text.x = element_text(angle=45)
    )+
    scale_y_log10(breaks=c(10, 100,10000, 1000000, 100000000),
                  labels = scales::comma)+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
    ggtitle(title)
  
}

type_trend("HACK", "Data breach sizes of Hacking Attacks")
type_trend("INSD", "Data breach sizes of Insider origins")




#distribution of large data breaches only
mon <- US.breaches %>% filter(Total.Records >= 1000)%>%
  mutate(date= mdy(Date.Made.Public))%>%
  mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
  summarise(total= mean(Total.Records))
yea <- mon %>% group_by(year)%>%
  summarise(ytotal=mean(total))


US.breaches %>% filter(Total.Records >= 1000)%>%
  mutate(date= mdy(Date.Made.Public))%>%
  ggplot()+
  geom_point(aes(x= date, y= (Total.Records)), size=.7)+
  #geom_line(data= mon, aes(x= make_date(year, month, "15"),
                           #y= (total)),color="green")+
  geom_line(data = yea, aes(x= make_date(year, "6", "30"),
                            y=(ytotal)), color="blue")+
  ylab("Breach Size")+
  xlab("Year")+
  scale_y_log10(breaks=c(1, 100,10000, 1000000, 100000000),
                labels = scales::comma)+
  theme(
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text.y = element_text(angle = 45),
    axis.text.x = element_text(angle=45)
  )
ggsave("descriptive/trend.png")



#general distribution of size of breaches

mon <- US.breaches %>%filter(Total.Records > -1,)%>% mutate(date= mdy(Date.Made.Public))%>%
  mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
  summarise(total= mean(Total.Records), mnumber = n())
yea <- mon %>% group_by(year)%>%
  summarise(ytotal=mean(total), ynumber = sum(mnumber)) %>% mutate(fyear=factor(year))

US.breaches %>% filter(Total.Records > 0) %>%
  mutate(date= mdy(Date.Made.Public))%>%
  mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
  full_join(yea, by= "year") %>%
  ggplot(aes(x=factor(year) )) +
  geom_boxplot(aes(y = Total.Records))+ 
  geom_jitter(aes(y = Total.Records), width=0.2,alpha=0.1)+
  geom_point(aes(y= ynumber*1000), size=2, color="red")+
  scale_y_log10(breaks=c(1, 100,10000, 1000000, 100000000),
                labels = scales::comma)+
  theme(
    axis.title = element_blank(),
    axis.ticks.x = element_blank(),
    panel.background = element_blank(),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text.y = element_text(angle = 45),
    axis.text.x = element_text(angle=45)
  )
ggsave("descriptive/boxplot.png")

#Function for Sector distribution of size of breaches
sector_dist <- function(sector, title="") {
  mon <- US.breaches %>%filter(Total.Records > -1, Type.of.organization==sector)%>%
    mutate(date= mdy(Date.Made.Public))%>%
    mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
    summarise(total= mean(Total.Records), mnumber = n())
  yea <- mon %>% group_by(year)%>%
    summarise(ytotal=mean(total), ynumber = sum(mnumber)) %>%
    mutate(fyear=factor(year))
  
  US.breaches %>% filter(Total.Records >-1, Type.of.organization== sector) %>%
    mutate(date= mdy(Date.Made.Public))%>%
    mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
    full_join(yea, by= "year") %>%
    ggplot(aes(x=factor(year) )) +
    geom_boxplot(aes(y = Total.Records))+ 
    geom_jitter(aes(y = Total.Records), width=0.2,alpha=0.1)+
    #geom_point(aes(y= ynumber*1000), size=2, color="red")+
    scale_y_log10(breaks=c(1, 100,10000, 1000000, 100000000),
                  labels = scales::comma)+
    ggtitle(title)+
    theme(
      axis.title = element_blank(),
      plot.title = element_text(color="red", size=10, face="bold.italic"),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      axis.text.y = element_text(angle = 45),
      axis.text.x = element_text(angle=45)
    )
}


sector_dist("MED", "")
ggsave("descriptive/medical.png")
sector_dist("GOV", "")
ggsave("descriptive/gov.png")
sector_dist("BSO", "")
ggsave("descriptive/bus.png")
US.breaches %>% drop_na(Total.Records) %>%
  group_by(Type.of.organization) %>% summarise(s= mean(Total.Records))%>%
  ggplot(aes(x=Type.of.organization, y=s)) + geom_col()+theme_classic()+
  scale_y_log10(breaks=c(1, 100,10000, 1000000, 100000000),
                labels = c("1","100","10k","1m", "100m"))+
  labs(y="", x= "")

ggsave("descriptive/org.png")



#Function for breach type distribution of size of breaches
type_dist <- function(type, title="") {
  mon <- US.breaches %>%filter(Total.Records > -1, Type.of.breach==type)%>%
    mutate(date= mdy(Date.Made.Public))%>%
    mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
    summarise(total= mean(Total.Records), mnumber = n())
  yea <- mon %>% group_by(year)%>%
    summarise(ytotal=mean(total), ynumber = sum(mnumber)) %>%
    mutate(fyear=factor(year))
  
  US.breaches %>% filter(Total.Records >-1, Type.of.breach== type) %>%
    mutate(size=cut(Total.Records,
                    breaks= c(-Inf, 1000, 10000, 50000, 100000, Inf),
                    labels=c("1000","10000","50000","100000", "+1000000")))%>%
    mutate(date= mdy(Date.Made.Public))%>%
    mutate(month=month(date), year=year(date)) %>%group_by(month, year) %>%
    full_join(yea, by= "year") %>%
    ggplot(aes(x=as.factor(year) )) + 
    geom_boxplot(aes(y = Total.Records))+ 
    geom_jitter(aes(y = Total.Records, size=size), width=0.2,alpha=0.1)+
    geom_point(aes(y= ynumber*1000), color="red")+
    scale_y_log10(breaks=c(1, 100,10000, 1000000, 100000000),
                  labels = scales::comma)+
    ggtitle(title)+
    theme(
      axis.title = element_blank(),
      plot.title = element_text(color="red", size=10, face="bold.italic"),
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      legend.position = "none",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      axis.text.y = element_text(angle = 45),
      axis.text.x = element_text(angle=45)
    )
}


type_dist("HACK", "")
ggsave("descriptive/hack.png")
type_dist("INSD", "")
ggsave("descriptive/ind.png")



#################################################################################
#Finding the fitted distributions for the data
#################################################################################

US.breaches %>% filter(Total.Records > 0) %>% 
  ggplot(aes(x=log10(Total.Records)))+
  geom_density(color="red") +  
  geom_text(aes(x= 4.5, y = 0.3, label= "data", color="data"))+
  geom_function(fun =dlnorm, args= list(1.2,.5))+
  geom_text(aes(x= 6.5, y = 0.1, label= "log normal"))+
  theme(legend.position = "none")

US.breaches %>% filter(Total.Records > 0) %>% group_by(Total.Records) %>%
  summarise(count= n()) %>% mutate(pr= count/sum(count)) %>%
  arrange(desc(Total.Records))%>%
  mutate(cum = cumsum(pr))%>%
  ggplot() + geom_point(aes(x=((Total.Records)), y=cum))+
  geom_function(fun =~1-plnorm(.x, 8 , 3), color="green")+
  scale_x_log10()+
  scale_y_log10()


US.breaches %>% filter(Total.Records > 0) %>% 
  ggplot()+
  geom_histogram(aes(log10(Total.Records)), stat = "density") +
  geom_density(aes(log10(Total.Records)), color="red")+
  geom_function(fun =dlnorm, args= list(1.2,.5), color="blue")+
  geom_function(fun =dlnorm, args= list(1.2, .35), color="green")



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

monthly = daily_dist %>% group_by(month, year) %>% summarise(month_number=sum(mnumber),.groups = 'drop') %>%
  arrange(desc(year), desc(month)) %>% ungroup()


monthly <- tibble::rowid_to_column(monthly, "ID")

monthly_lag1 <- monthly %>% filter(ID!=1) %>% 
  mutate(ID=ID-1, month_lag1=month_number) %>% 
  dplyr::select(ID, month_lag1)

monthly_lag2 <- monthly %>% filter(ID!=1 & ID !=2) %>% 
  mutate(ID=ID-2, month_lag2=month_number) %>%
  dplyr::select(ID, month_lag2) 

monthly <- monthly %>% left_join (monthly_lag1, "ID"="ID") %>%
  left_join(monthly_lag2, "ID"="ID")

daily_dist <- daily_dist %>% left_join(monthly, by =c("month", "year"))


daily_dist =daily_dist %>% mutate(l1 = lag(mnumber,1), l2= lag(mnumber, 2), l3= lag(mnumber, 3),
                      l4= lag(mnumber, 4), l5= lag(mnumber, 5)) 




#################################################################################
#distribution of daily breaches
#################################################################################
daily_dist %>%
  ggplot(aes(mnumber))+ geom_bar()+
  xlab("Number of Breaches")+
  ylab("Frequency")+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(color="red", size=10, face="bold.italic"),
    panel.background = element_blank(),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text.y = element_text(angle = 45)
  )
ggsave("descriptive/daily.png")


#################################################################################
#distribution of daily sizes
#################################################################################

daily_dist %>% mutate(size=cut(total,
                               breaks= c(-Inf, 1, 10000, 50000, 100000, Inf),
                               labels=c("Zero","10000","50000","100000", "+1000000")))%>%
  ggplot(aes(size))+ geom_bar()+
  xlab("Sizes of Breaches")+
  ylab("Frequency")+
  theme(
    axis.title.y = element_blank(),
    plot.title = element_text(color="red", size=10, face="bold.italic"),
    panel.background = element_blank(),
    legend.position = "none",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    axis.text.y = element_text(angle = 45)
    )
ggsave("descriptive/sizes.png")



US.breaches %>% filter(Total.Records > 0) %>% 
  mutate(date= mdy(Date.Made.Public))%>%
  mutate(month=month(date), year=year(date), day = day(date)) %>%
  group_by(day, month, year) %>%
  summarise(total= mean(Total.Records), mnumber = n())%>% 
  group_by(mnumber) %>% summarise(freq = n())%>% mutate(freq=freq/sum(freq)) %>%
  mutate(nbinom = dnbinom(mnumber, size=3.4, mu=1.7)) %>%
  ggplot(aes(x=mnumber))+
  geom_bar(aes(y= freq), stat = "identity")+
  geom_bar( aes(y=nbinom), stat = "identity", color="red")

 


US.breaches %>% mutate(size=cut(Total.Records,
  breaks= c(-Inf,0, 1000, 10000, 100000, 1000000, Inf),
  labels=c("0" ,"1000","10000","100000","1000000", "+1000000")))%>%
  ggplot()+
  geom_bar(aes(x=size))



#################################################################################
#visualize the sources of information
#################################################################################


US.breaches$Information.Source = case_when(US.breaches$Information.Source== 
                                             'US Department of Health and Human Services' ~ "HHS",
                                           TRUE ~ as.character(US.breaches$Information.Source))

US.breaches$Information.Source = as.factor(US.breaches$Information.Source)

US.breaches %>% ggplot(aes(Information.Source)) + geom_bar()+
  theme_classic()+
  theme(axis.text.x=element_text(angle=45, hjust=1))
ggsave("descriptive/2.png")

#################################################################################
#visualize the frequency of the size of breaches
#################################################################################

US.breaches %>% mutate(size=cut(Total.Records,
                                breaks= c(-Inf, 1, 10000, 50000, 100000, Inf),
                                labels=c("Not Reported","10000","50000","100000", "+1000000")))%>%
  ggplot(aes(size)) + geom_bar()+
  theme_classic()
ggsave("descriptive/3.png")


