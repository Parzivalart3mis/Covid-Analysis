#loading packages
library(tidyr)
library(dplyr)
library(ggplot2)

#Data Pre-processing
#importing dataset
dataset = read.csv("E:\\Semester 5\\CSE3505 - FDA\\Project\\COVID\\covid-19-main\\data\\time-series-19-covid-combined.csv")
dataset <- na.omit(dataset)
#the null values of Province/State are substituted by respective #Country/Region values
#the null values of Confirmed, Recovered and Deaths are filled with 0
dataset$Province.State <- ifelse(dataset$Province.State == "", dataset$Country.Region, dataset$Province.State)
dataset$Confirmed <- ifelse(is.na(dataset$Confirmed), 0, dataset$Confirmed)
dataset$Recovered <- ifelse(is.na(dataset$Recovered), 0, dataset$Recovered)
dataset$Deaths <- ifelse(is.na(dataset$Deaths),0, dataset$Deaths)

sum(is.na(dataset$Recovered))
sum(is.na(dataset$Confirmed))
sum(is.na(dataset$Deaths))
summary(dataset)

sapply(dataset,class)

# convert date column from character to date class
dataset$Date <- as.Date(dataset$Date, format = "%m/%d/%Y")

sapply(dataset,class)

# Extract specific country: India
india <- dataset %>% filter(Country.Region=="India")

# SUMMARY STATISTICS
summary(dataset)
by(dataset$Confirmed, dataset$Country.Region, summary)
by(dataset$Deaths, dataset$Country.Region, summary)
by(dataset$Recovered, dataset$Country.Region, summary)
summary(india)

# GRAPHS
# Barchart of cases over time

# World confirmed
ggplot(dataset, aes(x=Date, y=Confirmed, fill = Confirmed)) + geom_bar(stat="identity", width=0.9) +
  theme_gray() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Daily confirmed cases") +
  theme(plot.background=element_rect(fill="orange"))+theme(panel.background = element_rect(fill = "linen"))

# World Recovered
ggplot(dataset, aes(x=Date, y=Recovered, fill = Recovered)) + geom_bar(stat="identity", width=0.9) +
  theme_gray() +
  labs(title = "Covid-19 Global Recovered Cases", x= "Date", y= "Daily Recovered cases") +
  theme(plot.background=element_rect(fill="darkseagreen"))+theme(panel.background = element_rect(fill = "azure"))


# World deaths
ggplot(dataset, aes(x=Date, y=Deaths, fill = Deaths)) + geom_bar(stat="identity", width=0.9) +
  theme_gray() +
  labs(title = "Covid-19 Global Deaths", x= "Date", y= "Daily Deaths") +
  theme(plot.background=element_rect(fill="cornflowerblue"))+theme(panel.background = element_rect(fill = "azure"))


#India confirmed
ggplot(india, aes(x=Date, y=Confirmed, fill = Confirmed)) + geom_bar(stat="identity", width=0.9) +
  theme_gray() +
  labs(title = "Covid-19 Confirmed Cases in India", x= "Date", y= "Daily confirmed cases") +
  theme(plot.background=element_rect(fill="darkorange"))+theme(panel.background = element_rect(fill = "linen"))

#India Recovered
ggplot(india, aes(x=Date, y=Recovered, fill = Recovered)) + geom_bar(stat="identity", width=0.9) +
  theme_gray() +
  labs(title = "Covid-19 Recovered Cases in India", x= "Date", y= "Daily Recovered cases") +
  theme(plot.background=element_rect(fill="cadetblue"))+theme(panel.background = element_rect(fill = "azure"))

#India Deaths
ggplot(india, aes(x=Date, y=Deaths, fill = Deaths)) + geom_bar(stat="identity", width=0.9) +
  theme_gray() +
  labs(title = "Covid-19 Deaths in India", x= "Date", y= "Daily Deaths") +
  theme(plot.background=element_rect(fill="darksalmon"))+theme(panel.background = element_rect(fill = "linen"))


#top 10 country analysis
df = dataset[(dataset$Country.Region == "US" | dataset$Country.Region== "Spain" |  dataset$Country.Region== "Italy" |  dataset$Country.Region=="United Kingdom" |  dataset$Country.Region=="Russia" |  dataset$Country.Region=="France" |  dataset$Country.Region=="Germany" |  dataset$Country.Region=="Brazil" |  dataset$Country.Region=="Turkey" |  dataset$Country.Region=="Iran"),]

#plot of Confirmed cases
p = ggplot() + geom_line(data = df, aes(x = Date, y = Confirmed, color = Country.Region),size = 1) + theme_gray() + labs(title = "Covid-19 plot of Confirmed cases for top 10 countries",x = "Date", y = "Confirmed Cases")+theme(plot.background=element_rect(fill="darksalmon"))+theme(panel.background = element_rect(fill = "linen"))
print(p)

#plot of recovered cases
p = ggplot() + geom_line(data = df, aes(x = Date, y = Recovered, color = Country.Region),size = 1) + theme_gray() + labs(title = "Covid-19 plot of Recovered for top 10 countries",x = "Date", y = "Recovered")+theme(plot.background=element_rect(fill="lightblue"))+theme(panel.background = element_rect(fill = "azure"))
print(p)

#plot of deaths
p = ggplot() + geom_line(data = df, aes(x = Date, y = Deaths, color = Country.Region),size = 1) + theme_gray() + labs(title = "Covid-19 plot of Deaths for top 10 countries",x = "Date", y = "Deaths") + theme(plot.background=element_rect(fill="darkseagreen"))+theme(panel.background = element_rect(fill = "azure"))
print(p)

#Bar plot of Confirmed cases of top 10 countries
ggplot(df, aes(x=Country.Region, y=Confirmed, fill = Date)) + geom_bar(stat="identity", width=0.9) + theme_gray() + labs(title = "Covid-19 Bar plot of Confirmed cases of top 10 countries", x= "Country", y= "Confirmed cases") + theme(plot.background=element_rect(fill="coral"))+theme(panel.background = element_rect(fill = "linen"))




#Monthly analysis
#for january
df_jan = df[(df$Date<= '2020-01-31' & df$Date >= '2020-01-01'),]

#plot of Confirmed cases in january
p = ggplot() + geom_line(data = df_jan, aes(x = Date, y = Confirmed, color = Country.Region),size = 1) + theme_gray() + labs(title = "Covid-19 plot of Confirmed cases for top 10 countries in January",x = "Date", y = "Confirmed Cases")+theme(plot.background=element_rect(fill="darksalmon"))+theme(panel.background = element_rect(fill = "linen"))
print(p)

#plot of Recovered cases in january
p = ggplot() + geom_line(data = df_jan, aes(x = Date, y = Recovered, color = Country.Region),size = 1) + theme_gray() + labs(title = "Covid-19 plot of Recovered cases for top 10 countries in January",x = "Date", y = "Recivered Cases")+theme(plot.background=element_rect(fill="darkseagreen"))+theme(panel.background = element_rect(fill = "azure"))
print(p)

#plot of Deaths in january
p = ggplot() + geom_line(data = df_jan, aes(x = Date, y = Deaths, color = Country.Region),size = 1) + theme_gray() + labs(title = "Covid-19 plot of Deaths for top 10 countries in January",x = "Date", y = "Deaths")+theme(plot.background=element_rect(fill="darksalmon"))+theme(panel.background = element_rect(fill = "linen"))
print(p)

#Bar plot of Confirmed cases of top 10 countries in January
ggplot(df_jan, aes(x=Country.Region, y=Confirmed, fill = Date)) + geom_bar(stat="identity", width=0.9) + theme_gray() + labs(title = "Covid-19 Bar plot of Confirmed cases of top 10 countries", x= "Country", y= "Confirmed cases") + theme(plot.background=element_rect(fill="coral"))+theme(panel.background = element_rect(fill = "linen"))
