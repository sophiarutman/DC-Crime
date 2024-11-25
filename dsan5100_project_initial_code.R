
#read data
data<- read.csv("/Users/zp/Desktop/DC-Crime/2023 adult arrests for open data_0.csv")
d2<- read.csv("/Users/zp/Desktop/DC-Crime/Arrests by Year 2020.csv")
d3<-  read.csv("/Users/zp/Desktop/DC-Crime/Arrests by Year, 2019.csv")
d4<-  read.csv("/Users/zp/Desktop/DC-Crime/2022 adult arrests for open data.csv")

# changing date format since format is different by each csv file
d4 <- d4 %>%
  mutate(Arrest.Date = as.Date(Arrest.Date, format = "%m/%d/%y"))

data <- data %>%
  mutate(Arrest.Date = as.Date(Arrest.Date, format = "%Y-%m-%d"))

d2 <- d2 %>%
  mutate(Arrest.Date = as.Date(Arrest.Date, format = "%m/%d/%y"))

d3 <- d3%>%
  mutate(Arrest.Date = format(as.Date(Arrest.Date, format = "%m/%d/%Y"), "%Y-%m-%d"))
d3$Arrest.Date<- as.Date(d3$Arrest.Date)

# combining rows with covid(2019,2020) vs post covid(2022,2023)
crime_covid <- bind_rows(d3, d2)
crime_post <- bind_rows(d4, data)


#ones with same CCN numbers for covid data
#if CCN number is same, it has multiple criminal in a case
#so can be defined as not a solo crime, arrest in same date

#two years 2019-2020(covid)

ccn_different_dates_2020 <- crime_covid%>%
  group_by(CCN) %>%
  summarise(Distinct_Dates = n_distinct(Arrest.Date)) %>%
  filter(Distinct_Dates > 1)

repeated_crime_2020 <- crime_covid %>%
  filter(CCN %in% ccn_different_dates_2020$CCN)


head(repeated_crime_2020)


#ones with same CCN numbers for post covid data
#if CCN number is same, it has multiple criminal in a case
#so can be defined as not a solo crime, arrested in same date

#two years(post covid 2022-2023)
ccn_different_dates_2023 <- crime_post%>%
  group_by(CCN) %>%
  summarise(Distinct_Dates = n_distinct(Arrest.Date)) %>%
  filter(Distinct_Dates > 1)

repeated_crime_2023 <- crime_post %>%
  filter(CCN %in% ccn_different_dates_2023$CCN)


head(repeated_crime_2023)

#modified version for crimes that commited by multiple arrestee who all of the criminals arrested
#in different dates

repeated_crime_2020_1<- repeated_crime_2020 %>%
  distinct(Arrest.Date, CCN, .keep_all = TRUE)
head(repeated_crime_2020_1)

repeated_crime_2023_1<- repeated_crime_2023 %>%
  distinct(Arrest.Date, CCN, .keep_all = TRUE)
head(repeated_crime_2023_1)

