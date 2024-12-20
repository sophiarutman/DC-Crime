#
#
#
#
#
#
#
#
library(tidyverse)
library(dplyr)
library(DescTools)

#
#
#
#
adult <- read_csv("../2013-2023/arrests_2013-2023.csv") |> filter(Arrest.Year >= 2016)
adult$Arrest.Date <- as.Date(adult$Arrest.Date, format = "%Y-%m-%d")
grouped_adult <- adult |> group_by(Arrest.Date) |> summarize(count = n())
grouped_adult <- head(grouped_adult, -1) 
grouped_adult$count <- grouped_adult$count / sum(grouped_adult$count)
grouped_adult
# last date has absurd amount of arrests, on the scale of 10^8
barplot(height=grouped_adult$count, names.arg=grouped_adult$Arrest.Date, main = "Adult Arrests")
#
#
#
juv <- read_csv("../juvenile_arrest_data/all_juvenile_arrests.csv")
juv$ARREST_DATE <- as.Date(juv$ARREST_DATE, format = "%m/%d/%y")
juv$Arrest.Year <- format(juv$ARREST_DATE,"%y")
grouped_juv <- juv |> group_by(ARREST_DATE) |> summarize(count = n())
grouped_juv$count <- grouped_juv$count / sum(grouped_juv$count)
barplot(height=grouped_juv$count, names.arg=grouped_juv$ARREST_DATE, main = "Juvenile Arrests")
#
#
#
grouped_juv <- juv |> group_by(ARREST_DATE) |> summarize(count = n()) |> arrange(desc(count))
grouped_juv
#
#
#
#
ks.test(grouped_adult$count, grouped_juv$count)
#
#
#
grouped.adult.unscaled <- adult |> group_by(Arrest.Category) |> summarize(count = n()) |> arrange(desc(count))
grouped.adult$count <- grouped.adult.unscaled$count / sum(grouped.adult.unscaled$count)
sliced <- grouped.adult |> slice(0:20)

par(mar = c(30, 4, 4, 4))
barplot(height = sliced$count, names.arg = sliced$Arrest.Category, las = 2, main = "Adult Arrests")

grouped.juv.unscaled <- juv |> group_by(TOP_CHARGE_DESCRIPTION) |> summarize(count = n()) |> arrange(desc(count))
grouped.juv$count <- grouped.juv.unscaled$count / sum(grouped.juv.unscaled$count)
sliced <- grouped.juv |> slice(0:20)
#barplot(height = sliced$count, names.arg = sliced$TOP_CHARGE_DESCRIPTION, las = 2, main = "Juvenile Arrests")

names(grouped.juv) <- c("Arrest.Category", "count.juv")
names(grouped.juv.unscaled) <- c("Arrest.Category", "count.juv")

combined <- full_join(grouped.adult.unscaled, grouped.juv.unscaled, by = "Arrest.Category")

combined$count.juv <- as.numeric(combined$count.juv)
combined$count <- as.numeric(combined$count)
combined$count.juv[is.na(combined$count.juv)] <- 0
combined$count[is.na(combined$count)] <- 0
combined <- data.frame(Adult = combined$count, Juv = combined$count.juv)
combined
GTest(combined)
#
#
#
#
#
grouped.adult <- adult |> group_by(Arrest.Location.PSA) |> summarise(count = n())
grouped.adult$count <- grouped.adult$count / sum(grouped.adult$count)
grouped.adult$count[is.na(grouped.adult$count)] <- 0
grouped_adult <- grouped.adult[!is.na(grouped.adult$Arrest.Location.PSA),]

grouped.juv <- juv |> group_by(CRIME_PSA) |> summarise(count = n()) |> arrange(count)




#grouped.juv$count <- grouped.juv$count / sum(grouped.juv$count)
grouped.juv$CRIME_PSA <- as.double(grouped.juv$CRIME_PSA)
grouped.juv$count[is.na(grouped.juv$count)] <- 0
names(grouped.juv) <- c("Arrest.Location.PSA", "count.juv")

grouped.juv <- grouped.juv[!is.na(grouped.juv$Arrest.Location.PSA),]

combined <- full_join(grouped.adult, grouped.juv, by = "Arrest.Location.PSA")
combined <- data.frame(Adult = combined$count, Juv = combined$count.juv)

par(cex.axis = 1.5)
barplot(height = grouped.adult$count, names.arg = grouped.adult$Arrest.Location.PSA, las = 2, main = "Adult Arrests")

# barplot(height = grouped.juv$count.juv, names.arg=grouped.juv$Arrest.Location.PSA, las = 2, main = "Juvenile Arrests")

ks.test(combined$Adult, combined$Juv)
#
#
#

grouped_juv <- juv |> group_by(ARREST_DATE) |> summarize(count = n()) |> arrange(desc(count))


arrests_ts_juv <- ts(juv_data$count, 
                 start = c(2016, 1), 
                 frequency = 12)  # Monthly data
# Decompose the time series
decomposed_ts_juv <- decompose(arrests_ts_juv)

# Plot decomposition
plot(decomposed_ts_juv)
#
#
#
