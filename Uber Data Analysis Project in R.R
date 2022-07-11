# Nous allons utiliser des packages tels que ggplot2 qui nous permettra de tracer différents types
# de visualisations relatives à plusieurs périodes de l'année. Ainsi, on pourra déterminer comment 
# le temps affecte les déplacements des clients et  à la fin, nous allons réaliser un tracé 
# géographique de New York qui nous fournira des détails sur la façon dont les différents 
# utilisateurs ont effectué des voyages (ou trajets) à partir de différentes bases.
 

# 1. Importation des bibliothèques.

knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(dplyr)
library(tidyr)
library(DT)
library(scales)

setwd("C:\\Users\\user\\R projects\\Uber data analysis")


# Création d'un vecteur de couleurs à mettre en œuvre dans nos graphes

colors = c("#CC1011", "#665555", "#05a399", "#cfcaca", "#f5e840", "#0683c9", "#e075b0")

           
# Nous allons maintenant, lire plusieurs fichiers csv qui contiennent les données d'avril 2014 à septembre 2014.
# Nous les stockerons dans des data frames correspondants tels que apr_data, may_data, etc. et après avoir lu les 
# fichiers, nous combinerons toutes ces données dans un seul data frame appelé 'data_2014'.

apr_data <- read.csv("uber-raw-data-apr14.csv")
may_data <- read.csv("uber-raw-data-may14.csv")
jun_data <- read.csv("uber-raw-data-jun14.csv")
jul_data <- read.csv("uber-raw-data-jul14.csv")
aug_data <- read.csv("uber-raw-data-aug14.csv")
sep_data <- read.csv("uber-raw-data-sep14.csv")

data_2014 <- rbind(apr_data,may_data, jun_data, jul_data, aug_data, sep_data)

data_2014$Date.Time <- as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S")

data_2014$Time <- format(as.POSIXct(data_2014$Date.Time, format = "%m/%d/%Y %H:%M:%S"), format="%H:%M:%S")

data_2014$Date.Time <- ymd_hms(data_2014$Date.Time)

data_2014$day <- factor(day(data_2014$Date.Time))
data_2014$month <- factor(month(data_2014$Date.Time, label = TRUE))
data_2014$year <- factor(year(data_2014$Date.Time))
data_2014$dayofweek <- factor(wday(data_2014$Date.Time, label = TRUE))



data_2014$hour <- factor(hour(hms(data_2014$Time)))
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))


#** Tracons les trajets en fonction des heures de la journée*

# Nous utiliserons la fonction ggplot pour tracer le nombre de trajets effectués par les passagers en une journée et
# également dplyr pour agréger nos données. 


# Trajets par heure:
hour_data <- data_2014 %>%
            group_by(hour) %>%
                dplyr::summarize(Total = n()) 
datatable(hour_data)


ggplot(hour_data, aes(hour, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue", color = "red") +
  ggtitle("Trips Every Hour") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


#Trajets par heure et par mois:
month_hour <- data_2014 %>%
  group_by(month, hour) %>%
  dplyr::summarize(Total = n())

ggplot(month_hour, aes(hour, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Hour and Month") +
  scale_y_continuous(labels = comma)

# Dans les visualisations résultantes, nous pouvons comprendre comment le nombre de 
# passagers évolue au cours de la journée. Nous observons que le nombre de trajets est 
# plus élevé le soir vers 17h00 et 18h00.


#** Traçons des données par trajet pour chaque jour du mois**

#Dans cette section, nous allons apprendre à tracer nos données en fonction de chaque jour 
# du mois. 

day_group <- data_2014 %>%
  group_by(day) %>%
  dplyr::summarize(Total = n()) 
datatable(day_group)


ggplot(day_group, aes(day, Total)) + 
  geom_bar( stat = "identity", fill = "steelblue") +
  ggtitle("Trips Every Day") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

# Nous observons à partir de la visualisation résultante que le 30 du mois a eu le plus grand nombres de trajet dans 
# l'année qui est principalement contribué par le mois d'avril.


day_month_group <- data_2014 %>%
  group_by(month, day) %>%
  dplyr::summarize(Total = n())

ggplot(day_month_group, aes(day, Total, fill = month )) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) + 
  scale_fill_manual(values =colors)

# Nous observons, à partir de la visualisation résultante, que le 30 du mois a eu les voyages 
# les plus élevés dans l'année, ce qui est principalement contribué par le mois d'avril.


#**Nombre de voyages effectués au cours des mois de l'année*

# Dans cette section, nous allons visualiser le nombre de voyages qui eu ont lieu chaque mois
# de l'année. 


month_group <- data_2014 %>%
  group_by(month) %>%
  dplyr::summarize(Total = n()) 
datatable(month_group)


ggplot(month_group, aes(month, Total, fill = month)) + 
  geom_bar( stat = "identity") +
  ggtitle("Trips by month.") +
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)

# Dans la visualisation, nous observons que la plupart des courses ont
# été effectués au cours du mois de septembre. 

# On peut peut également obtenir les rapports visuels du nombre de courses effectués chaque
# jour de la semaine à travers le code suivant:

month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())

ggplot(month_weekday, aes(month, Total, fill = dayofweek)) + 
  geom_bar( stat = "identity", position = "dodge") +
  ggtitle("Trips by Day and Month") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colors)


#** Déterminons le nombre de course par base.*

# Dans la visualisation suivante, nous représenterons le nombre de courses effectués par les 
# passagers de chacune des bases. Il y a cinq bases en tout.

#**Trajets par Bases et par mois*

ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkgreen") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases")

ggplot(data_2014, aes(Base, fill = month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and Month.") +
  scale_fill_manual(values = colors)

#**Courses par bases et par jour de la semaine*

ggplot(data_2014, aes(Base, fill = dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips by Bases and DayofWeek.") +
  scale_fill_manual(values = colors)

# Parmis les bases nous observons que: B02617 a eu le plus grand nombre de courses. De plus, 
# cette base a enregistré le plus grand nombre de voyages au cours du mois B02617. 
# Jeudi est le jour ou on observe le plus grand nombre de voyages dans les trois bases B02598, 
# B02617, B02682.

#** Visualisation Heatmap*

# Dans cette section, nous allons apprendre à tracer des Heatmap à l'aide de ggplot(). 
# Nous allons en tracer cinq.

'Premierement par heure et par jour. Pour cela nous allons faire un groupby par jour et par heure:'

day_and_hour <- data_2014 %>%
         group_by(day, hour) %>%
            dplyr::summarize(Total = n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day")


'Deuxiement, par mois et par jour.'

ggplot(day_month_group, aes(day, month, fill = Total)) +
            geom_tile(color = "white") +
              ggtitle("Heat Map by Month and Day.")


'Troisièmement, par mois et jour de la semaine en passant par un groupe by'

month_weekday <- data_2014 %>%
  group_by(month, dayofweek) %>%
  dplyr::summarize(Total = n())
datatable(month_weekday)

ggplot(month_weekday, aes(dayofweek, month, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Month and Day of Week")


'Quatrièmement, une Heatmap qui délimite le Mois et les Bases.'

month_base <-  data_2014 %>%
                    group_by(Base, month) %>%
                        dplyr::summarize(Total = n()) 

ggplot(month_base, aes(Base, month, fill = Total)) +
            geom_tile(color = "white") +
              ggtitle("Heat Map by Month and Bases")


'Enfin, par bases et jour de la semaine.'

day0fweek_bases <-  data_2014 %>%
  group_by(Base, dayofweek) %>%
  dplyr::summarize(Total = n()) 

ggplot(day0fweek_bases, aes(Base, dayofweek, fill = Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Bases and Day of Week")


#** Création d'une visualisation cartographique des courses à New York*

# Dans la dernière section, nous visualiserons les trajets dans la ville de New York en créant 
# un géo-plot qui nous aidera à visualiser les trajets durant l'année 2014 (avril - septembre)
# et par les bases dans la même période.

min_lat <- 40.5774
max_lat <- 40.9176
min_long <- -74.15
max_long <- -73.7004

nyc_map <- ggplot(data_2014, aes(x=Lon, y=Lat)) +
  geom_point(size=1, color = "green") +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP)")

nyc_map 


nyc_map2<- ggplot(data_2014, aes(x=Lon, y=Lat, color = Base)) +
  geom_point(size=1) +
  scale_x_continuous(limits=c(min_long, max_long)) +
  scale_y_continuous(limits=c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")

nyc_map2 


