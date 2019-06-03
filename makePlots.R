rm(list = ls())

# load packages
library(readxl)
library(formatR)
library(tidyverse)
library(ggthemes)


# load data
girlschool <- read_excel("data/API_SE.PRM.CMPT.FE.ZS_DS2_en_excel_v2_10582352.xls", sheet = "Data", skip = 2)
colnames(girlschool) <- make.names(colnames(girlschool), unique = TRUE)
catastrophe <- read_csv("data/emdat.csv", skip = 1, n_max = 120)
colnames(catastrophe) <- make.names(colnames(catastrophe), unique = TRUE)
poppredict <- read_csv("data/WPP2017_PopulationByAgeSex_Medium.csv")
poverty <- read_csv("data/API_SI.POV.DDAY_DS2_en_csv_v2_10577366.csv", skip = 4) %>% select(-c(X64))
colnames(poverty) <- make.names(colnames(poverty), unique = TRUE)


# Mädchen Grunschule
girlschool <- girlschool %>% filter(Country.Code == "LIC") %>% select(-c(1, 3, 4)) %>% gather(year, girlperc, 
    -Country.Code) %>% mutate(year = as.numeric(substr(year, 2, 5)))
girlschool %>% ggplot(aes(x = year, y = girlperc)) + geom_area(position = "identity", fill = "grey90", 
    colour = "white") + theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + scale_x_continuous(limits = c(1972, 
    2017), breaks = c(1972, seq(1980, 2010, 10), 2017), expand = c(0.01, 0)) + scale_y_continuous(limits = c(0, 
    80), label = sprintf("%s%%", seq(0, 80, 20)), expand = c(0.01, 0)) + geom_vline(xintercept = 1980, 
    colour = "white") + geom_vline(xintercept = 1990, colour = "white") + geom_vline(xintercept = 2000, 
    colour = "white") + geom_vline(xintercept = 2010, colour = "white") + geom_line(size = 1.5) + annotate("text", 
    x = 1975, y = 70, family = "serif", size = 8, label = "Anteil der Mädchen die in\neinkommensschwachen Ländern\ndie Grundschule abschließen", 
    hjust = 0)

# Catastrophes
catastrophe %>% mutate(decade = year - year%%10) %>% group_by(decade) %>% summarise_at("Total.deaths", 
    sum, na.rm = TRUE) %>% ggplot(aes(x = decade, y = Total.deaths/1e+06)) + geom_area(position = "identity", 
    fill = "grey90", colour = "white") + theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + 
    geom_line(size = 1.5) + scale_x_continuous(limits = c(1900, 2010), breaks = seq(1900, 2010, 20), 
    expand = c(0.01, 0)) + scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2), expand = c(0.01, 
    0)) + annotate("text", x = 1940, y = 8, family = "serif", size = 8, label = "Todesopfer in Millionen\ndurch Naturkatastrophen je Jahrzent", 
    hjust = 0)

# Populationsvorhersage
poppredict %>% filter(AgeGrpStart <= 10, Location == "World") %>% group_by(Time) %>% summarise_at("PopTotal", 
    sum, na.rm = TRUE) %>% mutate(predic = ifelse(Time > 2019, 1, 0)) %>% ggplot(aes(x = Time, y = PopTotal/1e+06, 
    linetype = factor(predic))) + scale_linetype_discrete(guide = FALSE) + geom_line(size = 1.5) + theme_tufte(base_size = 15) + 
    theme(axis.title = element_blank()) + scale_x_continuous(expand = c(0.01, 0), breaks = seq(1950, 
    2100, 50)) + scale_y_continuous(expand = c(0.01, 0), breaks = seq(0.5, 2.5, 0.5), limits = c(0.5, 
    2.5)) + geom_segment(x = 2019, xend = 2019, y = 1.6, yend = 2.3, col = "red") + annotate("text", 
    x = 2020, y = 1.25, family = "serif", size = 8, label = "Anzahl an Kinder (0-14 Jahre)\nweltweit in Millionen", 
    hjust = 0) + annotate("text", x = 2050, y = 2.14, family = "serif", size = 5, label = "Vorhersage", 
    col = "grey25", hjust = 0)

# Poverty
poverty <- poverty %>% filter(Country.Code == "WLD") %>% select(-c(1, 3, 4)) %>% gather(year, extrpov, 
    -Country.Code) %>% mutate(year = as.numeric(substr(year, 2, 5)))
poverty %>% drop_na(extrpov) %>% ggplot(aes(x = year, y = extrpov)) + geom_area(position = "identity", 
    fill = "grey90", colour = "white") + theme_tufte(base_size = 15) + theme(axis.title = element_blank()) +
     scale_y_continuous(limits = c(0, 50), label = sprintf("%s%%", seq(0, 50, 
    10)), expand = c(0.01, 0)) + scale_x_continuous(limits = c(1981, 2017), breaks = c(1981, seq(1990, 
    2010, 10), 2015), expand = c(0.01, 0)) + geom_vline(xintercept = 1980, colour = "white") + geom_vline(xintercept = 1990, 
    colour = "white") + geom_vline(xintercept = 2000, colour = "white") + geom_vline(xintercept = 2010, 
    colour = "white")+geom_point()+geom_line(size = 1.5)+
  annotate("text", x = 1990, y = 45, family = "serif", size = 8, label = "Anteil an Menschen\nin extremer Armut (weniger als 2$/Tag)", hjust = 0)



catastrophe %>% mutate(rolldeath = rollapply(Total.deaths, 5, mean, fill = NA, align = "right", na.rm = TRUE)) %>% 
    ggplot(aes(x = year, y = rolldeath)) + geom_line()
