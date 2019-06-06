rm(list = ls())

# load packages
library(readxl)
library(formatR)
library(tidyverse)
library(ggthemes)
library(zoo)

# load data
girlschool <- read_excel("data/API_SE.PRM.CMPT.FE.ZS_DS2_en_excel_v2_10582352.xls", sheet = "Data", skip = 2)
colnames(girlschool) <- make.names(colnames(girlschool), unique = TRUE)
catastrophe <- read_csv("data/emdat.csv", skip = 1, n_max = 120)
colnames(catastrophe) <- make.names(colnames(catastrophe), unique = TRUE)
poppredict <- read_csv("data/WPP2017_PopulationByAgeSex_Medium.csv")
poverty <- read_csv("data/API_SI.POV.DDAY_DS2_en_csv_v2_10577366.csv", skip = 4) %>% select(-c(X64))
colnames(poverty) <- make.names(colnames(poverty), unique = TRUE)
popshare <- read_csv("data/API_SP.POP.TOTL_DS2_en_csv_v2_10576638/API_SP.POP.TOTL_DS2_en_csv_v2_10576638.csv", 
    skip = 4) %>% select(-c(X64))
colnames(popshare) <- make.names(colnames(popshare), unique = TRUE)

# Bevölkerungsentwicklung bzgl. Alter
kidspop <- poppredict %>% filter(Location == "World") %>% mutate(agegrp = cut(AgeGrpStart, breaks = c(-Inf, 
    10, 70, Inf), labels = c("Kids", "Middle", "Old"))) %>% group_by(agegrp, Time) %>% summarise_at("PopTotal", 
    sum, na.rm = TRUE) %>% ungroup() %>% mutate(predic = ifelse(Time > 2019, 1, 0))
kidspop$agegrp <- factor(kidspop$agegrp, levels = levels(kidspop$agegrp)[c(3, 2, 1)])
ggplot(kidspop, aes(x = Time, y = PopTotal/1e+06, fill = agegrp)) + geom_area(alpha = 0.6, size = 1, 
    color = "black") + theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + scale_fill_manual(guide = FALSE, 
    values = c("black", "grey", "white")) + geom_segment(x = 2019, xend = 2019, y = 0, yend = 12, col = "red", 
    alpha = 0.6) + scale_x_continuous(limits = c(1950, 2100), breaks = c(1950, 2000, 2019, 2050, 2100), 
    expand = c(0.01, 0)) + scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 3), expand = c(0.01, 
    0)) + geom_hline(yintercept = 3, col = "white", linetype = "dotted") + geom_hline(yintercept = 6, 
    col = "white", linetype = "dotted") + geom_hline(yintercept = 9, col = "white", linetype = "dotted") + 
    annotate("text", x = 2025, y = 1, family = "serif", size = 7, label = "Kindern (0-14 Jahre)", hjust = 0) + 
    annotate("text", x = 2025, y = 6.5, family = "serif", size = 7, label = "Erwachsenen (15-74 Jahre)", 
        hjust = 0) + annotate("text", x = 2025, y = 11, family = "serif", size = 7, label = "Alten (75+ Jahre)", 
    hjust = 0) + ggtitle("Weltweite Anzahl an ... (in Mrd.)") + theme(plot.title = element_text(hjust = 0.5))



# Anteil Bevölkerung in Einkommen über Zeit
wldpopshare <- popshare %>% filter(Country.Code %in% c("WLD")) %>% select(-c(1, 3, 4)) %>% gather(year, 
    wldpop, -Country.Code) %>% mutate(year = as.numeric(substr(year, 2, 5)))
popshare <- popshare %>% filter(Country.Code %in% c("LIC", "MIC", "HIC")) %>% select(-c(1, 3, 4)) %>% 
    gather(year, pop, -Country.Code) %>% mutate(year = as.numeric(substr(year, 2, 5))) %>% left_join(wldpopshare, 
    by = "year") %>% mutate(popratio = pop/wldpop) %>% select(-c("Country.Code.y")) %>% mutate(Country.Code.x = factor(Country.Code.x))
popshare$incometype <- factor(popshare$Country.Code.x, levels = levels(popshare$Country.Code.x)[c(1, 
    3, 2)])

ggplot(popshare, aes(x = year, y = popratio, fill = incometype)) + geom_area(alpha = 0.6, size = 1, colour = "black") + 
    theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + scale_x_continuous(limits = c(1960, 
    2017), breaks = c(seq(1960, 2010, 20), 2017), expand = c(0.01, 0)) + scale_y_continuous(limits = c(0, 
    1), breaks = seq(0, 1, 0.2), label = sprintf("%s%%", seq(0, 100, 20)), expand = c(0.01, 0)) + scale_fill_manual(guide = FALSE, 
    values = c("black", "grey", "white")) + geom_hline(yintercept = 0.2, col = "white", linetype = "dotted", 
    size = 0.5) + geom_hline(yintercept = 0.4, col = "white", linetype = "dotted", size = 0.5) + geom_hline(yintercept = 0.6, 
    col = "white", linetype = "dotted", size = 0.5) + geom_hline(yintercept = 0.8, col = "white", linetype = "dotted", 
    size = 0.5) + annotate("text", x = 1981, y = 0.03, family = "serif", size = 7, label = "niedrigem Einkommen", 
    hjust = 0) + annotate("text", x = 1981, y = 0.5, family = "serif", size = 7, label = "mittlerem Einkommen", 
    hjust = 0) + annotate("text", x = 1981, y = 0.9, family = "serif", size = 7, label = "hohem Einkommen", 
    hjust = 0) + ggtitle("Anteil der Weltbevölkerung mit ...") + theme(plot.title = element_text(hjust = 0.5))

# Mädchen Grunschule
girlschool <- girlschool %>% filter(Country.Code == "LIC") %>% select(-c(1, 3, 4)) %>% gather(year, girlperc, 
    -Country.Code) %>% mutate(year = as.numeric(substr(year, 2, 5)))
girlschool %>% ggplot(aes(x = year, y = girlperc)) + geom_area(position = "identity", fill = "grey90", 
    colour = "white") + theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + scale_x_continuous(limits = c(1972, 
    2017), breaks = c(1972, seq(1980, 2010, 10), 2017), expand = c(0.01, 0)) + scale_y_continuous(limits = c(0, 
    80), label = sprintf("%s%%", seq(0, 80, 20)), expand = c(0.01, 0)) + geom_hline(yintercept = 20, 
    col = "white") + geom_hline(yintercept = 40, col = "white") + geom_hline(yintercept = 60, col = "white") + 
    geom_line(size = 1.5) + annotate("text", x = 1975, y = 70, family = "serif", size = 8, label = "Anteil an Mädchen die in\neinkommensschwachen Ländern\ndie Grundschule abschließen", 
    hjust = 0)
ggsave("girlschool.png", width = 10, height = 6, units = "cm", dpi = 300, scale = 2.2)

# Catastrophes
catastrophe %>% mutate(decade = year - year%%10) %>% group_by(decade) %>% summarise_at("Total.deaths", 
    sum, na.rm = TRUE) %>% ggplot(aes(x = decade, y = Total.deaths/1e+06)) + geom_area(position = "identity", 
    fill = "grey90", colour = "white") + theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + 
    geom_hline(yintercept = 2, col = "white") + geom_hline(yintercept = 4, col = "white") + geom_hline(yintercept = 6, 
    col = "white") + geom_hline(yintercept = 8, col = "white") + geom_line(size = 1.5) + scale_x_continuous(limits = c(1900, 
    2010), breaks = seq(1900, 2010, 20), expand = c(0.01, 0)) + scale_y_continuous(limits = c(0, 10), 
    breaks = seq(0, 10, 2), expand = c(0.01, 0)) + annotate("text", x = 1930, y = 8, family = "serif", 
    size = 8, label = "Todesopfer von Naturkatastrophen\nüber die Jahrzente (in Millionen)", hjust = 0)
ggsave("catastroph.png", width = 10, height = 6, units = "cm", dpi = 300, scale = 2.2)


# Populationsvorhersage fpr Kinder
poppredict %>% filter(AgeGrpStart <= 10, Location == "World") %>% group_by(Time) %>% summarise_at("PopTotal", 
    sum, na.rm = TRUE) %>% mutate(predic = ifelse(Time > 2019, 1, 0)) %>% ggplot(aes(x = Time, y = PopTotal/1e+06, 
    linetype = factor(predic))) + geom_area(aes(fill = factor(predic)), colour = "white") + geom_hline(yintercept = 0.5, 
    col = "white") + geom_hline(yintercept = 1, col = "white") + geom_hline(yintercept = 1.5, col = "white") + 
    geom_hline(yintercept = 2, col = "white") + scale_x_continuous(expand = c(0.01, 0), breaks = seq(1950, 
    2100, 50)) + scale_y_continuous(expand = c(0.01, 0), breaks = seq(0.5, 2.5, 0.5), limits = c(0, 2.5)) + 
    scale_linetype_discrete(guide = FALSE) + geom_line(size = 1.5) + theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + 
    scale_fill_manual(guide = FALSE, values = c("grey90", "lightpink1")) + annotate("text", x = 1955, 
    y = 2.35, family = "serif", size = 8, label = "Weltweite Anzahl an Kindern (0-14 Jahre)\nin Millionen", 
    hjust = 0) + annotate("text", x = 2045, y = 2.14, family = "serif", size = 5, label = "Schätzung der UN", 
    col = "lightpink1", hjust = 0) + labs(caption = "Wie")
ggsave("kidspop.png", width = 10, height = 6, units = "cm", dpi = 300, scale = 2.2)
# geom_segment(x = 2019.5, xend = 2019.5, y = 1.6, yend = 2.3, col = 'red')


# Poverty
poverty <- poverty %>% filter(Country.Code == "WLD") %>% select(-c(1, 3, 4)) %>% gather(year, extrpov, 
    -Country.Code) %>% mutate(year = as.numeric(substr(year, 2, 5)))
poverty %>% drop_na(extrpov) %>% ggplot(aes(x = year, y = extrpov)) + geom_area(position = "identity", 
    fill = "grey90", colour = "white") + geom_hline(yintercept = 10, col = "white") + geom_hline(yintercept = 20, 
    col = "white") + geom_hline(yintercept = 30, col = "white") + geom_hline(yintercept = 40, col = "white") + 
    theme_tufte(base_size = 15) + theme(axis.title = element_blank()) + scale_y_continuous(limits = c(0, 
    50), label = sprintf("%s%%", seq(0, 50, 10)), expand = c(0.01, 0)) + scale_x_continuous(limits = c(1981, 
    2017), breaks = c(1981, seq(1990, 2010, 10), 2015), expand = c(0.01, 0)) + geom_point() + geom_line(size = 1.5) + 
    annotate("text", x = 1990, y = 45, family = "serif", size = 8, label = "Weltweiter Anteil an Menschen\nin extremer Armut", 
        hjust = 0)
ggsave("extrpov.png", width = 10, height = 6, units = "cm", dpi = 300, scale = 2.2)


catastrophe %>% mutate(rolldeath = rollapply(Total.deaths, 5, mean, fill = NA, align = "right", na.rm = TRUE)) %>% 
    ggplot(aes(x = year, y = rolldeath)) + geom_line()
