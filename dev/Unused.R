```{r load libraries, warning=FALSE, message = FALSE, include = FALSE}
packages.used = as.list(c("tidyverse", "haven", "devtools", 
                          "RColorBrewer", "data.table", "ggplot2"))

check.pkg = function(x){
  if(!require(x, character.only=T)) install.packages(x, character.only = T, dependence = T)
}

lapply(packages.used, check.pkg)
```


```{r read in data, message = F, include=FALSE}
library(haven)
anes_dat <- read_sav("../data/anes_timeseries_cdf.sav")

anes_dat_2008 <- read_sav("../data/anes_timeseries_2008.sav")
anes_dat_2012 <- read_sav("../data/anes_timeseries_2008.sav")
anes_dat_2016 <- read_sav("../data/anes_timeseries_2008.sav")


anes_pilot_2016 <- read_sav("../data/anes_pilot_2016.sav")
anes_pilot_2018 <- read_sav("../data/anes_pilot_2018.sav")
anes_pilot_2019 <- read_sav("../data/anes_pilot_2019.sav")
anes_pilot_2020 <- read_sav("../data/anes_pilot_2020ets_sav.SAV")
```

# Subsetted Datasets
# Five variables for 2016
# Five of the same variables for 2020 plus 2 variables unique to 2020

anes_pilot_2016_poi <- anes_pilot_2016[, c("state", "race", "pid3", "fttrump", "fthrc", "vote16dt")]
anes_pilot_2020_poi <- anes_pilot_2020[, c("state", "race7", "pid1r", "fttrump1", "ftbiden1", "vote20jb", "covidpres7", "healthcarepres7")]

# Final Plots baser and ggplot2

```{r, echo = FALSE}
par(mfrow = c(2, 2))
barplot(prop.table(table(anes_pilot_2016_poi$fttrump[(anes_pilot_2016_poi$fttrump < 101)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Trump - 2016", col = "red") 

barplot(prop.table(table(anes_pilot_2020_poi$fttrump1[(anes_pilot_2020_poi$fttrump1 < 101)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Trump - 2020", col = "red")

barplot(prop.table(table(anes_pilot_2016_poi$fthrc[(anes_pilot_2016_poi$fthrc < 101)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Clinton - 2016", col = "blue")

barplot(prop.table(table(anes_pilot_2020_poi$ftbiden1[(anes_pilot_2020_poi$ftbiden1 < 101)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Biden - 2020", col = "blue")


ggplot(data = anes_pilot_2016_poi) + 
  geom_bar(mapping = aes(x = fttrump, y = ..prop.., group = 1), stat = "count", fill = "red") +
  xlim(-1, 101) + ylim(0, 0.3) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Trump - 2016",) + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = anes_pilot_2020_poi) + 
  geom_bar(mapping = aes(x = fttrump1, y = ..prop.., group = 1), stat = "count", fill = "red") +
  xlim(-1, 101) + ylim(0, 0.3) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Trump - 2020",) + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = anes_pilot_2016_poi) + 
  geom_bar(mapping = aes(x = fthrc, y = ..prop.., group = 1), stat = "count", fill = "blue") +
  xlim(-1, 101) + ylim(0, 0.3) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Clinton - 2016",) + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = anes_pilot_2020_poi) + 
  geom_bar(mapping = aes(x = ftbiden1, y = ..prop.., group = 1), stat = "count", fill = "blue") +
  xlim(-1, 101) + ylim(0, 0.3) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Biden - 2020",) + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(trump2016, trump2020, clinton2016, biden2020, ncol = 2, nrow = 2)
```


```{r, echo = FALSE, fig.height = 4, warning = FALSE}
par(mfrow = c(1, 2))
barplot(prop.table(table(anes_pilot_2016_poi$vote16dt)))
barplot(prop.table(table(anes_pilot_2020_poi$vote20jb[anes_pilot_2020$vote20jb != 9])))

election2016 <- ggplot(data = anes_pilot_2016_poi) + 
  geom_bar(mapping = aes(x = vote16dt, y = ..prop.., group = 1), stat = "count") + 
  ylim(0, 0.5) + 
  xlab("Candidate") + ylab("Proportion") + 
  ggtitle("2016 Election Choice",) + theme(plot.title = element_text(hjust = 0.5))

election2020 <- ggplot(data = anes_pilot_2020_poi[anes_pilot_2020_poi$vote20jb != 9,] ) + 
  geom_bar(mapping = aes(x = vote20jb, y = ..prop.., group = 1), stat = "count") + 
  ylim(0, 0.5) + 
  xlab("Candidate") + ylab("Proportion") + 
  ggtitle("2020 Election Choice",) + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(election2016, election2020, ncol = 2, nrow = 1)
```


```{r, echo = FALSE}
par(mfrow = c(1, 2))

barplot(table(anes_pilot_2020_poi$covidpres7[which(anes_pilot_2020_poi$covidpres7 != 88)]))

barplot(table(anes_pilot_2020_poi$healthcarepres7[(anes_pilot_2020_poi$healthcarepres7 != 77) & (anes_pilot_2020_poi$healthcarepres7 != 99)]))

covid2020 <- ggplot(data = anes_pilot_2020_poi[which(anes_pilot_2020_poi$covidpres7 != 88),]) + 
  geom_bar(mapping = aes(x = covidpres7, y = ..prop.., group = 1), stat = "count") + 
  ylim(0, 0.5) + 
  xlab("Rating") + ylab("Proportion") + 
  ggtitle("Trump + COVID-19 Ratings",) + theme(plot.title = element_text(hjust = 0.5))

healthcare2020 <- ggplot(data = anes_pilot_2020_poi[(anes_pilot_2020_poi$healthcarepres7 != 77) & 
                                                      (anes_pilot_2020_poi$healthcarepres7 != 99), ]) + 
  geom_bar(mapping = aes(x = healthcarepres7, y = ..prop.., group = 1), stat = "count") + 
  ylim(0, 0.5) + 
  xlab("Rating") + ylab("Proportion") + 
  ggtitle("Trump + COVID-19 Ratings",) + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(election2016, election2020, ncol = 2, nrow = 1)

```


```{r, echo = FALSE}
par(mfrow = c(2, 2))
barplot(prop.table(table(anes_pilot_2016_poi$fttrump[(anes_pilot_2016_poi$fttrump < 101) & (anes_pilot_2016_poi$race == 4)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Trump - 2016", col = "red") 

barplot(prop.table(table(anes_pilot_2020_poi$fttrump1[(anes_pilot_2020_poi$fttrump1 < 101) & (anes_pilot_2020_poi$race7 == 3)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Trump - 2020", col = "red")

barplot(prop.table(table(anes_pilot_2016_poi$fthrc[(anes_pilot_2016_poi$fthrc < 101) & (anes_pilot_2016_poi$race == 4)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Clinton - 2016", col = "blue")

barplot(prop.table(table(anes_pilot_2020_poi$ftbiden1[(anes_pilot_2020_poi$ftbiden1 < 101) & (anes_pilot_2020_poi$race7 == 3)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Biden - 2020", col = "blue")



trump2016_asian <- ggplot(data = anes_pilot_2016_poi[(anes_pilot_2016_poi$fttrump < 101) & (anes_pilot_2016_poi$race == 4), ]) + 
  geom_bar(mapping = aes(x = fttrump, y = ..prop.., group = 1), stat = "count", fill = "red") +
  xlim(-1, 101) + ylim(0, 0.4) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Trump - 2016",) + theme(plot.title = element_text(hjust = 0.5))

trump2020_asian <- ggplot(data = anes_pilot_2020_poi[(anes_pilot_2020_poi$fttrump1 < 101) & 
                                                       (anes_pilot_2020_poi$race7 == 3), ]) + 
  geom_bar(mapping = aes(x = fttrump1, y = ..prop.., group = 1), stat = "count", fill = "red") +
  xlim(-5, 101) + ylim(0, 0.4) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Trump - 2020",) + theme(plot.title = element_text(hjust = 0.5))

clinton2016_asian <- ggplot(data = anes_pilot_2016_poi[(anes_pilot_2016_poi$fthrc < 101) & (anes_pilot_2016_poi$race == 4), ]) + 
  geom_bar(mapping = aes(x = fthrc, y = ..prop.., group = 1), stat = "count", fill = "blue") +
  xlim(-1, 101) + ylim(0, 0.4) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Clinton - 2016",) + theme(plot.title = element_text(hjust = 0.5))

biden2020_asian <- ggplot(data = anes_pilot_2020_poi[(anes_pilot_2020_poi$ftbiden1 < 101) & (anes_pilot_2020_poi$race7 == 3), ]) + 
  geom_bar(mapping = aes(x = ftbiden1, y = ..prop.., group = 1), stat = "count", fill = "blue") +
  xlim(-1, 101) + ylim(0, 0.4) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Biden - 2020",) + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(trump2016_asian, trump2020_asian, clinton2016_asian, biden2020_asian, ncol = 2, nrow = 2)
```

`
```{r, echo = FALSE}
par(mfrow = c(2, 2))
barplot(prop.table(table(anes_pilot_2016_poi$fttrump[(anes_pilot_2016_poi$fttrump < 101) & (anes_pilot_2016_poi$race == 2)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Trump - 2016", col = "red") 

barplot(prop.table(table(anes_pilot_2020_poi$fttrump1[(anes_pilot_2020_poi$fttrump1 < 101) & (anes_pilot_2020_poi$race7 == 2)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Trump - 2020", col = "red")

barplot(prop.table(table(anes_pilot_2016_poi$fthrc[(anes_pilot_2016_poi$fthrc < 101) & (anes_pilot_2016_poi$race == 2)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Clinton - 2016", col = "blue")

barplot(prop.table(table(anes_pilot_2020_poi$ftbiden1[(anes_pilot_2020_poi$ftbiden1 < 101) &
                                                        (anes_pilot_2020_poi$race7 == 2)])), 
        xlab = "Temperature", ylab = "Proportion", 
        main = "Biden - 2020", col = "blue")

trump2016_black <- ggplot(data = anes_pilot_2016_poi[(anes_pilot_2016_poi$fttrump < 101) &
                                                       (anes_pilot_2016_poi$race == 2), ]) + 
  geom_bar(mapping = aes(x = fttrump, y = ..prop.., group = 1), stat = "count", fill = "red") +
  xlim(-1, 101) + ylim(0, 0.5) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Trump - 2016 (Black)",) + theme(plot.title = element_text(hjust = 0.5))

trump2020_black <- ggplot(data = anes_pilot_2020_poi[(anes_pilot_2020_poi$fttrump1 < 101) & 
                                                       (anes_pilot_2020_poi$race7 == 2), ]) + 
  geom_bar(mapping = aes(x = fttrump1, y = ..prop.., group = 1), stat = "count", fill = "red") +
  xlim(-5, 101) + ylim(0, 0.5) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Trump - 2020 (Black)",) + theme(plot.title = element_text(hjust = 0.5))

clinton2016_black <- ggplot(data = anes_pilot_2016_poi[(anes_pilot_2016_poi$fthrc < 101) &
                                                         (anes_pilot_2016_poi$race == 2), ]) + 
  geom_bar(mapping = aes(x = fthrc, y = ..prop.., group = 1), stat = "count", fill = "blue") +
  xlim(-1, 101) + ylim(0, 0.5) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Clinton - 2016 (Black)",) + theme(plot.title = element_text(hjust = 0.5))

biden2020_black <- ggplot(data = anes_pilot_2020_poi[(anes_pilot_2020_poi$ftbiden1 < 101) &
                                                       (anes_pilot_2020_poi$race7 == 2), ]) + 
  geom_bar(mapping = aes(x = ftbiden1, y = ..prop.., group = 1), stat = "count", fill = "blue") +
  xlim(-1, 101) + ylim(0, 0.4) + 
  xlab("Temperature") + ylab("Proportion") + 
  ggtitle("Biden - 2020 (Black)",) + theme(plot.title = element_text(hjust = 0.5))

grid.arrange(trump2016_black, trump2020_black, clinton2016_black, biden2020_black, ncol = 2, nrow = 2)
```

















View(anes_dat_2008)
View(anes_dat_2012)
View(anes_dat_2016)

View(anes_pilot_2016)
View(anes_pilot_2018)
View(anes_pilot_2019)
View(anes_pilot_2020)


# Only Florida
anes_pilot_2016_poi_FL <- anes_pilot_2016_poi[anes_pilot_2016_poi$state == 12, ]
anes_pilot_2020_poi_FL <- anes_pilot_2020_poi[anes_pilot_2020_poi$state == 9, ]

# Proportion of Sampled Floridians vs. Proportion of Electoral Votes
29/538
nrow(anes_pilot_2016_poi_FL)/nrow(anes_pilot_2016_poi)
nrow(anes_pilot_2020_poi_FL)/nrow(anes_pilot_2020_poi)


## Vote-by-Mail

barplot(table(anes_pilot_2020$votemail1a))
barplot(table(anes_pilot_2020$votemail1b))

barplot(table(anes_pilot_2020$votemail2))

```{r labelled variables subset, include = FALSE}
Election_years = as.character(seq(1952, 2016, 4))

anes_use = anes_dat %>% mutate(
  year = as_factor(VCF0004),
  turnout = as_factor(VCF0703),
  vote = as_factor(VCF0706),
  race = as_factor(VCF0105a),
  gender = as_factor(VCF0104)) %>% filter(year %in% Election_years)

library(data.table)

data.table(anes_use %>% select(year, turnout, vote, race, gender) %>%
             filter(!is.na(turnout)) %>% sample_n(30))

anes_use = anes_use%>%select(year, turnout, vote, race, gender)

# save(anes_use, file = "../output/data_use.RData")

# load(file = "../output/data_use.RData")

anes_to_race_year = anes_use %>% filter(!is.na(race) & !is.na(turnout)) %>% 
  group_by(year, race) %>% count(turnout) %>% group_by(year, race) %>% 
  mutate(prop = n/sum(n))
```


barplot(table(anes_pilot_2018$fttrump))
barplot(table(anes_pilot_2019$fttrump))

barplot(table(anes_pilot_2018$fttrump))
barplot(table(anes_pilot_2019$fttrump))


ggplot(data = anes_pilot_2016) + 
  geom_bar(mapping = aes(x = fttrump, y = ..prop.., group = 1), stat = "count")
ggplot(data = anes_pilot_2018) + 
  geom_bar(mapping = aes(x = fttrump, y = ..prop.., group = 1), stat = "count")
ggplot(data = anes_pilot_2019) + 
  geom_bar(mapping = aes(x = fttrump, y = ..prop.., group = 1), stat = "count")
ggplot(data = anes_pilot_2020) + 
  geom_bar(mapping = aes(x = fttrump1, y = ..prop.., group = 1), stat = "count")

# COVID-19

```{r}
anes_pilot_2020$covid_elect
barplot(table(anes_pilot_2020$covid_elect))
```

anes_pilot_2016_poi <- anes_pilot_2016[, c(218:220, 222:223, 225, 227, 229, 232:234, 23, 25, 42)]

anes_pilot_2020_poi <- anes_pilot_2020[, c(375:376, 377,  44, 46, 75)]

unique(anes_pilot_2016$vote16dt)
unique(anes_pilot_2020$vote20jb)

# States

```{r}
unique(anes_pilot_2016$state)
anes_pilot_2016$state_1 <- if (state)
  
  table(anes_pilot_2016$state)


unique(anes_pilot_2020$state)
table(anes_pilot_2020$state)
```



# Florida

```{r}
par(mfrow = c(2, 2))
barplot(table(anes_pilot_2016_poi_FL$fttrump))

barplot(table(anes_pilot_2020_poi_FL$fttrump1))

barplot(table(anes_pilot_2016_poi_FL$fthrc))

barplot(table(anes_pilot_2020_poi_FL$ftbiden1))
```

```{r}
barplot(table(anes_pilot_2016_poi_FL$vote16dt))
barplot(table(anes_pilot_2020_poi_FL$vote20jb))
```


```{r}
table(anes_pilot_2016_poi_FL$pid3)
table(anes_pilot_2020_poi_FL$pid1r)
```


# In General

```{r, include = FALSE}
par(mfrow = c(1, 3))

barplot(prop.table(table(anes_pilot_2018$apppres)))

#barplot(table(anes_pilot_2019$apppres5))
#barplot(table(anes_pilot_2019$apppres7))
barplot(prop.table(table(anes_pilot_2019$apppres7) + c(380, 272/2, 272/2, 140, 130/2, 130/2, 640)))

barplot(prop.table(table(anes_pilot_2020$apppres7[anes_pilot_2020$apppres7 != 99])))
```


# One explanation for this is because, at the time, Biden has not yet become the Democratic nominee for 2020 yet and it is possible that other candidates, like Benie Sanders, would have been seen more favorably by among Blacks. 
```{r}
barplot(prop.table(table(anes_pilot_2020$ftsanders1[(anes_pilot_2020$ftsanders1 < 101) & (anes_pilot_2020$race7 == 2)])))

mean(anes_pilot_2020$ftsanders1[(anes_pilot_2020$ftsanders1 < 101) & (anes_pilot_2020$race7 == 2)])
```


```{r, include = FALSE}
round(mean(na.omit(anes_pilot_2016_poi$fttrump[(anes_pilot_2016_poi$fttrump < 101) & (anes_pilot_2016_poi$race == 2)])), 2)

round(mean(na.omit(anes_pilot_2020_poi$fttrump1[(anes_pilot_2020_poi$fttrump1 < 101) & (anes_pilot_2020_poi$race7 == 2)])), 2)


round(mean(na.omit(anes_pilot_2016_poi$fthrc[(anes_pilot_2016_poi$fthrc < 101) & (anes_pilot_2016$race == 2)])), 2)

round(mean(na.omit(anes_pilot_2020_poi$ftbiden1[(anes_pilot_2020_poi$ftbiden1 < 101) & (anes_pilot_2020_poi$race7 == 2)])), 2)


round(mean(na.omit(anes_pilot_2016_poi$fthrc[(anes_pilot_2016_poi$fthrc < 101) & (anes_pilot_2016$race == 2)])), 2) - round(mean(na.omit(anes_pilot_2016_poi$fttrump[(anes_pilot_2016_poi$fttrump < 101) & (anes_pilot_2016_poi$race == 2)])), 2)


round(mean(na.omit(anes_pilot_2020_poi$ftbiden1[(anes_pilot_2020_poi$ftbiden1 < 101) & (anes_pilot_2020_poi$race7 == 2)])), 2) - round(mean(na.omit(anes_pilot_2020_poi$fttrump1[(anes_pilot_2020_poi$fttrump1 < 101) & (anes_pilot_2020_poi$race7 == 2)])), 2)

```


# US Averages 
mean(na.omit(anes_pilot_2016$fttrump[anes_pilot_2016$fttrump < 101]))
mean(na.omit(anes_pilot_2020$fttrump1[anes_pilot_2020$fttrump1 < 101]))

mean(na.omit(anes_pilot_2016$fthrc[anes_pilot_2016$fthrc < 101]))
mean(na.omit(anes_pilot_2020$ftbiden1[anes_pilot_2020$ftbiden1 < 101]))

mean(na.omit(anes_pilot_2016$fttrump[anes_pilot_2016$fttrump < 101]))
mean(na.omit(anes_pilot_2020$fttrump1[anes_pilot_2020$fttrump1 < 101]))

mean(na.omit(anes_pilot_2016$fthrc[anes_pilot_2016$fthrc < 101]))
mean(na.omit(anes_pilot_2020$ftbiden1[anes_pilot_2020$ftbiden1 < 101]))

# Asian American Averages 


mean(na.omit(anes_pilot_2016_poi$fttrump[(anes_pilot_2016_poi$fttrump < 101) & (anes_pilot_2016_poi$race == 4)]))
mean(na.omit(anes_pilot_2020_poi$fttrump1[(anes_pilot_2020_poi$fttrump1 < 101) & (anes_pilot_2020_poi$race7 == 3)]))


mean(na.omit(anes_pilot_2016_poi$fthrc[(anes_pilot_2016_poi$fthrc < 101) & (anes_pilot_2016_poi$race == 4)]))
mean(na.omit(anes_pilot_2020_poi$ftbiden1[(anes_pilot_2020_poi$ftbiden1 < 101) & (anes_pilot_2020_poi$race7 == 3)]))