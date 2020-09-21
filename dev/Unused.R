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