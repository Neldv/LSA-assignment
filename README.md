# LSA-assignment

```{r}
library(tidyverse)
library(lubridate)
library(tibble)
library(ggpubr)

influenza_data <- read_csv("C:/Users/banou/Desktop/Documents/09 universiteit gent/Large Scale Analysis of Biomedical Data/our project/fluprint_export.csv" , col_types = "ccffccffdffdfddfcfcccfdffdffffffffffff") 

influenza_data <- as.data.frame(influenza_data)
summary(influenza_data)
```

#Data preprocessing
#Clean Data
split data into metadata
```{r}
metadata_flu<- influenza_data %>% 
  select(donor_id, mesurment_id, assay, name, name_formatted, subset, units, data)

metadata_flu

influenza_data <- influenza_data%>%
  select(-mesurment_id, -assay, -name, -name_formatted, -subset, -data, -units, -bmi)

influenza_data
```

remove the duplicate rows
```{r}
distinct_data <- distinct(influenza_data, donor_id, .keep_all = TRUE)
distinct_data
```
check if everything is good 
```{r}
summary(distinct_data)
```

#Research question 1: difference in vaccine response between vaccine type
## visualisation
```{r}
ggplot(distinct_data, aes(x = factor(vaccine, levels = c(1, 2, 3, 4, 5, 6, NA)), fill = vaccine_response)) +
  geom_bar(position = "stack") + # Use position = "dodge" for side-by-side bars
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y="respons count", fill = "vaccine respons") +
  theme_minimal()

distinct_data %>%
  ggplot(aes(x=factor(vaccine, levels = c(1, 2, 3, 4, 5, 6, NA)), fill = vaccine_response)) +
  geom_bar(position = "fill") +
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y = "relative respons count", fill = "vaccine respons") +
  theme_minimal()

noNA_distinct_data <- distinct_data %>% filter(!is.na(vaccine_response))

ggplot(noNA_distinct_data, aes(x = factor(vaccine, levels = c(1, 2, 3, 4, 5, 6, NA)), fill = vaccine_response)) +
  geom_bar(position = "stack") + # Use position = "dodge" for side-by-side bars
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y= "respons count", fill = "vaccine respons") +
  theme_minimal()

noNA_distinct_data %>%
  ggplot(aes(x=factor(vaccine, levels = c(1, 2, 3, 4, 5, 6, NA)), fill = vaccine_response)) +
  geom_bar(position = "fill")+
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y= "relative respons count", fill = "vaccine respons") +
  theme_minimal()
```


