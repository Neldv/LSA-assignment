# LSA-assignment

```{r}
library(tidyverse)
library(lubridate)
library(tibble)
library(ggpubr)

influenza_data <- read_csv("C:/Users/banou/Desktop/Documents/09 universiteit gent/Large Scale Analysis of Biomedical Data/our project/fluprint_export.csv" , col_types = "ccffccffdffdfddfcfcccfdffdffffffffffff") 

influenza_data <- as.data.frame(influenza_data)
```



remove the unnecessary colums
```{r}
metadata_flu<- influenza_data %>% 
  select(-subset, -name, -name_formatted, -units, -data, -assay, -mesurment_id)

metadata_flu
```

remove the duplicate rows
```{r}
distinct_data <- metadata_flu %>% distinct()
distinct_data
```
check if everything is good 
```{r}
summary(distinct_data)
```

