# LSA-assignment

First, we're going to read in the data and look at the first 20 columns. We also ask for the class of the input per column, as this is quite important for further processing.

```{r}
library(tidyverse)
library(lubridate)
library(tibble)
library(ggpubr)

influenza_data <- read_csv("C:/Users/banou/Desktop/Documents/09 universiteit gent/Large Scale Analysis of Biomedical Data/our project/fluprint_export.csv") 

influenza_data <- as.data.frame(influenza_data)
head(influenza_data,20)
str(influenza_data)
```
We notice that some of the data isn't in the class that we want. Change the data to the right class. We're going to read it in again and immediately specify the column types. 

```{r}
influenza_data <- read_csv("C:/Users/banou/Desktop/Documents/09 universiteit gent/Large Scale Analysis of Biomedical Data/our project/fluprint_export.csv" , col_types = "ccffccffdffdfddfcfcccfdffdffffffffffff")

influenza_data <- as.data.frame(influenza_data)
head(influenza_data,20)
str(influenza_data)

```
To get some basic stats for each of the columns in our data frame, we use `summary()`

```{r}
summary(influenza_data)
```
The first issue we encounter when looking the first 20 rows is that every patient id has multiple rows. We found the origin of this issue in the columns about cell types. We were advised to take these columns out and make a subset out of them. We want the donor IDs to be the first column, and we want the cell types as columns names so in the end we get a dataframe with per row one patient and the value for the cell type.

making a subset:

#Data preprocessing
#Clean Data
split data into metadata, select the rows that lead to the duplications.
```{r}
metadata_flu<- influenza_data %>% 
  select(donor_id, mesurment_id, assay, name, name_formatted, subset, units, data)

head(metadata_flu)
```
We want to create a dataframe where the columns are the cell names and where the donor_id's are placed in the first column so we have a value for every person
```{r}
metadata_wide <- metadata_flu %>%
  pivot_wider(
    id_cols = donor_id,
    names_from = name_formatted,
    values_from = data
  )

head(metadata_wide)
```


Remove columns that lead to duplications in the original dataset from that dataset
```{r}
influenza_data <- influenza_data%>%
  select(-mesurment_id, -assay, -name, -name_formatted, -subset, -data, -units)

influenza_data
```
Remove duplications, as at this point, after removing the columns that made the rows unique, they're not unique anymore and we can remove them without any loss of information.

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

noNA_distinct_influenza <- distinct_data %>% filter(vaccine_response != "NULL")

ggplot(noNA_distinct_influenza, aes(x = factor (vaccine,levels = c(1, 2, 4, NA)), fill = vaccine_response)) +
  geom_bar(position = "stack") + # Use position = "dodge" for side-by-side bars
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", fill = "vaccine respons") +
  theme_minimal()

noNA_distinct_influenza %>%
ggplot(aes(x=factor (vaccine,levels = c(1,2,4,NA)), fill = vaccine_response)) +
geom_bar(position = "fill") +
theme_classic()
```
## Fisher test
```{r}
contingency_table <- table(distinct_data$vaccine, distinct_data$vaccine_response)

contingency_table <- contingency_table[, colnames(contingency_table) != "NULL"]
contingency_table <- contingency_table[rownames(contingency_table) != "6", ]
contingency_table <- contingency_table[rownames(contingency_table) != "NULL", ]
contingency_table <- contingency_table[rownames(contingency_table) != "5", ]
contingency_table <- contingency_table[rownames(contingency_table) != "3", ]
contingency_table <- contingency_table[rownames(contingency_table) != "2", ]


print(contingency_table)

fisher.test(contingency_table)

```
