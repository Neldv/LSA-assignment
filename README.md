# LSA-assignment

First, we're going to read in the data and look at the first 20 columns. We also ask for the columns names and the class of the input per column, as this is quite important for further processing.

```{r}
library(tidyverse)
library(lubridate)
library(tibble)
library(ggpubr)

influenza_data <- read_csv("C:/Users/banou/Desktop/Documents/09 universiteit gent/Large Scale Analysis of Biomedical Data/our project/fluprint_export.csv") 

influenza_data <- as.data.frame(influenza_data)
head(influenza_data,20)

colnames(data)

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
** we move on with the data exploration**
```{r}
install.packages("skimr")
library(skimr)

skim(distinct_data)
```
Visualize distributions for numerical variables
```{r}
numeric_vars <- distinct_data %>% select(where(is.numeric))
for (col in colnames(numeric_vars)) {
  plot_num <- ggplot(distinct_data, aes_string(x = col)) + 
    geom_histogram(bins = 30, fill = "skyblue", alpha = 0.7) +
    ggtitle(paste("Distribution of", col)) +
    theme_minimal() +
    labs(x = col, y = "Frequency")
  
  print(plot_num)  # Explicitly print the ggplot object
}

```
The feedback of R tells us there are variables that are not numeric in the numeric columns. This indicates the presence of NAs. We should investigate this further

```{r}
 #Function to check for NA, empty strings, and NULL in all columns
check_na_null_empty <- function(distinct_data) {
  results <- data.frame(
    Column = colnames(distinct_data),
    NACount = sapply(distinct_data, function(x) sum(is.na(x))),  # Count NA values
    EmptyCount = sapply(distinct_data, function(x) sum(x == "")),  # Count empty strings
    NULLDetected = sapply(distinct_data, function(x) any(is.null(x))),  # Check for NULL
    stringsAsFactors = FALSE
  )
  
  print(results)
}

# Usage example
check_na_null_empty(distinct_data)

```
We should also check if there are outliers 
```{r}
for (col in colnames(numeric_vars)) {
  print(ggplot(distinct_data, aes_string(x = col)) + 
          geom_boxplot(fill = "coral", alpha = 0.7) +
          ggtitle(paste("Boxplot for", col)) +
          theme_minimal() +
          labs(x = col, y = "Value"))
}
```
```{r}
cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)
```
```{r}

# Heatmap of correlations
library(corrplot)
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)


```
Visualize distributions for categorical variables
```{r}
categorical_vars <- distinct_data %>% select(where(is.factor))
for (col in colnames(categorical_vars)) {
  plot_cat <- ggplot(distinct_data , aes_string(x = col)) + 
    geom_bar(fill = "salmon", alpha = 0.7) +
    ggtitle(paste("Distribution of", col)) +
    theme_minimal() +
    labs(x = col, y = "Count")
  
  print(plot_cat)
}
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






HYPOTHESIS TESTING

HYPOTHESE 2  Is there a difference in CD4 t cell percentage across BMI categories, in other words, do BMI categories influence the percentage of Cd4 t cells present in vaccinated individuals.


hypothesis 2 : Do people with a healthy weight have higher CD4 t cell count than people that are overweight?
first we're going to make a dataframe that contains only the columns about cd4 t cell count and bmi

```{r}
# Select columns from each dataframe

cd4_data <- metadata_flu %>% filter(name_formatted == "CD4_pos_T_cells" )



cd4_df <- cd4_data %>% select(donor_id, name_formatted, data)
bmi_df<- distinct_data %>% select(donor_id, bmi)

# Merge the dataframes
cd4_bmi_df <- left_join(cd4_df, bmi_df, by = "donor_id")

# View the merged dataframe
head(cd4_bmi_df)
summary (cd4_bmi_df)
```

some don't have data for bmi, 
```{r}
mean(is.na(cd4_bmi_df$bmi))
```

```{r}
install.packages("naniar")  # If not already installed
library(naniar)

# Visualize missing values
vis_miss(cd4_bmi_df) 
```



```{r}
cd4_bmi_cleaned <- cd4_bmi_df[!is.na(cd4_bmi_df$bmi), ]
summary(cd4_bmi_cleaned)
```
First we'll look at the range of the data. The maximul value for BMI is 46.89 and the minimum value is 13.12, both are plausible for BMI. the 'data' column, which contains the value for the percentage of parent cell population that is CD4 T cells, should be between 0 and 100, as its expressed in %. This is the case, so we conclude that neither column has outliers that are suspicious.


```{r}
cd4_bmi_cleaned%>%
  ggplot(aes(x= bmi))+
  geom_histogram(binwidth = 0.5, fill ='steelblue', color="black")+
  labs(title = "Histogram of BMI", x = "BMI", y="Frequency")+
  theme_minimal()
```

```{r}
cd4_bmi_cleaned%>%
  ggplot(aes(x= data))+
  geom_histogram(binwidth = 0.5, fill ='steelblue', color="black")+
  labs(title = "Histogram of CD4+ T cell percentage of parent population in the patient", x = "CD4+ T cell percentage of parent population", y="Frequency")+
  theme_minimal()
```
We're going to divide the BMI numbers in groups according to the universal standard : underweight (BMI>18.5), healthy(18.5=<BMI<25), overweight(25=<BMI<30) and obese (BMI>=30)

```{r}
cd4_bmicat <- cd4_bmi_cleaned %>%
  mutate(BMI_category = case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 & bmi < 25 ~ "healthy",
    bmi >= 25 & bmi < 30 ~ "overweight",
    bmi >= 30 ~ "obese"
  ))


cd4_bmicat$BMI_category<- as.factor(cd4_bmicat$BMI_category)
head(cd4_bmicat)
summary(cd4_bmicat)

```

-   Univariate data (BMI categories): four groups groups,independent groups (unpaired data)
-   Quantitative data (percentage CD4 tcells)
    - Normally distributed after filtering
    ==> **One-way ANOVA**

$H_0:$ There is no difference in percentage CD4 tcells between the four BMI categories

$H_A:$ There is a difference in percentage CD4 tcells between the four BMI categories

We have univariate data and quantitative data so we're going to do an ANOVA test. This is only allowed when the data is normally distributed and the variances are approximately equal across the BMI categories. These conditions will be tested first.

Normality testing

```{r}
shapiro.test(cd4_bmicat$data[cd4_bmicat$BMI_category == "underweight"])
shapiro.test(cd4_bmicat$data[cd4_bmicat$BMI_category == "healthy"])
shapiro.test(cd4_bmicat$data[cd4_bmicat$BMI_category == "overweight"])
shapiro.test(cd4_bmicat$data[cd4_bmicat$BMI_category == "obese"])

```
p-value > 0.05: The data is approximately normal for that category.
p-value ≤ 0.05: The data significantly deviates from normality.

From the Shapiro-Wilk test we can conclude that the data for every category is approximately normal for each category as the p value is always above 0.05.
The distribution of the data will be visualised below.


```{r}
ggqqplot((cd4_bmicat%>%filter(BMI_category=="underweight"))$data)+
  ggtitle("CD4 T cells percentage of parent population in underweight people")
ggqqplot((cd4_bmicat%>%filter(BMI_category=="healthy"))$data)+
  ggtitle("CD4 T cells percentage of parent population in healthy people")
ggqqplot((cd4_bmicat%>%filter(BMI_category=="overweight"))$data)+
  ggtitle("CD4 T cells percentage of parent population in overweight people")
ggqqplot((cd4_bmicat%>%filter(BMI_category=="obese"))$data)+
  ggtitle("CD4 T cells percentage of parent population in obese people")
```


```{r}
ggplot(cd4_bmicat, aes(sample = data)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~BMI_category, scales = "free") +
  labs(title = "Q-Q Plot for Normality Check",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```

```{r}
# Histogram for each BMI category
ggplot(cd4_bmicat, aes(x = data)) +
  geom_histogram(bins = 10, fill = "coral", color = "black") +
  facet_wrap(~BMI_category, scales = "free") +
  labs(title = "Histogram of CD4 Percentage by BMI Category",
       x = "CD4 Percentage",
       y = "Frequency")
```
Variance testing

```{r}
# 1) Center and spread of MYC expression
CD4_per_cat<-cd4_bmicat%>%
  group_by(BMI_category)%>%
  summarise(MeanData = mean(data),
            sdData = sd(data))
CD4_per_cat
```

```{r}
library(car)

# Perform Levene's test
leveneTest(data ~ BMI_category, data = cd4_bmicat)
```

Levene’s p-value > 0.05: The variances are approximately equal (homogeneous).
Levene’s p-value ≤ 0.05: Variances differ significantly (heterogeneous).

The p value is higher than 0.05 (0.30), therefore we conclude that the variance is approximately equal across all categories.

The two conditions are fullfilled, so we can perform the ANOVA test


2) Perform the selected test. Report your findings in an appropriate way.

```{r EX5_2}

Anova_cd4_bmi<-aov(data ~ BMI_category,
                   data = cd4_bmicat)

Anova_cd4_bmi

summary(Anova_cd4_bmi)

# Calculate the replicates in each group
cd4_bmicat%>%
  group_by(BMI_category)%>%
  summarise(Total = n())
```

**CONCLUSION:**

A **one-way ANOVA test** with significance level $\alpha$ of **0.05** indicated that the difference in **percentage of CD4 t cells** between the BMI categories is significant. A **p-value of 0.00723** was observed, indicating that, based on our data, the null hypothesis can be rejected.

Make a visualisation:

```{r}
cd4_bmicat %>%
  ggplot(aes(x=BMI_category, y=data, fill=BMI_category))+
  geom_boxplot()+
  theme_classic()+
  theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(), legend.position = "none")
```

The ANOVA showed a significant result (p-value < 0.05), so we'll conduct a post-hoc test to identify which specific BMI categories differ. The tukey test makes pairwise comparisons and automatically corrects for multiple testing. The p value that is given can be interpretted towards the normal treshold of 0.05.

```{r}
tukey_result <- TukeyHSD(Anova_cd4_bmi)
print(tukey_result)
```
Based on the post-hoc Tukey test we can conclude that there is only a significant difference between the categories underweight and healthy (p value = 0.0034) with a confidence interval (95%) of [1.066,7.333].

