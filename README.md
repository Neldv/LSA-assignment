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
##HYPOTHESIS TESTING

###Research question 1: 
hypothesis 1: difference in vaccine response between vaccine type

For this research question, we want to see if there is a difference in the abundance of a high vaccine response for the different vaccines that are given. Both the vaccine type and vaccine response are categorical variables. 

**visualisation**
```{r}
ggplot(distinct_data, aes(x = factor(vaccine, levels = c(1, 2, 3, 4, 5, 6, NA)), fill = vaccine_response)) +
  geom_bar(position = "stack") + # Use position = "dodge" for side-by-side bars
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y="response count", fill = "vaccine response") +
  theme_minimal()
```
We see that there is a big difference in the amount of patients allocated to the different vaccines. Therefore we will look at the relative response to visualize the differences in low and high vaccine response between the different vaccines. 

```{r}
distinct_data %>%
  ggplot(aes(x=factor(vaccine, levels = c(1, 2, 3, 4, 5, 6, NA)), fill = vaccine_response)) +
  geom_bar(position = "fill") +
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y = "relative response count", fill = "vaccine response") +
  theme_minimal()
```
Now we can see that for vaccine 3, 5 and 6, no vaccine response data is available. Therefore, we can only look at the difference in high response abundancy between vaccine 1, 2 and 4. But also for vaccine 1 and 4 we notice that there are patients for whom no vaccine response data was available. 

```{r}
noNA_distinct_influenza <- distinct_data %>% filter(vaccine_response != "NULL")

ggplot(noNA_distinct_influenza, aes(x = factor (vaccine,levels = c(1, 2, 4, NA)), fill = vaccine_response)) +
  geom_bar(position = "stack") + # Use position = "dodge" for side-by-side bars
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y = "response count", fill = "vaccine response") +
  theme_minimal()

noNA_distinct_influenza %>%
ggplot(aes(x=factor (vaccine,levels = c(1,2,4,NA)), fill = vaccine_response)) +
geom_bar(position = "fill") +
labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y = "relative response count", fill = "vaccine response") +
  theme_minimal()
```
**Fisher test**
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



###Research question 2
hypothesis 2 : Do people with a healthy weight have higher CD4 t cell count than people that are overweight?
first we're going to make a dataframe that contains only the columns about cd4 t cell count and bmi.

```{r}
# Select columns from each dataframe

cd4_data <- metadata_flu %>% filter(name_formatted == "CD4_pos_T_cells" )


#All measurements for CD4 T cells are in the same unit : % of parent cell population, therefore we won't include this column in our new dataframe.
cd4_df <- cd4_data %>% select(donor_id, name_formatted, data)
bmi_df<- distinct_data %>% select(donor_id, bmi)

# Merge the dataframes
cd4_bmi_df <- left_join(cd4_df, bmi_df, by = "donor_id")

# View the merged dataframe
head(cd4_bmi_df)
summary (cd4_bmi_df)
```

some don't have data for bmi.
```{r}
mean(is.na(cd4_bmi_df$bmi))
```
24.8% of the individuals with a measurement for CD4+ T cell have not reported their BMI. We suspect this is mostely due to reporting bias as some people don't know their length/weight or don't want to share it. 

```{r}
install.packages("naniar")  # If not already installed
library(naniar)

# Visualize missing values
vis_miss(cd4_bmi_df) 
```

We're going to take out all the individuals that miss the BMI value, as this is not useful for our hypothesis.

```{r}
cd4_bmi_cleaned <- cd4_bmi_df[!is.na(cd4_bmi_df$bmi), ]
summary(cd4_bmi_cleaned)
```
First we'll look at the range of the data. The maximum value for BMI is 46.89 and the minimum value is 13.12, both are plausible for BMI. the 'data' column, which contains the value for the percentage of parent cell population that is CD4 T cells, should be between 0 and 100, as its expressed in %. This is the case, so we conclude that neither column has outliers that are suspicious. Below, both distributions are visualised in a histogram.


```{r}
cd4_bmi_cleaned%>%
  ggplot(aes(x= bmi))+
  geom_histogram(binwidth = 0.5, fill ='darkseagreen', color="black")+
  labs(title = "Histogram of BMI", x = "BMI", y="Frequency")+
  theme_minimal()
```

```{r}
cd4_bmi_cleaned%>%
  ggplot(aes(x= data))+
  geom_histogram(binwidth = 0.5, fill ='darkseagreen4', color="black")+
  labs(title = "Histogram of CD4+ T cell percentage of parent population in the patient", x = "CD4+ T cell percentage of parent population", y="Frequency")+
  theme_minimal()
```
We're going to split the BMI numbers in groups according to the universal standard : underweight (BMI>18.5), healthy(18.5=<BMI<25), overweight(25<= BMI)

```{r}
cd4_bmicat <- cd4_bmi_cleaned %>%
  mutate(BMI_category = case_when(
    bmi < 18.5 ~ "underweight",
    bmi >= 18.5 & bmi < 25 ~ "healthy",
    bmi >= 25 ~ "overweight"
  ))


cd4_bmicat$BMI_category<- as.factor(cd4_bmicat$BMI_category)
head(cd4_bmicat)
summary(cd4_bmicat)

```

In this hypothesis we are going to compare individuals with a healthy weight to individuals that are overweight. Individuals that are underweight are not part of our hypothesis so we will take them out of the dataframe.
```{r}
cd4_hvso <- cd4_bmicat %>%
  filter(BMI_category != "underweight")

head(cd4_hvso)
summary(cd4_hvso)
```

This leaves us with 176 individuals in the category "healthy weight" and 100 individuals in the category "overweight"
All observations are obtained from different individuals, therefore we can conclude that the groups are independent. We'll test 2 groups : Healthy weight and overweight. The data for CD4 t cell percentage is quantitative. 

=> two sample t-test is the adequate statistical test`

The data needs to comply to certain conditions :
- the two groups need to be independent (no overlap in participants). This was stated before?
- The data within each group is approximately normally distributed (can check with Q-Q plots or Shapiro-Wilk test).
- Homogeneity of variances (can test using Levene’s test).

First, the normality will be tested


```{r}
shapiro.test(cd4_hvso$data[cd4_hvso$BMI_category == "healthy"])
shapiro.test(cd4_hvso$data[cd4_hvso$BMI_category == "overweight"])

```
p-value > 0.05: The data is approximately normal for that category.
p-value ≤ 0.05: The data significantly deviates from normality.

From the Shapiro-Wilk test we can conclude that the data for both categories is approximately normal for each category as the p value is always above 0.05.
The distribution of the data will be visualised below.



```{r}
install.packages("ggpubr")
library(ggpubr)
# plot for all data
ggqqplot(cd4_hvso$data)+
  ggtitle("CD4 T cells percentage of parent population")
# plot for data of healthy individuals
ggqqplot((cd4_hvso%>%filter(BMI_category=="healthy"))$data)+
  ggtitle("CD4 T cells percentage of parent population in healthy people")
# plot for data of overweight individuals
ggqqplot((cd4_hvso%>%filter(BMI_category=="overweight"))$data)+
  ggtitle("CD4 T cells percentage of parent population in overweight people")
```
```{r}
ggplot(cd4_hvso, aes(sample = data)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(~BMI_category, scales = "free") +
  labs(title = "Q-Q Plot for Normality Check",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")
```
```{r}
# Histogram for each BMI category
ggplot(cd4_hvso, aes(x = data)) +
  geom_histogram(bins = 10, fill = "lightsalmon", color = "black") +
  facet_wrap(~BMI_category, scales = "free") +
  labs(title = "Histogram of CD4 Percentage by BMI Category",
       x = "CD4 Percentage",
       y = "Frequency")
```
Secondly, the variance will be compared.


```{r}
# 1) Center and spread of CD4 T cell count
CD4_per_cat<-cd4_hvso%>%
  group_by(BMI_category)%>%
  summarise(MeanData = mean(data),
            sdData = sd(data))
CD4_per_cat
```

```{r}
library(car)

# Perform Levene's test
leveneTest(data ~ BMI_category, data = cd4_hvso)
```

Levene’s p-value > 0.05: The variances are approximately equal (homogeneous).
Levene’s p-value ≤ 0.05: Variances differ significantly (heterogeneous).

The p value is higher than 0.05 (0.21), therefore we conclude that the variance is approximately equal across all categories. Visualisation below.





```{r}
ggplot(CD4_per_cat, aes(x = BMI_category, y = MeanData, fill = BMI_category))+
  geom_col()+
  geom_errorbar(aes(ymin = MeanData - sdData,
                    ymax = MeanData + sdData),
                col = "grey30",
                width = 0.2)+
  theme_classic()+
  xlab("BMI category")+
  ylab("Average percentage of CD4 t cells")+
  scale_fill_brewer(palette= "Pastel2")+
  theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(), legend.position = "none")
```

```{r}
# Calculate IQR
IQR(cd4_bmi_cleaned$data)
# Visualize in boxplot
ggplot(cd4_bmi_cleaned, mapping=aes(y = data,
                            x = 1))+
  geom_boxplot(width = 0.2)+
  # Add data points
  geom_jitter(position=position_jitter(0.1),cex=1.2,
              col = "palevioletred1")+
  theme_classic()+
  ylab("percentage of CD4 t cells")+
  scale_fill_brewer(palette= "Accent")+
  # Remove unnecessary elements
  theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank())
```
The datas complies with all conditions, we can proceed with the two sample t test.


```{r}
T_cd4_bmi <- t.test(data ~ BMI_category,
     data = cd4_hvso)
T_cd4_bmi

# Extract the exact p-value
T_cd4_bmi$p.value

# Extract the exact t statistic
T_cd4_bmi$statistic

# Calculate the exact difference, only the estimates of the means
# of both groups are stored in the outcome of the t test
T_cd4_bmi$estimate
T_cd4_bmi$estimate[1]-T_cd4_bmi$estimate[2]

# Extract the confidence interval
T_cd4_bmi$conf.int
```
The two-sample t test(**two-tailed**, **unpaired**) with significance level of 0.05 indicated that the difference in percentage of CD4 t cells does not significantly differ between people that have a healthy weight(n=176) and people that are overweight(n=100).A difference of **-0.967** (95% CI: **-2.85 to 0.92**) was observed when comparing people with a healthy weight (mean=55.18) to overweight people (mean=56.15). The p value is 0.31 which means that the null hypothesis cannot be rejected.

```{r}
ggplot(cd4_hvso, aes(x=BMI_category, y= data, fill=BMI_category))+
  geom_boxplot()+
  theme_minimal()+
  scale_fill_brewer(palette= "Pastel2")+
  ylab("Percentage of CD4 T cells")+
  theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(), legend.position = "none")
```
