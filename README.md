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
## HYPOTHESIS TESTING

### Research question 1: 
hypothesis 1: difference in vaccine response between vaccine type

For this research question, we want to see if there is a difference in the abundance of a high vaccine response for the different vaccines that are given. This is a univariate test since we will only be looking at the variate vaccine type. Both the vaccine type and vaccine response are unranked categorical variables. 

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
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y = "relative response proportion", fill = "vaccine response") +
  theme_minimal()
```
Now we can see that for vaccine 3, 5 and 6, no vaccine response data is available. Therefore, we can only look at the difference in high response abundancy between vaccine 1, 2 and 4. But also for vaccine 1 and 4 we notice that there are patients for whom no vaccine response data was available. This missing data could be due to various reasons, like as or patients not completing the follow-up. Given these limitations, we will exclude these NULLs from our analysis to ensure accuracy.

```{r}
noNA_distinct_influenza <- distinct_data %>% filter(vaccine_response != "NULL")

ggplot(noNA_distinct_influenza, aes(x = factor (vaccine,levels = c(1, 2, 4, NA)), fill = vaccine_response)) +
  geom_bar(position = "stack") + # Use position = "dodge" for side-by-side bars
  labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y = "response count", fill = "vaccine response") +
  theme_minimal()

noNA_distinct_influenza %>%
ggplot(aes(x=factor (vaccine,levels = c(1,2,4,NA)), fill = vaccine_response)) +
geom_bar(position = "fill") +
labs(title = "High or Low vaccine response compaired to the different vaccine types", x = "vaccine type", y = "relative response proportion", fill = "vaccine response") +
  theme_minimal()
```
Now, we can better evaluate a possible difference in high vaccine responses between vaccines 1, 2 and 4. 
The amount of patients is very noticeably smaller compared to the vaccine 1 and vaccine 4 group. There where only 19 patients who received vaccine 2, while vaccine 1 and 4 got allocated to respectively 88 and 186. This is why we also discarded vaccine 2, the small size of this group could otherwise reduce the statistical power making it harder to detect true differences. While there is still a difference in patients between vaccine 1 and 4, we can use these two group and analyse their abundance of high vaccine responders using the Fisher test which is designed to handle small sample sizes and unbalanced groups and therefore doesn't rely on large sample sizes or equal group sizes. 

To bring this in context with the study, we will be comparing the proportion of high responders between patients who received the vaccine Flumist (1), given intranasally, and Fluzone (4), given intramuscularly.

**Fisher test**
```{r}
contingency_table <- table(distinct_data$vaccine, distinct_data$vaccine_response)

contingency_table <- contingency_table[, colnames(contingency_table) != "NULL"]
contingency_table <- contingency_table[rownames(contingency_table) != "6", ]
contingency_table <- contingency_table[rownames(contingency_table) != "NULL", ]
contingency_table <- contingency_table[rownames(contingency_table) != "5", ]
contingency_table <- contingency_table[rownames(contingency_table) != "3", ]
contingency_table <- contingency_table[rownames(contingency_table) != "2", ]

contingency_table <- contingency_table[, c("1", "0")]

print(contingency_table)

fisher.test(contingency_table)

```
After creating a contingency table for vaccine type 1 and 4 and high vaccine response (1) and low vaccine response(0), we performed the Fisher's exact test for count data. This gave us an odds ratio of 1.67 (95CI: {0.91 ; 3.14}) with a P-value of 0.09. Here an OR of 1.67 means that the odds of having a high vaccine response are 1.67 times higher in group 4 compared to group 1. So while there is a difference in the amount of high vaccine responders between vaccine 1 and vaccine 4, this difference was not significant (p > 0.05). We are not able to discard the null hypothesis.

```{r}
Response_to_vaccine <- noNA_distinct_influenza%>%
  filter(vaccine == 1 | vaccine == 4)
Response_to_vaccine <- Response_to_vaccine%>%
  select(vaccine, vaccine_response)
Response_to_vaccine$vaccine_response <- factor(Response_to_vaccine$vaccine_response, levels = c(0, 1), labels = c("Low", "High"))
Response_to_vaccine$vaccine <- factor(Response_to_vaccine$vaccine, levels = c(1, 4), labels = c("Flumist", "Fluzone"))

ggplot(Response_to_vaccine, aes(x = factor(vaccine), fill = factor(vaccine_response))) +
  geom_bar(position = "fill") +
  labs(title = "Comparison of Vaccine Response to Vaccine Type",
       x = "Vaccine Type",
       y = "Response proportion",
       fill = "Vaccine Response") +
  theme_minimal()
```

### Research question 2
hypothesis 2 : Do BMI category and gender have an effect on the C T cell count?

```{r}
# Select columns from each dataframe

cd4_data <- metadata_flu %>% filter(name_formatted == "CD4_pos_T_cells" )


#All measurements for CD4 T cells are in the same unit : % of parent cell population, therefore we won't include this column in our new dataframe.
cd4_df <- cd4_data %>% select(donor_id, name_formatted, data)
bmi_gender<- distinct_data %>% select(donor_id, bmi, gender)

# Merge the dataframes
cd4_bmi_gender <- left_join(cd4_df, bmi_gender, by = "donor_id")

# View the merged dataframe
head(cd4_bmi_gender)
summary (cd4_bmi_gender)
```

Some don't have data for bmi.
```{r}
mean(is.na(cd4_bmi_gender$bmi))
```
24.8% of the individuals with a measurement for CD4+ T cell have not reported their BMI. We suspect this is mostly due to reporting bias as some people don't know their length/weight or don't want to share it. 

```{r}
install.packages("naniar")  # If not already installed
library(naniar)

# Visualize missing values
vis_miss(cd4_bmi_gender) 
```

We're going to take out all the individuals that miss the BMI value, as this is not useful for our hypothesis.

```{r}
cd4_bmi_cleaned <- cd4_bmi_gender[!is.na(cd4_bmi_gender$bmi), ]
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
    bmi >= 25 & bmi <30 ~ "overweight",
    bmi>=30 ~ "obese"
  ))


cd4_bmicat$BMI_category<- as.factor(cd4_bmicat$BMI_category)
head(cd4_bmicat)
summary(cd4_bmicat)

```

In this hypothesis we are going to look at the effect of gender and BMI class on CD4 T cell percentage.
In total we have 329 individuals in our dataframe. All observations are obtained from different individuals, therefore we can conclude that the groups are independent. 

Below, we specified the variables.
CD4: Dependent variable (continuous).
BMI: Independent variable (categorical, ordinal).
Gender: Independent variable (categorical, nominal).

First, we fit a linear model to this

```{r}
model <- lm(data ~ bmi * gender, data = cd4_bmicat)
summary (model)
```
```{r}
cd4_bmicat$BMI_category <- relevel(cd4_bmicat$BMI_category, ref = "healthy")
model <- lm(data ~ BMI_category * gender, data = cd4_bmicat)
summary(model)
```

To test if the the model is significant as a whole, we'll use anove to compare models 



The data needs to comply to certain conditions :
- The data within each group is approximately normally distributed (can check with Q-Q plots or Shapiro-Wilk test).
- Homogeneity of variances (can test using Levene’s test).

Normality testing of the continuous variable

```{r}
install.packages("ggpubr")
library(ggpubr)
# plot for all data
ggqqplot(cd4_bmicat$data)+
  ggtitle("CD4 T cells percentage of parent population")
```
```{r}
shapiro.test(cd4_bmicat$data)
```
p-value > 0.05: The data is approximately normal for that category.
p-value ≤ 0.05: The data significantly deviates from normality.

```{r}
ggqqplot(cd4_bmicat, "data", ggtheme = theme_minimal()) +
  facet_grid(BMI_category ~ gender) +
  labs(title = "Q-Q Plots of CD4 Counts by BMI Class and Gender")
```


```{r}
# 1) Center and spread of CD4 T cell count
CD4_per_cat<-cd4_bmicat%>%
  group_by(BMI_category)%>%
  summarise(MeanData = mean(data),
            sdData = sd(data))
CD4_per_cat
```
```{r}
bartlett.test(data ~ BMI_category, data = cd4_bmicat)
```

```{r}
bartlett.test(data ~ gender, data = cd4_bmicat)
```
Interpreting the Results
The Bartlett test produces:

Statistic: Bartlett’s K-squared.

p-value: Used to determine if variances are equal.

Null Hypothesis (H₀): Variances across groups are equal.

Alternative Hypothesis (H₁): At least one group has a different variance.

Decision Rule:

If p-value < 0.05, reject H₀ (variances are not equal).
If p-value ≥ 0.05, fail to reject H₀ (variances are equal).


The p value is higher than 0.05 (0.21), therefore we conclude that the variance is approximately equal across all categories.

=> the conditions of normality and approximately equal variance are met => The anova test can be applied
```{r}
anova(model)
```
```{r}
plot(model, which = 1, main = "Residuals vs. Fitted")
```
```{r}
plot(model, which = 2, main = "Normal Q-Q Plot")
```
```{r}
ggplot(cd4_bmicat, aes(x = BMI_category, y = data, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "CD4 Cell Count by BMI Class and Gender",
    x = "BMI Class",
    y = "CD4 Cell Count"
  ) +
  scale_fill_manual(values = c("Male" = "royalblue", "Female" = "pink")) +
  theme_minimal()
```
```{r}
ggplot(cd4_bmicat, aes(x = BMI_category, y = data, fill = gender)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge()) +
  labs(
    title = "Mean CD4 Cell Count by BMI Class and Gender",
    x = "BMI Class",
    y = "Mean CD4 Cell Count"
  ) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "pink")) +
  theme_minimal()
```
```{r}
interaction.plot(
  x.factor = cd4_bmicat$BMI_category,
  trace.factor = cd4_bmicat$gender,
  response = cd4_bmicat$data,
  fun = mean,
  col = c("blue", "pink"),
  lty = 1,
  type = "b",
  legend = TRUE,
  xlab = "BMI Class",
  ylab = "Mean CD4 Count",
  main = "Interaction Plot: BMI Class and Gender on CD4 Count"
)
```
```{r}
# Scatter plot with jitter
ggplot(cd4_bmicat, aes(x = BMI_category, y = data, color = gender)) +
  geom_jitter(width = 0.2, size = 3) +
  labs(
    title = "CD4 Cell Count by BMI Class and Gender",
    x = "BMI Class",
    y = "CD4 Cell Count"
  ) +
  scale_color_manual(values = c("Male" = "blue", "Female" = "pink")) +
  theme_minimal()
```

Choosing the Visualization
Box Plot: Useful for understanding the distribution of CD4 counts.
Bar Plot: Best for comparing mean CD4 counts across groups.
Interaction Plot: Highlights potential interactions between BMI class and Gender.
Scatter Plot: Displays individual observations for detailed inspection.
You can pick a visualization based on the story you want to tell or use multiple plots to explore different aspects of the data.


