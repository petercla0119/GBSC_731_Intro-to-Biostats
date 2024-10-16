# Exam 1 ----------------------------------------------------------------------------------------------------------------

# Install required package
library(ggplot2)
library(pastecs)
library(car)
library(gridExtra)
library(tidyverse)
library(lmtest)
source("/Users/claireps/GBSC_731_Intro-to-Biostats/Module_3_CLT-and-Normality/functions.R")

## Q5 ----------------------------------------------------------------------------------------------------------------
setwd("/Users/claireps/GBSC_731_Intro-to-Biostats/Exams/Exam 1")
df_q5 <- read.delim("ExamIQ5.csv", sep = ",")

## Check Assumptions ---------------------------------------------------------------------------------------------------

# Create ggplot objects for histograms and boxplots
hist_DEXA <- ggplot(data = df_q5, mapping = aes(x = DEXA_mass)) + 
  geom_histogram(mapping = aes(y = ..density..), bins = 10, fill = 'gray', color = 'black') +
  stat_function(fun = dnorm, args = list(mean = mean(df_q5$DEXA_mass, na.rm = TRUE), sd = sd(df_q5$DEXA_mass, na.rm = TRUE)), color = 'red') + 
  ggtitle("Histogram of DEXA Mass")

hist_Actual <- ggplot(data = df_q5, mapping = aes(x = Actual_weight)) + 
  geom_histogram(mapping = aes(y = ..density..), bins = 10, fill = 'gray', color = 'black') +
  stat_function(fun = dnorm, args = list(mean = mean(df_q5$Actual_weight, na.rm = TRUE), sd = sd(df_q5$Actual_weight, na.rm = TRUE)), color = 'red') +
  ggtitle("Histogram of Actual Weight")

boxplot_DEXA <- ggplot(data = df_q5, mapping = aes(x = DEXA_mass)) + 
  geom_boxplot() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggtitle("Boxplot of DEXA Mass")

boxplot_Actual <- ggplot(data = df_q5, mapping = aes(x = Actual_weight)) + 
  geom_boxplot() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()) +
  ggtitle("Boxplot of Actual Weight")

# Create a grid layout for the ggplot histograms and boxplots
grid.arrange(hist_DEXA, hist_Actual, boxplot_DEXA, boxplot_Actual, nrow = 2, ncol = 2)

# Create the QQ plots using base R in the second row
par(mfrow = c(1, 2))  # Arrange the QQ plots side by side
qqnorm(df_q5$DEXA_mass, main = "QQ Plot of DEXA Mass", pch = 19)
qqline(df_q5$DEXA_mass)
qqnorm(df_q5$Actual_weight, main = "QQ Plot of Actual Weight", pch = 19)
qqline(df_q5$Actual_weight)

stat.desc.clean(dataset = df_q5, variable = DEXA_mass)
stat.desc.clean(dataset = df_q5, variable = Actual_weight)

stat.desc(df_q5$DEXA_mass, norm = TRUE)
stat.desc(df_q5$Actual_weight, norm = TRUE)

cor.test(df_q5$Actual_weight, df_q5$DEXA_mass, method = "kendall")

# TODO: 

# IDEA: 
# Variables have a linear relationship
### Scatterplot
ggplot(data = df_q5, aes())
plot(df_q5$DEXA_mass, df_q5$Actual_weight, main = "Scatterplot of DEXA Mass and Actual Weight", xlab = "DEXA Mass", ylab = "Actual Weight", col = "black", pch = 19)
abline(lm(df_q5$Actual_weight ~ df_q5$DEXA_mass))

# Formal tests for assumptions


# Homogeneity of variance
leveneTest(df_q5$Actual_weight, df_q5$DEXA_mass, center = mean)


shapiro.test(df_q5$DEXA_mass) # W = 0.92093, p-value = 0.4374
shapiro.test(df_q5$Actual_weight) # W = 0.95664, p-value = 0.7775


result_cor <- cor.test(df_q5$Actual_weight, df_q5$DEXA_mass, method = "pearson")









## Q6 ----------------------------------------------------------------------------------------------------------------
df_q6 <- read.delim("Exam1Q6.csv", sep=',')

### Assumptions ----

# Run model
agevsweightandoutdoors <- lm(Age ~ Weight + Time_Outdoors, data = df_q6)
summary(agevsweightandoutdoors)
#### Linearity ----
df_q6 %>% 
  pivot_longer(cols = c(Weight, Time_Outdoors), 
               names_to = "Metric", 
               values_to = "Value") %>% 
  ggplot(mapping = aes(x = Value, y = Age)) +
  geom_point() +
  geom_smooth(method='lm', se = FALSE) +
  facet_wrap(~ Metric, scales ="free")


#### Homoscedasticity of residuals ----
residuals <- df_q6 %>% 
  mutate(fitted = predict(agevsweightandoutdoors),
         resid = residuals(agevsweightandoutdoors))

leveneTest(Age ~ Weight*Time_Outdoors, data = df_q6)

# Homoscedasticity of residuals
ggplot(data = residuals, aes(x = fitted, y = resid)) +
  geom_point() + geom_hline(yintercept = 0)

#### Normality of residuals ----
##### Histogram ----
ggplot(data = residuals, mapping = aes(x = resid)) +
  geom_histogram(mapping = aes(y = ..density..), bins = 30, fill = 'gray', color ='black'
) + 
  stat_function(fun = dnorm, args = list(mean = mean(residuals$resid), sd = sd(residuals$resid)), color ='red')

##### QQ Plot ----
ggplot(data = residuals, mapping = aes(sample = resid)) + geom_qq() + geom_qq_line()

##### Boxplot ----
ggplot(data = residuals, mapping = aes(x = resid)) + geom_boxplot() + theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank())

stat.desc.clean(dataset = residuals, variable = resid)

#### Multicollinearity ----
vif(agevsweightandoutdoors)
