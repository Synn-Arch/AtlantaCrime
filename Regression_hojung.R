library(tidyverse)
library(GGally)
library(sjPlot)
library(car)
library(leaps)
library(stargazer)

getwd()
setwd("C:/Users/Hojung Yu/Documents/GitHub/AtlantaCrime/")

df <- read.csv("data_final.csv")

head(df)
nrow(df)
ncol(df)
summary(df)

colnames(df)

ggpairs(df[,reg_variables])

#Violent crime model
reg_variables = c("pop_den", "black_ratio", "median_incomeE", "Commercial", 
              "HighdensityResidential", "Industrial","Institutional",
              "LowdensityResidential", "ResidentialCommercial",
              "min_station_dist", "violent_ratio", "nonviolent_ratio")

# Normalization or Standardize
df <- df %>%
  mutate(violent_ratio_square = (violent_ratio*1000),
         nonviolent_ratio_square = (nonviolent_ratio*1000))

df$pop_den <- df$pop_den * 100
df$black_ratio <- df$black_ratio * 100
df$less_than_hs_ratio <- df$less_than_hs_ratio * 100

cor_matrix <- cor(df[reg_variables])
round(cor_matrix,2)

# bestsubsets <- regsubsets(violent_ratio_square ~ pop_den + black_ratio + median_incomeE +
#                             LowdensityResidential, data = df, nbest = 1)
# subsets(bestsubsets, statistic = "adjr2")

model_1 <- lm(violent_ratio_square ~ pop_den + median_incomeE +
              LowdensityResidential, data = df)
summary(model_1)

##Stepwise
null.model <- lm(violent_ratio_square ~ 1, data = df)
full.model <- lm(violent_ratio_square ~ pop_den + median_incomeE + black_ratio +
                   LowdensityResidential + less_than_hs_ratio, data = df)

step.model.for <- step(null.model,
                       scope = formula(full.model),
                       direction = "forward",
                       trace = 0)

step.model.back <- step(full.model,
                        direction = "backward",
                        trace = 0)

step.model.both <- step(null.model,
                        scope = formula(full.model),
                        direction = "both",
                        trace = 0)

stargazer(step.model.for, step.model.back, step.model.both,
          type = "text",
          add.lines = list(c("AIC", round(AIC(step.model.for),1), round(AIC(step.model.back),1), round(AIC(step.model.both),1))),
          column.labels = c("Forward", "Backward", "Both"))

subset.model <- regsubsets(formula(full.model),
                           data = df,
                           nvmax = 5,
                           method = "exhaustive")

##Subset models
reg.summary <- summary(subset.model)

par(mfrow = c(2,2)) # This code specifies that, instead of showing only one graph in the plots window, you want 4 (2 x 2) graphs.

plot(reg.summary$rsq, # This code plots the number of variables on x-axis and r-squared values on y-axis
     xlab = "Number of variables", 
     ylab = "R-Squared", 
     type = "l") # type = "l" means that you want to display the graph with a line, not points.

plot2 <- plot(reg.summary$adjr2, # Adjusted r-squared values on y-axis
              xlab = "Number of variables", 
              ylab = "Adjusted R-Sqaured", 
              type = "l")

points(which.max(reg.summary$adjr2), # This code inserts a red dot on the line graph. The red dot denotes the point where the adjusted R-squared is at its highest.
       reg.summary$adjr2[which.max(reg.summary$adjr2)], 
       col="red",
       cex=2,
       pch=20)

plot(reg.summary$rss, # sum of squares residual values on y-axis
     xlab = "Number of variables", 
     ylab = "RSS", 
     type = "l")

plot(reg.summary$bic, # BIC values on y-axis. BIC is very similar to AIC except that it puts more penalty to the number of variables included.
     xlab = "Number of variables", 
     ylab = "BIC", 
     type = "l")

points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col="red",cex=2,pch=20)

######################optimal regression in violent crime
subsetted.model_v1 <- lm(violent_ratio_square ~ pop_den + black_ratio +
                           LowdensityResidential + less_than_hs_ratio, data = df)
summary(subsetted.model_v1)

#Violent crime model
model_2 <- lm(nonviolent_ratio_square ~ pop_den + median_incomeE + LowdensityResidential, data = df)
summary(model_2)
plot(fitted(model_2), resid(model_2), abline(h=0), pch =19)

model_2_1 <- lm(nonviolent_ratio_square ~ pop_den + median_incomeE + Commercial, data = df)
summary(model_2_1)
plot(fitted(model_2_1), resid(model_2_1), abline(h=0), pch = 19)

AIC(model_2, model_2_1)

##Best subset
full.model_n <- lm(nonviolent_ratio_square ~ pop_den + median_incomeE + black_ratio +
                     Commercial + less_than_hs_ratio, data = df)

subset.model_n <- regsubsets(formula(full.model_n),
                             data = df,
                             nvmax = 6,
                             method = "exhaustive")

reg.summary_n <- summary(subset.model_n)
reg.summary_n

par(mfrow = c(2,2)) # This code specifies that, instead of showing only one graph in the plots window, you want 4 (2 x 2) graphs.

plot(reg.summary_n$rsq, # This code plots the number of variables on x-axis and r-squared values on y-axis
     xlab = "Number of variables", 
     ylab = "R-Squared", 
     type = "l") # type = "l" means that you want to display the graph with a line, not points.

plot2 <- plot(reg.summary_n$adjr2, # Adjusted r-squared values on y-axis
              xlab = "Number of variables", 
              ylab = "Adjusted R-Sqaured", 
              type = "l")

points(which.max(reg.summary_n$adjr2), # This code inserts a red dot on the line graph. The red dot denotes the point where the adjusted R-squared is at its highest.
       reg.summary_n$adjr2[which.max(reg.summary_n$adjr2)], 
       col="red",
       cex=2,
       pch=20)

plot(reg.summary_n$rss, # sum of squares residual values on y-axis
     xlab = "Number of variables", 
     ylab = "RSS", 
     type = "l")

plot(reg.summary_n$bic, # BIC values on y-axis. BIC is very similar to AIC except that it puts more penalty to the number of variables included.
     xlab = "Number of variables", 
     ylab = "BIC", 
     type = "l")

points(which.min(reg.summary_n$bic), reg.summary_n$bic[which.min(reg.summary_n$bic)], col="red",cex=2,pch=20)

######################optimal regression in nonviolent model
subsetted.model_v1_n <- lm(nonviolent_ratio_square ~ pop_den + LowdensityResidential + Commercial + less_than_hs_ratio, data = df)
summary(subsetted.model_v1_n)
