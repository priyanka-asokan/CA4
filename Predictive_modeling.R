#Dataset 1 constains the details about the production of different diary products such as cheese,butter and skimmed milk.
Diray_products_production_details <- read.csv("Production of Dairy Products (000 Tonnes) by Product and Month.csv")

#Dataset 2 constains the details about the Diary Industrial Turnover Index.
Diray_products_turnover_details<- read.csv("Diary Industrial Production and Turnover Index.csv")

#Merging the two datasets
Merge_data <- merge(Diray_products_production_details,Diray_products_turnover_details)
str(Merge_data)
head(Merge_data)


# Plotting Diary_Industrial_Turnover_Index (Response variable)
# and Diary product Butter production (predictor)to see relationship between them

plot(Merge_data$Butter,Merge_data$Diary_Industrial_Turnover_Index,
     xlab="Production of Butter",
     ylab="Diary Industry Turnover",
     main = "Scatter plot showing regression line for Diary Industry Turnover predicted 
     from Production of Diary product Butte")
abline(linearMod)


# Scatter plots to visualise the linear relationships between the 
# dependent (response) Diary_Industrial_Turnover_Index variable 
# and independent (predictor) Butter variable

scatter.smooth(x = Merge_data$Butter, 
               y = Merge_data$Diary_Industrial_Turnover_Index, 
               main = "Butter ~ Diary_Industrial_Turnover_Index",
               xlab = "Production of Butter ",
               ylab = "Diary Industrial Turnover")

# Box Plot for Production of Butter

par(mfrow = c(1, 2)) # To divide graph area into  tow columns
boxplot(Merge_data$Butter, main = "Production of Butter", 
        sub = paste("Outlier rows: ", boxplot.stats(Merge_data$Butte)$out)) 

# Box Plot for Diary Industrial Turnover
boxplot(Merge_data$Diary_Industrial_Turnover_Index, main = "Diary Industrial Turnover", 
        sub = paste("Outlier rows: ", boxplot.stats(Merge_data$Diary_Industrial_Turnover_Index)$out)) 


# using Skewness function, Examining the normality of data

library(e1071)
par(mfrow = c(1, 2))

plot(density(Merge_data$Butter), 
     main = "Density Plot :Production of Butter",
     ylab = "Frequency",
     sub = paste("Skewness:",round(e1071::skewness(Merge_data$Butter), 2)))
polygon(density(Merge_data$Butter), col = "red")


plot(density(Merge_data$Diary_Industrial_Turnover_Index),
     main = "Density Plot :Diary Industrial Turnover",
     ylab = "Frequency",
     sub = paste("Skewness:", round(e1071::skewness(Merge_data$Diary_Industrial_Turnover_Index), 2)))
polygon(density(Merge_data$Diary_Industrial_Turnover_Index), col = "blue")

# Calculating correlation test between Diary Industry Turnover 
# and Production of Butter variable

cor(Merge_data$Butter, Merge_data$Diary_Industrial_Turnover_Index)


# Building linear regression model Using diary produsts production and 
# trunover industry turnover 
linearMod <- lm(Diary_Industrial_Turnover_Index ~ Butter, data = Merge_data)
linearMod

# Examining the confidence intervals of the model
confint(linearMod)

# model summary
summary(linearMod)

Model_Summary <- summary(linearMod)
# model coefficients
Model_Coefficents <- Model_Summary$coefficients
Model_Coefficents
# getting beta estimate for Butter
Beta.Estimate <- Model_Coefficents["Butter", "Estimate"]
Beta.Estimate
# getting std.error for Butter
Standard_Error <- Model_Coefficents["Butter", "Std. Error"]
Standard_Error
# calculating t statistic
T_Value <- Beta.Estimate / Standard_Error
T_Value
# calculating p Value
p_value <- 2 * pt(-abs(T_Value), df = nrow(Merge_data) - ncol(Merge_data))
p_value
# calculating parameters for model p-value 
f <- summary(linearMod)$fstatistic 
model_p <- pf(f[1], f[2], f[3], lower = FALSE)
model_p

#Splitting the data into Training and Testing Data
num_of_records <- sample(1:nrow(Merge_data), 0.8 * nrow(Merge_data))
training_data <- Merge_data[num_of_records,]
testing_data <- Merge_data[-num_of_records,]


# Build the model on training data
LM_model <- lm(Diary_Industrial_Turnover_Index ~ Butter, data = training_data)

# model summary
summary(LM_model)

# predict rent from testing data
LM_predicted <- predict(LM_model, testing_data)


# make actuals_predicteds dataframe.
LM_actual_prediction <- data.frame(cbind(actuals = testing_data$Diary_Industrial_Turnover_Index, 
                                     predicted = LM_predicted))
LM_actual_prediction

# Validations

#AIC
AIC(linearMod)

#BIC
BIC(linearMod)

#Correlation Accuracy
LM_correlation_accuracy <- cor(LM_actual_prediction)
LM_correlation_accuracy

#Min_Max Accuracy
LN_min_max_accuracy <- mean(apply(LM_actual_prediction, 1, min) 
                            / apply(LM_actual_prediction, 1, max))
LN_min_max_accuracy

#Mape
LM_mape <- mean(abs(LM_actual_prediction$predicted 
                    - LM_actual_prediction$actuals) / LM_actual_prediction$actuals)
LM_mape

# Global validation of linear model assumption
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)

########################################################################################

#Building Polynomial Model

polynomialMod <- lm(Diary_Industrial_Turnover_Index ~ Butter + 
                      I(Diary_Industrial_Turnover_Index ^ 2), data = Merge_data)
# Examining the confidence intervals of the model
confint(polynomialMod)

# model summary
summary(polynomialMod)


Model_Summary_poly <- summary(polynomialMod)
# model coefficients
Model_Coefficents_poly <- Model_Summary_poly$coefficients
Model_Coefficents_poly
# getting beta estimate for Butter
Beta.Estimate_poly <- Model_Coefficents_poly["Butter", "Estimate"]
Beta.Estimate_poly
# getting std.error for Butter
Standard_Error_poly <- Model_Coefficents_poly["Butter", "Std. Error"]
Standard_Error_poly
# calculating t statistic
T_Value_poly <- Beta.Estimate_poly / Standard_Error_poly
T_Value_poly
# calculating p Value
p_value_poly <- 2 * pt(-abs(T_Value_poly), df = nrow(Merge_data) - ncol(Merge_data))
p_value_poly
# calculating parameters for model p-value 
f <- summary(linearMod)$fstatistic 
model_p_poly <- pf(f[1], f[2], f[3], lower = FALSE)
model_p_poly




# predict Diary_Industrial_Turnover_Index from testing data
poly_predicted <- predict(polynomialMod, testing_data)
summary(poly_predicted)

# make actuals_predicteds dataframe.
poly_actuals_prediction <- data.frame(cbind(actuals = testing_data$Diary_Industrial_Turnover_Index, 
                                       predicted = poly_predicted))
poly_actuals_prediction

# Validations

#AIC
AIC(polynomialMod)

#BIC
BIC(polynomialMod)

#Correlation Accuracy
LM_correlation_accuracy <- cor(poly_actuals_prediction)
LM_correlation_accuracy

#Min_Max Accuracy
LN_min_max_accuracy <- mean(apply(poly_actuals_prediction, 1, min) 
                            / apply(poly_actuals_prediction, 1, max))
LN_min_max_accuracy

#Mape
LM_mape <- mean(abs(poly_actuals_prediction$predicted 
                    - poly_actuals_prediction$actuals) / poly_actuals_prediction$actuals)
LM_mape

# Global validation of linear model assumption
library(gvlma)
gvmodel <- gvlma(linearMod)
summary(gvmodel)









