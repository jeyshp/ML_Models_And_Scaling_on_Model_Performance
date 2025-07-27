#Libraries installs and imports 
install.packages("gModel")
install.packages("emmeans")
library(emmeans)
library(gModel)
library(MASS)

#Reading the collected data in
dataset <- read.csv("data.csv", header = TRUE)
head(dataset)
dim(dataset)

#Renaming column names and setting columns as factors
colnames(dataset)[colnames(dataset) == "Datasets"] <- "Dataset"
colnames(dataset)[colnames(dataset) == "Models"] <- "Model"
colnames(dataset)[colnames(dataset) == "Scaler"] <- "Scaler"

dataset$Dataset <- as.factor(dataset$Dataset)
dataset$Model <- as.factor(dataset$Model)
dataset$Scaler <- as.factor(dataset$Scaler)

#Plotting main effects
options(contrasts = c("contr.sum", "contr.poly"))
plot.design(RMSE ~ Dataset + Model + Scaler, data = dataset)

#Anova - main effects
main_effects_aov <- aov(RMSE ~ Dataset + Model + Scaler, data = dataset)
summary(main_effects_aov)

#Anova - 2 factor interactions 
interactions_aov <- aov(RMSE ~ Dataset + Model + Scaler + 
                              Dataset:Model + Dataset:Scaler +
                              Model:Scaler, data = dataset)
summary(interactions_aov)

#2 factor interactions plot
plot(interactions_aov)

#BoxCox plot
boxcox(RMSE ~ Dataset + Model + Scaler + 
         Dataset:Model + Dataset:Scaler +
         Model:Scaler, data = dataset,
       lambda = seq(-4, 2, len = 21), ylab = "Log likelihood")

#Transform response variable 
RMSE_transformed <- (dataset$RMSE)^0.25
dataset <- cbind(dataset, RMSE_transformed)

#BoxCox - transformed response variable
boxcox(RMSE_transformed ~ Dataset + Model + Scaler + 
         Dataset:Model + Dataset:Scaler +
         Model:Scaler, data = dataset,
       lambda = seq(-4, 2, len = 21), ylab = "Log likelihood")

#Anova - 2 factor interactions - transformed response variable
anova_model <- aov(RMSE_transformed ~ Dataset + Model +
                                          Scaler + Dataset:Model + 
                                          Dataset:Scaler + Model:Scaler,
             data = dataset)
summary(anova_model)

#Investigating interaction effects -interactions plot
interaction.plot(dataset$Dataset, dataset$Model, dataset$RMSE_transformed,
                 xlab = "Dataset" , trace.label = "Model")
interaction.plot(dataset$Model, dataset$Dataset, dataset$RMSE_transformed,
                 xlab = "Model" , trace.label = "Dataset")

#Anova -  3 factor interaction 
anova_3 <- aov(RMSE_transformed ~ Dataset * Model * Scaler,
               data = dataset)
summary(anova_3)


#Contrasts - First check for alphabetical order
levels(dataset$Model)

#Set up contrasts for factor: Model
DTvLR <- c(-1, 0, 1)
DTLRvKNN <- c(1, -2, 1)
Model_contrast <- cbind (DTvLR, DTLRvKNN)

summary(anova_model, 
        split = list(Model = list("DTvLR" = 1, "DTLRvKNN" = 2)))
coefficients(anova_model, split=list( Model = list("DTvLR" = 1, "DTLRvKNN" = 2)))

#Getting SE and estimates of contrasts
emmeans_result <- emmeans(anova_model, ~ Model)
contrasts_result_1 <- contrast(emmeans_result, list("DTvLR" = c(-1, 0, 1))) 
contrasts_result_2 <- contrast(emmeans_result, list("DTLRvKNN" = c(1, -2, 1))) 
confint(contrasts_result_1)
confint(contrasts_result_2)


