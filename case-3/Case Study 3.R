library(readxl)
# Loading Data
data <- read.csv("./change.csv")
data$gender <- as.factor(data$gender)
data$occupation <- as.factor(data$occupation)
data$medianwage <- as.factor(data$medianwage)

# Descriptive Statistics
summary(data[, c(1,6,4)])

get_mode <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(mode = NA, frequency = NA))
  tab <- table(x)
  max_count <- max(tab)
  modes <- names(tab)[tab == max_count]
  rel_freq <- max_count / length(x)
  return(list(mode = modes, relative_frequency = rel_freq))
}
paste("Mode for Gender: ", 
      ifelse(get_mode(data$gender)$mode == 0, "Men", "Women"), 
      " they make up for ", round(get_mode(data$gender)$relative_frequency, 4) * 
        100, "% ", "of the data", sep="")

paste("Mode for occupation: ", 
      ifelse(get_mode(data$occupation)$mode == 0, "Blue Collar","White Collar"), 
      " it makes up for ", 
      round(get_mode(data$occupation)$relative_frequency, 4) * 100, "% ", 
      "of the data", sep="")

paste("Mode for wage category: ", 
      ifelse(get_mode(data$medianwage)$mode == 1, "Lowest Wage Category",
             ifelse(get_mode(data$medianwage)$mode == 2,"2nd lowest wage category", 
                    ifelse(get_mode(data$medianwage)$mode == 3, "Middlemost wage category", 
                           ifelse(get_modedata$medianwage)$mode == 1, 
                           "2nd highest wage category", "Highest wage category"))), 
      " it makes up for ", round(get_mode(data$medianwage)$relative_frequency, 4) * 
        100, "% ", "of the data", sep="")

medianwage_freq <- table(data$medianwage)
breaks <- as.numeric(names(medianwage_freq))
freqs <- as.numeric(medianwage_freq)

bp_medwage <- barplot(freqs,
                      names.arg = breaks,
                      col = "#9ECAE1",
                      border = "white",
                      space = 0,
                      ylim = c(0, max(freqs) * 1.2),
                      main = "Frequency of Wage Categories",
                      xlab = "Wage Category", ylab = "Frequency",
                      las = 1)
text(x = bp_medwage, y = freqs, labels = freqs, pos = 3, cex = 0.9, font = 2)

nchange_freq <- table(data$nchange)
breaks <- as.numeric(names(nchange_freq))
freqs <- as.numeric(nchange_freq)

bp_nchange <- barplot(freqs,
                      names.arg = breaks,
                      col = "#9ECAE1",
                      border = "white",
                      space = 0,
                      ylim = c(0, max(freqs) * 1.2),
                      main = "Frequency of Employer Changes",
                      xlab = "Employer Changes", ylab = "Frequency",
                      las = 1)
text(x = bp_nchange, y = freqs, labels = freqs, pos = 3, cex = 0.9, font = 2)

par(mfrow = c(1,2))
boxplot(nchange ~ gender, data = data,
        main = "Employer Changes by Gender",
        xlab = "Gender",
        col = c("#FEE090", "#91BFDB"),
        ylab = "Number of Employer Changes",
        names = c("Men", "Women"), cex.main = 0.8,
        cex.lab = 1.1, outline = TRUE)

boxplot(nchange ~ medianwage, data = data,
        main = "Employer Changes by Wage Category",
        xlab = "Wage Category (1=Lowest, 5=Highest)",
        ylab = "Number of Employer Changes", cex.main = 0.8,
        col = c("#D73027", "#FC8D59", "#FEE090", "#91BFDB", "#4575B4"),
        names = c("1", "2", "3", 
                  "4", "5"),
        outline = TRUE)

par(mfrow = c(1,2))
boxplot(nchange ~ periodsincome, data = data,
        main = "Employer Changes by Periods\nof Positive Income",
        xlab = "Periods of Positive Income",
        col = c("#FEE090", "#91BFDB"),
        ylab = "Number of Employer Changes",
        cex.main = 0.8, cex.lab = 1.1,
        outline = TRUE)

boxplot(nchange ~ occupation, data = data,
        main = "Employer Changes by Type of Occupation",
        xlab = "Occupation",
        col = c("#FEE090", "#91BFDB"),
        ylab = "Number of Employer Changes",
        names = c("Blue Collar", "White Collar"),
        cex.main = 0.8, cex.lab = 1.1, outline = TRUE)

# 1. Linear Model Estimation
linear_model1 <- lm(nchange ~ gender + occupation + age + periodsincome + 
                      medianwage, data=data)
summary(linear_model1)

library(car)
linearHypothesis(linear_model1, c("medianwage4=medianwage5"))

# 2. Linear Model Estimation
linear_model2 <- lm(nchange ~ gender + occupation + age + periodsincome + 
                      I(periodsincome^2) + medianwage, data=data)
summary(linear_model2)

linearHypothesis(linear_model1, c("medianwage2=0"))

x_vertex <- -linear_model2$coefficients["periodsincome"]/
  (2*linear_model2$coefficients["I(periodsincome^2)"])
min_periodsincome <- min(data$periodsincome)
max_periodsincome <- max(data$periodsincome)

if(x_vertex >= min_periodsincome & x_vertex <= max_periodsincome){
  paste("The vertex (", round(x_vertex,3) ,") lies within the range (", 
        round(min_periodsincome, 3), ";", round(max_periodsincome,3), 
        ") of periodsincome: Non monotonic effect", sep="")
}else{
  print("The vertex (", round(x_vertex,3) ,") does not lie within the range (", 
        round(min_periodsincome, 3), ";", round(max_periodsincome,3), ") of 
        periodsincome: Monotone effect", sep="")
}

periodsincome_mean <- mean(data$periodsincome)

exact_effect <- 2*linear_model2$coefficients["periodsincome"] + 
  4*linear_model2$coefficients["I(periodsincome^2)"]*periodsincome_mean + 
  4*linear_model2$coefficients["I(periodsincome^2)"]

approx_effect <- (linear_model2$coefficients["periodsincome"] +
                    2*linear_model2$coefficients["I(periodsincome^2)"] * 
                    periodsincome_mean)*2

paste("Exact effect of two additional years", round(exact_effect, 3))
paste("Approximated effect of two additional years", round(approx_effect, 3))


# 3. Linear Model Estimation
linear_model3 <- lm(nchange ~ gender + occupation + occupation:gender + age + 
                      periodsincome + I(periodsincome^2) + medianwage, data=data)
summary(linear_model3)

beta_gender <- function(occupation){
  return(linear_model3$coefficients["gender1"] + 
           linear_model3$coefficients["gender1:occupation1"] * occupation)
}

beta_occupation <- function(gender){
  return(linear_model3$coefficients["occupation1"] + 
           linear_model3$coefficients["gender1:occupation1"] * gender)
}

cat("Effect of being woman (vs man):\n", 
    "  Blue collar:", round(beta_gender(0), 3), "\n", 
    "  White collar:", round(beta_gender(1), 3), "\n\n")

cat("Effect of being white collar (vs blue collar):\n", 
    "  Men:", round(beta_occupation(0), 3), "\n", 
    "  Women:", round(beta_occupation(1), 3), "\n")

# Point Prediction
new_data <- data.frame(age=35,
                       gender=factor(1,levels=c(0,1)),
                       occupation=factor(0,levels=c(0,1)),                            
                       periodsincome=11, 
                       medianwage=factor(2, levels = 1:5))
predict(linear_model3, newdata=new_data)


# Model Comparison
AICs <- c(AIC(linear_model1),AIC(linear_model2),AIC(linear_model3))
BICs <- c(BIC(linear_model1),BIC(linear_model2),BIC(linear_model3))
AIC_BIC_table <- cbind(AICs, BICs)
rownames(AIC_BIC_table) <- c("Model 1", "Model 2", "Model 3")
AIC_BIC_table

best_AIC_model <- rownames(AIC_BIC_table)[which.min(AIC_BIC_table[,1])]
best_AIC_value <- round(min(AIC_BIC_table[,1]), 2)

best_BIC_model <- rownames(AIC_BIC_table)[which.min(AIC_BIC_table[,2])]
best_BIC_value <- round(min(AIC_BIC_table[,2]), 2)

cat("According to AIC ", best_AIC_model, " should be selected with a value of", 
    best_AIC_value, "\n", "According to BIC ", best_BIC_model, 
    " should be selected with a value of", best_BIC_value, sep="")

cat("Adjusted R-squared Model 2: ", summary(linear_model2)$adj.r.squared, "\n",
    "Adjusted R-squared Model 3: ", summary(linear_model3)$adj.r.squared, sep="")

# Residual Diagnostics
resids <- residuals(linear_model3)
plot(linear_model3, which=1)
abline(h=0, lty=2, col="red")

plot(linear_model3, which=2)

tseries::jarque.bera.test(resids)





