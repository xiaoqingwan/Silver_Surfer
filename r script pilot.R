# install.packages('boot')
#library(foreign)
library(dplyr)
library(tidyverse)
library(glmnet)
options(scipen=999)
#convert SPSS to csv
# df01 <- read.spss('h16f2a.sav', to.data.frame = TRUE)
# write.csv(df01,'h16f2a.csv')

# Read csv and simple cleaning ----------------------------------------------------------------
df = read.csv("prolific_pilot_210802.csv", header = TRUE)
# remove character columns
df <- df[, !sapply(df, is.character)]
df <- subset(df, age > 60)
#variable descriptions
#str(df1)

plot(df$exer_sum, df$software)
plot((df$sage - df$age), df$software)
plot(df$imm_sum, df$software)
plot(df$sage, df$constraints)
plot(df$delay_sum, df$imm_sum)
plot(df$all_proficiency, df$age)
plot(df$mastery, df$software)
plot(df$mastery, df$constraints)
plot(df$all_proficiency, df$inheritance_1)
plot(df$social, df$software)
plot(df$social, df$all_proficiency)
plot(df$imm_sum, df$exer_sum)
plot(df$SES, df$all_proficiency)
plot(df$duration, df$all_proficiency)
plot(df$duration, df$software)


all <- cor(df, df$all_proficiency)
write.csv(as.matrix(all), file='all_cor.csv')

software <- cor(df, df$software)
write.csv(as.matrix(software), file='software_cor.csv')

# Bootstrap correlations --------------------------------------------------

library('boot')

b <- boot(df, 
           statistic = function(df, i) {
             cor(df[i, 'hardware'], df[i, 'software'], method='pearson')
           },
           R = 1000
)
# b
# boot.ci(b, type = c("norm", "basic", "perc", "bca")) #bootstrapped CI. 
plot(density(b$t))
abline(v = 0, lty = "dashed", col = "grey60")
b
boot.ci(b, type = c("norm", "basic", "perc", "bca")) #bootstrapped CI. 
colMeans(b$t)  #bootstrapped r

a <- df$hardware + df$software + df$all_proficiency
hist(a)

df$SES <- df$checking + df$retirement + df$work + df$vehicle + df$debts
hist(df$SES)



# DV correlation --------------------------------------------------------


test <- cor.test(df$software, df$wnpq)
test

# Pearson's product-moment correlation
# 
# data:  df$software and df$all_proficiency
# t = 2.4689, df = 27, p-value = 0.02017
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.07435415 0.68752809
# sample estimates:
# cor 
# 0.4291646 


test <- cor.test(df$software, df$all_proficiency)
test

test <- cor.test(df$imm_sum, df$delay_sum)
test

# ceiling effects --------------------------------------------------------

t.test(df$all_proficiency)

# One Sample t-test

# data:  df$all_proficiency
# t = 55.268, df = 28, p-value < 0.00000000000000022
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   140.6552 151.4828
# sample estimates:
#   mean of x 
# 146.069 
hist(df$all_proficiency)
hist(df$software)
