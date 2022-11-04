# STAT E-100
# Mission #9
# Confidence Intervals and Hypothesis Testing
# One-sample t-test, one-sample z-test, one-sample t-/z-intervals

# Export R data file into Excel spreadsheet
# install.packages("openxlsx")
# library("openxlsx")
# write.xlsx(EmployedACS, "Employed_ACS.xlsx")

install.packages("BSDA") # For z.test() function
library("BSDA")

mean(EmployedACS$HoursWk) # Sample estimate: 38.4898 hours
sd(EmployedACS$HoursWk) # Standard deviation: 13.18866 hours
nrow(EmployedACS) # Sample size: 343 participants

# One-sample t-test for population means
one_sample_t <- t.test(EmployedACS$HoursWk, mu = 37, alternative = "two.sided")
# one_sample_t <- t.test(EmployedACS$HoursWk, mu = 37, alternative = "greater")
# one_sample_t <- t.test(EmployedACS$HoursWk, mu = 37, alternative = "less")
one_sample_t$p.value

# Calculating the t-interval by hand
qt(p = .025, df = (343-1)) # left tail: 0.025 + right body: 0.975 = 1
# -1.966925
qt(p = .975, df = (343-1)) # left body: 0.975 + right tail: 0.025 = 1
# 1.966925
# Formula for 95% confidence for t-interval: sample mean +/- t-critical * (s)/sqrt(n)
# 38.4898 + 1.966925*(13.18866/343^.5) = 39.89049
# 38.4898 - 1.966925*(13.18866/343^.5) = 37.08911
one_sample_t$conf.int # (37.08911, 39.89048)

# One-sample z-test
one_sample_z <- z.test(EmployedACS$HoursWk,
       alternative='two.sided',
       mu=37,
       sigma.x=sd(EmployedACS$HoursWk),
       conf.level=.95)
# Confidence interval (z-interval)
one_sample_z$statistic

######################################################################

mean(EmployedACS$Age) # Sample estimate: 43.99125 years
sd(EmployedACS$Age) # Standard deviation: 14.15629 years
nrow(EmployedACS) # Sample size: 343 participants

######################################################################
# One-way analysis of variance
boxplot(Income~HealthInsurance,data=EmployedACS)
one.way <- aov(Income ~ HealthInsurance, data = EmployedACS)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

boxplot(Income~Race,data=EmployedACS)
one.way <- aov(Income ~ Race, data = EmployedACS)
summary(one.way)
tukey.one.way<-TukeyHSD(one.way)
tukey.one.way

# Independent samples t-test
t.test(Income~HealthInsurance, data = EmployedACS, var.equal=TRUE)

# Correlation Coefficient
cor(EmployedACS$Income, EmployedACS$Age)

# Simple Linear Regression
plot(EmployedACS$Income, EmployedACS$Age, pch=19)
abline(lm(Income ~ Age, data = EmployedACS, col="red"))
model <- lm(Income ~ Age, data = EmployedACS)
model

# Paired t-test
t.test(x = EmployedACS$Age, y = EmployedACS$HoursWk, paired = TRUE, alternative = "two.sided")

# Chi-square Test of Independence
test <- chisq.test(table(EmployedACS$Poor, EmployedACS$YoungAdult))
test
table1 <- table(EmployedACS$Poor, EmployedACS$YoungAdult)
mosaicplot(table1)