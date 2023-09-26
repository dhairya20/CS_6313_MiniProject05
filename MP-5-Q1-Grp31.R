# STATS-Mini-Project-5
# Setting working directory to Proj-5 folder.
#setwd("C:/Users/dpd140130/OneDrive - The University of Texas at Dallas/CS 6313/Projects/05")
setwd("C:/Users/dpd140130.CAMPUS/OneDrive - The University of Texas at Dallas/CS 6313/Projects/05")
getwd()
#install.packages("BSDA")
library(BSDA)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("boot")
library(boot)

# Observing our .csv file
records <- read.csv("bodytemp-heartrate.csv")
#typeof(data$gender)
m_records <- records[records$gender==1,]
f_records <- records[records$gender==2,]

# Question-1(a)
# Hypothesis Testing for body_temp metric for M and F data.
# Since number of obv is 65 we can assume that it follows Normal Distribution from the Law of Large Numbers.
print('Mean BodyTemps are unequal as shown by z-test on M-F data.')
z.test(x=m_records$body_temperature,y=f_records$body_temperature,mu=0,sigma.x=sd(m_records$body_temperature),sigma.y=sd(f_records$body_temperature),alternative="two.sided")
summary(m_records$body_temperature)
summary(f_records$body_temperature)

# Question-1(b)
# Hypothesis Testing for heart_rate metric for M and F data.
# Since number of obv is 65 we can assume that it follows Normal Distribution from the Law of Large Numbers.
print('Mean Heart-Rates are unequal as shown by z-test on M-F data.')
z.test(x=m_records$heart_rate,y=f_records$heart_rate,mu=0,sigma.x=sd(m_records$heart_rate),sigma.y=sd(f_records$heart_rate),alternative="two.sided",conf.level = 0.99)
summary(m_records$heart_rate)
summary(f_records$heart_rate)

# Question-1(c)
rho <- cor(m_records$heart_rate,m_records$body_temperature)
# finding Pearson's correlation coeff.
rho
# found the point estimate of rho.
ggpubr::ggscatter(m_records, x = "heart_rate", y = "body_temperature", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Heart Rate", ylab = "Body Temperature", col = "blue")


rho <- cor(f_records$heart_rate,f_records$body_temperature) 
# finding Pearson's correlation coeff.
rho
# found the point estimate of rho.
ggpubr::ggscatter(f_records, x = "heart_rate", y = "body_temperature", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Heart Rate", ylab = "Body Temperature", col = "blue")


