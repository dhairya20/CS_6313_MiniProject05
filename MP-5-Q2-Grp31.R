library(boot)

# Given: Investigation focuses on Confidence lvl (1-alpha) = 0.95

n <- c(5,10,30,100)
lamda <- c(0.01,0.1,1,10)

lwr.z = numeric(0)
upr.z = numeric(0)
lwr.boot = numeric(0)
upr.boot= numeric(0)
z_coverage = numeric(0)
boot_coverage = numeric(0)

mean.boot<-function(rec,sub){
  return(mean(rec[sub]))
  } 

data_mat = matrix(,nrow = 1,ncol = 4,dimnames = list(c(1),c("lamda","n","z_coverage","boot_coverage")))
data_mat = data_mat[-c(1),]
for(param in lamda){
  CI_LLN = numeric(0)
  CI_BootPerCI = numeric(0)
  k=1
  for(datasize in n){
    
    
    for (i in 1:5000){
      
      # Generating data following Exponential Distribution.
      data <- rexp(datasize, param)
      
      # Calculating the Confidence Interval using Large Sample Mean formula.
      lwr.z <- mean(data) - (qnorm(.975)*sd(data)/sqrt(datasize))  
      upr.z <- mean(data) + (qnorm(.975)*sd(data)/sqrt(datasize))
      
      # Calculating Parametric Bootstrap Percentile CI.
      BootR <- boot(data=data, statistic = mean.boot, R= 999)
      lwr.boot <- quantile(BootR$t,.025)
      upr.boot <- quantile(BootR$t,.975)
      
      z_coverage[i] <- ((1/param) >= lwr.z) & ((1/param) <= upr.z)
      boot_coverage[i] <- ((1/param) >= lwr.boot) & ((1/param) <= upr.boot)
    }
    
    CI_LLN[k] <- mean(z_coverage)
    CI_BootPerCI[k] <- mean(boot_coverage)
    
    data_mat = rbind(data_mat,c(param,datasize,CI_LLN[k],CI_BootPerCI[k]))
    k=k+1
  }
  plot(n,CI_LLN,type = 'l', col="magenta",lwd=2, ylab="")
  title(main = paste("Plotting graph of Coverage Probability vs N for lambda = ",param), ylab = 'Coverage Probability')
  lines(n,CI_BootPerCI,type = 'l', col="darkgreen",lwd=2)
  legend("bottomright", legend = c('LS-CI','Boot-CI'), fill = c('magenta','darkgreen'))
}

data_mat

