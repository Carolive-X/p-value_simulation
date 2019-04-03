#P-value Simulation Study
#---------------------------------------------------------



pvalSim <- function(nSims=100000, ngroup=100, mean1, mean2, SD = 20) {
  p <- c(NA, nSims)
for(i in 1:nSims){ 
    
    #produce 100 simulated participants, with mean=100 and SD=20
    x<-rnorm(n = ngroup, mean = mean1, sd = SD) 
    #produce 100 simulated participants, #with mean=100 and SD=20
    y<-rnorm(n = ngroup, mean = mean2, sd = SD) 
    z<-t.test(x,y) #perform the t-test
    
    p[i]<-z$p.value #get the p-value and store it
  
  }
  
  return(p)
  
}

  p = pvalSim(mean1 = 100, mean2 =  100)
  quantile(p,0.05)
  hist(p, main="Histogram of p-values", xlab=("Observed p-value"))
  
  p2 = pvalSim(mean1 = 100, mean2 =  101)
  quantile(p2,0.05)
  hist(p2, main="Histogram of p-values", xlab=("Observed p-value"))
  
  

  
  
  



