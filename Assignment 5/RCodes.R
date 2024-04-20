library(MASS)
library(knitr)


#-------------Q7(R)----------------
null.probs = c(1/5,1/5,1/5,1/5,1/5)
freqs = c(18,16,10,10,16)
chisq.test(freqs, p=null.probs)

#-------------Q8(R)----------------

# read data and create two-way table
t1 = table(survey$Smoke, survey$Exer) 

# view table 
t1  

# change table
t2 = cbind(t1[,"Freq"], t1[,"None"] + t1[,"Some"]) 

# Apply colnames
colnames(t2) <- c("Freq", "None & Some")
# viewtable 
t2

# run chi test
chisq.test(t2) 
