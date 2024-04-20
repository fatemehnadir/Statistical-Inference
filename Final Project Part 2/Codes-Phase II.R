# Import some libraries
library(tidyverse)
library(stats)
theme_set(theme_minimal())


# Load the dataset
car <- read.csv("./Downloads/UT/Statistical Inference/Project/Phase II/Car_Insurance_Claim_Prediction.csv")

#------------------------------> Question 1 <------------------------------
# categorical variables
car.categorical <- car %>% select_if(negate(is.numeric))
colnames(car.categorical)

# number of unique variables for each columns
car.categorical.t <- as.data.frame(t(car.categorical))
apply(car.categorical.t, 1, function(car.categorical) length(unique(car.categorical)))

# numerical variables
car.numeric <- car %>% select_if((is.numeric))
colnames(car.numeric)

# number of unique variables for each columns
car.numeric.t <- as.data.frame(t(car.numeric))
apply(car.numeric.t, 1, function(car.numeric) length(unique(car.numeric)))

# convert some types of numberical variables
car$population_density <- as.factor(car$population_density)
car$airbags <- as.factor(car$airbags)
car$displacement <- as.factor(car$displacement)
car$cylinder <- as.factor(car$cylinder)
car$turning_radius <- as.factor(car$turning_radius)
car$length <- as.factor(car$length)
car$width <- as.factor(car$width)
car$height <- as.factor(car$height)
car$gross_weight <- as.factor(car$gross_weight)
car$ncap_rating <- as.factor(car$ncap_rating)

# categorical variables
car.categorical <- car %>% select_if(negate(is.numeric))
colnames(car.categorical)

# number of unique variables for each columns
car.categorical.t <- as.data.frame(t(car.categorical))
apply(car.categorical.t, 1, function(car.categorical) length(unique(car.categorical)))


# sampling
car_sample <- sample_n(car.categorical, 1000)

# calc each two combinations of categorical variables  
n <- 2:21
for(i in n){
  x <- i+1
  m <- x:22
  for(j in m){
    print("#------")
    print(paste0(colnames(car_sample)[i],", ",colnames(car_sample)[j]))
    print(table(car_sample[,i],car_sample[,j]))
    print("------#")
  }
}

# change categorical to numerical 
car_sample$ncap_rating <- sapply(car_sample$ncap_rating, unclass)

# merge 3 last columns
car_sample <- within(car_sample, {   
  ncap_rating.new <- NA # need to initialize variable
  ncap_rating.new[ncap_rating == 1] <- "0"
  ncap_rating.new[ncap_rating == 2] <- "2"
  ncap_rating.new[ncap_rating >= 3] <- "more than 2"
  
} )

# show the new contingency table
print(paste0(colnames(car_sample)[10],", ",colnames(car_sample)[23]))
print(table(car_sample[,10],car_sample[,23]))

# select the two categorical variables
ncap_rating <- car_sample$ncap_rating.new
is_parking_camera <- car_sample$is_parking_camera

unique(ncap_rating)
unique(is_parking_camera)

#Part A ---------
# contingency table of ncap_rating and is_parking_camera
contingency_table <- addmargins(table(is_parking_camera, ncap_rating))
contingency_table

# calculate proportions
contingency_table_p <- contingency_table
contingency_table_p[1,] <- round((contingency_table[1,]/contingency_table[1,4]),4)
contingency_table_p[2,] <- round((contingency_table[2,]/contingency_table[2,4]),4)
contingency_table_p[1:2,1:3]

#calculate confidence intervar for each ratings
n1 <- contingency_table[1,4]
n2 <- contingency_table[2,4]

for(i in 1:length(colnames(contingency_table_p[1:2,1:3]))){

  p1 <- contingency_table_p[1,i]
  p2 <- contingency_table_p[2,i]
  
  pointest <- p1-p2
  
  se <- sqrt(((p1*(1-p1))/n1)+((p2*(1-p2))/n2))
  
  c <- 0.95
  zscore <- qnorm((1-c)/2,lower.tail = FALSE)
  
  me <- zscore * se
  
  lowerinterval <- round(pointest - me,3)
  upperinterval <- round(pointest + me,3)
  
  print(paste0("Cofidence Interval for ", colnames(contingency_table_p)[i]," start(s) : (",lowerinterval, ", ", upperinterval, ")"))
}


#Part B ---------

# Perform a chi-squared test of independence on the contingency table
chi_squared_test <- chisq.test(contingency_table)

# Print the p-value of the test to determine if the two variables are independent
print(chi_squared_test$p.value)


#------------------------------> Question 2 <------------------------------
# chose transmission_type as binary variable

car_sample <- sample_n(car, 15)

transmission <- car_sample$transmission_type

Manual <- transmission[transmission =='Manual']
Automatic <- transmission[transmission =='Automatic']

frequencies<-c(length(Manual),length(Automatic))
percentage  <- round(100*frequencies/sum(frequencies), 2)

transmission.categorized <- data.frame(types = c("Manual", "Automatic"),percentage = percentage, value = frequencies)

ggplot(data=transmission.categorized, aes(x=types, y=percentage, fill=types)) + 
  geom_bar(stat="identity", alpha = 0.7, width = 0.6)+
  labs(title="Barplot of Transmission Type")+
  geom_text(aes(label=paste(percentage, "%")),vjust=-0.4,size=4)

# run simulation
# method 1
p.hat <- table(transmission)[2]/15
transmission.simulation <-  data.frame(replicate(n = 1000, mean(sample(levels(as.factor(transmission)), size = 15, replace = TRUE)=='Manual')))
p_value <- mean(transmission.simulation >= p.hat)
p_value

# method 2
source("./Downloads/UT/Statistical Inference/Project/Phase II/inference.R")
inference(transmission,est="proportion",
          type="ht", success = "Manual", 
          method = "simulation",
          null=0.5, 
          alternative = "greater")


#------------------------------> Question 3 <------------------------------
# Part A ---------

# random sampling
sample1 <- sample_n(car, 100)$ncap_rating
Number <- table(sample1)

# calculate proportions randomly sample
Proportion <- round((prop.table(Number)),3)
car.sample.unbiased <- addmargins(rbind(Number, Proportion))[1:2,]
car.sample.unbiased


# biased sampling
pro <- ifelse(car$ncap_rating > 3 , 0.9, 0.1)
sample2 <- sample(car$ncap_rating, 100, prob = pro)
Number <- table(sample2)

# calculate proportions
Proportion <- round((prop.table(Number)),3)
car.sample.biased <- addmargins(rbind(Number, Proportion))[1:2,]
car.sample.biased

# merge 2 last columns
sample1.new <- ifelse(sample1 > 3, "more than 3", sample1)
sample2.new <- ifelse(sample2 > 3, "more than 3", sample2)

# sample1 : random sampling
Number <- table(sample1.new)
Proportion <- round((prop.table(Number)),3)
car.sample.unbiased.new <- addmargins(rbind(Number, Proportion))[1:2,]
car.sample.unbiased.new

# sample2 : biased sampling
Number <- table(sample2.new)
Proportion <- round((prop.table(Number)),3)
car.sample.biased.new <- addmargins(rbind(Number, Proportion))[1:2,]
car.sample.biased.new

# Expected value for NCAP rating from original population
pop <- car$ncap_rating
p <- round(prop.table(table(ifelse(pop > 3, "more than 3", pop))),3)
expected <- rbind(p*100,p)
expected

df <- 4-1

# chi square test for random sampling
chi.unbiased <- sum(((car.sample.unbiased.new[1:4]-expected[1:4])^2)/(expected[1:4]))
chi.unbiased

pvalue.unbiased <- pchisq(chi.unbiased, df, lower.tail = FALSE)
pvalue.unbiased

# chi square test for biased sampling
chi.biased <- sum(((car.sample.biased.new[1:4]-expected[1:4])^2)/(expected[1:4]))
chi.biased

pvalue.biased <- pchisq(chi.biased, df, lower.tail = FALSE)
pvalue.biased

# chi test with R functions
chisq.test(car.sample.unbiased.new[1,1:4], p = expected[2,1:4])
chisq.test(car.sample.biased.new[1,1:4], p = expected[2,1:4])

#Part B ---------

ncap_rating <- car$ncap_rating
table(car$transmission_type,ncap_rating) 

# observed
ncap_rating.new <- ifelse(ncap_rating > 2, "more than 2", ncap_rating)
contingency.table <- addmargins(table(car$transmission_type,ncap_rating.new))
contingency.table

# expected
expected<- contingency.table

numberOfcol <- ncol(expected)
numberOfrow <- nrow(expected)
table.total <- expected[numberOfrow,numberOfcol]

R <- 1:nrow(expected)
C <- 1:ncol(expected)

for (r in R) {
  for (c in C) {
    row.total   <- expected[r,numberOfcol]
    col.total   <- expected[numberOfrow,c]
    expected[r,c] <- round(row.total * col.total/ table.total)
  }
}

expected

# chi square test
chi <- sum(((contingency.table[1:2,1:3] - expected[1:2,1:3])^2)/(expected[1:2,1:3 ]))
chi

df <- (length(R)-1) * (length(C)-1)

pvalue <- pchisq(chi, 2, lower.tail = FALSE)
pvalue

# chi test with R functions
chisq.test(contingency.table,  rescale.p = T)




#------------------------------> Question 4 <------------------------------
# Part A ---------

# response
policy <- car$policy_tenure

# explanatory
age <- car$age_of_car
displacement <- car$displacement

# Part B ---------
model.with.age <- lm(car$policy_tenure ~ car$age_of_car)
model.with.dis <- lm(car$policy_tenure ~ car$displacement)

# age of the car

# Sub Part a -----
# method 1
library(ggplot2)
library(ggfortify)
autoplot(model.with.age)
#--------

# method 2
#Linearity
plot(model.with.age,1)
#2.Nearly normal residuals
plot(model.with.age,2)
# 3.Constant variability
plot(model.with.age,3)

# Sub Part b -----
summary(model.with.age)

# Sub Part c -----


# Sub Part d -----

ggplot(car, aes(y=policy_tenure, x=age_of_car)) + 
  geom_point( alpha = .2) +
  geom_smooth(method=lm, col = 'blue',se = FALSE,formula = 'y ~ x',linetype="dashed") +
  labs(title=paste("Scatter Plot of Age of car and policy tenure and fitted linear regression curve"))



# displacement

# Sub Part a -----
# method 1
autoplot(model.with.dis)
#--------

# method 2
#Linearity
plot(model.with.dis,1)
#2.Nearly normal residuals
plot(model.with.dis,2)
# 3.Constant variability
plot(model.with.dis,3)

# Sub Part b -----
summary(model.with.dis)

# Sub Part c -----


# Sub Part d -----

ggplot(car, aes(y=policy_tenure, x=displacement)) + 
  geom_point( alpha = .2) +
  geom_smooth(method=lm, col = 'blue',se = FALSE,formula = 'y ~ x',linetype="dashed") +
  labs(title=paste("Scatter Plot of Age of car and policy tenure and fitted linear regression curve"))


# Part C ---------
# Part D ---------
anova(model.with.age)
anova(model.with.dis)

# Part E ---------
# Part F ---------
set.seed(1) 
car_sample <- sample_n(car, 100)

train.car <- car_sample[1:90, ]
test.car <- car_sample[91:100, ]

# Sub Part a -----
model2.with.age<- lm(policy_tenure ~ age_of_car, train.car)
summary(model2.with.age)

model2.with.dis<- lm(policy_tenure ~ displacement, train.car)
summary(model2.with.dis)


# Sub Part b -----
b1 <- model2.with.age$coefficients[2]
se1 <- summary(model2.with.age)$coefficient[4]
tstar <- abs(qt(0.025, df = 90-1-1))
me1 <- se1 * tstar

lowerinterval1 <- b1 - me1
lowerinterval1

upperinterval1 <- b1 + me1
upperinterval1


b2 <- model2.with.dis$coefficients[2]
se2 <- summary(model2.with.dis)$coefficient[4]
tstar <- abs(qt(0.025, df = 90-1-1))
me2 <- se2 * tstar

lowerinterval2 <- b2 - me2
lowerinterval2

upperinterval2 <- b2 + me2
upperinterval2

# Sub Part c -----
predict1 <- predict(model2.with.age,select(test.car,age_of_car))
predict2 <- predict(model2.with.dis,select(test.car,displacement))

# Sub Part d -----
actual1  <- select(test.car,policy_tenure)
diff1    <- round(abs(predict1-actual1),1)

actual2  <- select(test.car,policy_tenure)
diff2    <- round(abs(predict2-actual2),1)

success.rate1 <- length(diff1[diff1== 0])/nrow(diff1)
success.rate1

success.rate2 <- length(diff2[diff2== 0])/nrow(diff2)
success.rate2

#------------------------------> Question 5 <------------------------------
# Part A ---------
# choose all numerical var

data <- select_if(car, is.numeric)         # Identify numeric columns
data <- data[,!names(data) %in% c("X")]

#create pairs plot
ggpairs(data)


car.sample <- data %>% select(policy_tenure,width, displacement,ncap_rating,age_of_car,height)
ggpairs(car.sample)


# Part B ---------
model_partB <- lm(policy_tenure ~ . , data = car.sample)
summary(model_partB)

# Part C ---------
summary(model)$r.squared

# Part D ---------

build_model = function(res, exp) {
  as.formula(paste(res, paste(exp, collapse=" + "), sep=" ~ "))
}

full.variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")

#-----> method 1 <----- 
#backward elimination - adjusted r square
full.model <- lm(build_model('policy_tenure',full.variables)  , data = data)
summary(full.model) #0.0947

#step 1
#delete age_of_car
variables <- c("age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s11 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s11) # 0.09012

#delete age_of_policyholder
variables <- c("age_of_car","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s12 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s12) # 0.07158

#delete population_density
variables <- c("age_of_car","age_of_policyholder","airbags","displacement","cylinder","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s13 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s13) # 0.08951

#delete airbags
variables <- c("age_of_car","age_of_policyholder","population_density","displacement","cylinder","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s14 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s14) # 0.09467

#delete displacement
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","cylinder","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s15 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s15) # 0.0971

#delete cylinder
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s16 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s16) # 0.09472

#delete gear_box
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s17 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s17) # 0.09436

#delete turning_radius
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","length","width","height","gross_weight","ncap_rating","is_claim")
model.s18 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s18) # 0.09326

#delete length
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","width","height","gross_weight","ncap_rating","is_claim")
model.s19 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s19) # 0.09465

#delete width
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","height","gross_weight","ncap_rating","is_claim")
model.s110 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s110) # 0.09418

#delete height
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","width","gross_weight","ncap_rating","is_claim")
model.s111 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s111) # 0.09374

#delete gross_weight
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","width","height","ncap_rating","is_claim")
model.s112 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s112) # 0.09454

#delete ncap_rating
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","width","height","gross_weight","is_claim")
model.s113 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s113) # 0.09464

#delete is_claim
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","cylinder","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating")
model.s114 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s114) # 0.08925

### delete cylinder

#step 2
#delete age_of_car
variables <- c("age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s21 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s21) # 0.08971

#delete age_of_policyholder
variables <- c("age_of_car","population_density","airbags","displacement","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s22 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s22) # 0.07161

#delete population_density
variables <- c("age_of_car","age_of_policyholder","airbags","displacement","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s23 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s23) # 0.08953

#delete airbags
variables <- c("age_of_car","age_of_policyholder","population_density","displacement","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s24 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s24) # 0.09443

#delete displacement
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s25 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s25) # 0.0973

#delete gear_box
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model.s27 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s27) # 0.09434

#delete turning_radius
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","length","width","height","gross_weight","ncap_rating","is_claim")
model.s28 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s28) # 0.09289

#delete length
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","width","height","gross_weight","ncap_rating","is_claim")
model.s29 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s29) # 0.09467

#delete width
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","height","gross_weight","ncap_rating","is_claim")
model.s210 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s210) # 0.09201

#delete height
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","width","gross_weight","ncap_rating","is_claim")
model.s211 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s211) # 0.08958

#delete gross_weight
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","width","height","ncap_rating","is_claim")
model.s212 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s212) # 0.09302

#delete ncap_rating
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","width","height","gross_weight","is_claim")
model.s213 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s213) # 0.09384

#delete is_claim
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating")
model.s214 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model.s214) # 0.08927

# backward Elimination choose model.s16
best.model.back <- model.s16

#-----> method 2 <----- 
#forward selection -  adjusted r square

forward_select <- function(data, response) {
  predictors <- names(data)
  predictors <- predictors[!predictors %in% response]
  selected <- c()
  current_score <- -100
  
  for (i in 1:length(predictors)) {
    best_predictor <- NULL
    best_new_score <- -100
    
    for (predictor in setdiff(predictors, selected)) {
      model <- lm(as.formula(paste(response, paste(c(selected, predictor), collapse = " + "), sep = " ~ ")), data = data)
      new_score <- summary(model)$r.squared
      
      if (new_score > best_new_score) {
        best_predictor <- predictor
        best_new_score <- new_score
      }
    }
    
    if (best_new_score > current_score) {
      selected <- c(selected, best_predictor)
      current_score <- best_new_score
    } else {
      break
    }
  }
  
  return(selected)
}

best.variable <- forward_select(data, "policy_tenure")
best.model.for <- lm(build_model('policy_tenure', best.variable)  , data = data)

#-----> method 3 <----- 
#backward elimination - p value
full.model2 <- lm(build_model('policy_tenure',full.variables)  , data = data)
summary(full.model2) #0.0947

#step 1
#delete cylinder
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","displacement","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model2.s11 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model2.s11) # 0.09472

#step 2
#delete displacement
variables <- c("age_of_car","age_of_policyholder","population_density","airbags","gear_box","turning_radius","length","width","height","gross_weight","ncap_rating","is_claim")
model2.s12 <- lm(build_model('policy_tenure', variables)  , data = data)
summary(model2.s12) # 0.09473

best.model.back.pvalue <- model2.s12


#-----> method 4 <----- 
modell <- lm(policy_tenure ~ ., data = data)
best.model.for.pvalue <- step(modell, direction = "forward", scope = formula(modell), trace = 0, k = log(nrow(data)))


#----->  Results <----- 
summary(best.model.back) #method 1
summary(best.model.for) #method 2
summary(best.model.back.pvalue) #method 3
summary(best.model.for.pvalue) #method 4

# Part E ---------
library(caret)
train_control <- trainControl(method = "cv", number = 5)

# part B
predictors <- names(model_partB$coefficients)
predictors <- predictors[!predictors %in% '(Intercept)']

model.last <- train(build_model('policy_tenure', predictors), data = data , method = "lm",trControl = train_control)
model.last

 
# part D
predictors <- names(best.model.back.pvalue$coefficients)
predictors <- predictors[!predictors %in% '(Intercept)']

model.last2 <- train(build_model('policy_tenure', predictors), data = data , method = "lm",trControl = train_control)
model.last2

# Part F ---------
autoplot(best.model.back.pvalue)
plot(best.model.back.pvalue,1)
plot(best.model.back.pvalue,2)
plot(best.model.back.pvalue,3)
# Part G ---------

