# Import Libraries

require(qqplotr)
library(gridExtra)
library(moments)
library(magrittr) 
library(ggfortify)
library(ggplot2)
library(Hmisc)
library(plyr)
library(hexbin)
library("plot3D")
library(plotly)
library(scatterplot3d)
library(RNHANES)
library(GGally)
library(dplyr)
require(ggpubr)
library(ggmosaic)
require(corrplot)
library(patchwork)
library(ggExtra)
library(ggcorrplot)
library(reshape)
theme_set(theme_minimal())


# read dataset
path = './Car_Insurance_Claim_Prediction.csv'
car<-read.csv(path)


#------------------------------> Question 0 <------------------------------
#Part A ---------
#Part B ---------
summary(car)

#Part C ---------
any(is.na(car))
missingValue <- data.frame(colSums(is.na.data.frame(car)))

#Part D ---------

#------------------------------> Question 1 <------------------------------
#Part A ---------
# Freedman-Diaconis rule
bins_fd <- function(vec) {
  diff(range(vec)) / (2 * IQR(vec) / length(vec)^(1/3))}
bin_fd = bins_fd(car$age_of_car)

# plot
ggplot(car, aes(x = age_of_car)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "#F7D302",
                 bins = bin_fd) +
  labs(
    title = "Histogram and Density of Age Of Cars",
    x = "age of car",
    y = "count"
  ) +
  geom_density(color = "#631919", size = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

# sqrt(n) rule
bins_sqrt <- function(vec) {
  sqrt(length(vec))}
bin_sq <- bins_sqrt(car$age_of_car)

# plot
ggplot(car, aes(x = age_of_car)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "#F7D302",
                 bins = bin_sq) +
  labs(
    title = "Histogram and Density of Age Of Cars",
    x = "age of car",
    y = "count"
  ) +
  geom_density(color = "#631919", size = 1)  +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
    )

# default
ggplot(car, aes(x = age_of_car)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "#F7D302") +
  labs(
    title = "Histogram and Density of Age Of Cars",
    x = "age of car",
    y = "count"
  ) +
  geom_density(color = "#631919", size = 1) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  )

# qqplot
qqnorm(car$age_of_car, pch = 1, frame = FALSE)
qqline(car$age_of_car, col = "steelblue", lwd = 2)

#Part A ---------

#Part C ---------
quantile(car$age_of_car)

boxplot(car$age_of_car,
      main = "BoxPlot for Age Of Car",
      names = c("age_of_car"),
      xlab = "Age",
      horizontal = TRUE)


#Part D ---------
aoc <- car$age_of_car
length(aoc[which(aoc < boxplot$stats[1] | aoc > boxplot$stats[5])])

#Part E ---------
mean(car$age_of_car)
median(car$age_of_car)
var(car$age_of_car)
sd(car$age_of_car)

#Part F ---------
aoc <- car$age_of_car
mean = mean(aoc)


Fo = aoc[aoc > .75*mean]
Th  = aoc[aoc >= .5*mean & aoc<= .75*mean]
Se = aoc[aoc < .5*mean & aoc >= .25*mean]
Fi = aoc[aoc < .25*mean]

frequencies<-c(length(Fo),length(Th),length(Se),length(Fi))
percentage  <- round(100*frequencies/sum(frequencies), 2)

aoc.categorized <- data.frame(names = c("Fourth", "Third", "Second","First"),value = percentage)

ggplot(aoc.categorized, aes(x="", y = value, fill = names)) +
  geom_bar(stat = "identity") + coord_polar("y") +
  geom_text(aes(label = paste0(value, "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title="Age Of Car Pie Chart", x = 'Frequency', y = 'Age Of Car')

#Part G ---------
ggplot(car, aes(x = age_of_car)) + geom_density( size = 1) +
  geom_vline(aes(xintercept = median(age_of_car)), linetype = "dashed", size = .7,col= 'red') +
  geom_vline(aes(xintercept = mean(age_of_car)), linetype = "dashed", size = .7,col= 'green') +
  theme_classic() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  ) +
  annotate("text", x = mean(car$age_of_car) + .02 , label = "mean", y = 6, size = 3, angle = 90 , color = 'green') +
  annotate("text", x = median(car$age_of_car) - 0.02 , label = "median", y = 0.6, size = 3, angle = 90, color = 'red') +
  labs(
    title = "Age of Car Density",
    caption = "green line is about mean and red is about median variable on the diagram.",
    x = "Age Of The Car",
    y = "Count"
  ) 

#------------------------------> Question 2 <------------------------------
#Part A ---------
transmission <- car$transmission_type
gp <- unique(transmission)

Manual = transmission[transmission =='Manual']
Automatic = transmission[transmission =='Automatic']

frequencies<-c(length(Manual),length(Automatic))
percentage  <- round(100*frequencies/sum(frequencies), 2)

transmission.categorized <- data.frame(types = c("Manual", "Automatic"),percentage = percentage, value = frequencies)

#Part B ---------
transmission.categorized <- transmission.categorized[order(percentage),]
ggplot(data=transmission.categorized, aes(x=types, y=frequencies, fill=types)) + 
  geom_bar(stat="identity", alpha = 0.7, width = 0.7) +
  labs(title="Barplot of Transmission Type") +
  coord_flip()

#Part C ---------
ggplot(data=transmission.categorized, aes(x=types, y=percentage, fill=types)) + 
  geom_bar(stat="identity", alpha = 0.7, width = 0.6)+
  labs(title="Barplot of Transmission Type")+
  geom_text(aes(label=paste(percentage, "%")),vjust=-0.4,size=4)

#Part D ---------
ggplot(data = car, aes(x=transmission_type, y=age_of_car, fill=transmission_type))+
  geom_violin( trim=FALSE, alpha = 0.7)+ 
  labs(title="Violin Plot of age of car by transmission type")


#------------------------------> Question 3 <------------------------------
#Part A ---------
ggplot(car, aes(y=age_of_policyholder, x=age_of_car)) + 
  geom_point(size=2, alpha = .07) +
  geom_smooth(method=lm, col = '#8ACA88') +
  labs(title=paste("Scatter Plot of Age of car and Age of policyholder"))

#Part B ---------
ggplot(car, aes(y=age_of_policyholder, x=age_of_car, shape = fuel_type, color = fuel_type)) + 
  geom_point(size=1.5, alpha = 1) +
  labs(title=paste("Scatter Plot of Age of car and Age of policyholder with fuel type"))

#Part C ---------
cor.test(car$age_of_car, car$age_of_policyholder)

#Part D ---------
# bin 7
p7 <- ggplot(car, aes(age_of_car, age_of_policyholder)) +
  geom_point() + 
  geom_hex(bins = 7) + 
  geom_smooth(color='#FFFF00', se=FALSE) + 
  ggtitle("Hexbin Plot - Age of car and policyholder with binsize 7")

ggMarginal(p7, type="histogram", size=3, fill='lightblue')
#-------

# bin 12
p12 <- ggplot(car, aes(age_of_car, age_of_policyholder)) +
  geom_point() + 
  geom_hex(bins = 12) + 
  geom_smooth(color='#FFFF00', se=FALSE) + 
  ggtitle("Hexbin Plot - Age of car and policyholder with binsize 12")

ggMarginal(p12, type="histogram", size=3, fill='lightblue')
#-------

# bin 30
p30 <- ggplot(car, aes(age_of_car, age_of_policyholder)) +
  geom_point() + 
  geom_hex(bins = 30) + 
  geom_smooth(color='#FFFF00', se=FALSE) + 
  ggtitle("Hexbin Plot - Age of car and policyholder with binsize 30")

ggMarginal(p30, type="histogram", size=3, fill='lightblue')
#----

# bin 100
p100 <- ggplot(car, aes(age_of_car, age_of_policyholder)) +
  geom_point() + 
  geom_hex(bins = 100) + 
  geom_smooth(color='#FFFF00', se=FALSE) + 
  ggtitle("Hexbin Plot - Age of car and policyholder with binsize 100")

ggMarginal(p100, type="histogram", size=3, fill='lightblue')

#Part E ---------
ggplot(car, aes(x=age_of_car, y=age_of_policyholder) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") +
  scale_fill_continuous(type = "viridis") +
  labs(title = "2D density plot Age of car and Age of policyholder")

#------------------------------> Question 4 <------------------------------
#Part A ---------

# Some preprocessing
car$is_claim <- as.logical(car$is_claim)
car <- subset(car, select = -c(X))

# Separate Numerical features
car.numeric <- Filter(is.numeric, car)

# correlation matrix
cor_mat <- cor(as.matrix(car.numeric), method="pearson")
cor_mat <- round(cor_mat, 2)

# calc p_values
p.mat <- cor_pmat(car.numeric)

ggcorrplot(
  cor_mat, hc.order = TRUE, type = "upper", 
  lab = TRUE , p.mat = p.mat, sig.level = 0.05,
  title = "heatmap correlogram",
  colors = c("blue", "white", "red"),
  hc.method = "complete", lab_size = 2,
  as.is = FALSE)

#Part B ---------
selected_var = data.frame("policy tenure" = car$policy_tenure,
                          "car's age" = car$age_of_car,
                          "policyholder's age_"=car$age_of_policyholder,
                          "population density"= car$population_density,
                          "displacement"=car$displacement,
                          "turning radius"=car$turning_radius,
                          "length"=car$length,
                          "width"=car$width,
                          "height"=car$height,
                          "gross_weight" = car$gross_weight
                          )


#ggpairs(dplyr::select_if(car, is.numeric), title = "Correlogram",options(expressions=10000))

ggpairs(selected_var, title = "Correlogram",
        lower = list(continuous = wrap("smooth", alpha = 0.1, colour="gray")),
        upper = list(continuous = wrap("density",colour="#8ACA88", alpha = 0.5), combo = "dot_no_facet"))


#Part C ---------
colors <- c("#999999", "#E69F00", "#56B4E9")

fuel_type <- colors[as.numeric(factor(car$fuel_type))]

scatterplot3d(
  main="3D Scatter Plot",
  car$population_density,
  car$age_of_policyholder,
  car$displacement,
  color=fuel_type,
  xlab = "population_density",
  ylab = "age_of_policyholder ",
  zlab = "displacement",
  pch = 16)

legend("right", legend = unique(car$fuel_type), pch = 16, col =  colors)

#------------------------------> Question 5 <------------------------------
#Part A ---------
Total <- sum
t<-as.data.frame.matrix(addmargins(table(car[,c('fuel_type', 'transmission_type')]), FUN = Total))

#Part B ---------
ggplot(car, aes(x =fuel_type , fill = transmission_type)) + 
  geom_bar(position = "dodge", alpha = 0.6)  +
  geom_text(aes(label = ..count..), stat = "count", vjust = -.3,  size = 3, color = 'black', position = position_dodge(width = 1)) +
  labs(title="Grouped Bar Chart", x="fuel_type")

#Part C ---------
ggplot(car, aes(x = fuel_type, fill = transmission_type)) + 
  geom_bar(position = "stack", alpha = 0.6) +
  geom_text(aes(label=..count..), stat='count', vjust = -.3,  size = 3, color = 'black', position = position_stack(0.5)) +
  labs(title=paste("Segmented barplot"))

#Part D ---------
ggplot(data = car) +
  geom_mosaic(aes(x = product(transmission_type), fill = fuel_type ), alpha = 0.6) +
  annotate("text", x = 0.195, y = 0.35,label=paste(round(t[2,1]/t[4,1],2)*100,"%"),fontface = "bold" ,size=4,color="black") +
  annotate("text", x = 0.195, y = 0.855,label=paste(round(t[3,1]/t[4,1],2)*100,"%"), fontface = "bold",size = 4,color="black") +
  annotate("text", x = 0.7, y = 0.25,label=paste(round(t[1,2]/t[4,2],2)*100,"%"),fontface = "bold" ,size = 4,color="black") +
  annotate("text", x = 0.7, y = 0.57,label=paste(round(t[2,2]/t[4,2],2)*100,"%"), fontface = "bold",size = 4,color="black") +
  annotate("text", x = 0.7, y = 0.8,label=paste(round(t[3,2]/t[4,2],2)*100,"%"), fontface = "bold",size = 4,color="black") +
  labs(title=paste("Mosaic plot"))

#------------------------------> Question 6 <------------------------------
#Part A ---------
car_sample <- sample_n(car, 25)
car_sample$diff <- car_sample$policy_tenure - car_sample$displacement

#Part B ---------
t.test(car_sample$policy_tenure, car_sample$displacement, paired = TRUE, var.equal = TRUE)

# calculate CI
xbar1 <- mean(car_sample$policy_tenure)
s1 <- sd(car_sample$policy_tenure)
xbar2 <-  mean(car_sample$displacement)
s2 <- sd(car_sample$displacement)

sp = ((n-1)*s1^2+(n-1)*s2^2)/(n+n-2)
sp

margin <- qt(0.975,df=n+n-1)*sqrt(sp/n + sp/n)
margin

lowerinterval <- (xbar1-xbar2) - margin
lowerinterval

upperinterval <- (xbar1-xbar2) + margin
upperinterval

cat("95% confidence interval is= (", lowerinterval,",",upperinterval,")")

#------------------------------> Question 7 <------------------------------

ggplot(car, aes(x = age_of_policyholder)) +
  geom_histogram(aes(y = ..density..),alpha=0.5, colour = "black", fill = "gray")+
  labs(
    title = "Histogram for age of policyholder",
    x = "age of policyholder",
    y = "count"
  ) +
  geom_density(color = "blue", size = 1)  +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
  )

#Part A ---------
n = 100

# Reads the dataset 'car' and take the 100 rows as sample
sdf<- sample(1:nrow(car), n)

#sample 10 rows
sub_car <- car[sdf,]

xbar = mean(sub_car$age_of_policyholder)
  
ci = 0.98
z = qnorm((1-ci)/2)
  
se <- sd(sub_car$age_of_policyholder) / sqrt(n) #SE = s/sqrt(n)
  
lower <- xbar + z * se
upper <- xbar - z * se

cat("98% confidence interval is= (", lower,",",upper,")")

#Part B ---------
ggplot(sub_car, aes(x = age_of_policyholder)) +
  geom_histogram(aes(y = ..density..),alpha=0.5, colour = "black")+
  labs(
    title = "Histogram for sample of age of policyholder",
    x = "age of policyholder",
    y = "count"
    ) +
  geom_vline(aes(xintercept = mean(age_of_policyholder)), linetype = "dashed", size = .7,col= 'red') +
  geom_vline(aes(xintercept = lower), linetype = "dashed", size = .7,col= 'green') +
  geom_vline(aes(xintercept = upper), linetype = "dashed", size = .7,col= 'blue') +
  geom_vline(aes(xintercept = mean(car$age_of_policyholder)), linetype = "dashed", size = .7,col= 'orange') +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  ) +
  annotate("text", x = mean(sub_car$age_of_policyholder) + 0.007 , label = "sample mean", y = 6, size = 4, angle = 90 , color = 'red') +
  annotate("text", x = lower - 0.01 , label = "lower", y = 6, size = 4, angle = 90, color = 'green') +
  annotate("text", x = upper - 0.01 , label = "upper", y = 6, size = 4, angle = 90, color = 'blue') +
  annotate("text", x = mean(car$age_of_policyholder) - 0.01 , label = "actual mean", y = 6, size = 4, angle = 90, color = 'orange')

#Part D ---------
null.value = 0.469
alpha = 0.02

x_bar <-mean(sub_car$age_of_policyholder)
s <- sd(sub_car$age_of_policyholder)
se <- s/sqrt(length(sub_car$age_of_policyholder))

z_score <- abs((x_bar - null.value)) / se

pvalue <- pnorm(z_score, lower.tail = FALSE)

if (pvalue < alpha) {
  print("Reject null hypothesis.")
} else {
  print("Fail to reject null hypothesis.")
}

#Part F ---------
ci = 0.98 # alpha = 0.02
z = qnorm((1-ci)/2)

s = sd(sub_car$age_of_policyholder)
se <- s / sqrt(n) #SE = s/sqrt(n)

lower <- xbar + z * se
upper <- xbar - z * se

muactual = mean(car$age_of_policyholder)

Zleft <- (lower-muactual)/(se)
Zright <-(upper-muactual)/(se)
b <- pnorm(Zright)-pnorm(Zleft,lower.tail = FALSE)

power = 1-b
power
#-------

# calc effectsize ~ power
d_seq <- seq(0, 2, by = 0.1)
pwr_list <- lapply(d_seq, function(d){
  power.t.test(n = 100, delta = d, sd = sd(sub_car$age_of_policyholder), type="one.sample")
})
pwr <- sapply(pwr_list, '[[', 'power')

dfpwr <- data.frame(power = pwr, effect.size = d_seq)

ggplot(dfpwr, aes(effect.size, power)) +
  geom_point(size = 2, colour = "#8ACA88") +
  geom_line(size = 0.5, colour = "black") +
  scale_y_continuous(labels = scales::percent) +
  xlab("effect size") +
  ylab(expression("test power =" ~ 1 - beta))

#------------------------------> Question 8 <------------------------------
boxplot(car$age_of_car,
        main = "BoxPlot for Age Of Car",
        names = c("age_of_car"),
        xlab = "Age",
        horizontal = TRUE)

#Part A ---------
q <- quantile(car$age_of_car,c(.025,.975))
cat('Confidence Interval for Original population!! : (', q[1],q[2], ')')

ggplot(car, aes(x = age_of_car)) +
  geom_histogram(aes(y = ..density..),alpha=0.5, colour = "black")+
  labs(
    title = "Histogram for age of car",
    x = "age of car",
    y = "count"
  ) +
  geom_vline(aes(xintercept = mean(age_of_car)), linetype = "dashed", size = .7,col= 'red') +
  geom_vline(aes(xintercept = q[1]), linetype = "dashed", size = .7,col= 'green') +
  geom_vline(aes(xintercept = q[2]), linetype = "dashed", size = .7,col= 'blue') +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  ) +
  annotate("text", x = mean(car$age_of_car) - 0.01 , label = "actual mean", y = 7, size = 4, angle = 90 , color = 'red') +
  annotate("text", x =  q[1] - 0.015 , label = "lower", y = 7.5, size = 4, angle = 90, color = 'green') +
  annotate("text", x = q[2] - 0.01 , label = "upper", y = 7, size = 4, angle = 90, color = 'blue') 

#Part B ---------
car.sample = sample_n(car, 20)

boots.dist <- replicate(1000, mean(sample(car.sample$age_of_car, size = 20, replace=TRUE) ))

boots.mean <- mean(boots.dist)
boots.sd = sd(boots.dist)
boots.se = boots.sd / sqrt(1000)

upper = boots.mean + qt((1-0.95)/2, df=1000-1, lower.tail = FALSE) * boots.se
lower = boots.mean - qt((1-0.95)/2, df=1000-1, lower.tail = FALSE) * boots.se

cat('Confidence Interval for bootstrap dusribution : (', lower,upper, ')')
df.boots.dist = data.frame(age_of_car = matrix(data = boots.dist, ncol = 1, byrow = TRUE)) 

ggplot(df.boots.dist, aes(x = age_of_car)) +
  geom_dotplot( stackratio = 1.01,dotsize = .25)+
  labs(
    title = "Dot plot - Bootstrap Distribution for age of car",
    x = "age of car",
    y = "count"
  ) +
  geom_vline(aes(xintercept = mean(age_of_car)), linetype = "dashed", size = .8,col= 'red') +
  geom_vline(aes(xintercept = lower), linetype = "dashed", size = .5,col= 'green') +
  geom_vline(aes(xintercept = upper), linetype = "dashed", size = .5,col= 'blue') +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.caption = element_text(face = "italic")
  ) +
  annotate("text", x = .032 , label = "actual mean", y = .8, size = 4, angle = 0 , color = 'red') +
  annotate("text", x = .03 , label = "lower", y = .77, size = 4, angle = 0, color = 'green') +
  annotate("text", x = .03 , label = "upper", y = .75, size = 4, angle = 0, color = 'blue') 

#Part C ---------
qqnorm(car$age_of_car, pch = 1, frame = FALSE)
qqline(car$age_of_car, col = "steelblue", lwd = 2)


qqnorm(df.boots.dist$age_of_car, pch = 1, frame = FALSE)
qqline(df.boots.dist$age_of_car, col = "steelblue", lwd = 2)


#------------------------------> Question 9 <------------------------------
car.sample = sample_n(car, 200)

#draw box plot
boxplot(car.sample$age_of_car  ~ car.sample$fuel_type, ylab = "Age of car", xlab = "Fuel type",
        main = "side-by-side boxplots") 

#Part A ---------
fisher <- aov(age_of_car  ~  as.factor(fuel_type), data = car.sample)
summary(fisher)

#Part B ---------
gp <- unique(car.sample$fuel_type)
gp
# petrol vs, diesel
Petrol <- car.sample$age_of_car[which(car.sample$fuel_type==gp[1])]
Diesel <- car.sample$age_of_car[which(car.sample$fuel_type==gp[2])]

car.t.test <- t.test(Petrol , Diesel)
car.t.test

# petrol vs, CNG
Petrol <- car.sample$age_of_car[which(car.sample$fuel_type==gp[1])]
CNG <- car.sample$age_of_car[which(car.sample$fuel_type==gp[3])]

car.t.test <- t.test(Petrol , CNG)
car.t.test
