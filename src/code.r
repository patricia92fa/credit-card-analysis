#Import data
library(readxl)
default <- read_excel("~/data/default of credit card clients.xls", 
    col_names = FALSE, skip = 1)
View(default)

#Remame field
names(default)[names(default) == 'default payment next month'] <- 'DEFAULT_RES'

#Check for null values
apply(default, 2, function(x) any(is.na(x)))

#Check value distribution for attribute
table(default$EDUCATION)
#Plot histogram for attribute
hist(default$EDUCATION,
      main = "Distribucion valores nivel educativo",
      ylab = "Frecuencia",
      xlab = "Nivel educativo",
      col = "springgreen")

#Check percentage of zero values
(sum(default$EDUCATION==0)/sum(default$EDUCATION!=0))*100
#Substitute multiple values
default[default$EDUCATION %in% c(0,5,6),]<-4

#Substitute values that meet a condition
default <- default[which(MARRIAGE!=0),]

#Plot boxplot for attribute
boxplot.stats(default$LIMIT_BAL,
col=alpha("springgreen", 0.3))

#Create subgroups by attribute value
default_true <- subset(default, DEFAULT_RES == "1") 
default_false <- subset(default, DEFAULT_RES == "0")

#Compute probability cumulative function for set of attributes
ecdf(default_false[1,c(13:18)])

#Add attribute as sum of N attributes
default <- transform(default,
AMT_SUM=rowSums(default[,c(13:18)]))

#Compute scattering matrix for multiple attributes
pairs(default[,c(6,26)], col=alpha("springgreen", 0.3))
