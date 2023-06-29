library("readtext")
library(reshape2)
library(ggplot2)


file <- "C:/Users/gasto/OneDrive/Desktop/test.txt"

data <- read.table(file, header = TRUE, sep = " ", dec = ".")

data <- data.frame(x=unlist(data))

st <- boxplot.stats(data$A)
st
out_index <- which(data %in% st$out)
A <- data$A[!(data$A %in% out_index)]


st <- boxplot.stats(data$B)
st
out_index <- which(data %in% st$out)
B <- data$B[!(data$B %in% out_index)]

data <- stack(list(A = A, B = B))
data <- na.omit(data)


ggplot(don, aes(y=E, x=Concentration,colour=Concentration,fill=Concentration))+
  geom_boxplot(alpha=0.5, outlier.alpha=0)+
  geom_jitter(width=0.25)+
  theme_classic()



#perform Kolmogorov-Smirnov test (normality test, p<0.05 then data not normal)
ks.test(A, B)

#Shapiro–Wilk Test for normality (p<0.05 then not normal)
shapiro.test(A)
shapiro.test(B)

#t-test
t.test(A, B, alternative = "two.sided", var.equal = FALSE)

#Wilcoxon test
wilcox.test(A, B, alternative = "two.sided")





