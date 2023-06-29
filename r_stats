library("readtext")
library(reshape2)


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


