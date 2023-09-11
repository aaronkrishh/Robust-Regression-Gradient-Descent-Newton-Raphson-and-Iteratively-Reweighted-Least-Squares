
###Q1
kurtosis <- function(x){((sum((x-mean(x))^4)/length(x))/(sum((x-mean(x))^2)/length(x))^2)-3}

kurtosis2 <- function(x){((sum((x-mean(x))^4)/length(x))/(sum((x-mean(x))^2)/(length(x)-1)^2))-3}

set.seed(341)
pop <- rt(1000,10)

sc = function(y.pop, y, attr) {
  N <- length(y.pop) +1
  sapply( y, function(y.new) { N*(attr(c(y.new, y.pop)) - attr(y.pop)) } )
}

par(mfrow=c(1,1))
plot(y, (sc(pop,y,kurtosis)), type = 'l',
     main = "Sensitivity curve for the Kurtosis",
     ylab="sensitivity")
plot(y, (sc(pop,y,kurtosis)), type = 'l', 
     main = "Sensitivity curve for the Kurtosis (zoomed)",
     ylab="sensitivity",
     xlim=c(-5,5),ylim=c(-20,20))

##Q2
directory <- "C:/Users/Aaron/OneDrive/Documents/TERM 3A/STAT 341/A1/data"
dirsep <-"/"
filename <- paste(directory, "data.csv", sep=dirsep)
data <- read.csv(filename, header=TRUE)


plot(data$SepalWidth, data$SepalLength, 
     col = factor(data$Species), 
     pch = 16,
     main = "Sepal Width VS Sepal Length",
     xlab = "Sepal Width",
     ylab = 'Sepal Length')

legend("topright",
       legend = levels(factor(data$Species)),
       pch = 16,
       col = factor(levels(factor(data$Species))))

medians <- sapply(levels(factor(data$Species)), 
                  function(i) median(data$SepalLength[data$Species==i]))

stripchart(data$SepalLength~data$Species, 
           vertical = TRUE, 
           method = 'jitter', 
           col  = factor(levels(factor(data$Species))), 
           pch = 16
           )
points(medians, col = 'blue', pch = 17, cex = 1.5)
lines(medians, col = 'blue')

par(mfrow=c(1,3))
for (i in levels(factor(data$Species))){
  plot( data$PetalWidth[data$Species == i],
       data$SepalLength[data$Species == i],
       main = paste("Sepal Width VS Sepal Length", i),
       col=adjustcolor("black", alpha = 0.3 ),
       pch = 16)
  points(mean(data$PetalWidth[data$Species == i]),
        mean(data$SepalLength[data$Species == i]), 
         col = 'red', pch = 16)
  
}

drawBoxPlot <- function(df) {
  # number of columns in the data frame
  m <- ncol(df)
  
  boxplot_stats <- sapply(df, function(x) boxplot.stats(x)$stats)
  
  
  # loop through each column and calculate the five-number summary
  plot(0,0, main = 'BoxPlot Function', xlab = 'Columns', ylab = 'Measurement (mm)' ,xlim = c(1,m+.5), ylim = range(unlist(boxplot_stats)),xaxt = 'n')
  axis(1, at = seq(1.5, m+0.5, 1), labels = colnames(df))
  for (i in 1:m) {
    col <- df[,i]
    stats <- boxplot_stats[,i]
    
    
    # extract the five-number summary
    min_val <- stats[1]
    max_val <- stats[5]
    medians <- stats[3]
    q1 <- stats[2]
    q3 <- stats[4]
    
    # calculate the inter-quartile range (IQR)
    IQR <- q3 - q1
    
    # calculate the lower and upper limits for the whiskers
    lower_limit <- q1 - 1.5 * IQR
    upper_limit <- q3 + 1.5 * IQR
    
    whisker_min <- min(col[col >= (lower_limit)])
    whisker_max <- max(col[col <= (upper_limit)])
    
    outliers <-  col[(col < whisker_min) | (col > whisker_max)]
    
    # plot the boxplot
    rect(i + 0.25, q1, i+0.75, q3)
    segments(i + 0.25, medians, i+0.75, medians, col = 'red')
    segments(i + 0.25, whisker_max, i+0.75, whisker_max, col = 'red')
    segments(i + 0.25, whisker_min, i+0.75, whisker_min, col = 'red')
    points(rep(i +.5, length(outliers)),outliers)
  }
}




