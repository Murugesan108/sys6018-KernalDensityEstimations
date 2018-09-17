### KERNEL DENSITY ENSTIMATION ###

#Sample for univariate
#Getting samples from multiple normal distributions
sample_points <- c(rnorm(40,mean = 10,sd = 3 ),rnorm(40,mean = 5,sd =1 ),rnorm(40,mean = -5,sd =1 ) )
x <- seq(min(sample_points),max(sample_points),0.1)
h <- 1



#Function to execute one dimentional kde
est.kde.univariate = function(x, sample, bandwidth) {
  
  
  KDE_1d <- c()
  
  #Looping through each x values to find its corresponding KDE
  for(each in x){
    
    K_input = (each - sample)/bandwidth
    K_value = (1/sqrt(2 * pi)) * exp(-K_input^2/2)
    
    K_average <- sum(K_value)/(length(sample) * bandwidth)
    KDE_1d <- c(KDE_1d, K_average)
  }
  
  return(KDE_1d)
  
}

kde_points <- est.kde.univariate(x,sample_points,h)




#Plotting the KDE for different bandwidth values
hist(sample_points, breaks = 50, freq = FALSE)
lines(x, est.kde.univariate(x,sample_points,0.1) ,col = "red", type = "o")
lines(x, est.kde.univariate(x,sample_points,0.5) ,col = "blue", type = "o")
lines(x, est.kde.univariate(x,sample_points,1) ,col = "green", type = "o")
lines(x, est.kde.univariate(x,sample_points,10) ,col = "yellow", type = "o")



######## 2 DIMENSIONAL ##########

sample_points_x1 <- c(rnorm(40,mean = 10,sd = 0.5 ),rnorm(40,mean = 5,sd =1 ))
sample_points_x2 <- c(rnorm(40,mean = 2,sd = 1 ),rnorm(40,mean = 9,sd = 2 ))
sample_2d <- data.frame(sample_points_x1,sample_points_x2)

x_1 <- seq(min(sample_points_x1)-2,max(sample_points_x1)+2,0.3)
x_2 <- seq(min(sample_points_x2)-2,max(sample_points_x2)+2,0.3)


x_2d <- as.matrix(expand.grid(x_1,x_2))
bandwidth_2d <- c(0.9,0.9)


## Calculating th KDE for 2-dimensional points

#x and sample are dataframe with 2 dimenstions (2 varaibles)

#x <- x_2d
#sample <- sample_2d
#bandwidth <- bandwidth_2d

est.kde.bivariate = function(x, sample, bandwidth) {
  
  kde_2d <- c()
  
  for(each in 1:nrow(x)){
    #print(each)
    
    
    x_diff_1 <- x[each,][1] - sample[1]
    x_diff_2 <- x[each,][2] - sample[2]
    
    difference_sample <- data.frame(x_diff_1,x_diff_2)
    
    diff_by_h <- dplyr::transmute(difference_sample,x1 = sample_points_x1/bandwidth[1],
                                    x2 = sample_points_x2/bandwidth[2])
    
    z_value <- rowSums(diff_by_h^2)
    bivariate <- (1/(2*pi)) * exp(-z_value/2)
    
    kde_2d <- c(kde_2d,sum(bivariate)/(prod(bandwidth) * nrow(sample)))
    
    
  }
  return(kde_2d)
  
}


kde_2d_points <- est.kde.bivariate(x_2d,sample_2d,bandwidth_2d)


library(RColorBrewer)
my.cols <- rev(brewer.pal(k, "RdYlBu"))

plot(sample_2d , pch=19, cex=0.8)
contour(x =x_1,y=x_2,matrix(kde_2d_points,nrow = length(x_1) ), 
        drawlabels=FALSE,col = my.cols, nlevels = 50, add = TRUE)


