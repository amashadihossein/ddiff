#' obtain and compare custom summary statistics from given two datasets
#'
#' @description obtain and compare custom summary statistics from given two datasets. The comparison are made on three main aspects : continuous, categorical and binary
#' @import ggplot2
#' @param dat1 First Data need to be compared.
#' @param dat2 Second Data need to be compared.
#' @param key Unique identifiers to link two datasets
#' @param ddiff_objective First Data need to be compared.
#' @param ... potential argument to added
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' n = 100
#' d = generate_test_data(n)
#' #result = get_variable_stat(d$identical$new, d$identical$old, "id")
#' #result$result_con
#' #result$result_cat
#' #result$result_bin
#' @export
#'

fpca_multi_stat <- function(dat1, dat2, key, ddiff_objective, ...){
  abc <- function(...){
    library(ddiff)
    d = generate_test_data(50)
    dat2 = d$records_added$new
    dat1 = d$records_added$old
    key = "id"
    measure_arg_con = list(cov = cov)
    n1 = nrow(dat1); d1 = ncol(dat1); n2 = nrow(dat2); d2 = ncol(dat2)
    objective = get_variable_class(dat1, dat2, "id")
    dat1_svd = svd(cov(dat1[, unlist(objective$data_1$continous)]))
    dat1_svd$u
    sqrt(dat1_svd$d)
    prcomp(dat1[, unlist(objective$data_1$continous)])
    #pilots.pca
    dat2_svd = svd(cov(dat2[, unlist(objective$data_2$continous)]))
    ddiff_objective = get_variable_multi_stat(d$records_added$new, d$records_added$old, "id")
    library(fda)
    daybasis65 <- create.fourier.basis(c(0, 365), nbasis=65, period=365)

    harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
    harmfdPar     <- fdPar(daybasis65, harmaccelLfd, lambda=1e5)
    daytempfd <- smooth.basis(day.5, CanadianWeather$dailyAv[,,"Temperature.C"],
                              daybasis65, fdnames=list("Day", "Station", "Deg C"))$fd

    daytemppcaobj <- pca.fd(daytempfd, nharm=4, harmfdPar)
    daytemppcaVarmx <- varmx.pca.fd(daytemppcaobj)
    #  plot harmonics
    op <- par(mfrow=c(2,2))
    plot.pca.fd(daytemppcaobj, cex.main=0.9)

    plot.pca.fd(daytemppcaVarmx, cex.main=0.9)
    par(op)

    plot(daytemppcaobj$harmonics)
    plot(daytemppcaVarmx$harmonics)

    library(fda)
    # setwd("/Users/cao/Dropbox/Teaching/FDA/SummerCourse2018/R")


    # First with canadian weather data
    # The usual

    daybasis365 = create.fourier.basis(c(0,365),365)

    # harmonic acceleration differential operator
    harmLfd = vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
    tempfdPar = fdPar(daybasis365,harmLfd,1e4)
    tempfd = smooth.basis(1:365,daily$tempav,tempfdPar)
    ?smooth.basis

    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(tempfd$fd,xlab='day',ylab='temperature',cex.lab=1.5,cex.axis=1.5)

    daily$place
    length(daily$place)
    # calculate the variance-covariance of the functional data
    tempvar = var.fd(tempfd$fd)
    tvvals = eval.bifd(1:365,1:365,tempvar)
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    contour(1:365,1:365,tvvals,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)
    install.packages("fields")
    library(fields)
    quartz()
    image.plot(1:365,1:365,tvvals,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)

    # Correlation Coefficient
    temp.cor = cor.fd(1:365,tempfd$fd)
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    contour(1:365,1:365,temp.cor,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)
    image.plot(1:365,1:365,temp.cor,xlab='day',ylab='day',cex.lab=1.5,cex.axis=1.5)


    ?smooth.basis
    # Do functional principal component analysis on the 35 temperature curves
    # We choose 4 FPCs
    ?pca.fd
    temppca = pca.fd(tempfd$fd,nharm=4)
    names(temppca)
    temppca$varprop
    #temppca$values are the eigenvalues
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(temppca$values[1:8],xlab='component',ylab='variance',col="red",
         cex.lab=1.5,cex.axis=1.5,cex=2)

    # plot the cumulative percentage explained total variations
    # It shows that the top 3 FPCs explains more than 99% of total variations
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(cumsum(temppca$values[1:10])/sum(temppca$values),xlab='Number of Components',
         ylab='cumulative variance explained',col=2,cex.lab=2,
         cex.axis=2,cex=2)
    abline(h=0.99)

    # Show the mean curves - temppca$meanfd
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(tempfd$fd,xlab='day',ylab='temperature',cex.lab=1.5,cex.axis=1.5,col=4)
    lines(temppca$meanfd,lwd=2.5,col=2)

    # functional principal components
    harmfd = temppca$harmonics
    harmvals = eval.fd(1:365,harmfd)
    dim(harmvals) # The top 4 FPCs

    # plot the second FPC
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(1:365,harmvals[,2],xlab='day',ylab='PCs',
         lwd=4,lty=1,cex.lab=2,cex.axis=2,type='l')

    # plot all 4 FPCs
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    matplot(1:365,harmvals,xlab='day',ylab='PCs',
            lwd=4,lty=1,cex.lab=2.5,cex.axis=2.5,type='l')
    legend(0,-0.07,c('PC1','PC2','PC3','PC4'),col=1:4,lty=1,lwd=5)
    title('Temperature Principle Component Functions')

    # plot the first FPC scores vs. the second FPC scores
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(temppca$scores[,1:2],xlab='PC Score 1',ylab='PC Score 2',col=4,
         cex.lab=1.5,cex.axis=1.5,cex=1)
    text(temppca$scores[,1],temppca$scores[,2],labels=daily$place,cex=1)

    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(tempfd$fd[17],xlab='day',ylab='temperature',cex.lab=2.5,cex.axis=2.5,col="black",lwd=4,ylim=c(-40,20))
    lines(tempfd$fd[24],xlab='day',ylab='temperature',cex.lab=2.5,cex.axis=2.5,col="red",lwd=4)
    lines(tempfd$fd[35],xlab='day',ylab='temperature',cex.lab=2.5,cex.axis=2.5,col="blue",lwd=4)
    legend(-1, 15, c("Winnipeg", "Calgary","Resolute"),col = c("black","red","blue"),lty=1,lwd=4)

    daily$place[c(17,24,35)]
    # Remove the mean function
    quartz()
    par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
    plot(tempfd$fd[17]-temppca$meanfd,xlab='day',ylab='temperature',cex.lab=2.5,cex.axis=2.5,col="black",lwd=4,ylim=c(-40,20))
    lines(tempfd$fd[24]-temppca$meanfd,xlab='day',ylab='temperature',cex.lab=2.5,cex.axis=2.5,col="red",lwd=4)
    lines(tempfd$fd[35]-temppca$meanfd,xlab='day',ylab='temperature',cex.lab=2.5,cex.axis=2.5,col="blue",lwd=4)
    legend(-1, 15, c("Winnipeg", "Calgary","Resolute"),col = c("black","red","blue"),lty=1,lwd=4)

  ##functional pca
  n = 100
  d = generate_test_data1(n)
  dat1 = d$records_added$old
  dat2 = d$records_added$new
  head(dat1)
  summary(dat1)
  contin_dat1 <- cbind(dat1$nm_1, dat1$nm_2, dat1$nm_3)
  library(fda)
  ?smooth.basis
  basisfd <- create.bspline.basis(rangeval=c(0, 10), nbasis=NULL, norder=4, breaks=NULL)
  datafdPar <- fdPar(basisfd, 2, 1e-6)
  tempfd = smooth.basis(dat1$time, contin_dat1, datafdPar)
  dev.off()
  quartz()
  plot(tempfd$fd,xlab='day',ylab='normal response',cex.lab=1.5,cex.axis=1.5)
  points(dat1$time, dat1$nm_1, col = "red")
  points(dat1$time, dat1$nm_2, col = "blue")
  points(dat1$time, dat1$nm_3, col = "black")
  temppca = pca.fd(tempfd$fd,nharm=2)


  # plot the cumulative percentage explained total variations
  # It shows that the top 3 FPCs explains more than 99% of total variations
  quartz()
  par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
  plot(cumsum(temppca$values[1:2])/sum(temppca$values),xlab='Number of Components',
       ylab='cumulative variance explained',col=2,cex.lab=2,
       cex.axis=2,cex=2)
  abline(h=0.99)

  harmfd = temppca$harmonics
  harmvals = eval.fd(1:10, harmfd)
  #quartz()
  par(mfrow=c(1,1),mar = c(8, 8, 4, 2))
  matplot(1:10,harmvals,xlab='day',ylab='PCs',
          lwd=4,lty=1,cex.lab=2.5,cex.axis=2.5,type='l')
  legend(0,-0.07,c('PC1','PC2'),col=1:2,lty=1,lwd=5)
  title('Temperature Principle Component Functions')
  }


  return(p)
}
