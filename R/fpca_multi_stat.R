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

  #### settings
  I <- 50 # number of subjects
  J <- 3000 # dimension of the data
  t <- (1:J)/J # a regular grid on [0,1]
  N <- 4 #number of eigenfunctions
  sigma <- 2 ##standard deviation of random noises
  lambdaTrue <- c(1,0.5,0.5^2,0.5^3) # True eigenvalues
  case = 1
  ### True Eigenfunctions
  if(case==1) phi <- sqrt(2)*cbind(sin(2*pi*t),cos(2*pi*t),
                                   sin(4*pi*t),cos(4*pi*t))
  if(case==2) phi <- cbind(rep(1,J),sqrt(3)*(2*t-1),
                           sqrt(5)*(6*t^2-6*t+1),
                           sqrt(7)*(20*t^3-30*t^2+12*t-1))
  ###################################################
  ######## Generate Data #############
  ###################################################
  library(refund)
  xi <- matrix(rnorm(I*N),I,N);
  xi <- xi %*% diag(sqrt(lambdaTrue))
  X <- xi %*% t(phi); # of size I by J
  Y <- X + sigma*matrix(rnorm(I*J),I,J)
  nrow(Y)
  ncol(Y)
  ?fpca.face
  results <- fpca.face(Y, center = TRUE, argvals=t, knots=100, pve=0.99)
  ###################################################
  #### FACE ########
  ###################################################
  Phi <- results$efunctions
  eigenvalues <- results$evalues
  score <- results$scores
  check_Phi <- t(Phi %*% t(score))
  nrow(check_Phi)
  ncol(check_Phi)
  sum(abs(check_Phi[1, ] - Y[1,]))
  for(k in 1:N){
    if(Phi[,k] %*% phi[,k]< 0)
      Phi[,k] <- - Phi[,k]
  }
  ### plot eigenfunctions
  par(mfrow=c(N/2,2))
  seq <- (1:(J/10))*10
  for(k in 1:N){
    plot(t[seq],Phi[seq,k]*sqrt(J),type="l",lwd = 3,
         ylim = c(-2,2),col = "red",
         ylab = paste("Eigenfunction ",k,sep=""),
         xlab="t",main="FACE")
    lines(t[seq],phi[seq,k],lwd = 2, col = "black")
  }
  }

  fdata <- generate_test_functional_data()
  ?fpca.lfda

  ## as in Sec. 6.2 of Huang, Shen, Buja (2008):
  set.seed(2678695)
  n <- 101
  m <- 101
  s1 <- 20
  s2 <- 10
  s <- 4
  t <- seq(-1, 1, l=m)
  v1 <- t + sin(pi*t)
  v2 <- cos(3*pi*t)
  V <- cbind(v1/sqrt(sum(v1^2)), v2/sqrt(sum(v2^2)))
  U <- matrix(rnorm(n*2), n, 2)
  D <- diag(c(s1^2, s2^2))
  eps <- matrix(rnorm(m*n, sd=s), n, m)
  Y <- U%*%D%*%t(V) + eps

  smoothSV <- fpca.ssvd(Y, verbose=TRUE)

  layout(t(matrix(1:4, nr=2)))
  clrs <- sapply(rainbow(n), function(c)
    do.call(rgb, as.list(c(col2rgb(c)/255, .1))))
  matplot(V, type="l", lty=1, col=1:2, xlab="",
          main="FPCs: true", bty="n")
  matplot(smoothSV$efunctions, type="l", lty=1, col=1:5, xlab="",
          main="FPCs: estimate", bty="n")
  matplot(1:m, t(U%*%D%*%t(V)), type="l", lty=1, col=clrs, xlab="", ylab="",
          main="true smooth Y", bty="n")
  matplot(1:m, t(smoothSV$Yhat), xlab="", ylab="",
          type="l", lty=1,col=clrs, main="estimated smooth Y", bty="n")

  ?pca.fd
  library(fda)
  daybasis65 <- create.fourier.basis(c(0, 365), nbasis=65, period=365)

  harmaccelLfd <- vec2Lfd(c(0,(2*pi/365)^2,0), c(0, 365))
  harmfdPar     <- fdPar(daybasis65, harmaccelLfd, lambda=1e5)
  daytempfd <- smooth.basis(day.5, CanadianWeather$dailyAv[,,"Temperature.C"],
                            daybasis65, fdnames=list("Day", "Station", "Deg C"))$fd

  daytemppcaobj <- pca.fd(daytempfd, nharm=4, harmfdPar)

  ########################################
  ### Illustration with simulated data ###
  ########################################

  ###########################################################################################
  # data generation
  ###########################################################################################
  set.seed(1)
  n <- 100 # number of subjects
  ss <- seq(0,1,length.out=101)
  TT <- seq(0, 1, length.out=41)
  mi <- runif(n, min=6, max=15)
  ij <- sapply(mi, function(a) sort(sample(1:41, size=a, replace=FALSE)))

  # error variances
  sigma <- 0.1
  sigma_wn <- 0.2

  lambdaTrue <- c(1,0.5) # True eigenvalues
  eta1True <- c(0.5, 0.5^2, 0.5^3) # True eigenvalues
  eta2True <- c(0.5^2, 0.5^3) # True eigenvalues

  phi <- sqrt(2)*cbind(sin(2*pi*ss),cos(2*pi*ss))
  psi1 <- cbind(rep(1,length(TT)), sqrt(3)*(2*TT-1), sqrt(5)*(6*TT^2-6*TT+1))
  psi2 <- sqrt(2)*cbind(sin(2*pi*TT),cos(2*pi*TT))

  zeta1 <- sapply(eta1True, function(a) rnorm(n = n, mean = 0, sd = a))
  zeta2 <- sapply(eta2True, function(a) rnorm(n = n, mean = 0, sd = a))

  xi1 <- unlist(lapply(1:n, function(a) (zeta1 %*% t(psi1))[a,ij[[a]]] ))
  xi2 <- unlist(lapply(1:n, function(a) (zeta2 %*% t(psi2))[a,ij[[a]]] ))
  xi <- cbind(xi1, xi2)

  Tij <- unlist(lapply(1:n, function(i) TT[ij[[i]]] ))
  i <- unlist(lapply(1:n, function(i) rep(i, length(ij[[i]]))))
  j <- unlist(lapply(1:n, function(i) 1:length(ij[[i]])))

  X <- xi %*% t(phi)
  meanFn <- function(s,t){ 0.5*t + 1.5*s + 1.3*s*t}
  mu <- matrix(meanFn(s = rep(ss, each=length(Tij)), t=rep(Tij, length(ss)) ) , nrow=nrow(X))

  Y <- mu +  X +
    matrix(rnorm(nrow(X)*ncol(phi), 0, sigma), nrow=nrow(X)) %*% t(phi) + #correlated error
    matrix(rnorm(length(X), 0, sigma_wn), nrow=nrow(X)) # white noise

  matplot(ss, t(Y[which(i==2),]), type='l', ylab="", xlab="functional argument",
          main="observations from subject i = 2")

  nrow(Y)
  est <- fpca.lfda(Y = Y,
                   subject.index = i, visit.index = j, obsT = Tij,
                   funcArg = ss, numTEvalPoints = length(TT),
                   newdata = data.frame(i = c(1:3), Ltime = c(Tij[1], 0.2, 0.5)),
                   fbps.knots = 35, fbps.p = 3, fbps.m = 2,
                   LongiModel.method='fpca.sc',
                   mFPCA.pve = 0.95, mFPCA.knots = 35, mFPCA.p = 3, mFPCA.m = 2,
                   sFPCA.pve = 0.95, sFPCA.nbasis = 10, sFPCA.npc = NULL,
                   gam.method = 'REML', gam.kT = 10)

  par(mfrow=c(1,2))
  persp(x=TT, y = ss, z= t(sapply(TT, function(a) meanFn(s=ss, t = a))),
        xlab="visit times", ylab="s", zlab="true mean fn")
  persp(x = TT, y = ss, est$bivariateSmoothMeanFunc,
        xlab="visit times", ylab="s", zlab="estimated mean fn", col='light blue')
  par(mfrow=c(1,2))

  # marginal covariance fn (true vs. estimated)
  image(phi%*%diag(lambdaTrue)%*%t(phi))
  image(est$mFPCA.covar)

  # eigenfunctions (true vs. estimated)
  matplot(ss, phi, type='l')
  matlines(ss, cbind(est$mFPCA.efunctions[,1], est$mFPCA.efunctions[,2]), type='l', lwd=2)

  # scree plot
  plot(cumsum(est$mFPCA.scree.eval)/sum(est$mFPCA.scree.eval), type='l',
       ylab = "Percentage of variance explained")
  points(cumsum(est$mFPCA.scree.eval)/sum(est$mFPCA.scree.eval), pch=16)


  par(mfrow=c(1,2))
  # fitted
  matplot(ss, t(Y[which(i==1),]), type='l', ylab="", xlab="functional argument")
  matlines(ss, t(est$fitted.values[which(i==1),]), type='l', lwd=2)

  # sanity check : expect fitted and predicted (obtained using info from newdata)
  #                values to be the same

  plot(ss, est$fitted.values[1,], type='p', xlab="", ylab="", pch = 1, cex=1)
  lines(ss, est$predicted.values[1,], type='l', lwd=2, col='blue')
  all.equal(est$predicted.values[1,], est$fitted.values[1,])

  data(DTI)
  MS <- subset(DTI, case ==1)  # subset data with multiple sclerosis (MS) case

  index.na <- which(is.na(MS$cca))
  Y <- MS$cca
  Y[index.na] <- fpca.sc(Y)$Yhat[index.na];
  sum(is.na(Y))
  id <- MS$ID
  visit.index <- MS$visit
  visit.time <- MS$visit.time/max(MS$visit.time)

  lfpca.dti <- fpca.lfda(Y = Y, subject.index = id,
                         visit.index = visit.index, obsT = visit.time,
                         LongiModel.method = 'lme',
                         mFPCA.pve = 0.95)

  TT <- seq(0,1,length.out=41); ss = seq(0,1,length.out=93)

  # estimated mean function
  persp(x = ss, y = TT, z = t(lfpca.dti$bivariateSmoothMeanFunc),
        xlab="s", ylab="visit times", zlab="estimated mean fn", col='light blue')

  # first three estimated marginal eigenfunctions
  matplot(ss, lfpca.dti$mFPCA.efunctions[,1:3], type='l', xlab='s', ylab='estimated eigen fn')

  # predicted scores function corresponding to first two marginal PCs
  matplot(TT, do.call(cbind, lapply(lfpca.dti$sFPCA.xiHat.bySubj, function(a) a[,1])),
          xlab="visit time (T)", ylab="xi_hat(T)", main = "k = 1", type='l')
  matplot(TT, do.call(cbind, lapply(lfpca.dti$sFPCA.xiHat.bySubj, function(a) a[,2])),
          xlab="visit time (T)", ylab="xi_hat(T)", main = "k = 2", type='l')

  # prediction of cca of first two subjects at T = 0, 0.5 and 1 (black, red, green)
  matplot(ss, t(lfpca.dti$fitted.values.all[[1]][c(1,21,41),]),
          type='l', lty = 1, ylab="", xlab="s", main = "Subject = 1")
  matplot(ss, t(lfpca.dti$fitted.values.all[[2]][c(1,21,41),]),
          type='l', lty = 1, ylab="", xlab="s", main = "Subject = 2")


}
