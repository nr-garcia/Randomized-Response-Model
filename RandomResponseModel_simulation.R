#****************************Start Simulation Code**************************************
#************************************************************************************
nTot <- 1000 #number of experiments
n <- 1000 #number of observations per experiment
theta1 <- 0.6 #fixed parameter, corresponds to probability of choosing question "1"
theta2 <- 0.3 #fixed parameter, corresponds to probability of choosing question "2"
theta3 <- 0.1 #fixed parameter, corresponds to probability of choosing question "3"
alpha <- 0.5 #fixed parameter, used as binomial parameter when question "3" is chosen
p_start <- 0.01 #fixed parameter, starting "true" proportion
p_end <- 0.99 #fixed parameter, ending "true" proportion
p_increments <- 10 #fixed parameter, number of increments for "true" proportion
p <- p_start #set initial p
p_hat <- as.data.frame(matrix(0,nrow=n,ncol=p_increments)) #initialize p_hat
var_p_hat <- as.data.frame(matrix(0,nrow=n,ncol=p_increments)) #initialize var_p_hat
for (k in 1:p_increments) { #loop over number of increments
  p_prelim <- as.data.frame(t(cbind(t(rep(c(1),round(p*n))),t(rep(c(0),round(n-p*n))))))
  colnames(p_prelim) <- "p"
  Results <- as.data.frame(matrix(c(rep(0,nTot))))
  for (j in 1:nTot) { #loop over number of experiments
    x <- t(as.data.frame((rmultinom(n, size=1, prob=c(theta1,theta2,theta3)))))
    y <- t(c(rep(0,n)))
    colnames(x) <- c("theta1","theta2", "theta3")
    FinalDataFrame <- cbind(p_prelim, x, alpha, y)
    for (i in 1:n) { #loop over number of observations
      if( FinalDataFrame$theta1[i] == 1 ) FinalDataFrame$y[i] <- FinalDataFrame$p[i]
      else if( FinalDataFrame$theta2[i] == 1 ) FinalDataFrame$y[i] <- (FinalDataFrame$p[i])*(-1)+1
      else if( FinalDataFrame$theta3[i] == 1 ) FinalDataFrame$y[i] <- rbinom(1,1,prob=alpha)
    }
    Results[j,1] <- sum(FinalDataFrame$y)
  }
  p <- p + (p_end-p_start)/(p_increments-1)
  p_hat[,k] <- ((Results/n)-(theta2+theta3*alpha))/(theta1-theta2)
  var_p_hat[,k] <- ((Results/n)*(1-(Results/n)))/(n*(theta1-theta2)^2)
}


  #****************************Start Graph Code******************************************
  #************************************************************************************
  #plot p_hat histograms

  z <- p_start
get( getOption( "device" ) )()
par(mfrow=c(4,3), oma=c(2,2,2,2))
#frame()
for (g in 1:p_increments) {
  hist(p_hat[,g],lwd=2,breaks=100,main=paste("p = ",round(z,digits=3)),xlab=expression(hat(p)))
  #hist(p_hat[,g],lwd=2, breaks = c(seq(min(p_hat[,g])*0.95,max(p_hat[,g])*1.05,by=(max(p_hat[,g])*1.05-min(p_hat[,g])*0.95)/100)))
  abline(v=z,col="red")
  z <- z + (p_end-p_start)/(p_increments-1)}
mtext(bquote(theta[1]==.(theta1)~" "~theta[2]==.(theta2)~" "~theta[3]==.(theta3)~" "~alpha==.(alpha)), outer=T)
#plot var(p_hat) histograms
zz <- p_start
get( getOption( "device" ) )()
par(mfrow=c(4,3), oma=c(2,2,2,2))
for (g in 1:p_increments) {
  p_star <- (zz*theta1+(1-zz)*theta2+alpha*theta3)
  var_p <- ((p_star)*(1-(p_star)))/(n*(theta1-theta2)^2)
  hist(var_p_hat[,g],lwd=2,breaks=100,main=bquote("p = "~.(zz)~", var("~hat(p)~") = "~.(var_p),5),xlab=expression("var("~hat(p)~")"))
  #hist(var_p_hat[,g],lwd=2, breaks = c(seq(min(var_p_hat[,g])*0.95,max(var_p_hat[,g])*1.05,by=(max(var_p_hat[,g])*1.05-min(var_p_hat[,g])*0.95)/100)))
  abline(v=var_p,col="red")
  zz <- zz + (p_end-p_start)/(p_increments-1)}
mtext(bquote(theta[1]==.(theta1)~" "~theta[2]==.(theta2)~" "~theta[3]==.(theta3)~" "~alpha==.(alpha)), outer=T)
#plot various group boxplots
Group1 <- Trial_p[Trial_p$Trial %in% c("T04","T05","T06","T14","T15"),]
Group2 <- Trial_p[Trial_p$Trial %in% c("T01","T02","T03","T08","T11","T12","T13","T19"),]
Group3 <- Trial_p[Trial_p$Trial %in% c("T07","T09","T10"),]
Group4 <- Trial_p[Trial_p$Trial %in% c("T16","T17"),]
Trial_p <- subset(Trial_var,Trial!="T17")
p <- ggplot(Trial_var_v1, aes(factor(Trial), P1))
p + geom_boxplot()
g1<-ggplot(Group4, aes(factor(Trial), P1)) + geom_boxplot() + xlab("Trial") + ggtitle("Simulation Data p = 0.10") + ylab(expression(hat("p")))+geom_hline(aes(yintercept=0.1), colour="#990000", linetype="dashed")
Appendix II
- 38 -
  grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,ncol=3,main="Group 1 Trials by Proportion - Truncated")
#scatterplots for var(p_hat) vs theta1-theta2
par(mfrow=c(3,3), oma = c(0, 0, 2, 0))
for (i in 1:9) {
  plot(Theta_v_var[,8+i]~Theta1.Theta2, xlab=bquote(theta[1]~" - "~theta[2]),
       ylab=expression("var("~hat(p)~")"), main=paste("p = ",i/10), cex=0.75, data=Theta_v_var)
  grid(5, 5, lwd = 1)
  lines(smooth.spline(Theta_v_var[,8+i]~Theta_v_var[,8]), col='red', lwd=1)}
mtext(expression("var("~hat(p)~") vs. "~theta[1]~"-"~theta[2]), outer = TRUE, cex = 1)
scatterplot(Theta1.Theta2~P1, reg.line=lm, smooth=TRUE, spread=TRUE,
            id.method='mahal', id.n = 2, boxplots='xy', span=0.5, data=Theta_v_var)
breaks=100,main=bquote("p = "~.(zz)~", var("~hat(p)~") = "~.(var_p),5),xlab=expression("var("~hat(p)~")"))
#plots for posterior and prior
windows(width=8, height=5)
plot(post, cex=c(0.3), main="Prior and Posterior Distribution", xlab="p", ylab="Density")
lines(z, kernel_prior(z), cex=c(0.3), col=2)
abline(v=hpd1_post[1], col="red")
abline(v=hpd1_post[2], col="red")
#abline(v=Exp_p) #add vertical line for mean
#abline(v=median_p,col=2)
#abline(v=mode_p,col=3)
abline(v=credint_post[1])
abline(v=credint_post[2])
legend('topright', c("Posterior","Prior"), lty=c(1,1), lwd=c(1,1),col=c("black","red"))


  #****************************Start Bayesian Analysis Code*********************************
  #************************************************************************************
a <- 1 #define beta prior parameter
b <- 1 #define beta prior parameter
theta1 <- 0.6 #define likelihood parameter
theta2 <- 0.1 #define likelihood parameter
theta3 <- 1-theta1-theta2
alpha <- 0.5
n <- 100
S <- 50
x <- seq(0,1,by=c(0.000001))
y <- as.data.frame(matrix(c(rep(0,10000))))
z <- seq(0,1,by=c(0.00001))
zz <- as.data.frame(matrix(c(rep(0,10000))))
#NC <- 1/as.numeric(integrate(prop_post,lower=0,upper=1)[1]) #normalizing constant
Bayes_Factor <- as.numeric(integrate(kernel_post, lower=0.65, upper=1)[1])/as.numeric(integrate(kernel_post, lower=0, upper=0.65)[1])
kernel_prior <- function(p) {(p^(a-1))*((1-p)^(b-1))}
like <- function(p) {(choose(n,S))*((1-(p*theta1+(1-p)*theta2+alpha*theta3))^(n-S))*
                       ((p*theta1+(1-p)*theta2+alpha*theta3)^(S))}
kernel_post <- function(p) {(prior(p))*((1-(p*theta1+(1-p)*theta2+alpha*theta3))^(n-S))*
                              (((p*theta1+(1-p)*theta2+alpha*theta3)^(S)))}
post <- function(p) {(1/as.numeric(integrate(kernel_post,lower=0,upper=1)[1]))*kernel_post(p)}
Exp_post <- function(p) {p*post(p)}
Var_post <- function(p) {p*p*post(p)}
#NC_Special_Case <- 1/(as.numeric(beta(a+S,b+n-S)))
#Exp_Special_Case <- (S+a)/(b+n+a)
#Var_Special_Case <- ((S+a)*(n-S+b))/(((a+b+n)^2)*(a+b+n+1))
median_post <- function(y) {
  for(i in 1:nrow(y)) {
    y[i,1] <- integrate(post,lower=0,upper=i/nrow(y))[1]}
  which(abs(y-0.5)==min(abs(y-0.5)))/nrow(y)}
mode_post <- function(y) {
  match(max(post(y)), post(y))/length(y)}
cred_int_post <- function(y) {
  for(i in 1:nrow(y)) {
    y[i,1] <- integrate(post,lower=0,upper=i/nrow(y))[1]}
  c(which(abs(y-0.025)==min(abs(y-0.025)))/nrow(y),
    Appendix II
    - 40 -
      which(abs(y-0.975)==min(abs(y-0.975)))/nrow(y))}
rden <- function(n, den)
{
  diffs <- diff(den$x)
  stopifnot(all(abs(diff(den$x) - mean(diff(den$x))) < 1e-9))
  total <- sum(den$y)
  den$y <- den$y / total
  ydistr <- cumsum(den$y)
  yunif <- runif(n)
  indices <- sapply(yunif, function(y) min(which(ydistr > y)))
  x <- den$x[indices]
  return(x)
}
#hpd interval function from Prof. R. Kushler
hpdint<-function(x,prob=0.99) {
  xx<-sort(x)
  nn<-length(xx)
  len<-function(a)xx[floor((a+prob)*nn)]-xx[floor(a*nn)+1]
  tail<-optim((1-prob)/2,len,method="L-BFGS-B",lower=0,upper=1-prob)$par
  c(xx[floor(tail*nn)+1],xx[floor((tail+prob)*nn)])}
Exp_p <- as.numeric(integrate(Exp_post,lower=0,upper=1)[1])
Var_p <- as.numeric(integrate(Var_post,lower=0,upper=1)[1])
median_p <- median_post(y)
mode_p <- mode_post(x)
credint_post <- cred_int_post(y)
post_x <- seq(0,1,by=c(0.000001))
post_y <- post(x)
hpd1_post <- p.interval(rden(100000,density(post_x,weights=post_y/(length(post_y)-1))), hpd=TRUE, MM=FALSE, prob=0.95, plot=FALSE)
hpd2_post <- hpdint(rden(100000,density(post_x,weights=post_y/(length(post_y)-1))),prob=0.9