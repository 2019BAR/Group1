D = faithful$eruptions  # copy to a short name

# Frame
plot(0,0,xlim=c(1.5,5.25),ylim=c(0,1.1),xlab="噴發時間(分鐘)", 
     ylab="密度 or (累計)機率", main="分布、機率與密度")
abline(h=1, col='lightgray', lwd=0.25, lty=2)

# Empirical PDF
rug(D)
# Empirical CDF
plot(ecdf(D), cex=0, verticals=T, lwd=2, col='darkgray', add=T)

# Histogram PDF
Bins = 20                             # no. bins
bx = seq(min(D), max(D), length=Bins) # break sequence 
hist(D, col="#B3FFFF7F", border="white", ylim=c(0,1.1),
     freq=F, breaks=bx, add=T)
abline(h=0, col='lightgray', lwd=0.25)
# Histogram CDF
adj = (bx[2] - bx[1])/2
steps = stepfun(bx-adj, c(0, sapply(bx, function(b) mean(D <= b))))
plot(steps, cex=0, col='#33CC337F', lwd=3, lty=1, add=T)

# Smooth PDF
Adjust = 1    # bandwidth adjustment
DEN = density(D, adjust = Adjust)
lines(DEN, col='gold', lwd=3)
# Smooth CDF
PDF = approxfun(DEN$x, DEN$y, yleft=0, yright=0)
x = seq(1,6,0.1)
y = sapply(x, function(i) integrate(PDF, -Inf, i)$value)
lines(x, y, col='red', lwd=3, lty=2) 

# Mark Range
x1 = 3.8; x2 = 4.8
rect(x1,-0.1,x2,1.2,col= rgb(0,1,0,alpha=0.2),border=NA)
# x = seq(x1, x2, length=100)
# polygon(c(x, x2, x1),  c(PDF(x), 0, 0), col="#FF99003F", border=NA)
# Calculate Probability
(integrate(PDF, x1, x2)$value)

###############################
rbinom(1000000, 1000, 0.2) %>% hist(breaks=100,freq=F)
curve(dnorm(x,mean=200,sd=sqrt(160)), col='red', lwd=2, add=T)


