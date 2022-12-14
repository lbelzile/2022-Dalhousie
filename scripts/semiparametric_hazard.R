# Spline smoothing for Istat
# According to the specifications, this conforms with left-truncation and right-censoring
# I am altogether sceptical, since there is no support for left-truncation via Surv,
# ... despite claims to the contrary
data(italcent, package = "mev")
u <- 38351L
# Calendar date at which individual reaches 105 years
xcal <- italcent$birth + u
# Calendar time for sampling frame
c1 <- lubridate::dmy("01-01-2009")
c2 <- lubridate::dmy("01-01-2016")
slow <- as.numeric(pmax(0, c1 - xcal))
# Exceedances
dat <- italcent$numdays - u
rightcens <- italcent$rightcens

library(bshazard)
library(boot)
smooth_haz <- bshazard(Surv(slow/365.25, dat/365.25, !rightcens) ~ 1, nk = 100, degree = 3)
smooth_haz$time <- smooth_haz$time + 105
plot


high <- as.numeric(c2 - xcal)
data <- data.frame(x=dat, t=high, s=slow, d=1-italcent$rightcens,
                   yob=lubridate::year(italcent$birth),
                   gender=italcent$gender)


# Use ordinary polynomial splines, with corresponding X matrix made with function make.X
make.X <- function(len=15, n.knots=5, knot.spacing=365, noise=0){
   # X matrix for discrete GPD
   x <- c(1:(len*365))/365
   one <- rep(1,len*365)
   X <- cbind(one,x)
   if (n.knots>0) for (i in 1:n.knots)
   { knot <- round( i*knot.spacing+noise*rnorm(1) )
   z <- pmax(x[knot]-x,0)
   X <- cbind(X, z^3) }
   list(X=X, nrows=nrow(X), ncols=ncol(X))
}

# X matrix for exponential model
make.X.exp <- function(X){
   Y <- X
   Y$X <- Y$X[,-2]
   Y$ncols <- Y$ncols - 1
   Y$X <- matrix(Y$X, nrow=X$nrows)
   Y
}

make.rh <- function(lam, X) list( rh=(X$X %*% lam), x=c(1:nrow(X$X)) ) #
# make discrete hazard function for given parameters and X matrix
make.hazard <- function(th, X){
   rh <- make.rh(th, X)
   haz <- 1/pmax(rh$rh,0)
   Haz <- cumsum(haz)/365
   surv <- exp(-Haz)
   surv[is.nan(surv)] <- 0
   dens <- haz*surv
   list(x=rh$x/365, dens=dens, surv=surv, y=dens/surv)
}

nlogL.rh <- function(th, X, d){
   m <- make.hazard(th, X)
   denom <- m$surv[1+d$s]
   numer <- d$d*m$dens[d$x] + (1-d$d)*m$surv[d$x]
   ts <- Inf
   if (all(!is.nan(c(numer,denom)))) ts <- -sum(log(numer/denom))
   ts
}



boot.stat <- function(data, i, X){
   d <- data[i,]
   fit.rh <- optim(init, nlogL.rh, hessian=T, method="BFGS", X=X, d=d)
   c(fit.rh$par,fit.rh$value,fit.rh$convergence)
}

boot.stat1 <- function(data, i, n.knots=5, knot.spacing=365, noise=0){
   d <- data[i,]  # bootstrap dataset

   X <- make.X(n.knots=n.knots, knot.spacing=knot.spacing, noise=noise) # matrix X with random knots
   X.exp <- make.X.exp(X)  # cut-down matrix for exponential fit
   init <- c(1.6,rep(0,n.knots)) # initial values for optimisation

   fit.rh.exp <- optim(init, nlogL.rh, method="BFGS", X=X.exp, d=d) # exp optimisation
   h.exp <- make.hazard(fit.rh.exp$par, X.exp)

   init2 <- fit.rh.exp$par
   init2 <- c(init2[1],0,init2[-1])
   fit.rh <- optim(init2, nlogL.rh, method="Nelder-Mead", X=X, d=d) # GP optimisation
   fit.rh <- optim(fit.rh$par, nlogL.rh, method="BFGS", X=X, d=d) # GP optimisation
   h <- make.hazard(fit.rh$par, X)
   c(fit.rh$par,fit.rh$value,fit.rh$convergence, fit.rh.exp$par,fit.rh.exp$value,fit.rh.exp$convergence, length(h$x), h$x, h$y, h.exp$y)
}

# Warning: computationally intensive! Reduce number of bootstrap replicates to speed up calculations
# the paper used R=5000
system.time( rh.boot <- boot(data=data, statistic=boot.stat1, R=5000L,
                             n.knots=5, knot.spacing=365, noise=60))
# indices for plotting output
n.knots <- 5
N <- rh.boot$t0[2*n.knots+8]
ind.x <- 2*n.knots+8 + c(1:N)
ind.h <- 2*n.knots+8 + N + c(1:N)
ind.h.exp <- 2*n.knots+8 + 2*N + c(1:N)

# average locations of knots (in years)
knots <- c(1:n.knots)

# make envelopes from bootstrap output
# sometimes there are NaNs in the output,
# the next lines remove those bootstrap replicates
haz.out <- rh.boot$t[, ind.h]

notnumber <- rep(0, nrow(haz.out))
for (i in 1:nrow(haz.out)) notnumber[i] <- sum(is.nan(haz.out[i, ]))
which(notnumber>0)  # which lines are dud?

haz.out <- rh.boot$t[-which(notnumber>0), ind.h]
haz.out.exp <- rh.boot$t[-which(notnumber>0), ind.h.exp]


haz.env <- envelope( mat= haz.out)
haz.env.exp <- envelope(mat = haz.out.exp)
# plotting
data <- data.frame(x = italcent$numdays - 105*365.25,
                   d =  I(italcent$rightcens))
ind.x <- 2*n.knots+8 + c(1:N)
ind.h <- 2*n.knots+8 + N + c(1:N)
ind.h.exp <- 2*n.knots+8 + 2*N + c(1:N)
subseq <- seq(1, by = 25, length.out = 219)
ind.x <- ind.x[subseq]
ind.h <- ind.h[subseq]
ind.h.exp <- ind.h.exp[subseq]
library(tikzDevice)
options(tikzLatexPackages =
c("\\usepackage{tikz}\n",
"\\usepackage[active,tightpage,psfixbb]{preview}\n",
"\\usepackage{amsmath}",
"\\PreviewEnvironment{pgfpicture}\n",
"\\setlength\\PreviewBorder{0pt}\n",
"\\usepackage{lmodern}\n",
"\\usepackage{times}\n"
))


fig <- "figures/italcent_hazard.tex"
tikz(fig, width = 6, height = 3, standAlone = TRUE)
par(mfrow=c(1, 2), mar = c(4, 4, 0.1, 0.4), bty = "l")
plot(smooth_haz,
     xlim = c(105,115),
     xlab = "age (years)",
     ylab = "hazard (1/year)",
     bty = "l",
     yaxs = 'i',
     ylim = c(0,1.5))

plot(rh.boot$t0[ind.x]+105, rh.boot$t0[ind.h],
     type="n", ylim = c(0, 1.5),
     xlab = "age (years)",
     ylab = "hazard (1/year)",
     yaxs = "i",
     panel.first = {
        abline(v=knots + 105, lty=2, col="grey");
        # rug(105 + jitter(data$x[as.logical(I(data$x >365.25*3)*I(data$d==1))])/365.25, ticksize=-0.02, col="red");
        # rug(105 + jitter(data$x[as.logical(I(data$x >365.25*3)*I(data$d==0))])/365.25, ticksize=0.02);
     } )

for (r in 1:100) lines(rh.boot$t[r, ind.x] + 105,
                       rh.boot$t[r, ind.h], col="grey", lwd=0.5)
lines(rh.boot$t0[ind.x] + 105, rh.boot$t0[ind.h], lwd=2)


lines(rh.boot$t0[ind.x]+105, haz.env$point[1,subseq ], lty=3,lwd = 2)
lines(rh.boot$t0[ind.x]+105, haz.env$point[2, subseq], lty=3,lwd = 2)
lines(rh.boot$t0[ind.x]+105, haz.env$overall[1, subseq], lty=2,lwd = 2)
lines(rh.boot$t0[ind.x]+105, haz.env$overall[2, subseq], lty=2,lwd = 2)
dev.off()
