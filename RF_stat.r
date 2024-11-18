###bogdan romocea, 2004 and later
###R functions for statistics, time series, modeling and graphics

# Table of Contents:
# acft <- function(x, lag=10*log10(length(x)), ...)
# binplot <- function(bin, cont, bins=100, estimate="percent", ylab=NULL, xlab=NULL, xlog=FALSE, pred=NULL, ...)
# binplot.DF <- function(DF, tgt, pred, bins=100, estimate="percent", xlog=FALSE, logonly=FALSE)
# contdisc.plot <- function(x, BY=NULL, lab, boxord=NULL, allx=NULL, ...)
# corr.test <- function(DF, pred, tgt, mtd="spearman")
# den <- function(x, digits=1, brk=NULL, CI=FALSE, showdens=TRUE, dorug=FALSE, border="gray50", ...)
# ep <- function(x, plots=2, mp=TRUE, full=TRUE, ...)
# ep.DF <- function(DF, full=TRUE)
# ep2 <- function(x, y, all4=TRUE, xlab="x", ylab="y", ...)
# gby <- function(v, BY, byname="BY", digits=NULL)
# gbyDF <- function(DF, id, v, keep=c('N','mean'))
# hboxp <- function(x, cex=1, xlim=NULL, topax=FALSE, ord='default', log="", trim=0, samp=TRUE,
# highly.correlated <- function(DF, vars=NULL, vc=NULL, exclude=NULL)
# logistic.Cidx.trval <- function(DF, mform, N, tol=1e-7)
# logistic.fastbw.boot <- function(DF, mform, N, tol=1e-7, eps=1e-9, 
# logistic.oddr.stdcoef <- function(DF, mform)
# logistic.report <- function(DF, tgt, obj, bins=10, prob=NULL)
# logistic.validate <- function(mfit, N=50, tofile=TRUE, ttl=project)
# lst.coll <- function(lst, name)
# NA.impute.tree <- function(DF, misspred)
# NA.impute.tree.DF <- function(DF, toimpute)
# NA.impute.univ <- function(x, type, lab=NULL)
# NA.impute.univ.DF <- function(DF, numtype, factype, exclude=NULL)
# norm.plot <- function(x, ...)
# outlier.find <- function(DF, ratio, probcount=c(0.01, 0.99), except='')
# outlier.replace <- function(DF, ol)
# parplot <- function(DF, clr=NULL, tgtlab=NULL, topo=TRUE, digits=0, cex=0.7, rand=NULL, decreasing=TRUE, bins=NULL, ...)
# pcbd <- function(DF, tgt, fname='plt', topo=TRUE, digits=0, bins=10)
# pred.kept.rep <- function(pred.kept, minpct=0.04)
# qdif <- function(v1, v2, ...)
# rmerge <- function(ldfr)
# scatter <- function(x, y, pts=TRUE, kde=FALSE, p=200, dorug=FALSE, xjit=NULL, 
# sdcoef <- function(obj, noeq=TRUE)
# stdize <- function(DF, method="STD")
# stemDoc <- function(doc)
# stemWords <- function(x, suffixes=c('','s','d','r','ed','ly','e','ing','ings','er','ers','ies','y',
# tbl.collapse <- function(tb, N=19, byfreq=TRUE)
# tomatrix <- function(x, y, value)
# topoclr <- function(x, bins=NULL, maxbinjump=3, nbeg='blue', nend='slategray1', 
# transf <- function(DF, tgt, v, lth=0.01)
# treeRules <- function(tr, extra=NULL)
# treeRulesCollapse <- function(DF, rules)
# trim <- function(x, probs=c(0.01,0.99))
# univ.pval <- function(DF, tgt, pred, type)
# vcna.plot <- function(x, main='', ...)
# wassoc <- function(doc, W=2, print=TRUE)

#-------------------------------------------------------------------------------

#br 2005-03
#plot the ACF and the p-values for several ACF and stationarity tests

acft <- function(x, lag=10*log10(length(x)), ...)
{
tests <- vector()
tests[1] <- paste("N = ", length(x))
tests[2] <- "H0: ACF=0"
tests[3] <- paste("  p=", round(Box.test(x, type="Box-Pierce", lag=lag)$p.value, 4), 
                  " Box-Pierce", sep="")
tests[4] <- paste("  p=", round(Box.test(x, type="Ljung-Box", lag=lag)$p.value, 4), 
                  " Ljung-Box", sep="")
tests[5] <- "H0: unit root"
if (!exists("adf.test")) require(tseries)
tests[6] <- paste("  p=", round(PP.test(x)$p.value, 4), " Phillips-Perron", sep="")
tests[7] <- "  Augmented Dickey-Fuller:"
tests[8] <- paste("  p=", round(adf.test(x, alternative="stationary")$p.value, 4), 
                  " (H1: stationary)", sep="")
tests[9] <- paste("  p=", round(adf.test(x, alternative="explosive")$p.value, 4), 
                  " (H1: explosive)", sep="")
acf(x, lag.max=lag, ...)
legend("topright", xjust=1, legend=tests)
abline(h=0.1*1:10, lty="dotted", col="gray70")
print(tests)
}

#-------------------------------------------------------------------------------

#br 2006-04
#function that plots a binary variable against a continuous one
#When the target is binary,  scatterplots of the target vs. each variable are not very helpful.
#	A useful plot to detect nonlinear relationships is a plot of the empirical logits. These logits
#	use a minimax estimate of the proportion of events in each bin of the input variable.
#For each bin of the input variable,  
#	Empirical Logit = log( (events + sqrt(cases)/2)) / (cases - events + sqrt(cases)/2)) )
#If the standard logistic model were true,  the plots below should be linear.

#REMEDIES against non-linear relationships:
#1. Hand-crafted new input variables >> transform or discretize the input variables. This can
#become impractical with high-dimensional data and increases the risk of overfitting.
#2. Polynomial models
#3. Flexible multivariate function estimators
#Classification trees,  generalized additive models,  projection pursuit,  multivariate adaptive
#	regression splines,  radial basis function networks and multilayer perceptrons (neural
#	networks) are flexible alternatives to logistic regression (Hastie and Tibshirani 1990; 
#	Ripley 1996).
#4. Do nothing >> big winner among 2003-01 Atlanta SAS logistic modeling course attendants
#Standard (linear) logistic regression can produce powerful and useful classifiers even when 
#	the estimates of the posterior probabilities are poor. Often more flexible approaches do not
#	show enough improvement to warrant the effort.

#arguments:
#	- bin, cont = the binary and continuous vectors
#	- bins = cut() the continuous variable into this many intervals
#	- estimate = the type of estimate for the quantity of binary events per continuous bin
#	(one of "percent", "count", "minimax")
#	- xlog = whether to plot the continuous variable on the log scale; is very useful and
#	sometimes necessary when dealing with highly skewed but usually positive distributions,
#	such as wealth, income, deposit balances
#	- pred = the name of the predictor variable, needed for the title

binplot <- function(bin, cont, bins=100, estimate="percent", ylab=NULL, xlab=NULL, xlog=FALSE, pred=NULL, ...)
{
if (xlog) cont[cont < 1] <- 1  #set up for log scale
#While rank() can deal with NAs and they will end up in separate bins if enough of
#	them are available, remove the NAs from the start to avoid possible confusions
#	further down the road.
keep <- which(!is.na(bin) & !is.na(cont))
pctmissing <- round(100 * (length(bin) - length(keep)) / length(bin))
if (pctmissing > 90) {
    cat(paste('Too many NAs (', pctmissing, '%); aborting.', sep=''), '\n')
    return(NULL)
}
bin <- bin[keep] ; cont <- cont[keep]
#limit the no. of bins if the predictor contains few unique values
#binstouse <- min(bins, length(unique(cont)))
#---bin the predictor variable
prank <- rank(cont, ties.method="first")
pbin <- as.numeric(cut(prank, breaks=bins))
binsize <- as.data.frame(table(pbin))
#---for each bin, sum the target and average the predictor
yes <- sapply(split(as.numeric(as.character(bin)), pbin), sum)
yes <- as.data.frame(yes) ; yes$pbin <- rownames(yes)
avg <- sapply(split(cont, pbin), mean)
avg <- as.data.frame(avg) ; avg$pbin <- rownames(avg)
#---combine and compute the estimates of the target event
d1 <- merge(binsize, yes, all=TRUE)
d2 <- merge(d1, avg, all=TRUE)
d2$elogit <- log((d2$yes + sqrt(d2$Freq)/2) / (d2$Freq - d2$yes + sqrt(d2$Freq)/2))
d2$pct <- d2$yes / d2$Freq
#---choose the estimate to use
if (tolower(estimate) == "percent") {
    est <- d2$pct ; cttl <- " % Per Bin"
} else if (tolower(estimate) == "count") {
    est <- d2$yes ; cttl <- " Events Per Bin"
} else if (tolower(estimate) == "minimax") {
    est <- d2$elogit ; cttl <- " Empirical Logits Per Bin"
} else stop("Type of target event estimate is unknown (must be one of 'percent', 'count', 'minimax').")
#---plot: if the estimate has fewer than 'bins' unique values, apply some random noise
#	so that the visual weights of the points are not lost
if (is.null(ylab)) ylab <- paste('1s:', cttl) else ylab <- paste(ylab, cttl)
if (is.null(xlab)) xlab <- 'continuous variable (average per bin)'
plotlog <- if (xlog) 'x' else ''
#if (length(unique(est)) < bins) {
#	plot(d2$avg, jitter(est, factor=0.5), ylab=ylab, xlab=xlab, 
#			main=paste(ylab, " of ", pred, sep=""), log=plotlog, ...)
#} else {
plot(d2$avg, est, ylab=ylab, xlab=xlab, main=paste(ylab, " of ", pred, sep=""), log=plotlog, ...)
#}
ow <- options("warn")
options(warn=2)  #convert warnings to errors to prevent plotting a smoother in cases where it doesn't make sense
if (xlog) {
    smooth <- try(loess.smooth(log(d2$avg), est))
    if (class(smooth) != 'try-error') {
        smooth$x <- exp(smooth$x)  #back to the proper scale for plotting with the original values
        lines(smooth, col="slateblue")
    }
} else {
    smooth <- try(loess.smooth(d2$avg, est))
    if (class(smooth) != 'try-error') lines(smooth, col="slateblue")
}
mtext(paste('N=', length(keep), ', NA=', pctmissing, '%, bins=', bins, sep=''), line=-1, cex=0.8, col='navy')
options(ow)
}

#-------------------------------------------------------------------------------

#br 2006-04
#do plots of a binary variable against the specified variables from a data frame
#arguments:
#	- DF = the data frame
#	- tgt, pred = vectors with the names of the binary target and the predictors
#	- bins = cut() the continuous variables into this many intervals
#	- xlog: whether to plot the bin averages on the log scale (this is helpful for
#	highly skewed / lognormal variables, where the regular plot shows an agglomeration
#	at lower values and just a few points with higher values)

binplot.DF <- function(DF, tgt, pred, bins=100, estimate="percent", xlog=FALSE, logonly=FALSE)
{
pred <- setdiff(pred, tgt)  #just to make sure
factpred <- pred[sapply(as.list(DF[, pred]), class) == "factor"]
numpred <- setdiff(pred, factpred)
oldpar <- par()
if (length(numpred) > 3) mp(2, 2, b=0.8, l=0.8)
for (i in numpred) {
    xlab <- paste(i, '(bin average)')
    if (!logonly) try(binplot(DF[,tgt], DF[,i], bins=bins, estimate=estimate, ylab=tgt, xlab=xlab, xlog=FALSE, pred=i))  #do a regular plot first
    if (xlog) {  #then add another plot on the log scale
        xlab <- paste(i, '(bin average, log scale)')
        try(binplot(DF[,tgt], DF[,i], bins=bins, estimate=estimate, ylab=tgt, xlab=xlab, xlog=TRUE, pred=i))
    }
}
if (length(factpred) > 3) mp(2, 2, b=0.8, l=0.8)
p <- list()
for (i in factpred) {
    keep <- which(!is.na(DF[, tgt]) & !is.na(DF[, i]))
    pctmissing <- round(100 * (nrow(DF) - length(keep)) / nrow(DF))
    if (pctmissing > 90) {
        cat(paste('Too many NAs (', pctmissing, '%); aborting.', sep=''), '\n')
        next
    }
    #br 2016-02: This probably stopped working as it used to quite a while ago, but I noticed it only now
    #	try(plot(DF[, tgt], DF[, i], main=paste(tgt, " vs. ", i, " (N=", length(keep), ", NA=", pctmissing, "%)", sep=""), 
    #		xlab=tgt, ylab=i, col=paste('gray', seq(90, 30, by=-10), sep='')))
    p[[i]] <- vplot(tapply(DF[, tgt], DF[, i], mean), size=3) + labs(title=i, y=paste('%', tgt), x=i)
}
if (length(factpred) > 0) mpgg(p,2,2)
#on.exit(par(oldpar))
}

#-------------------------------------------------------------------------------

#br 2006-10
#this function does plots of a 'continous-discrete' x for each value of BY
#'continous-discrete' means that the underlying values are continuous,  but were binned and are
#	available only as a limited number of values. Since in this case histograms, 
#	density/scatter/bar/quantile plots etc are not effective,  I chose to plot the percentage
#	distributions (computed as if x were categorical),  while at the same time enforcing the 
#	correct scale on x's axis.
#Note: this was written to plot data appended from Equifax, such as net worth, income etc, which
#	are given as intervals which were then translated to numerics (mid-point).
#x = the 'continous-discrete' variable
#BY = the grouping variable
#lab = label to be included in all plot titles
#boxord = a vector with the distinct values from BY that specifies the order of the plots
#allx = a distribution for comparision (if provided,  it will appear as an additional line on the plots)

contdisc.plot <- function(x, BY=NULL, lab, boxord=NULL, allx=NULL, ...)
{
if (is.null(BY)) fp <- list(x) else fp <- split(x, BY)
#---remove the nodes where x is not available
remove <- which(is.na(lapply(fp, unique))) ; if (length(remove) > 0) fp <- fp[-remove]
fptb <- lapply(fp, tbl)
if (!is.null(allx)) all <- tbl(na.omit(allx))
xax <- c(min(sapply(fptb, function(x) {min(x[, 1])})), max(sapply(fptb, function(x) {max(x[, 1])})))
yax <- c(min(sapply(fptb, function(x) {min(x[, 3])})), max(sapply(fptb, function(x) {max(x[, 3])})))
if (!is.null(boxord)) {
    forord <- rep(NA, length(fptb))
    for (i in 1:length(fptb)) if (names(fptb)[i] %in% boxord) forord[i] <- which(boxord == names(fptb)[i])
}
for (i in 1:length(fptb)) {
    j <- which(forord == i)
    plot(fptb[[j]][, 1], fptb[[j]][, 3], xlim=xax, ylim=yax, type="n", 
         main=paste(lab, "\n", names(fptb)[j], " (N=", sum(fptb[[j]][, 2]), ")", sep=""), ylab="Percent", ...)
    grid()
    if (!is.null(allx)) {
        lines(all[, 1], all[, 3], type="o", col="gray50", lty=2, pch=1, cex=1.5)
        
        legend("topright", xjust=1, legend="OSB", lwd=2, lty=2, col="gray50")
    }
    lines(fptb[[j]][, 1], fptb[[j]][, 3], type="o", ...)
}
}

#-------------------------------------------------------------------------------

#2006-04,  from Andy Liaw
#plot a correlation matrix
#also see color2D.matplot in the plotrix package

corrmat.plot <- function (x,  rlab = if (is.null(rownames(x))) 1:nrow(x) else rownames(x),  
                      clab = if (is.null(colnames(x))) 1:ncol(x) else colnames(x),  
                      cex.row=0.8,  cex.col=0.8,  showcorr=TRUE,  main = deparse(substitute(x)),  ...)
{
op <- par(mgp=c(4,  0.1,  0)) ; on.exit(par(op))
nr <- nrow(x) ; nc <- ncol(x)
image(1:nc, 1:nr, t(x)[, nr:1], axes=FALSE, xlab="", ylab="", main=main,  ...)
axis(2, 1:nr, rev(rlab), cex.axis=cex.col, tick=FALSE, las=2)
axis(1, 1:nc, clab, cex.axis=cex.row, tick=FALSE, las=2)
#invisible(x)
if (showcorr) {
    for (i in 1:nrow(x)) {
        text(i, ncol(x):1, labels=round(x[i, ], 2), cex=0.8)
    }
}
}

#-------------------------------------------------------------------------------

#br 2006-04
#function which performs correlation tests given a data frame,  a target and some predictors
#the default method is Spearman (Pearson is more sensitive to nonlinearities and outliers)
#arguments:
#	- DF = the data frame
#	- pred, tgt = the names of predictors/target (character vectors)
#	- mtd = the method to use to compute correlations (one of "pearson", "spearman", "kendall")

corr.test <- function(DF, pred, tgt, mtd="spearman")
{
corr <- cases <- list()
for (i in pred) {
    cat(i, "\n")
    corr[[i]] <- cor.test(DF[, i], DF[, tgt], alternative="two.sided", method=mtd)
    cases[[i]] <- nrow(na.omit(DF[, c(tgt, i)]))
}
est <- round(sapply(corr, function(x) {x$estimate}), 5)
pval <- round(sapply(corr, function(x) {x$p.value}), 5)
cases <- unlist(cases)
rep <- data.frame(pval, est, cases)
rep <- rep[order(-rep$pval), ]
colnames(rep) <- c("p value", mtd, "cases")
rep
}

#===FYI===
#br 2006-04,  found on R-help
#It is well known that Pearson correlation is not robust (it is sensitive to nonlinearities
#	and outliers). This example shows that,  contrary to popular belief,  rank-based procedures 
#	(Spearman) are NOT robust either.
#Conclusion: better use robust measures of correlation (such as cov.rob from MASS).
#Example:
#  x<-c(1:10, 100) ; y<-c(1:10+rnorm(10, sd=.25), -100) ; plot(x, y)
#  cor.test(x, y, method='pearson')	#awful
#  cor.test(x, y, method='spearman')	#better
#  require(MASS)
#  cov.rob(cbind(x, y), cor=TRUE)	#best; look at $cor

#-------------------------------------------------------------------------------

#br 2004-11
#function which produces a customized univariate plot:
#	- histogram with customizable number of breaks
#	- density plot
#	- rug plot + the graphical representation of the mean,  median and P25/75
#	- the 95% confidence interval of the mean is also shown

den <- function(x, digits=1, brk=NULL, CI=FALSE, showdens=TRUE, dorug=FALSE, border="gray50", ...)
{
na.test(x)
sttl <- paste("N=", length(which(!is.na(x))), " / NA=", round(100*(length(which(is.na(x))) / length(x)), 1),
              "% / Av=", round(mean(x, na.rm=TRUE), digits), " / Md=", round(median(x, na.rm=TRUE), digits), sep="")
#---different plot for vectors with few distinct values
if (length(unique(x)) >= 1 & length(unique(x)) <= 30) {
    hst <- hist(x, breaks=300, plot=FALSE)  #freq=TRUE
    plt <- which(hst$counts != 0)
    plot(hst$breaks[plt], hst$counts[plt], xlab="", ylab="Frequency", type="o", 
         pch=7, cex=1.5, col="navy", lty=2, ...) ; grid()
    showdens <- FALSE
} else {
    if (is.null(brk)) {
        hst <- hist(x, border=border, freq=!showdens, ...) 
    } else {
        hst <- hist(x, border=border, breaks=brk, freq=!showdens, ...)
    }
}
if (dorug) rug(x, col="gray50")
mtext(sttl, line=-1, cex=0.8)
if (showdens) {dens <- density(x, na.rm=TRUE) ; lines(dens, col="navyblue")}
#dens <- density(x,  from=min(x),  to=max(x),  give.Rkern=FALSE)
#---show the 95% CL of the mean
if (CI) {
    #this is used only to position the CI on the Y axis
    if (showdens) ciy <- range(dens$y)[2] * 1/10 else ciy <- range(hst$counts)[2] * 1/10
    #why try(): because if all values are the same t.test() returns an error
    ci <- try(round(as.numeric(t.test(x, conf.level=0.95)$conf.int), digits))
    if (class(ci) != "try-error") {
        segments(ci[1], ciy, ci[2], ciy, col="brown")
        points(ci[1], ciy, pch="[", col="brown") ; points(ci[2], ciy, pch="]", col="brown")
        text(mean(ci), ciy, label="95% CI", pos=3, col="brown")
        text(mean(ci), ciy, label=paste(ci, collapse=" - "), pos=1, col="brown")
    }
}
#---also show the 25-50-75th percentiles + mean
axis(side=1, at=median(x, na.rm=TRUE), col="navy", lwd=4, labels=FALSE)
axis(side=1, at=mean(x, na.rm=TRUE), col="brown", lwd=4, labels=FALSE)
axis(side=1, at=quantile(x, probs=0.25, na.rm=TRUE), col="blue", lwd=3, labels=FALSE)
axis(side=1, at=quantile(x, probs=0.75, na.rm=TRUE), col="blue", lwd=3, labels=FALSE)
box()
}

#-------------------------------------------------------------------------------

#br 2006-02
#exploratory univariate plots for numeric variables or factors
#x = numeric or factor
#plots = if 4, do 4 plots else do only 2
#mp = whether to control the number of plots per window from this function
#2010-12: Added a new parameter, full, which if FALSE restricts the range
#	plotted between the 1st and 99th percentiles. The objective is to make
#	highly skewed variables easier to read, without bothering to transform
#	the data or correct wrong values.

ep <- function(x, plots=2, mp=TRUE, full=TRUE, ...)
{
if (!is.factor(x)) {
    axlim <- if (!full) quantile(x, c(0.01, 0.99), na.rm=TRUE) else quantile(x, c(0, 1), na.rm=TRUE)
    if (plots == 4) {
        if (mp) oldpar <- par(mfrow=c(2, 2), mai=c(0.7, 0.6, 0.4, 0.4))
        try(den(x, dorug=FALSE, xlim=axlim, ...))
        try(boxp(x, overwrite=TRUE, pts=TRUE))
        qpl(x, ylim=axlim, ...)
        try(norm.plot(x,...))
    } else if (plots == 2) {
        if (mp) oldpar <- par(mfrow=c(1, 2), mai=c(0.8, 0.9, 0.5, 0.4))
        den(x, xlim=axlim,  ...) ; qpl(x, ylim=axlim, ...)
    } else {
        mp <- FALSE
        if (all.NA(x)) qpl(x, ...) else qpl(x, ylim=axlim, ...)
    }
    #plot.ecdf(x, verticals=TRUE, pch="", ylab="%") ; grid()
    #plot(ecdf(x), main=paste(vname, " - Empirical CDF", sep="")) ; grid(col="purple")
    if (mp) on.exit(par(oldpar))
} else gtbl(x, pch=15, cex=1, col="navy", ...)
}

#-------------------------------------------------------------------------------

#br 2006-08
#exploratory bivariate plots for continuous variables

ep2 <- function(x, y, all4=TRUE, xlab="x", ylab="y", ...)
{
if (all4) oldpar <- par(mfrow=c(2, 2), mai=c(0.65, 0.6, 0.3, 0.3), omi=c(0.2, 0, 0, 0))
scatter(x, y, xlab=xlab, ylab=ylab)
scatter(x, y, pts=TRUE, xlab=xlab, ylab=ylab)
qdif(x, y)
lst <- list(x, y) ; names(lst) <- c(xlab, ylab) ; mcomp(lst)
if (all4) on.exit(par(oldpar))
}

#-------------------------------------------------------------------------------

#br 2006-10
#produce exploratory plots for all vars in a data frame

ep.DF <- function(DF, full=TRUE)
{
fact <- sapply(lapply(DF, class), function(x) {length(intersect(x, c("factor","character"))) > 0})
if (FALSE %in% fact) {
    #	oldpar <- par(mfrow=c(2, 2), mai=c(0.7, 0.6, 0.5, 0.4))
    p <- list()
    for (i in colnames(DF)[!fact]) {
        now(i, after="\n")
        p[[i]] <- qpl(DF[, i], main=i, full=full)
        #		try(ep(DF[, i], plots=plots, mp=FALSE, main=i, full=full))
    }
    #	on.exit(par(oldpar))
}
if (TRUE %in% fact) {
    #	oldpar <- par(mfrow=c(2, 2), mai=c(0.7, 0.6, 0.5, 0.4))
    for (i in colnames(DF)[fact]) {
        now(i, after="\n")
        #try(ep(DF[, i], main=i))
        p[[i]] <- gtbl(DF[, i], main=i)
    }
    #	on.exit(par(oldpar))
}
mpgg(p, 2, 2)
}

#-------------------------------------------------------------------------------

#br 2005-05
#function similar to SQL group by
#2006-05: switched from by() to summarize() from Hmisc
#NB: | in byname will break summarize()

gby <- function(v, BY, byname="BY", digits=NULL)
{
if (!exists("summarize")) library(Hmisc)
grp <- summarize(v, BY, function(x) {c(
    N=length(x), min=min(x), max=max(x),
    med=median(x), mean=mean(x),
    sd=sd(x), CV=sd(x)/mean(x),
    sum=sum(x), PctPos=length(x[x>0])/length(x))})
colnames(grp) <- c(byname,"N","min","max","median","mean","sd","CV","sum","PctPos")
out <- if (!is.null(digits)) cbind(grp[,c(byname,'N')], round(grp[,c("min","max","median","mean","sd","CV","sum","PctPos")], digits)) 
else grp
out
}

#deprecated
#dfr <- do.call("rbind", by(var,grby,function(x) {c(min=min(x),max=max(x),
#	median=round(median(x),digits=2),mean=round(mean(x),digits=2),
#	sum=sum(x),pts=length(x))}))
#final <- cbind(BY=as.numeric(rownames(dfr)),dfr)
#rownames(final) <- 1:nrow(final)
#final

#-------------------------------------------------------------------------------

#br 2008-04
#this function is an extension of gby() which gets closer to the standard SQL group by
#DF = data frame
#id = vector with the names of the columns to be used as IDs for group by
#v = the name of the variable (only one at this time) to be summarized
#keep = which stats to keep in the final output

gbyDF <- function(DF, id, v, keep=c('N','mean'))
{
#BY <- apply(DF[,id], 1, function(x) {paste(x, collapse='_')})
#this is rather ugly but in testing ran more than 10 times faster than the apply above
eval(parse(text=paste('BY <- paste(', paste(paste("DF[,'", id, "']", sep=''), collapse=', '), ", sep='_')", sep='')))
summ <- gby(DF[,v], BY) ; summ <- summ[,c('BY',keep)]  #summarize
#---add back the IDs in their original format
ID <- list()
for (i in 1:length(id)) ID[[id[i]]] <- as.numeric(sapply(strsplit(summ$BY, '_'), function(x) x[[i]][1]))
out <- cbind(do.call('cbind', ID), summ)
out[['BY']] <- NULL
out <- sortDF(out, id)
out
}

#-------------------------------------------------------------------------------

#br 2007-03
#function that does horizontal box plots, with the specific intention of concisely
#	plotting a large number of distributions on a single graph
#x = matrix, data frame or list
#ord = a statistic used to order the box plots; one of MEAN, MEDIAN, IQR (inter-quartile range = P75-P25)
#narrow = whether to plot only the portion between the 25th and 75th percentiles; this can be
#	helpful with highly skewed distributions, where the extremes are so far away that they
#	obscure the mean and median

hboxp <- function(x, cex=1, xlim=NULL, topax=FALSE, ord='default', log="", trim=0, samp=TRUE,
              narrow=FALSE, pts=FALSE, top=NULL, ...)
{
if (class(x) == "data.frame") x <- as.list(x)
if (class(x) == "matrix") x <- as.list(as.data.frame(x))
if (class(x) != "list") stop("Only matrices, data frames and lists are accepted.")
#---reorder the list if requested
if (toupper(ord) == 'MEAN') x <- x[order(sapply(x, function(x) mean(x, na.rm=TRUE, trim=trim)))]
if (toupper(ord) == 'MEDIAN') x <- x[order(sapply(x, function(x) median(x, na.rm=TRUE)))]
if (toupper(ord) == 'IQR') x <- x[order(sapply(x, function(x) quantile(x, 0.75, na.rm=TRUE) - quantile(x, 0.25, na.rm=TRUE)))]
if (!is.null(top)) x <- tail(x, top)
labels <- if (!is.null(names(x))) names(x) else as.character(1:length(x))
if (samp) labels <- paste(labels, ' (', sapply(x, function(x) length(which(!is.na(x)))), ')', sep='')
#---boxplot stats
ptl <- sapply(x,function(x) {quantile(x, probs=c(0,0.05,0.25,0.5,0.75,0.95,1),na.rm=TRUE)})
avg <- sapply(x,function(x) {mean(x, na.rm=TRUE, trim=trim)})
#---setup the plot
plot.new()
linch <- if (!is.null(labels)) max(strwidth(labels, "inch"), na.rm = TRUE) else 0
ginch <- 0 ; goffset <- 0
if (!is.null(labels)) {
    nmai <- par("mai") ; nmai[2] <- nmai[4] + max(linch + goffset, ginch) + 0.1
    par(mai = nmai)
}
n <- length(x)
if (is.null(xlim)) {
    if (narrow) {
        xlim <- range(unlist(lapply(x, function(x) c(mean(x, na.rm=TRUE, trim=trim),
                                                     quantile(x, c(0.25, 0.75), na.rm=TRUE)))))
    } else xlim <- range(ptl)
}
o <- 1:n ; y <- o ; ylim <- c(0, n + 1)
plot.window(xlim = xlim, ylim = ylim, log = log)
lheight <- par("csi") ; linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
loffset <- (linch + 0.1)/lheight ; labs <- labels[o]
mtext(labs, side=2, line=loffset, at=y, adj=0, col="black", las=2, cex=cex)
#---box plots
mwd <- 0.38  #segment height for mean
owd <- 0.2  #segment height for others
qn <- c("0%","5%","25%","50%","75%","95%","100%") ; qwd <- c(2,1,2,2,2,1,2)
qc <- c("gray50","magenta","blue","black","blue","magenta","gray50")
for (i in 1:length(x)) {
    if (pts) points(x[[i]], rep(i, length(x[[i]])), col='grey70')
    abline(h=i, lty="dotted", col="gray80")
    segments(ptl["25%",i], i, ptl["75%",i], i, col="blue", lty='dotted')
    segments(avg[i], i-mwd, avg[i], i+mwd, col="brown", lwd=2)
    for (q in 1:length(qn)) {
        segments(ptl[qn[q],i], i-owd, ptl[qn[q],i], i+owd, col=qc[q], lwd=qwd[q])
    }
}
axis(1); axis(3) ; box() ; grid(ny=NA)
if (topax) axis(3)
title(...)
legend("top", xjust=1, legend=c('Avg','Med','P25/P75','P5/P95','Min/Max'), lwd=3, 
       col=c('brown','black','blue','magenta','gray'), x.intersp = 0, y.intersp = 0, ncol=5, bty="n")
}

#-------------------------------------------------------------------------------

#br 2006-12
#This function identifies and returns groups of highly correlated variables. The
#  variables not similar to any others at a given threshold (tree height) are ignored.
#Primary objective: In some cases attempts to fit a logistic model will only produce
#  singularity errors. The cause is very highly correlated predictors: the information
#  matrix used by the fitting algorithm is found to be singular using a particular
#  algorithm under a certain tolerance level (in English: not good). The singular
#  information matrix error can be avoided by making the tolerance smaller (in lrm.fit()
#  from package Design the default value of the singularity criterion is 1E-7),  however
#  this is probably not the best thing to do given the risk of confounding the results
#  (besides,  it's not feasible in some cases). To get over computational singularities, 
#  remove predictors from the groups of highly correlated vars,  perhaps in a stepwise
#  fashion.
#Algorithm:
#  - Run a hierarchical cluster analysis on the numeric variables based on squared
#  Spearman correlations.
#  - Cut the dendogram at various heights and keep only the groups with more than
#  one variable. These represent clusters of highly correlated variables from which
#  only one (usually) should be kept.
#Arguments:
#  - DF = the data frame
#  - vars = vector with the names of the variables of interest
#  - vc = output from a previous varclus run
#  - exclude = variables to exclude (because for example they have many missing values)
#Value:
#  - a named list of lists; the names are the heights at which the dendogram was cut
#  - lower heights indicate higher correlation thresholds for putting vars in the same
#  groups,  and therefore will contain fewer/smaller groups of highly correlated variables.
#2010-08: Added another piece of output, a list with the names of the variables to be
#  dropped when the dendogram is cut at various heights. This is done by keeping the
#  first variable from each group, with the rest being singled out for deletion.

highly.correlated <- function(DF, vars=NULL, vc=NULL, exclude=NULL)
{
if (!is.null(exclude)) DF <- DF[,setdiff(colnames(DF), exclude)]
if (is.null(vc)) {
    if (!exists("varclus")) require(Hmisc)
    if (!is.null(vars)) DF <- DF[, vars]
    #---identify the numeric variables
    fact <- rep(0, ncol(DF))
    fact[sapply(DF, class) == "factor"] <- 1
    vcpred <- colnames(DF)[fact == 0]  #only the numerics are eligible for VARCLUS
    #---run VARCLUS
    vcform <- as.formula(paste(" ~ ", paste(vcpred, collapse =" + "), sep=""))
    vc <- varclus(vcform, similarity="spearman", data=DF)
}
#---form groups of similar vars, at various cutoffs
similar <- drop <- list()
for (h in seq(0.05, 0.5, by=0.05)) {
    vclust <- cutree(vc$hclust, h=h)
    vgr <- split(names(vclust), vclust)
    sim <- vgr[sapply(vgr, length) > 1]
    similar[[as.character(h)]] <- sim
    todrop <- list()
    if (length(sim) > 0) for (i in 1:length(sim)) {
        if (length(sim[[i]] >= 2)) todrop[[sim[[i]][1]]] <- data.frame(drop=sim[[i]][2:length(sim[[i]])])
    }
    if (length(todrop) > 0) {
        todrop <- lst.coll(todrop, 'keep')
        drop[[as.character(h)]] <- todrop[,c('keep','drop')]
    }
}
list(varclus=vc, hcorr=similar, drop=drop)
}

#-------------------------------------------------------------------------------

#br 2006-08
#get a series of C indexes (=area under the ROC curve) from models fit on training
#	samples and applied to validation samples
#algorithm:
#	- randomly divide the data into training and validation samples
#	- fit a model on the training sample and then apply directly to the validation sample
#	- extract the C indexes
#	- repeat the above a number of times.
#arguments:
#	- DF = the data frame
#	- mform = a list with the formula(s) to fit; if more than one formula is provided, 
#	training/validation C statistics are computed for each model based on the same
#	training/validation samples. So this approach facilitates model comparisons a bit,  because
#	the possible variation in models performance due to how the sample gets split (into training
#	and validation parts) is eliminated.
#	- N = number of train-validate iterations
#	- tol = singularity criterion
#value:
#	- a list of C indexes from the same models fit on training and validation samples
#Another approach is to run validate() from Hmisc,  print it and look at "index.corrected"
#	under the Dxy row.  Somer's D rank correlation is related to AUC (C) by Dxy=2(C - 0.5)

logistic.Cidx.trval <- function(DF, mform, N, tol=1e-7)
{
if (!exists("lrm")) require(Design)
training <- validation <- matrix(nrow=N, ncol=length(mform), 
                                 dimnames=list(1:N, paste("model", 1:length(mform), sep="")))
for (i in 1:N) {
    cat(i, " ") ; if (i/20 == floor(i/20)) cat("\n")
    train <- sample(1:nrow(DF), round(nrow(DF) / 2), replace=FALSE)
    for (j in 1:length(mform)) {
        trmod <- lrm(mform[[j]], data=DF[train, ], tol=tol)
        valmod <- lrm(mform[[j]], data=DF[-train, ], tol=tol, initial=trmod$coef, maxit=1)
        training[i, j] <- trmod$stats["C"]
        validation[i, j] <- valmod$stats["C"]
    }
}
list(training=training, validation=validation, predictors=lapply(mform, function(x) {x[[3]]}))
}

#-------------------------------------------------------------------------------

#br 2006-11
#This function does fast backward selection on a specified number of 
#  bootstrap samples, and reports which predictors were kept, and how often.
#Arguments:
#  - DF = the data frame
#  - mform = the formula to fit (all predictors)
#  - N = the number of bootstrap samples to run
#  - tol = singularity criterion for lrm.fit()
#  - eps = singularity criterion for fastbw()
#  - fname = the name of the PDF file where 2 plots will be saved
#Value:
#  - a list with all predictors kept (and how often), and the frequency distribution of
#    the number of predictors per model
#  - a file with 2 plots depicting the info above

logistic.fastbw.boot <- function(DF, mform, N, tol=1e-7, eps=1e-9, 
                             fname=paste(project, "7_boot_fast_backward", sep=""))
{
if (!exists("project")) stop("Create a variable 'project' with the project name,  
or provide a name for the PDF file that will contain the output plots.")
if (!exists("lrm")) require(Design)
pred.kept <- list()
for (i in 1:N) {
    now(paste("iteration", i, "of", N), after="\n")
    bootDF <- DF[sample(1:nrow(DF), nrow(DF), replace=TRUE), ]
    fmod <- lrm(mform, data=bootDF, tol=tol)
    fb <- fastbw(fmod, rule="aic", eps=eps)
    pred.kept[[i]] <- fb$names.kept
}
topdf(fname, landscape=FALSE)
rep <- pred.kept.rep(pred.kept, N)
dev.off()
rep
}

#-------------------------------------------------------------------------------

#br 2006-05
#function that returns the odds ratios and the standardized coefficients for a logistic model
#arguments:
#	- DF = the data frame
#	- mform = the model formula

logistic.oddr.stdcoef <- function(DF, mform)
{
tmod <- glm(mform, family=binomial(link=logit), data=DF)
#The parameters in the linear predictor in logistic regression are log odds ratios 
#	(assuming the default contrast,  contr.treatment). So just exponentiate it!,  and 
#	do the same with the confidence limits.
#Another alternative: confint(tmod)	(this uses likelihood profiling)
oddr <- exp(summary(tmod)$coef[, 1:2] %*% rbind( c(1, 1, 1), 1.96*c(0, -1, 1))) 
colnames(oddr) <- c("Odds Ratios", "Lower 95% CI", "Upper 95% CI")
sc <- sdcoef(tmod)
list(odds.ratios=oddr, std.coef=sc)
}

#-------------------------------------------------------------------------------

#br 2006-04
#function that creates a report and two charts for a logistic model
#arguments:
#	- DF = the training data frame
#	- tgt = the name of the binary target (actual events - 0/1)
#	- obj = the model object
#	- prob = the predicted probabilities from the scored model,  for example:
#		mform <- as.formula(paste(tgt, " ~ ", paste(pred, collapse =" + "), sep=""))
#		tmod <- glm(mform, family=binomial(link=logit), data=DF)
#		score <- predict(tmod, DF) ; prob <- 1/(1 + exp(-score))
#	  If NULL (default),  prob is computed as shown above.
#	- bins = the number of ranks/segments/deciles

logistic.report <- function(DF, tgt, obj, bins=10, prob=NULL)
{
#---actual classification
if (is.factor(DF[, tgt])) bt <- as.numeric(as.character(DF[, tgt]))
random.rate <- sum(bt) / length(bt)	#assuming tgt is coded 0/1
#---get the ranks via predicted & binned probabilities
if (is.null(prob)) {score <- predict(obj, DF) ; prob <- 1/(1 + exp(-score))}
prank <- rank(-prob, ties.method="first")
crank <- as.numeric(cut(prank, breaks=bins))
binsize <- as.data.frame(table(crank))
#---actual and predicted responses by rank
actual.events <- tapply(as.numeric(as.character(DF[, tgt])), crank, sum)
actual.events <- as.data.frame(actual.events)
actual.events$crank <- rownames(actual.events)
predicted.rate <- sapply(split(prob, crank), mean)
predicted.rate <- as.data.frame(predicted.rate)
predicted.rate$crank <- rownames(predicted.rate)
d <- merge(binsize, actual.events) ; d <- merge(d, predicted.rate) ; 
d <- d[order(as.numeric(as.character(d$crank))), ]
d$predicted.events <- round(d$Freq * d$predicted.rate)
d$actual.rate <- d$actual.events / d$Freq
d <- d[, c(1, 2, 3, 6, 5, 4)] ; colnames(d)[1] <- "Rank" ; rownames(d) <- 1:nrow(d)
d$CumFreq <- cumsum(d$Freq)
d$CumActEv <- cumsum(d$actual.events)
d$CumPredEv <- round(cumsum(d$predicted.events))
d$CumRandEv <- round(d$CumFreq * random.rate)
#---plots
#NB: since the actual classification is known,  use the % of actual - and not predicted -
#	events per rank to gauge the performance of the model
plot(c(0, d$CumFreq)/max(d$CumFreq), c(0, d$CumActEv)/max(d$CumActEv), type="o", col="red", 
     ylab="% of Cumulative Actual Events", xlab="% of Cumulative Training Sample", 
     main=paste("Lift Chart", project, sep=" - "), lty=2, lwd=1)
lines(c(0, d$CumFreq)/max(d$CumFreq), c(0, d$CumRandEv)/max(d$CumRandEv), type="o") ; grid()
legend("topleft", xjust=1, legend=c("Targeted Selection", "Random Selection"), lwd=3, 
       lty=c(2, 1), col=c("red", "black"))
#plot(as.numeric(d$Rank), d$actual.rate, type="o", ylab="% Actual Events", xlab="Rank", 
#	main=paste("Proportion of Actual Events", project, sep=" - "), lwd=1)
#abline(h=random.rate, col="blue", lty=2) ; grid()
#legend("topright", xjust=1, legend=c("Per Rank", "Overall"), lwd=3, lty=c(1, 2), 
#	col=c("black", "blue"))
boxplot(split(prob, crank), xlab="Rank", ylab="Probability",
        main=paste("Probabilities by Rank", project, sep=" - "))
abline(h=mean(prob), col="darkgreen", lty=2)
lines(as.numeric(d$Rank), d$actual.rate, type="o",col="blue")
abline(h=random.rate, col="brown", lty=3)
grid()
legend("topright", xjust=1, legend=c("Predicted (boxplot)", "Actual", "Mean Predicted", 
                                     "Mean Actual"), lwd=3, lty=c(1,1,2,3), col=c("black", "blue", "darkgreen", "brown"))
d
}

#-------------------------------------------------------------------------------

#br 2006-12
#function which combines a couple of model validation functions from package Design
#mfit = model fit object of class 'lrm'
#N = number of bootstrap validations

logistic.validate <- function(mfit, N=50, tofile=TRUE, ttl=project)
{
#---optimism-corrected model stats + actual vs. predicted
val <- validate(tmod, method="boot", B=N)
Cidx <- (val["Dxy", ]+1) / 2  #as documented on the help page
cal <- calibrate(tmod, B=N)
#---reports
if (tofile) {
    topdf(paste(ttl, "validation_predicted_vs_actual"))
    sink(paste(ttl, "validation.txt"))
}
cat("======Bootstrap model validation for ======", "\n") ; print(val)
cat("\n===C index===", "\n") ; print(Cidx[c(1:3, 5)])
cat("\n\n\n======Overfitting-corrected estimates of predicted vs. observed values======", "\n")
plot(cal, main=paste(ttl, "\noptimism-corrected estimates of Predicted vs. Actual"))
print(summary(cal))
if (tofile) {
    sink()
    dev.off()
}
}

#-------------------------------------------------------------------------------

#br 2008-01
#collapse a list of matrices or data frames, and add the names of the list nodes
#	as a column in the output matrix or data frame
#lst = list of data frame or matrices
#name = the name of the new column that will contain the list names
#2009-11: Changed idx from character to factor to improve efficiency.

lst.coll <- function(lst, name)
{
if (length(lst) == 0) return(lst)
lst <- lst.nonull(lst)  #delete NULL nodes
idx <- list()
for (i in 1:length(lst)) {
    x <- names(lst)[i]
    times <- if (class(lst[[i]]) %in% c('matrix','data.frame')) nrow(lst[[i]]) else 1
    idx[[i]] <- rep(x, times)
}
idx <- unlist(idx)
num <- isnum(idx)
out <- as.data.frame(cbind(do.call('rbind', lst)))
out[,name] <- if (num) as.numeric(as.character(idx)) else idx
rownames(out) <- 1:nrow(out)
out
}

#-------------------------------------------------------------------------------

#br 2006-11
#This function does single imputation (=only one predicted value is returned for
#  each missing value) of missing values using recursive partitioning,  which is 
#  an attractive alternative in many situations given the way trees can handle NAs.
#A regression model that predicts missing values of Xi based on Xj will not solve
#  the problem 100% if Xj is itself missing for some subjects; in such cases, two
#  possible solutions include:
#  - An algorithm that fits regression models in a loop and cycles through all 
#  sometimes-missing variables (such as transcan() from package Hmisc).
#  - Recursive partitioning with surrogate splits.
#Arguments:
#  - DF = the data frame. Caution: all variables from DF (except misspred) will
#  be considered for inclusion in the tree model, so pay attention to the data 
#  frame that you feed to this function (eliminate nonsensical predictors first).
#  - misspred = the name of the variable whose missing values are to be imputed
#Value:
#  - The original variable whose missing values have been filled in with predicted
#  values, and a report.

NA.impute.tree <- function(DF, misspred)
{
if (!exists("rpart")) require(rpart)
missform <- as.formula(paste(misspred, "~", paste(setdiff(colnames(DF), misspred), 
                                                  collapse=" + ")))
miss <- which(is.na(DF[, misspred]))
if (class(DF[, misspred]) == "factor") {
    treem <- rpart(missform, data=DF, method="class", control=rpart.control(xval=10, 
                                                                            maxcompete=4, maxsurrogate=5))
    pruned <- prune(treem, cp=0.01)
    pred <- predict(pruned, newdata=DF, type="class")
} else {
    treem <- rpart(missform, data=DF, method="anova", control=rpart.control(xval=10, 
                                                                            maxcompete=4, maxsurrogate=5))
    pruned <- prune(treem, cp=0.01)
    pred <- predict(pruned, newdata=DF, type="vector")
}
ximp <- DF[, misspred] ; ximp[miss] <- pred[miss]  #impute the NAs
#---report
cat(paste("======Recursive Partitioning imputation of NAs for", misspred, "======"), "\n")
printcp(pruned)
cat("===Original distribution", "\n") ; print(summary(DF[, misspred]))
cat("===NA replacements", "\n") ; print(summary(pred[miss]))
if (class(DF[, misspred]) != "factor") {
    cat("===Node predictions", "\n")
    print(tbl(pred[miss], name="Prediction"))
}
cat("===Imputed distribution", "\n") ; print(summary(ximp)) ; cat("\n\n")
ximp
}

#-------------------------------------------------------------------------------

#br 2006-11
#apply NA.impute.tree() to a data frame
#arguments:
#	- DF = the data frame with variables to impute (NB: do not include variables which
#	wouldn't make any sense as predictors of NAs in other variables,  because all vars
#	from DF are considered as candidates for tree predictions of missing values)
#	- toimpute = vector with the names of the predictors to impute
#value:
#	- a data frame whose missing values have been imputed via recursive partitioning,  and a report

NA.impute.tree.DF <- function(DF, toimpute)
{
impDF <- DF
for (i in toimpute) impDF[, i] <- NA.impute.tree(DF, i)
impDF
}

#-------------------------------------------------------------------------------

#br 2006-11
#function which does simple univariate imputation of missing values
#'univariate' means that all other information is ignored: the missing values
#	are replaced with a sample of the non-missing values,  or with the 
#	mean/median/mode of the non-missing values
#NB: for numerics all methods will work (though depending on the values using
#	type = "mode" may not make any sense whatsoever); for factors,  the valid
#	univariate imputation methods are 'sample' and 'mode'
#arguments:
#	- x = numeric or factor with missing values
#	- type = the type of imputation to use ('sample', 'mean', 'median', 'mode')
#	- lab = label for reporting
#value:
#	- a vector of the same class as x with the missing values filled in with a sample
#	from the non-missing values,  their mean,  median or mode

NA.impute.univ <- function(x, type, lab=NULL)
{
if (!type %in% c("sample", "mean", "median", "mode")) stop("type needs to be one of 'sample', 'mean', 'median', 'mode'.")
miss <- which(is.na(x))
if (type == "sample") {
    filling <- sample(x[-miss], length(miss), replace=TRUE)
} else if (type == "mean") {
    if (class(x) == "factor") stop("can not use 'mean' for factors.")
    filling <- mean(x[-miss])
} else if (type == "median") {
    if (class(x) == "factor") stop("can not use 'median' for factors.")
    filling <- median(x[-miss])
} else if (type == "mode") {
    filling <- names(which.max(table(x[-miss])))
    if (class(x) != "factor") {
        cat("===WARNING: 'mode' may not make any sense whatsoever for numerics.", "\n")
        filling <- as.numeric(filling)
    }
}
ximp <- x ; ximp[miss] <- filling
#---a report
if (is.null(lab)) lab <- deparse(substitute(x))
if (type != "sample") cat(paste("The", length(miss), "missing values of", lab, "were replaced with", filling, "."), "\n")
if (type == "sample") {
    cat(paste("The", length(miss), "missing values of", lab, "were replaced with random values from this distribution:"), "\n")
    print(summary(filling))
}
ximp
}

#-------------------------------------------------------------------------------

#br 2006-11
#this function takes a data frame with missing values and returns one where the missing
#	values were imputed via a simple univariate imputation method (mean, median, mode or sample)
#arguments:
#	- DF = the data frame
#	- numtype/factype = the imputation method to use for numerics/factors 
#	(see NA.impute.univ() for details)
#	- exclude = variables to exclude (for example transformations of another variable)

NA.impute.univ.DF <- function(DF, numtype, factype, exclude=NULL)
{
if (!is.null(exclude)) DF <- DF[,setdiff(colnames(DF), exclude)]
num <- colnames(DF)[sapply(as.list(DF), class) != "factor"]
fact <- colnames(DF)[sapply(as.list(DF), class) == "factor"]
impDF <- DF
for (i in num) if (NA %in% DF[, i]) impDF[, i] <- NA.impute.univ(DF[, i], numtype, lab=i)
for (i in fact) if (NA %in% DF[, i]) impDF[, i] <- NA.impute.univ(DF[, i], factype, lab=i)
impDF
}

#-------------------------------------------------------------------------------

#br 2006-08
#function which produces a normal Q-Q plot and does two normality tests
#for whatever reason,  the Shapiro-Wilk test in R (and S-Plus) only works when
#	the sample size is between 3 and 5000

norm.plot <- function(x, ...)
{
qqnorm(x, col="gray30", ...)
qqline(x, col="red", lwd=2) ; grid()
norm.tests <- vector()
norm.tests[1] <- paste("N=", length(which(!is.na(x))), "; H0=normality", sep="")
if (!exists("jarque.bera.test")) require(tseries)	
norm.tests[2] <- paste("p=", round(jarque.bera.test(na.omit(x))$p.value, 4), " Jarque Bera", sep="")
if (length(x) <= 5000) {
    norm.tests[3] <- paste("p=", round(shapiro.test(na.omit(x))$p.value, 4), " Shapiro-Wilk", sep="")
}
legend("topleft", xjust=1, legend=norm.tests)
}

#-------------------------------------------------------------------------------

#br 2008-02
#Given a data frame, this function reports the names of the variables identified as containing outlier values.
#	The algorithm compares the range of the most extreme percentile (first/last) with the range of
#	the next percentile going in the direction towards the mean. If the ratios of these ranges exceed X,
#	the variable is flagged as containing outliers.
#Arguments:
#	- DF = data frame
#	- ratio = ratio of percentile ranges which, if exceeded, triggers an outlier warning
#	- probcount = vector with probabilities to be used for reports (counts and percentages of the points
#	beyond these thresholds)
#Output:
#	- a data frame indicating the names of the vars with suspected outliers, plus: number and percentage
#	of points affected in both directions (before/after 2 given probabilities), the thresholds to be
#	used for corrections, and whether outliers were identified near the min or the max

outlier.find <- function(DF, ratio, probcount=c(0.01, 0.99), except='')
{
fact <- rep(0, ncol(DF)) ; fact[sapply(as.list(DF), class) %in% c("factor","character")] <- 1
if (sum(fact) == ncol(DF)) return("There are no numeric vars in this data frame.")
num <- colnames(DF)[sapply(as.list(DF), class) %in% c("numeric","integer")]
num <- setdiff(num, except)
outlier <- countsup <- countsdn <- outmax <- outmin <- thmax <- thmin <- vmin <- vmax <- N <- rep(NA, length(num))
for (i in 1:length(num)) {
    v <- DF[,num[i]]
    if (length(unique(v)) < 20) next
    qtl <- quantile(v, probs=c(0, 0.01, 0.02, 0.98, 0.99, 1), na.rm=TRUE)
    quart <- quantile(v, probs=c(0.25, 0.75), na.rm=TRUE)
    iqr <- (quart[2] - quart[1])/50  #interquartile range / 50
    dmax1 <- qtl['100%'] - qtl['99%'] ; dmax2 <- qtl['99%'] - qtl['98%']
    dmin1 <- qtl['1%'] - qtl['0%'] ; dmin2 <- qtl['2%'] - qtl['1%']
    thup <- quantile(v, max(probcount), na.rm=TRUE)  #upper threshold
    thdn <- quantile(v, min(probcount), na.rm=TRUE)
    counts.up <- length(which(v > thup))  #the no. of points in the extremes
    counts.dn <- length(which(v < thdn))
    if (counts.up > nrow(DF)/5) counts.up <- 0  #possible in the case of many identical values
    if (counts.dn > nrow(DF)/5) counts.dn <- 0
    #NB: the ranges of the first/last couple of percentiles may be 0
    if (dmax2 > 0) {
        ratio.max <- (dmax1) / (dmax2)
    } else if (dmax1 > 0) {
        ratio.max <- if (dmax1 > iqr*4) ratio+1 else ratio-1
    } else ratio.max <- 0
    if (dmin2 > 0) {
        ratio.min <- (dmin1) / (dmin2)
    } else if (dmin1 > 0) {
        ratio.min <- if (dmin1 > iqr*4) ratio+1 else ratio-1
    } else ratio.min <- 0
    if (ratio.max >= ratio | ratio.min >= ratio) {
        outlier[i] <- num[i]
        countsup[i] <- (ratio.max >= ratio) * counts.up
        countsdn[i] <- (ratio.min >= ratio) * counts.dn
        outmax[i] <- ratio.max >= ratio
        outmin[i] <- ratio.min >= ratio
        thmax[i] <- thup ; thmin[i] <- thdn
        vmin[i] <- qtl['0%'] ; vmax[i] <- qtl['100%']
        N[i] <- length(which(!is.na(v)))
    }
}
pct <- round((countsup + countsdn) / nrow(DF), 3) * 100
out <- data.frame(outlier, N, vmin, thmin, outmin, countsdn, vmax, thmax, outmax, countsup, pct, stringsAsFactors=FALSE)
out <- out[!is.na(outlier),]
out
}

#-------------------------------------------------------------------------------

#br 2006-04
#Use the output returned by outlier.find() to replace outlier values in a data frame.
#	The previous version of outlier.replace() is less convenient to use because
#	it starts from scratch and doesn't take advantage of the processing done by
#	outlier.find().
#DF = the data frame
#ol = the output from outlier.find

outlier.replace <- function(DF, ol)
{
tDF <- DF
for (i in 1:nrow(ol)) {
    v <- ol$outlier[i]
    if (ol$outmin[i]) {
        cat(paste(v, ": ", ol$countsdn[i], " values in [", ol$vmin[i], ", ", ol$thmin[i], 
                  ") were replaced with ", ol$thmin[i], sep=''), "\n")
        tDF[,v][tDF[,v] < ol$thmin[i]] <- ol$thmin[i]
    }
    if (ol$outmax[i]) {
        cat(paste(v, ": ", ol$countsup[i], " values in (", ol$thmax[i], ", ", ol$vmax[i], 
                  "] were replaced with ", ol$thmax[i], sep=''), "\n")
        tDF[,v][tDF[,v] > ol$thmax[i]] <- ol$thmax[i]
    }
}
invisible(tDF)
}

#-------------------------------------------------------------------------------

#br 2005-09, rewritten 2008-03
#customized version for parallel coordinate plots
#2008-08 - added a very significant enhancement, combining the two functions initially written
#	to address only numerical or only categorical variables. The updated function accepts and correctly
#	plots data frame containing characters and factors in addition to numerics.
#2019-03: Made a number of tweaks and improvements.
#function that plots each row of a data frame as a line
#DF = data frame with the rows to plot
#tgt = a vector whose values are used to generate line colors
#topo = whether to generate topographical or quantile colors

parplot <- function(DF, clr=NULL, tgtlab=NULL, topo=TRUE, digits=0, cex=0.7, rand=NULL, decreasing=TRUE, bins=NULL, ...)
{
#---setup the line colors
if (is.null(clr)) {
    linecol <- rep("gray60", nrow(DF))  #line colors
} else {
    #sidx <- order(tgt, decreasing=decreasing)
    tgt <- DF[,clr]
    sidx <- if (decreasing) order(tgt) else order(-1*tgt)
    # DF <- cbind(tgt, DF) ; 
    DF <- DF[sidx,]  #arrange to plot from least to most interesting
    tgt <- tgt[sidx]  
    # colnames(DF)[1] <- if (is.null(tgtlab)) 'target' else tgtlab
    if (topo) {
        linecol <- topoclr(tgt, bins=bins) 
    } else {
        #probs <- if (decreasing) c(100, 95, 90, 80, 30, 0)/100 else sort(c(100, 95, 90, 80, 30, 0)/100)
        forcol <- if (decreasing) tgt else -1*tgt
        qtl <- as.vector(quantile(forcol, probs=c(100, 95, 90, 80, 30, 0)/100))
        colvec <- c("red", "purple", "limegreen", "tan", "yellow")
        linecol <- rep("yellow", length(tgt))
        for (i in 1:(length(qtl)-1)) {
            linecol[forcol <= qtl[i] & forcol > qtl[i+1]] <- colvec[i]
        }
    }
}
cc <- sapply(DF, class)
nDF <- DF[,cc %in% c('numeric','integer'),drop=FALSE]
cDF <- DF[,!cc %in% c('numeric','integer'),drop=FALSE]
if (ncol(nDF) > 0) {
    #---rescale between 0 and 1 and compute some labels
    xs <- apply(nDF, 2, function(x) {
        if (!is.numeric(x)) x <- as.numeric(x)
        if (length(unique(x)) == 1) return(rep(0.5, length(x)))
        out <- (x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
        out <- jitter(out, amount=rand)
        out})
    xlab <- lapply(nDF, function(x) {
        if (!is.numeric(x)) x <- as.numeric(x)
        if (length(unique(x)) == 1) return(data.frame(x=unique(x), y=0.5))
        if (length(unique(x)) < 30) {
            lab <- sort(unique(x))
            num <- (lab - min(lab, na.rm = TRUE))/(max(lab, na.rm = TRUE) - min(lab, na.rm = TRUE))
            return(data.frame(x=lab, y=num))
        } else {
            out <- min(x, na.rm = TRUE) + seq(0, 1, by=0.1) * (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
            data.frame(x=out, y=seq(0, 1, by=0.1))
        }
    })
}
#this needs to be revised, it breaks with character variables
if (ncol(cDF) > 0) {
    #---rescale the predictors
    pts <- lbl <- list()
    for (i in 1:ncol(cDF)) {
        idx <- colnames(cDF)[i]
        # table(as.numeric(cDF[,i]), cDF[,i])
        #NB: If a subset of the data is passed as an input, then some factors may not have all the levels present.
        numhere <- sort(unique(as.numeric(cDF[,i])))
        lev <- levels(cDF[,i])
        lev <- data.frame(x=as.numeric(lev), y=1:length(lev))
        lev$y <- lev$y / nrow(lev)  #must calculate the rescaled values before any levels are dropped
        lev <- lev[numhere, ]
        #the rescaled values
        if (nrow(lev) == 1) {
            pts[[idx]] <- rep(0.5, nrow(cDF))
        } else {
            pts[[idx]] <- pmin(1, pmax(0, as.numeric(cDF[,i]) / length(levels(cDF[,i]))))
        }
        lbl[[idx]] <- lev
        #do not use the same values,  otherwise a thousand lines appear as one and the plot is _useless_
        pts[[idx]] <- jitter(pts[[idx]], amount=rand)
    }
    pts <- do.call('cbind', pts)
}
if (ncol(nDF)*ncol(cDF) > 0) {
    ax <- cbind(xs, pts) ; alab <- c(xlab, lbl)
} else if (ncol(nDF) > 0) {
    ax <- xs ; alab <- xlab
} else if (ncol(cDF) > 0) {
    ax <- pts ; alab <- lbl
}
ax <- ax[, colnames(DF)]  #keep the same column order, instead of moving any factors to the end
#---plot
plot(c(1, ncol(DF)), c(0, 1), type="n", axes=FALSE, xlab='', ylab='', ...)
#axis(2, tick=TRUE) ; axis(4, tick=TRUE)
axis(1, at=1:ncol(ax), lab=colnames(ax), tick=FALSE)
box(which="plot", lty="solid") ; grid(nx=NA, ny=NULL)
for (i in 1:nrow(DF)) {lines(1:ncol(DF), ax[i, ], col=linecol[i])}
#boxp(xs, add=TRUE, pts=FALSE, border="blue", stats=FALSE, noname=TRUE)  #add box plots
for (i in 1:length(alab)) {
    id <- which(colnames(ax) == names(alab)[i])
    text(rep(id, nrow(alab[[i]])), alab[[i]]$y, round(alab[[i]]$x, digits), cex=cex)
}
}

#-------------------------------------------------------------------------------

#2019-03: Parallel Coordinates plots split By Decile. Must be redirected to a file, as it
#	uses the base R Graphics.
pcbd <- function(DF, tgt, fname='plt', topo=TRUE, digits=0, bins=10)
{
probs <- round(seq(0, 1, by=1/bins), 2)
ranges <- quantile(DF[,tgt], probs=probs)
topdf(fname)
for (i in 2:length(ranges)) {
    idx <- which(DF[,tgt] >= ranges[i-1] & DF[,tgt] < ranges[i])
    if (i == length(ranges)) idx <- which(DF[,tgt] >= ranges[i-1] & DF[,tgt] <= ranges[i])
    pDF <- DF[idx, ]
    parplot(pDF, clr=tgt, digits=digits, topo=topo, main=paste(names(ranges)[(i-1):i], collapse=' to '))
}
dev.off()
}

#-------------------------------------------------------------------------------

#br 2006-11
#given a list of vectors with the names of the predictors retained after backward
#  eliminations on bootstrap samples, this function produces two reports
#pred.kept = list of factors retained, each vector representing the content of fastbw(...)$names.kept
#minpct = threshold in percentage points (if lower than this, the predictor is not plotted)

pred.kept.rep <- function(pred.kept, minpct=0.04)
{
N <- length(pred.kept)
#---counts of predictors kept in all models
allkept <- unlist(pred.kept)
akt <- tbl(allkept, name="variable")[, c(1, 2)]
akt <- akt[order(-akt$freq, akt$variable), ]
akt$iter <- rep(N, nrow(akt)) ; akt$PctKept <- akt$freq / akt$iter
akt$variable <- as.character(akt$variable)
rownames(akt) <- 1:nrow(akt)
akt <- akt[akt$PctKept >= minpct,]
dotpl(akt$PctKept, lbl=akt$variable, xlab="Percent", 
      main=paste("Percent each predictor was kept,  out of", N, 
                 "\nfast backward eliminations on bootstrap samples"))
#---predictors per model
ppm <- sapply(pred.kept, length)
gtbl(ppm, main="Number of Predictors per model")
ppm <- tbl(ppm, name="predictors")[, c(1:3)]
list(pred.retained=akt, pred.per.model=ppm)
}

#-------------------------------------------------------------------------------

#br 2005-10
#quantile-difference plot

qdif <- function(v1, v2, ...)
{
q1 <- quantile(v1, probs=(0:100)/100)
q2 <- quantile(v2, probs=(0:100)/100)
plot(q1-q2, type="o", xlab="Quantiles", ylab="Difference", col="gray30", pch=20, ...)
grid() ; abline(h=0, col="red", lty=2)
mtext(paste("N = ", length(v1), " / ", length(v2), sep=""), col="navy", line=-1)
#mtext(paste("[", deparse(substitute(srs1)), " - ", deparse(substitute(srs2)), "] quantile diff", sep=""), line=-1, cex=0.8)
}

#-------------------------------------------------------------------------------

#br 2006-08 - recursive merging (outer joins) of data frames
#This function takes a list of data frames and merges them into one data frame, similar to
#	what a SAS DATA STEP with a MERGE statement does. The data frames are assumed to
#	have one common column name (the one used for joins) and unique name(s) for the
#	remaining column(s).

rmerge <- function(ldfr)
{
if (class(ldfr) != "list") stop("The input needs to be a list of data frames.")
dfr <- ldfr[[1]]
if (length(ldfr) == 1) return(dfr)
#cat(paste("Started with",nrow(ldfr[[1]]),"rows from",names(ldfr)[1]),"...","\n")
for (i in 2:length(ldfr)) {
    dfr <- merge(dfr,ldfr[[i]],all=TRUE)
    #	cat(paste("Adding ",nrow(ldfr[[i]])," rows from ",names(ldfr)[i]," (",nrow(dfr)," rows so far)...",sep=""),"\n")
}
if (length(grep("[a-z]",dfr[,1],ignore.case=TRUE)) == 0) {
    dfr <- dfr[order(dfr[,1]),]
} else dfr <- dfr[order(as.character(dfr[,1])),]
rownames(dfr) <- 1:nrow(dfr)
#cat(paste("Got",nrow(dfr),"rows after recursive merging:"),"\n")
#print(head(dfr)) ; print(tail(dfr))
dfr
}

#-------------------------------------------------------------------------------

#br 2004-12
#This function produces a highly customized bivariate plot, combining a scatter
#  plot, image and contour plot, loess, rug and box plots.
#Arguments:
#  - x, y = the variables to plot
#  - pts = TRUE to show the individual points, FALSE otherwise
#  - kde = TRUE to add the image and contour plots, FALSE otherwise; based on 
#    two-dimensional kernel density estimation evaluated on a square grid
#  - p = the number of grid points in each direction (used if kde = TRUE)
#  - xjit / yjit = the random noise to add to the x/y values
#  - kdesamp = size of random sample for kde estimation (to be used when the vectors
#    are so large that not enough memory is available to compute the 2d density)
#	- dorug = whether to add a rug plot
#	- loess = whether to add a loess line

scatter <- function(x, y, pts=TRUE, kde=FALSE, p=200, dorug=FALSE, xjit=NULL, 
                yjit=NULL, kdesamp=NULL, loess=TRUE, log='', col='purple', ...)
{
#---remove NA/NaNs,  if any
na.test(x) ; na.test(y)
naobs <- unique(c(which(is.na(x)), which(is.na(y))))
if (length(naobs) > 0) {x <- x[-naobs] ; y <- y[-naobs]}
if (!is.null(xjit)) x <- x + runif(length(x), min=-xjit, max=xjit)
if (!is.null(yjit)) y <- y + runif(length(y), min=-yjit, max=yjit)
plot(x, y, type="n", log=log, ...) #setup the plot area
if (pts) points(x, y, col=col, ...)
#---image/contour plot
if (kde) {
    if (!exists("kde2d")) library(MASS)
    forkde <- if (!is.null(kdesamp)) sample(1:length(x), kdesamp, replace=FALSE) else 1:length(x)
    d <- kde2d(x[forkde], y[forkde], n=p)
    #image(d, add=TRUE, col=colorRampPalette(c("white", "gray55"))(12))
    contour(d, add=TRUE)
}
grid()
#---loess,  rug and box plots
if (loess) try(lines(loess.smooth(x, y), col="red"))
if (dorug) {rug(jitter(x), side=1, col="gray50") ; rug(jitter(y), side=2, col="gray50")}
#---also show the 25-50-75th percentiles
axis(side=1, at=median(x), col="navy", lwd=4, labels=FALSE)
axis(side=1, at=quantile(x, probs=0.25), col="blue", lwd=2, labels=FALSE)
axis(side=1, at=quantile(x, probs=0.75), col="blue", lwd=2, labels=FALSE)
axis(side=2, at=median(y), col="navy", lwd=4, labels=FALSE)
axis(side=2, at=quantile(y, probs=0.25), col="blue", lwd=2, labels=FALSE)
axis(side=2, at=quantile(y, probs=0.75), col="blue", lwd=2, labels=FALSE)
mtext(paste("N=", length(x), sep=""), line=-1, col="navy")
}

#-------------------------------------------------------------------------------

#br 2006-05
#function that computes the standardized coefficient for a predictor,  given the model object
#code borrowed from relimp() from package relimp
#arguments:
#	- obj = a fitted model
#2008-02: some errors came up when trying to use this function on a model fit with ols()
#	from Design. It turned out the issue was that for factors, '=' was used in some
#	places but not in others (so for example BUSINESS showed up as both BUSINESS=Y
#	and BUSINESSY). Given this, I added another argument to this function, noeq,
#	which if TRUE uses both version, with and without '='.

sdcoef <- function(obj, noeq=TRUE)
{
#covmat <- vcov(obj)
coefs <- coef(obj) ; pred <- names(coefs)
if (!is.matrix(obj$x)) modelmatrix <- model.matrix(obj) else modelmatrix <- obj$x
sdc <- list()
for (p in setdiff(pred, "Intercept")) {
    pnoeq <- if (noeq) gsub('=','', p) else p
    X <- modelmatrix[TRUE, pnoeq, drop=FALSE]
    X <- sweep(X, 2, apply(X, 2, mean))
    beta <- coefs[p, drop=FALSE]
    sdc[[p]] <- sd(X %*% beta) * sign(coefs[pred == p])
}
asdc <- do.call("rbind", sdc)
asdc <- as.data.frame(asdc[order(-abs(asdc[, 1])), ])
colnames(asdc) <- "Standardized Coefficients"
asdc
}

#-------------------------------------------------------------------------------

#br 2006-08
#function that standardizes each column from a data frame (factors are discarded)
#the coded standardization methods are:
#	- STD: x[i] = (x[i] - mean(x)) / sd(x)
#	- MAD (more robust): x[i] = (x[i] - median(x)) / mad(x)
#arguments:
#	- DF = a data frame
#	- method = the standardization method to use

stdize <- function(DF, method="STD")
{
if (!method %in% c('STD', 'MAD')) stop("Standardization method currently needs to be one of STD/MAD.")
#---discard the factors,  if any
fact <- rep(0, ncol(DF))
for (i in 1:ncol(DF)) if (is.factor(DF[, i])) fact[i] <- 1
DF <- DF[, fact == 0]
#---get the center and scale to use for each column
if (toupper(method) == "STD") {
    centers <- colMeans(DF, na.rm=TRUE)
    scales <- sapply(DF, sd, na.rm=TRUE)
} else if (toupper(method) == "MAD") {
    centers <- sapply(DF, median, na.rm=TRUE)
    scales <- sapply(DF, mad, na.rm=TRUE)
}
#---standardize
#step1 <- sweep(DF, 2, centers, "-") ; sdfr <- (step1, 2, scales, "/")
sdfr <- scale(DF, center=centers, scale=scales)
invisible(as.data.frame(sdfr))
}

#-------------------------------------------------------------------------------

#br 2011-04
#Given a document, apply some pre-processing and then replace all related words. Serves basically
#	as a handle for stemWords(). The output is a vector of words where the related words
#	were replaced by the shortest one.

stemDoc <- function(doc)
{
if (!exists('Corpus')) require(tm)
if (!exists('getDict')) {require(wordnet) ; setDict('C:/work/dict')}
#doc <- removeWords(tolower(doc), stopwords("english"))
#x <- Content(doc)
#x <- gsub("[[:punct:]]+", "", x)
#x <- gsub("[[:space:]]+", " ", x)
#con <- textConnection(x) ; words <- scan(con, what="character", quiet=TRUE) ; close(con)
x <- stdTxt(Content(doc))
words <- v2w(x)
sw <- stemWords(unique(words))
uniw <- length(unique(words))
for (x in sw$match) words[words %in% x] <- x[which.min(nchar(x))]
cat(paste('\nStarted with', uniw, 'unique words, ended with', length(unique(words))), '\n')
words
}

#-------------------------------------------------------------------------------

#br 2011-04
#I actually ended up writing my own function to stem a given vector of words, because at 1st sight
#	I wasn't entirely pleased with the output produced by Porter's stemming algorithm.
#	The idea is to group words based on their first 3, 4, ... characters, and pick as
#	matching the words whose suffixes are included in a given list of legal suffixes.
#	I built the list of suffixes manually, by examining several scenarios, and this may
#	need some tweaks going forward. While the function can be expected to miss some words
#	as well as falsely group others, the impact should be minor. A large advantage is
#	that, unlike with Porter's algorithm, all words in the output are legal English words,
#	so this can be quite helpful for pre-processing the words to be fed into a synonym
#	replacing function. Further comparisons with Porter's stemming algorithm are advisable.
#x = character vector with the words of interest

stemWords <- function(x, suffixes=c('','s','d','r','ed','ly','e','ing','ings','er','ers','ies','y',
                                'ial','es','ion','rs','ning','ised','ising','ist','ation','ial','ialist','isation','ise',
                                'itive','itively','ical','ism','isms'),
                  print=TRUE)
{
match <- nomatch <- list()
for (i in 3:max(nchar(x))) {
    y <- split(x, substr(x, 1, i))
    del <- which(sapply(y, length) == 1)  #no matches can be found here
    check <- which(sapply(y, length) > 1)  #the groups that need to be checked
    #why i:i+4: to be able to group words which belong together but are separated by some of the
    #	longer suffixes, for example existentialism and existentialist
    maybe <- y[check][sapply(y[check], function(x) min(nchar(x))) %in% i:(i+4)]
    if (length(maybe) > 0) {
        #get the suffixes and determine the correct pairs; NB: the groups of words that match
        #	should be formed word by word, not by requiring that all suffixes match the vector
        #	given, as that fails when compound or unrelated words are present (e.g. car, cars, carmaker)
        takeout <- listlike(names(maybe))
        for (s in names(maybe)) {
            suff <- gsub(s, '' ,maybe[[s]])
            fine <- which(suff %in% suffixes)
            if (length(fine) >= 2) {
                mat <- maybe[[s]][fine] ; nomat <- setdiff(maybe[[s]], mat)
                x <- setdiff(x, takeout)
                match <- c(match, list(mat))
                nomatch <- c(nomatch, list(nomat))
            } else {
                nomat <- maybe[[s]]
            }
            takeout[[s]] <- nomat[nchar(nomat) == i]
        }
        #---previous approach; discontinued as testing all suffixes simultaneously was prone to failure
        #suffok <- sapply(suff, function(x) !FALSE %in% (x %in% suffixes))
        #mat <- maybe[suffok] ; nomat <- unlist(maybe[!suffok])
        #match <- c(match, mat)
        #nomatch <- c(nomatch, nomat)
    } else mat <- takeout <- NULL
    #words to remove: those without any pair; those matched; and those not matched but of length i
    nomat <- unlist(nomatch)[nchar(unlist(nomatch)) == i]
    remove <- c(unlist(y[del]), unlist(match), unlist(takeout))
    x <- setdiff(x, remove)
}
unmatched <- sort(setdiff(unlist(nomatch), unlist(match)))
if (print) {
    cat('======Matches found:', '\n')
    forord <- sapply(match, function(x) x[which.min(nchar(x))])
    match <- match[order(forord)]
    mt <- strwrap(paste(sapply(match, function(x) paste(x, collapse=' + ')), collapse='  |  '), width=110)
    for (m in mt) cat(m, '\n')
    cat('\n======Unmatched:', '\n')
    um <- strwrap(paste(unmatched, collapse=' '), width=110)
    for (u in um) cat(u, '\n')
}
list(match=match, nomatch=unmatched)
}

#-------------------------------------------------------------------------------

#bogdan romocea 2010-01
#Given a frequency distribution created by tbl(), trim it to show no more than N rows.
#	The issue is that some factors have a huge number of levels (in the hundreds or
#	thousands, or more), and printing or plotting them all is not helpful at all.
#tb = frequency table from tbl()
#N = max number of rows to keep
#byfreq = whether the retained rows should be chosen by descending frequency

tbl.collapse <- function(tb, N=19, byfreq=TRUE)
{
if (byfreq) tb <- sortDF(tb, 'freq', decreasing=TRUE)
if (nrow(tb) <= N) {
    colltb <- tb
} else {
    keep <- head(tb, N)
    if (is.numeric(keep$what)) keep$what <- n2c(keep$what)
    others <- data.frame(paste('OTHERS (', nrow(tb)-N, ')', sep=''), sum(tb$freq[-c(1:N)]),
                         round(100*sum(tb$freq[-c(1:N)]) / sum(tb$freq), 2), NA, NA)
    colnames(others) <- colnames(keep)
    colltb <- rbind(others, keep)  #assuming here that in many cases OTHERS will have the highest frequency
}
rownames(colltb) <- NULL
#NB: cumfreq and cumpct must be recomputed.
colltb$cumfreq <- cumsum(colltb$freq)
colltb$cumpct <- round(100*colltb$cumfreq/sum(colltb$freq), 2)
colltb
}

#-------------------------------------------------------------------------------

#br 2007-08
#given 3 vectors, form a matrix

tomatrix <- function(x, y, value)
{
mat <- matrix(nrow=length(unique(x)), ncol=length(unique(y)))
rownames(mat) <- sort(unique(x))
colnames(mat) <- sort(unique(y))
for (i in 1:length(x)) mat[as.character(x)[i], as.character(y)[i]] <- value[i]
#mat[which(is.na(mat))] <- 0
mat
}

#-------------------------------------------------------------------------------

#2019-03: Produce a terminal node report from trees built with the new partykit package. The rules
#	are included. Some formatting is needed, in particular to get rid of NAs and quotes and make the
#	rules more compact.
#Added another feature: the capability to summarize other fields ('extra') by the tree nodes if requested.
#	That argument is assumed to be the data frame from which the tree was built, but including only
#	the columns to be summarized.
treeRules <- function(tr, extra=NULL)
{
lrp <- utils::getFromNamespace(".list.rules.party", "partykit")  #found through web search
rules <- lrp(tr)
if (is.null(names(rules))) return(NULL)
rules <- ggsub(rules, c('\"NA\", ', ', \"NA\"', '\"'))  #clean up
rules <- data.frame(node=as.numeric(names(rules)), rule=rules)
nodes <- predict(tr, type = "node")
pred <- data.frame(node = nodes, resp = predict(tr, type = "response"))
pred <- do.call(data.frame, aggregate(data=pred, resp ~ node, function(x) c(avg=mean(x), N=length(x))))
out <- merge(pred, rules)
if (!is.null(extra)) {
    ext <- as.data.frame(rstack(lapply(split(extra, nodes), function(x) apply(x, 2, function(x) round(mean(x), 4)))))
    ext$node <- as.numeric(rownames(ext))
    out <- merge(ext, out)
}
out
}

#-------------------------------------------------------------------------------

#2019-03: Decided to collapse the tree rules as reported by the partykit package. The problem there
#	is that all splits are reported in hierarchical fashion, which makes them however much harder to
#	read especially as the number of terminal nodes goes up. For example: [v <= 20 & v <= 10 and v > 5] 
#	simply means that v=(5,10] or even shorter, v=10.
#The algorithm is a little odd but it works. Binary splits are assumed at this time (no %in%).
treeRulesCollapse <- function(DF, rules)
{
if (is.null(rules)) return(rules)
ru <- f2c(rules$rule)
simple <- ru
for (i in 1:length(ru)) {
    crit <- strsplit(ru[i], ' & ')[[1]]
    pred <- fromstr(crit, ' ', 1)
    tp <- table(pred)
    if (max(tp) > 1) for (av in names(tp)[tp > 1]) {  #only those with 2+ criteria need to be collapsed
        todo <- grep(paste0(av, ' '), crit)  #NB: note the blank - to avoid cases such as 'l' also matching 'lg', 'lm', etc
        toadj <- crit[todo]
        tmp <- DF[,av]
        #xx was chosen to reduce the likelihood of collisions (such as 'sel' becoming 'seltmp' after ggsub if av=l)
        expr <- paste('xx <- which (', paste(toadj, collapse=' & '), ')')
        expr <- ggsub(expr, av, 'tmp')
        eval(parse(text=expr))  #create the xx vector
        actual <- as.character(range(DF[xx, av]))
        coll <- if (length(unique(actual)) == 1) paste0(av, '=', actual[1]) else paste0(av, '=[', actual[1], ', ', actual[2], ']')
        crit <- c(crit[-todo], coll)  #the collapsed criteria
    }
    simple[i] <- paste(crit, collapse=' & ')
}
out <- rules
out$rule <- simple
out
}

#-------------------------------------------------------------------------------

#br 2006-06
#replace the extreme values of a vector with the specified quantiles

trim <- function(x, probs=c(0.01,0.99))
{
orig <- quantile(x, probs=c(0,1), na.rm=TRUE)
quant <- quantile(x, probs=probs, na.rm=TRUE)
x[x < min(quant)] <- min(quant)
x[x > max(quant)] <- max(quant)
rep <- data.frame(OriginalRange=orig, NewRange=quant)
print(rep)
x
}

#-------------------------------------------------------------------------------

#br 2006-01
#function that returns a decent set of topographic colors
#nbeg, pbeg, nend, pend = colors starting/ending the range of negative/positive numbers
#2007-11: so far the color algorithm used only the order of the values, leading to
#	poor differentiation between moderate and very large values when there were
#	few values in between. For example, 5 and 500 will appear far apart if there
#	are many numbers in between, but will be color neighbors in case there are no
#	values in between. This defficiency was corrected by assigning the values to
#	bins of given length, then keeping only the colors of the bins containing values
#	plus those of some empty bins in between, as appropriate, to create the contrast
#	the numbers themselves exhibit.
#Actually, it became apparent one additional step is needed: don't allow more than
#	3-4 empty consecutive bins, otherwise a top value like 50000 will overshadow
#	all hundreds, which will appear in the same yellowish color, even though the
#	bin classification is fine. The same problem of color mis-representation is
#	propagated in the other direction without a correction - the outliers would be
#	too visible and would make everyone else look largely the same.
#binsize = the length of the interval used to classify the values of x
#maxbinjump = the max number of empty consecutive bins to tolerate, so that the
#	outliers don't become too visible at the expense of the remaining values

topoclr <- function(x, bins=NULL, maxbinjump=3, nbeg='blue', nend='slategray1', 
                pbeg='lemonchiffon', pend='brown4', zerocol='darkseagreen3')
{
maxx <- max(x, na.rm=TRUE)
minx <- min(x, na.rm=TRUE)
binsize <- if (is.null(bins)) (maxx - minx) / 50 else (maxx - minx) / bins
#---generate the colors to use
if (minx < 0) {
    negbins <- seq(0, minx-binsize, by=-binsize)
    ptsperbin <- table(cut(x[x < 0], negbins))
    #---don't allow too many empty bins between points, or the outliers will obscure the other values
    finpts <- rep(NA, length(ptsperbin))
    conszero <- 0
    for (i in 1:length(ptsperbin)) {
        if (ptsperbin[i] > 0) {finpts[i] <- ptsperbin[i] ; conszero <- 0}
        if (ptsperbin[i] == 0) conszero <- conszero + 1
        if (conszero > maxbinjump) next
        if (ptsperbin[i] == 0) finpts[i] <- 0
    }
    finpts <- finpts[!is.na(finpts)]
    negcol <- colorRampPalette(c(nbeg, nend))(length(finpts))
    finnc <- vector('list', length(finpts))
    for (i in 1:length(finpts)) finnc[[i]] <- rep(negcol[i], finpts[i])
    cneg <- unlist(finnc)
}
if (maxx > 0) {
    posbins <- seq(0, maxx+binsize, by=binsize)
    ptsperbin <- table(cut(x[x > 0], posbins))
    #---don't allow too many empty bins between points, or the outliers will obscure the other values
    finpts <- rep(NA, length(ptsperbin))
    conszero <- 0
    for (i in 1:length(ptsperbin)) {
        if (ptsperbin[i] > 0) {finpts[i] <- ptsperbin[i] ; conszero <- 0}
        if (ptsperbin[i] == 0) conszero <- conszero + 1
        if (conszero > maxbinjump) next
        if (ptsperbin[i] == 0) finpts[i] <- 0
    }
    finpts <- finpts[!is.na(finpts)]
    poscol <- colorRampPalette(c(pbeg, pend))(length(finpts))
    finpc <- vector('list', length(finpts))
    for (i in 1:length(finpts)) finpc[[i]] <- rep(poscol[i], finpts[i])
    cpos <- unlist(finpc)
}
#deprecated: version which oworked only with the order and ignored the magnitude
#cneg <- colorRampPalette(c(nbeg, nend))(length(which(x < 0)))
#cpos <- colorRampPalette(c(pbeg, pend))(length(which(x > 0)))
#---put the colors in the proper order (ouch!)
aclr <- vector(mode="character", length=length(x))
rneg <- rank(x[!is.na(x) & x < 0]) ; rpos <- rank(x[!is.na(x) & x > 0])
cn <- cp <- 1
for (i in 1:length(x)) {
    if (is.na(x[i])) {
        aclr[i] <- NA
    }else if (x[i] < 0) {
        aclr[i] <- cneg[rneg[cn]] ; cn <- cn + 1
    } else if (x[i] > 0) {
        aclr[i] <- cpos[rpos[cp]] ; cp <- cp + 1
    } else aclr[i] <- zerocol
}
aclr
}

#-------------------------------------------------------------------------------

#br 2006-05
#plot a couple of transformations
#arguments:
#	- DF = the data frame
#	- tgt = the name of the target
#	- v = the name of the predictor
#	- lth = min value to use to avoid log/sqrt errors

transf <- function(DF, tgt, v, lth=0.01)
{
DFw <- data.frame(DF[, tgt], DF[, v], log(pmax(DF[, v], lth)), sqrt(pmax(DF[, v], lth)), DF[, v]^2)
colnames(DFw) <- c(tgt, v, paste(c("LOG of", "SQRT of", "^2 of"), v))
tpred <- setdiff(colnames(DFw), tgt)
#oldpar <- par(mfrow=c(2, 2), mai=c(0.65, 0.6, 0.3, 0.3), omi=c(0.2, 0, 0, 0))
emp.logit.plot(DFw, tgt, tpred, bins=100)
#on.exit(par(oldpar))
}

#-------------------------------------------------------------------------------

#br 2006-10
#this function fits a series of univariate linear or logistic models and returns the p values
#purpose: get rid quickly of clearly irrelevant predictors (those where p value > X)
#arguments:
#	- DF = the data frame
#	- tgt = the name of the target
#	- pred = a vector with the names of the predictors

univ.pval <- function(DF, tgt, pred, type)
{
if (!type %in% c("logistic", "multinom", "linear")) stop("type needs to be one of 'logistic', 'multinom', 'linear'.")
if (type == "logistic") if (!exists("lrm")) require(Design)
if (type == "multinom") if (!exists("multinom")) require(nnet)
Pval <- list()
for (v in pred) {
    now(v, after="\n")
    mform <- as.formula(paste(tgt, " ~ ", v, sep=""))
    if (type == "logistic") {
        tmod <- try(lrm(mform, data=DF, tol=1e-20))
        if (class(tmod)[1] == "try-error") pval <- NA else pval <- tmod$stats["P"]
    } else if (type == "linear") {
        tmod <- lm(mform)
        pval <- anova(tmod)[["Pr(>F)"]][1]
    } #else if (type == "multinom") {
    #	}
    Pval[[v]] <- data.frame(predictor=v, pval)
}
allp <- do.call("rbind", Pval) ; allp <- allp[order(-allp$pval), ] ; rownames(allp) <- 1:nrow(allp)
allp$predictor <- as.character(allp$predictor)
allp
}

#-------------------------------------------------------------------------------

#br 2006-11
#function that customizes the plot of an object from varclus() or naclus() from package Hmisc
#the plot is intended to be manually rotated in an image editor,  and its dimensions
#	are customized for a large number of variables
#x = an object of class 'varclus'

vcna.plot <- function(x, main='', ...)
{
plot(x, lwd=2, ...)
title(main)
abline(h=0.2*0:3, lty=2, col=c("firebrick3", "red", "tomato", "orange"))
}

#-------------------------------------------------------------------------------

#br 2011-04
#Given a text document (created by Corpus() from package tm), apply all processing needed -
#	dividing the text into sentences, cleaning up and stemming - to end up with all X-word
#	associations, where X is typically 2 or 3. The words are considered to be associated
#	if they appear together in the same sentence. The strength of the relationship is
#	assessed via the overall frequency counts.
#Something similar exists in the package tm (e.g. TermDocumentMatrix()), however that is
#	designed for detecting associations among documents and not sentences. I find relying
#	on sentences necessary since my intention is to get an idea about the contents of an
#	article and not about the relationships between documents.
#doc = text document from Corpus()
#W = how many words to combine; values higher than 3 or 4 are impractical and may have little meaning
#print = whether to print a report for stemming

wassoc <- function(doc, W=2, print=TRUE)
{
if (!exists('sentDetect')) require(openNLP)
sent <- sentDetect(doc, language='en')
sent <- stdTxt(sent)
words <- v2w(sent)
sw <- stemWords(unique(words), print=print)
#---replace words with their determined stems
for (x in sw$match) {
    keep <- x[which.min(nchar(x))]
    repl <- setdiff(x, keep)
    for (r in repl) sent <- gsub(r, keep, sent)
}
#---back to words, combinations and their frequency counts
wsent <- vector('list', length(sent))
for (i in 1:length(sent)) wsent[[i]] <- sort(setdiff(unlist(strsplit(sent[i], ' ')), ''))
pairs <- lapply(wsent, function(x) {
    if (length(x) <= W) return(paste(x, collapse=' - '))
    tmp <- combn(x, W)
    apply(tmp, 2, function(x) paste(x, collapse=' - '))
})
tb <- t2v(table(unlist(pairs)))
sort(tb[tb >= 2], decreasing=TRUE)
}

#-------------------------------------------------------------------------------
