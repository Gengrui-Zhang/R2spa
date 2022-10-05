#### SEM Scatterplot Syntax #####

# This file contains R syntax for creating the scatterplots (Figure 2) of SEM data.
# Steps include (1) simulating data, (2) fitting data in SEM, (3) extracting factor score estimates from a fitted SEM, (4) creating data visualization, and (5) creating path diagram
# Syntax by Kevin Hallgren (khallgre@uw.edu) 2018


# load libraries (must be installed already) 
library(lavaan)   # for SEM 
library(semPlot)  # for making SEM path models
library(gplots)   # for color functions (e.g., col2hex)

# set parameters to use in graphs for later
cols = c("cyan4", "white")  

# save figures as PNG files (add "#" to beginning of png line to show image in R console instead of saving)
fig.dir = "C:\\Users\\kevin\\UW Office 365\\OneDrive - UW Office 365\\School\\Atkins Lab\\SEM data viz\\paper figs\\"
png(paste0(fig.dir, "CFA_XY_1.png"), width=190, height=215, unit="mm", res=1000, pointsize=8)    

# set some other layout parameters
layout(matrix(1:6, nrow=3, byrow=T))   # create a 3x2 grid layout for the visualizations below
par(mar=c(5.1,5.1,4.1,2.1))   # set plot margins, making the left-hand margin a tad bigger than default so axis label isn't cut off


########## simulate linear data ############
### Step 1: simulate values for two latent variables, x and y 
# Note that these values won't actually get saved in the dataset we use for analysis.  Instead, we will generate
# four indicator variables (x1-x4, y1-y4) for each of these latent variables that are imperfect reflections of the latent variables
set.seed(123)   # this will return the same randomly-sampled values every time the syntax is run
x = rnorm(400, sd=0.5)   # simulate x as random normal data with a mean of 0 and sd of 0.5
y = x + rnorm(400, sd=0.56)   # simulate y as equal to x plus some random normal data with an sd of 0.56 

# normalize x and y (mean of 0, SD of 1) by subtracting the mean and dividing by the SD
x = (x-mean(x))/sd(x)
y = (y-mean(y))/sd(y)

# make observed indicator variables, x1-x4 and y1-y4 (these are the only variables that we will actually be kept for data analysis).
# each indicator has a component that is correlated to the latent variable values (x or y) and random noise/error variance
x1 = .8*x + rnorm(400, sd=.5)
x2 = .8*x + rnorm(400, sd=.5)
x3 = .8*x + rnorm(400, sd=.5)
x4 = .8*x + rnorm(400, sd=.5)

y1 = .8*y + rnorm(400, sd=.5)
y2 = .8*y + rnorm(400, sd=.5)
y3 = .8*y + rnorm(400, sd=.5)
y4 = .8*y + rnorm(400, sd=.5)

# combine observed variables into a data frame 
data.xy = data.frame(x1=x1,x2=x2,x3=x3,x4=x4, y1=y1,y2=y2,y3=y3,y4=y4)

### Step 2: Fit a latent variable model (confirmatory factor analysis, or CFA) using the cfa() command in the lavaan package
# Specify the latent variable model (see lavaan package for more information)
model.xy = 'X =~ x1 + x2 + x3 + x4    # X is a latent variable composed of x1-x4 
			Y =~ y1 + y2 + y3 + y4'   # Y is a latent variable composed of y1-y4

# Fit the data to the specified model, with standardized latent variables (SD=1 for each latent variable), and display the summary of the model fit
fit.xy = cfa(model.xy, data=data.xy, std.lv=T)   
summary(fit.xy, fit.measures=T)

### Step 3: extract latent variable estimates using lavPredict() command
latent_estimates = lavPredict(fit.xy)

# examine the correlations of factor score estiamtes compared to the correlation in the latent variable model summary
# note that these correlations are not identical -- 0.70 vs. 0.65 -- which reflects the issue of factor score indeterminacy (see Grice, 2001, cited in manuscript)
cor(latent_estimates)   

### Step 4: Create data visualization
# Create a plot of factor scores (see ?plot and ?par for explanation of arguments in the function below)
plot(latent_estimates[,"X"], latent_estimates[,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="Latent Craving (X) Estimate", main="", pch=16, ps=1.25, col=cols[1], cex.lab=1.8, cex.axis=1.5)


### Step 5: Create a path diagram (see semPlot package for more info, including explanation of parameters below)
# Note that we're customizing our path diagram.  A non-customized path diagram could be generated more simply by using the code below
# semPaths(fit.xy)

# Here we pass a lot of arguments to make an SEM path diagram that I think looks a bit nicer...
pathdiagram = semPaths(fit.xy, what="est", DoNotPlot=T, weighted=F, sizeLat=30, sizeLat2=30, sizeMan=20, sizeMan2=20, edge.color="black", edge.label.cex=3, edge.label.position=.5, mar=c(10,6,13,6), asize=8, fade=F)

# ..because we set "DoNotPlot=T" in the line above, the path diagram won't actually be displayed.  
# Instead, the parameters used for plotting will be saved to the object called "pathdiagram".  
#  We can manipulate the contents of the object "pathdiagram" a bit more to make the image a bit more crisp and professional looking:
edgelabels = pathdiagram$graphAttributes$Edges$labels  #this and the next line remove leading zeros from factor loading coefficients for easier reading (e.g., 0.78 becomes .78)
edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)] = gsub("0[.]", ".", edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)])
pathdiagram$graphAttributes$Edges$lty[pathdiagram$graphAttributes$Edges$lty != 1] = 1    # this and the next line change dotted lines to solid lines
pathdiagram$graphAttributes$Edges$labels = edgelabels

# this will actually present the path diagram on the R console or in the PNG file
plot(pathdiagram)  

# This syntax extracts fit statistics from the lavaan CFA object, then puts them into a long string that we print on the path diagram.
# Although summary statistics are often reported in tables, we decided to put it right on the path diagram this way.
fitstats = fitMeasures(fit.xy, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))   # extracts the summary fit statistics
fitstring1 = paste0("χ²(",fitstats["df"],")=",sprintf("%.2f",fitstats["chisq"]))   # this string holds the chi-square fit info
fitstring2 = paste0("p=",sprintf("%.2f",fitstats["pvalue"]))   # this string holds the p-value info, etc.
fitstring3 = paste0("CFI=",sprintf("%.2f",fitstats["cfi"]))   
fitstring4 = paste0("TLI=",sprintf("%.2f",fitstats["tli"]))
fitstring5 = paste0("RMSEA=",sprintf("%.2f",fitstats["rmsea"]))
fitstring6 = paste0("SRMR=",sprintf("%.2f",fitstats["srmr"]))
fitstringfull = paste(fitstring1, fitstring2, fitstring3, fitstring4, fitstring5, fitstring6, sep="\n")   # this combines all the previous string in one long string, separated by new lines ("\n")
text(1.3, 1.8, fitstringfull, adj=c(1,1), family="mono", cex=1.2)   # this plops the string into the current plotting area (the path diagram)

# Add a panel title
text(-1.8, 2, "A.  Linear Relationship", font=2, xpd=NA, cex=1.75, adj=0.5)



########## simulate curvilinear data ############
####  This follows the same steps as the section above (linear data), the only difference being the underlying data generation procedures.
####  Comments will be a bit more sparse here; see the section above for more thorough commenting
# Simulate latent variables, x and y
set.seed(456)
x = rnorm(400, mean=0, sd=0.75)   # x is random normal data
y = rep(0, 400)   # y is curvelinear data, being quadratically related to x when x>0 and being unrelated to x when x<=0.
y[x > 0] = (x[x>0]*2)^2 
y = y+rnorm(400, sd=1.68)

# normalize x and y
x = (x-mean(x))/sd(x)
y = (y-mean(y))/sd(y)

# make observed variables, x1-x4 and y1-y4
x1 = .8*x + rnorm(400, sd=.5)
x2 = .8*x + rnorm(400, sd=.5)
x3 = .8*x + rnorm(400, sd=.5)
x4 = .8*x + rnorm(400, sd=.5)

y1 = .8*y + rnorm(400, sd=.5)
y2 = .8*y + rnorm(400, sd=.5)
y3 = .8*y + rnorm(400, sd=.5)
y4 = .8*y + rnorm(400, sd=.5)

# save observed variables to a data frame
data.xy = data.frame(x1=x1,x2=x2,x3=x3,x4=x4, y1=y1,y2=y2,y3=y3,y4=y4)

# fit latent variable model (CFA) in lavaan
model.xy = 'X =~ x1 + x2 + x3 + x4
			Y =~ y1 + y2 + y3 + y4'
fit.xy = cfa(model.xy, data=data.xy, std.lv=T)
summary(fit.xy, fit.measures=T)

# extract latent variable estimates
latent_estimates2 = lavPredict(fit.xy)
cor(latent_estimates)

# create scatterplot 
plot(latent_estimates2[,"X"], latent_estimates2[,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="Latent Craving (X) Estimate", main="", pch=16, ps=1.25, col=cols[1], cex.lab=1.8, cex.axis=1.5)

# create path model diagram
pathdiagram = semPaths(fit.xy, what="est", DoNotPlot=T, weighted=F, sizeLat=30, sizeLat2=30, sizeMan=20, sizeMan2=20, edge.color="black", edge.label.cex=3, edge.label.position=.5, mar=c(10,6,13,6), asize=8, fade=F)
edgelabels = pathdiagram$graphAttributes$Edges$labels  #this and the next line remove leading zeros from factor loading coefficients for easier reading
edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)] = gsub("0[.]", ".", edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)])
pathdiagram$graphAttributes$Edges$lty[pathdiagram$graphAttributes$Edges$lty != 1] = 1    # change dotted lines to solid lines
pathdiagram$graphAttributes$Edges$labels = edgelabels
plot(pathdiagram)

# get fit statistics 
fitstats = fitMeasures(fit.xy, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
fitstring1 = paste0("χ²(",fitstats["df"],")=",sprintf("%.2f",fitstats["chisq"]))
fitstring2 = paste0("p=",sprintf("%.2f",fitstats["pvalue"]))
fitstring3 = paste0("CFI=",sprintf("%.2f",fitstats["cfi"]))
fitstring4 = paste0("TLI=",sprintf("%.2f",fitstats["tli"]))
fitstring5 = paste0("RMSEA=",sprintf("%.2f",fitstats["rmsea"]))
fitstring6 = paste0("SRMR=",sprintf("%.2f",fitstats["srmr"]))
fitstringfull = paste(fitstring1, fitstring2, fitstring3, fitstring4, fitstring5, fitstring6, sep="\n")
text(1.3, 1.8, fitstringfull, adj=c(1,1), family="mono", cex=1.2)

# add panel title
text(-1.8, 2, "B.  Curvilinear Relationship", font=2, xpd=NA, cex=1.75, adj=0.5)









########## simulate discontinuous data ############
### See first section above (simulating linear data) for more thorough commenting
# make latent variables, x and y
set.seed(789)
x = rnorm(400, sd=0.5)    # x and y are initially unrelated.  Both are random, independent normal data
y = rnorm(400, sd=1.1)

y[x > 0] = y[x > 0] + 2   # here we shift values of y up when y>0  (i.e., y=y+2, but only for values of y that were initially higher than 0)
x[x > 0] = x[x > 0] + 1   # here we shift values of x up when x>0, creating disjointed data

# normalize x and y
x = (x-mean(x))/sd(x)
y = (y-mean(y))/sd(y)


# make observed variables, x1-x4 and y1-y4
x1 = .8*x + rnorm(400, sd=.5)
x2 = .8*x + rnorm(400, sd=.5)
x3 = .8*x + rnorm(400, sd=.5)
x4 = .8*x + rnorm(400, sd=.5)

y1 = .8*y + rnorm(400, sd=.5)
y2 = .8*y + rnorm(400, sd=.5)
y3 = .8*y + rnorm(400, sd=.5)
y4 = .8*y + rnorm(400, sd=.5)

# save observed variables to a data frame
data.xy = data.frame(x1=x1,x2=x2,x3=x3,x4=x4, y1=y1,y2=y2,y3=y3,y4=y4)

# fit latent variable model (CFA) in lavaan
model.xy = 'X =~ x1 + x2 + x3 + x4
			Y =~ y1 + y2 + y3 + y4'
fit.xy = cfa(model.xy, data=data.xy, std.lv=T)
summary(fit.xy, fit.measures=T)

# extract latent variable estimates
latent_estimates3 = lavPredict(fit.xy)
cor(latent_estimates3)

# create scatterplot
plot(latent_estimates3[,"X"], latent_estimates3[,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="Latent Craving (X) Estimate", main="", pch=16, ps=1.25, col=cols[1], cex.lab=1.8, cex.axis=1.5)

# create path model diagram 
pathdiagram = semPaths(fit.xy, what="est", DoNotPlot=T, weighted=F, sizeLat=30, sizeLat2=30, sizeMan=20, sizeMan2=20, edge.color="black", edge.label.cex=3, edge.label.position=.5, mar=c(10,6,13,6), asize=8, fade=F)
edgelabels = pathdiagram$graphAttributes$Edges$labels  #this and the next line remove leading zeros from factor loading coefficients for easier reading
edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)] = gsub("0[.]", ".", edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)])
pathdiagram$graphAttributes$Edges$lty[pathdiagram$graphAttributes$Edges$lty != 1] = 1    # change dotted lines to solid lines
pathdiagram$graphAttributes$Edges$labels = edgelabels
plot(pathdiagram)

# extract fit statistics 
fitstats = fitMeasures(fit.xy, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
fitstring1 = paste0("χ²(",fitstats["df"],")=",sprintf("%.2f",fitstats["chisq"]))
fitstring2 = paste0("p=",sprintf("%.2f",fitstats["pvalue"]))
fitstring3 = paste0("CFI=",sprintf("%.2f",fitstats["cfi"]))
fitstring4 = paste0("TLI=",sprintf("%.2f",fitstats["tli"]))
fitstring5 = paste0("RMSEA=",sprintf("%.2f",fitstats["rmsea"]))
fitstring6 = paste0("SRMR=",sprintf("%.2f",fitstats["srmr"]))
fitstringfull = paste(fitstring1, fitstring2, fitstring3, fitstring4, fitstring5, fitstring6, sep="\n")
text(1.3, 1.8, fitstringfull, adj=c(1,1), family="mono", cex=1.2)

# add panel title
text(-1.8, 2, "C.  Discontinuous Relationship", font=2, xpd=NA, cex=1.75, adj=0.5)


dev.off()   # close the PNG file writing connection




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
##~~~~~~~~~~~~~~~ FIT NON-LINEAR RELATIONSHIPS USING FACTOR SCORE REGRESSION ~~~~~~~~~~~~~~~~~~~~~~~~ ##
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# load stargazer package to print regression tables in nice format
library(stargazer)

# Open a PNG file for saving images
png(paste0(fig.dir, "Panel B, alternative model fits.png"), width=190, height=145, unit="mm", res=1000, pointsize=8)    
layout(matrix(1:4, nrow=2, byrow=T))
par(mar=c(5.1,5.1,4.1,2.1))   # set plot margins, making the left-hand margin a tad bigger than default so axis label isn't cut off


####### EXAMPLE 2: LINEAR FIT ########
# code new transformations of X variables:
latent_estimates2 = data.frame(latent_estimates2)

# fit regression model
lm.linear = lm(Y ~ X, data=latent_estimates2)
summary(lm.linear)

# create scatterplot 
plot(latent_estimates2[,"X"], latent_estimates2[,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="Latent Craving (X) Estimate", main="", pch=16, ps=1.25, col=cols[1], cex.lab=1.8, cex.axis=1.5)

# add model-fitted line
latent_estimates2$y_fitted = predict(lm.linear)
latent_estimates2 = latent_estimates2[order(latent_estimates2$X),]
lines(latent_estimates2$X, latent_estimates2$y_fitted, lwd=2)

# add regression table into figure
plot(NA, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=F)  # create a blank plotting area
modeltable = stargazer(lm.linear, title="Curvilinear data, linear fit", type="text", single.row=T, digits=2, align=T, star.cutoffs=c(.05,.01,.001))  # use stargazer to extract a nicely formatted regression table
modeltable = paste(modeltable, collapse="\n")   # convert the stargazer text vector into a single string
text(0,1, modeltable, adj=c(0,1), family="mono", cex=1.0, xpd=NA)   # add the text into the plotting area




####### EXAMPLE 2: PIECEWISE FIT ########
# code new transformations of X variables:
latent_estimates2 = data.frame(latent_estimates2)
latent_estimates2$X_cut = (latent_estimates2$X > 0)*1   # binary variable, =0 if X<0, =1 if X>0
latent_estimates2$X_cut_linear = latent_estimates2$X*latent_estimates2$X_cut   # linear effect when X>0
latent_estimates2$X_cut_quadratic = latent_estimates2$X_cut_linear^2   # quadratic effect when X>0

# fit regression model
lm.twopartgrowth = lm(Y ~ X + X_cut_linear + X_cut_quadratic, data=latent_estimates2)
summary(lm.twopartgrowth)

# create scatterplot 
plot(latent_estimates2[,"X"], latent_estimates2[,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="Latent Craving (X) Estimate", main="", pch=16, ps=1.25, col=cols[1], cex.lab=1.8, cex.axis=1.5)

# add model-fitted line
latent_estimates2$y_fitted = predict(lm.twopartgrowth)
latent_estimates2 = latent_estimates2[order(latent_estimates2$X),]
lines(latent_estimates2$X, latent_estimates2$y_fitted, lwd=2)

# add regression table into figure
plot(NA, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=F)  # create a blank plotting area
modeltable = stargazer(lm.twopartgrowth, title="Curvilinear data, piecewise linear & quadratic effects", type="text", single.row=T, digits=2, align=T, star.cutoffs=c(.05,.01,.001))  # use stargazer to extract a nicely formatted regression table
modeltable = paste(modeltable, collapse="\n")   # convert the stargazer text vector into a single string
text(0,1, modeltable, adj=c(0,1), family="mono", cex=1.0, xpd=NA)   # add the text into the plotting area

dev.off()



####### EXAMPLE 3: CONTINUOUS FIT ########
png(paste0(fig.dir, "Panel C, alternative model fits.png"), width=190, height=145, unit="mm", res=1000, pointsize=8)    
layout(matrix(1:4, nrow=2, byrow=T))
par(mar=c(5.1,5.1,4.1,2.1))   # set plot margins, making the left-hand margin a tad bigger than default so axis label isn't cut off

# code new transformations of X variables:
latent_estimates3 = data.frame(latent_estimates3)

# fit continuous regression model
lm.continuous = lm(Y ~ X, data=latent_estimates3)
summary(lm.continuous)

# create scatterplot 
plot(latent_estimates3[,"X"], latent_estimates3[,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="Latent Craving (X) Estimate", main="", pch=16, ps=1.25, col=cols[1], cex.lab=1.8, cex.axis=1.5)

# add model-fitted line
latent_estimates3$y_fitted = predict(lm.continuous)
latent_estimates3 = latent_estimates3[order(latent_estimates3$X),]
lines(latent_estimates3$X, latent_estimates3$y_fitted, lwd=2)

# add regression table into figure
plot(NA, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=F)  # create a blank plotting area
modeltable = stargazer(lm.continuous, title="Discontinuous data, modeled with linear X", type="text", single.row=T, digits=2, align=T, star.cutoffs=c(.05,.01,.001))  # use stargazer to extract a nicely formatted regression table
modeltable = paste(modeltable, collapse="\n")   # convert the stargazer text vector into a single string
text(0,1, modeltable, adj=c(0,1), family="mono", cex=1.0, xpd=NA)   # add the text into the plotting area



####### EXAMPLE 3: TREAT X AS BINARY ########
latent_estimates3$X_cut = (latent_estimates3$X > 0)*1   # binary variable, =0 if X<0, =1 if X>0

# fit discontinuous regression model
lm.discontinuous = lm(Y ~ X_cut, data=latent_estimates3)
summary(lm.discontinuous)

# create scatterplot 
plot(latent_estimates3[,"X"], latent_estimates3[,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="Latent Craving (X) Estimate", main="", pch=16, ps=1.25, col=cols[1], cex.lab=1.8, cex.axis=1.5)

# add model-fitted line
latent_estimates3$y_fitted = predict(lm.discontinuous)
latent_estimates3 = latent_estimates3[order(latent_estimates3$X),]
with(latent_estimates3[latent_estimates3$X<0,], lines(X, y_fitted, lwd=2))
with(latent_estimates3[latent_estimates3$X>0,], lines(X, y_fitted, lwd=2))

# add regression table into figure
plot(NA, type="n", xlab="", ylab="", xlim=c(0, 1), ylim=c(0, 1), axes=F)  # create a blank plotting area
modeltable = stargazer(lm.discontinuous, title="Discontinuous data, modeled with binary X", type="text", single.row=T, digits=2, align=T, star.cutoffs=c(.05,.01,.001))  # use stargazer to extract a nicely formatted regression table
modeltable = paste(modeltable, collapse="\n")   # convert the stargazer text vector into a single string
text(0,1, modeltable, adj=c(0,1), family="mono", cex=1.0, xpd=NA)   # add the text into the plotting area

dev.off()   # close the PNG file writing connection

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

