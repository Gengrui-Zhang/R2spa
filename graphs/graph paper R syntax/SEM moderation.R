#### SEM Moderation syntax #####

# This file contains R syntax for creating the moderation visualizations (Figures 3 and 4) with SEM data.
# Steps include (1) simulating data, (2) fitting data in SEM, (3) extracting factor score estimates from a fitted SEM, (4) creating data visualization, and (5) creating path diagram
# Syntax by Kevin Hallgren (khallgre@uw.edu) 

# load libraries.  Must install each package if this causes an error.
library(lavaan)   # for fitting SEM
library(semPlot)  # for making SEM path models
library(gplots)   # for color functions

# set parameters to use in graphs for later
cols = c("cyan4", "white")  

# where to save PNG files?
fig.dir = "C:\\Users\\kevin\\UW Office 365\\OneDrive - UW Office 365\\School\\Atkins Lab\\SEM data viz\\paper figs\\"


########## simulate latent interaction data ############
### Step 1: simulate values for three latent variables, x, y, and z
# Note that these values won't actually get saved in the dataset we use for analysis.  Instead, we will generate
# four indicator variables for each of these latent variables (x1-x4, y1-y4, z1-z4) that are imperfect reflections of the latent variables

set.seed(123)
x = rnorm(400, sd=0.5)   # x is random, independent data
z = rnorm(400, sd=0.5)   # z is also random, independent data
y = x - 1*z - 2.1*x*z + rnorm(400, sd=0.5)   # here's where we create the interaction of x*z impacting y
x[z > 0] = x[z > 0] / 2 - 1  # here's where we introduce an anomaly: people with very high values of z will have a restricted range of x

# normalize x, y, and z
x = (x-mean(x))/sd(x)
y = (y-mean(y))/sd(y)
z = (z-mean(z))/sd(z)

# just a sanity check to make sure y is predicted by the interaction of x*z, using linear regression of the latent variables that we'll eventually throw away
summary(lm(y ~ x*z))

# make observed variables, x1-x4, y1-y4, z1-z4
x1 = .8*x + rnorm(400, sd=.5)
x2 = .8*x + rnorm(400, sd=.5)
x3 = .8*x + rnorm(400, sd=.5)
x4 = .8*x + rnorm(400, sd=.5)

y1 = .8*y + rnorm(400, sd=.5)
y2 = .8*y + rnorm(400, sd=.5)
y3 = .8*y + rnorm(400, sd=.5)
y4 = .8*y + rnorm(400, sd=.5)

z1 = .8*z + rnorm(400, sd=.5)
z2 = .8*z + rnorm(400, sd=.5)
z3 = .8*z + rnorm(400, sd=.5)
z4 = .8*z + rnorm(400, sd=.5)

# make interaction indicators, one method for assessing interactions in SEM (see Marsh et al., 2004).  The interaction term should be mean centered.
x1z1 = x1*z1 - mean(x1*z1)
x2z2 = x2*z2 - mean(x2*z2)
x3z3 = x3*z3 - mean(x3*z3)
x4z4 = x4*z4 - mean(x4*z4)

# save the observed variables to a data frame
data.xyz = data.frame(x1=x1,x2=x2,x3=x3,x4=x4, y1=y1,y2=y2,y3=y3,y4=y4, z1=z1,z2=z2,z3=z3,z4=z4, x1z1=x1z1,x2z2=x2z2,x3z3=x3z3,x4z4=x4z4)


###### FIT DATA USING STRUCTURAL EQUATION MODELING #####
# fit SEM model, which shows latent Y is predicted by latent X, Z, and X*Z interaction
model.xyz = 'X =~ x1 + x2 + x3 + x4
			 Y =~ y1 + y2 + y3 + y4
			 Z =~ z1 + z2 + z3 + z4
			 XZ =~ x1z1 + x2z2 + x3z3 + x4z4
			 Y ~ X + Z + XZ'
fit.xyz = sem(model.xyz, data=data.xyz, std.lv=T)
summary(fit.xyz, fit.measures=T)


######## Graph simple slope plots #######
# (see also McCabe, Kim, & King's paper "Improving Present Practices in the Visual Display of Interactions", cited in main paper accompanying this supplemental syntax, for more comprehensive tools for plotting interactions)
# First we'll extract latent variable estimates... 
# To do this, we will use a CFA without structural regressions to obtain estimates of X, Y, and Z (and not XZ) that approximate a normal distribution with mean=0, sd=1
model.xyz.cfa = 'X =~ x1 + x2 + x3 + x4
				 Y =~ y1 + y2 + y3 + y4
				 Z =~ z1 + z2 + z3 + z4'
fit.xyz.cfa = cfa(model.xyz.cfa, data=data.xyz, std.lv=T)
latent_estimates = lavPredict(fit.xyz.cfa)

# then, cut the data into 4 equal quantiles
z_quantiles = quantile(latent_estimates[,"Z"], c(0,.25,.5,.75,1))   # identify 4 equally sized quartiles of participants based on latent variable estimate Z ("mindfulness")
subgroup = as.numeric(cut(latent_estimates[,"Z"], z_quantiles, include.lowest=T))

# then, compute the simple slopes for the latent variable estimates for graphing
lm.xyz = lm(Y ~ X*Z, data=data.frame(latent_estimates))    # regression model for estimating the interaction of X*Z  (Note the we are using upper-case X, Y, and Z, which are the factor score estimates of the latent values.  These are different from lower-case x, y, and z, which were used previously as temporary variables to simulate data

# the "predict" commands below will estimate the predicted value of Y assuming a given set of values for X and Z
predicted.X = seq(-5,5,length.out=11)   # we will predict values from X=-5 to 5
predicted.1 = predict(lm.xyz, newdata=data.frame(X=predicted.X, Z=rep(mean(latent_estimates[subgroup==1,"Z"]), 11)))   # predict Y for values of X ranging from -5 to 5 with a mean value of Z within quartile 1.
predicted.2 = predict(lm.xyz, newdata=data.frame(X=predicted.X, Z=rep(mean(latent_estimates[subgroup==2,"Z"]), 11)))   # predict Y for values of X ranging from -5 to 5 with a mean value of Z within quartile 2.
predicted.3 = predict(lm.xyz, newdata=data.frame(X=predicted.X, Z=rep(mean(latent_estimates[subgroup==3,"Z"]), 11)))   # predict Y for values of X ranging from -5 to 5 with a mean value of Z within quartile 3.
predicted.4 = predict(lm.xyz, newdata=data.frame(X=predicted.X, Z=rep(mean(latent_estimates[subgroup==4,"Z"]), 11)))   # predict Y for values of X ranging from -5 to 5 with a mean value of Z within quartile 4.

# start writing PNG file
png(paste0(fig.dir, "Moderation_1.png"), width=190, height=190, unit="mm", res=1000, pointsize=8)

# create a grid layout with 5 plotting areas cut out within a 2x4 grid.  It will essentially look like this:
# 1234   [4 separate plotting areas in the top row]
# 5555   [1 long plotting area in the bottom row]
# where each unique number represents one plotted area.  So essentially, 4 unique plotting areas in the top row and one long plotting area on the bottom row
par(oma=c(0,0,2,0))
layout(matrix(c(1,2,3,4,
				5,5,5,5), nrow=2, byrow=T), heights=c(0.3,0.7))
				
# plot the 4 small multiples scatter plots
plot(latent_estimates[subgroup==1,"X"], latent_estimates[subgroup==1,"Y"], ylab="Latent Drinking (Y) Estimate", xlab="", main="1st Quartile\n(Lowest Mindfulness)", xlim=c(-2,3), ylim=c(-3.2,4.8), pch=16, ps=1.25, col=cols[1], cex.lab=1.45, cex.axis=1.3, cex.main=1.45, xpd=NA)
lines(x=predicted.X, y=predicted.1) 
plot(latent_estimates[subgroup==2,"X"], latent_estimates[subgroup==2,"Y"], ylab="", xlab="", main="2nd Quartile", xlim=c(-2,3), ylim=c(-3.2,4.8), pch=16, ps=1.25, col=cols[1], cex.lab=1.45, cex.axis=1.3, cex.main=1.45, xpd=NA)
lines(x=predicted.X, y=predicted.2)
plot(latent_estimates[subgroup==3,"X"], latent_estimates[subgroup==3,"Y"], ylab="", xlab="", main="3rd Quartile", xlim=c(-2,3), ylim=c(-3.2,4.8), pch=16, ps=1.25, col=cols[1], cex.lab=1.45, cex.axis=1.3, cex.main=1.45, xpd=NA)
lines(x=predicted.X, y=predicted.3)
plot(latent_estimates[subgroup==4,"X"], latent_estimates[subgroup==4,"Y"], ylab="", xlab="", main="4th Quartile\n(Highest Mindfulness)", xlim=c(-2,3), ylim=c(-3.2,4.8), pch=16, ps=1.25, col=cols[1], cex.lab=1.45, cex.axis=1.3, cex.main=1.45, xpd=NA)
lines(x=predicted.X, y=predicted.4)
mtext("Quartile of Latent Mindfulness Estimates (Z)", side=3, outer=T, adj=0.5, font=2, cex=1)   # add margin text explaining what each graph is
mtext("Latent Craving (X) Estimate", side=3, outer=T, adj=0.5, font=1, cex=1, line=-24.5)   # add text explaining what the x-axis is

# plot the SEM path diagram  (See "SEM scatterplots.R" for more detailed commenting)
pathdiagram = semPaths(fit.xyz, what="est", edge.color="black", fade=F, weighted=F, DoNotPlot=T, sizeLat=15, sizeLat2=15, sizeMan=9, sizeMan2=9,  edge.label.cex=1.5, edge.label.position=.5, mar=c(4,6,4,6), asize=4)

pathdiagram$graphAttributes$Edges$lty[pathdiagram$graphAttributes$Edges$lty != 1] = 1    # change dotted lines to solid lines
pathdiagram$graphAttributes$Nodes$labels = names(pathdiagram$graphAttributes$Nodes$labels)   # this line returns the original variable names as labels displayed in the path diagram (e.g., semPaths was shortening x1z1 to x11)
edgelabels = pathdiagram$graphAttributes$Edges$labels  #this and the next few lines remove leading zeros from factor loading coefficients for easier reading
edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)] = gsub("0[.]", ".", edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)])
pathdiagram$graphAttributes$Edges$labels = edgelabels
plot(pathdiagram)

# add fit statistics to the path diagram image (See "SEM scatterplots.R" for more detailed commenting)
fitstats = fitMeasures(fit.xyz, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
fitstring1 = paste0("χ²(",fitstats["df"],")=",sprintf("%.2f",fitstats["chisq"]))
fitstring2 = paste0("p=",sprintf("%.2f",fitstats["pvalue"]))
fitstring3 = paste0("CFI=",sprintf("%.2f",fitstats["cfi"]))
fitstring4 = paste0("TLI=",sprintf("%.2f",fitstats["tli"]))
fitstring5 = paste0("RMSEA=",sprintf("%.2f",fitstats["rmsea"]))
fitstring6 = paste0("SRMR=",sprintf("%.2f",fitstats["srmr"]))
fitstringfull = paste(fitstring1, fitstring2, fitstring3, fitstring4, fitstring5, fitstring6, sep="\n")
text(1.3, 0.6, fitstringfull, adj=c(1,1), family="mono", cex=1.5, xpd=NA)

dev.off()   # close the PNG file writing connection





##### PLOT IN ONE GRAPH USING MULTIPLE ENCODINGS ####
# let's extract the estimated latent variable values and save them to X, Y, and Z for shorter syntax  (Note, these are upper-case X, Y, and Z, which are factor score estimates; these are different from lower-case x, y, and z, which were temporary variables used to simulate data in the earlier in the syntax file)
X = latent_estimates[1:400,"X"]
Y = latent_estimates[1:400,"Y"]
Z = latent_estimates[1:400,"Z"]

# this syntax creates 4 colors/sizes corresponding to the 4 quartiles groups
mycolors = colorRampPalette(c("#008080","#F1F8F8"))(4)   # part of gplots package, create a gradient of 4 colors ranging from white to green
z_quantiles = quantile(latent_estimates[,"Z"], c(0,.25,.5,.75,1))   # identify 4 equally sized quartiles of participants based on latent variable estimate Z ("mindfulness")

# open PNG file connection for saving
png(paste0(fig.dir, "Moderation_2.png"), width=140, height=140, unit="mm", res=1000, pointsize=7)

# because this graph uses multiple encodings, I find it easier to first plot a blank graphing area with no data, then manually add the data points with the features we want.
plot(NA, xlim=c(-2,3), ylim=c(-3.2,4.8), xlab="Latent Craving (X) Estimate", ylab="Latent Drinking (Y) Estimate", cex.lab=1.25, bg="white", cex.axis=1.1)

# this will identify which of the 4 subgroups the participant belongs to, based on their value of Z
subgroup = as.numeric(cut(latent_estimates[,"Z"], z_quantiles, include.lowest=T))
pointcols = mycolors[subgroup]  # this assigns a color for each point based on the subgroup (i.e., quartile) associated with Z
pointcols = paste0(pointcols,"AA")  # this adds some transparency to the colors so overlapping points can be seen (important with the large size of some points)
pointsizes = (subgroup+1)/2   # this assigns a pointsize for each point based on the subgroup (i.e., quartile) associated with Z; the values are linearly transformed so they are most easily viewed
points(x=X, y=Y, cex=pointsizes, bg=pointcols, col="#000000AA", pch=21)  # this puts the points on the graph


# The following lines can be uncommented to add lines showing the association within each subgroup
# # the "predict" commands below will estimate the predicted value of Y assuming a given set of values for X and Z
# predicted.X1 = range(latent_estimates[subgroup==1,"X"])   # we will select X values spanning the range of X within this subgroup/quartile
# predicted.X2 = range(latent_estimates[subgroup==2,"X"])   # we will select X values spanning the range of X within this subgroup/quartile
# predicted.X3 = range(latent_estimates[subgroup==3,"X"])   # we will select X values spanning the range of X within this subgroup/quartile
# predicted.X4 = range(latent_estimates[subgroup==4,"X"])   # we will select X values spanning the range of X within this subgroup/quartile

# predicted.Y1 = predict(lm.xyz, newdata=data.frame(X=predicted.X1, Z=rep(mean(latent_estimates[subgroup==1,"Z"]), 2)))   # predict Y for quartile 1.
# predicted.Y2 = predict(lm.xyz, newdata=data.frame(X=predicted.X2, Z=rep(mean(latent_estimates[subgroup==2,"Z"]), 2)))   # predict Y for quartile 2.
# predicted.Y3 = predict(lm.xyz, newdata=data.frame(X=predicted.X3, Z=rep(mean(latent_estimates[subgroup==3,"Z"]), 2)))   # predict Y for quartile 3.
# predicted.Y4 = predict(lm.xyz, newdata=data.frame(X=predicted.X4, Z=rep(mean(latent_estimates[subgroup==4,"Z"]), 2)))   # predict Y for quartile 4.


# # add lines for model-predicted values
# lines(x=predicted.X1, y=predicted.Y1, lwd=3.5, col="black")   # this gives a black outline (#000000 = black, AA = slightly transparent)
# lines(x=predicted.X1, y=predicted.Y1, lwd=3, col=mycolors[1])  # this paints the color line (
# lines(x=predicted.X2, y=predicted.Y2, lwd=3.5, col="black") 
# lines(x=predicted.X2, y=predicted.Y2, lwd=3, col=mycolors[2]) 
# lines(x=predicted.X3, y=predicted.Y3, lwd=3.5, col="black") 
# lines(x=predicted.X3, y=predicted.Y3, lwd=3, col=mycolors[3]) 
# lines(x=predicted.X4, y=predicted.Y4, lwd=3.5, col="black") 
# lines(x=predicted.X4, y=predicted.Y4, lwd=3, col=mycolors[4]) 

# add a legend
legend("bottomright", legend=c("1st Quartile (Lowest)","2nd Quartile","3rd Quartile","4th Quartile (Highest)"), title="Latent Mindfulness (Z) Estimate", pch=21, pt.bg=mycolors, col="#000000AA", pt.cex=((1:4)+1)/2, y.intersp=1.3, x.intersp=1.5, bg="white", cex=1.3)

dev.off()   # close the PNG file writing connection
