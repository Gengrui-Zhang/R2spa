#### SEM Mediation syntax #####

# This file contains R syntax for creating the mediation visualizations (Figures 5 and 6) with SEM data.
# Steps include (1) simulating data, (2) fitting data in SEM, (3) extracting factor score estimates from a fitted SEM, (4) creating data visualization, and (5) creating path diagram
# Syntax by Kevin Hallgren (khallgre@uw.edu) 2018

# load libraries.  Must install each package if this causes an error.
library(lavaan)   # for fitting SEM
library(semPlot)  # for making SEM path models
library(gplots)   # for color functions

# set parameters to use in graphs for later
cols = c("cyan4", "white")   # define the colors we'll ue in visualizations
cols.hex = sapply(cols, col2hex)    # convert color names into color hexcodes, e.g., "cyan4" becomes "#008B8B"
cols.transluscent = paste0(cols.hex, "99")   # adding 2 more hex-digits to the color specifies transluscency -- e.g., "#008B8B" becomes "#008B8B99" -- helpful for more easily seeing overlapping points

# directory to save  PNG files
fig.dir = "C:\\Users\\kevin\\UW Office 365\\OneDrive - UW Office 365\\School\\Atkins Lab\\SEM data viz\\paper figs\\"

########## simulate mediation data ############
# simulate latent variables, x, y, and z (these normally would not be present in a dataset, and are only temporarily used to simulate observable data in the next section)
set.seed(123)
x = rnorm(200, sd=0.5)   # x is normal random data
m = x*0.75 + rnorm(200, sd=0.5)   # m is simulated to equal 0.75*x plus normal random data
y = x*0.5 + m*0.35 + rnorm(200, sd=0.5)   # y is simulated to equal 0.5*x + 0.35*m plus normal random data

# normalize x, m, and y
x = (x-mean(x))/sd(x)
y = (y-mean(y))/sd(y)
m = (m-mean(m))/sd(m)

# make observed variables, x1-x4, m1-m4, y1-y4 (these are the only variables that we will actually be kept for data analysis)
x1 = .8*x + rnorm(200, sd=.5)
x2 = .8*x + rnorm(200, sd=.5)
x3 = .8*x + rnorm(200, sd=.5)
x4 = .8*x + rnorm(200, sd=.5)

m1 = .8*m + rnorm(200, sd=.5)
m2 = .8*m + rnorm(200, sd=.5)
m3 = .8*m + rnorm(200, sd=.5)
m4 = .8*m + rnorm(200, sd=.5)

y1 = .8*y + rnorm(200, sd=.5)
y2 = .8*y + rnorm(200, sd=.5)
y3 = .8*y + rnorm(200, sd=.5)
y4 = .8*y + rnorm(200, sd=.5)

# save observed variables to data frame
data.xmy = data.frame(x1=x1,x2=x2,x3=x3,x4=x4, y1=y1,y2=y2,y3=y3,y4=y4, m1=m1,m2=m2,m3=m3,m4=m4)


###### FIT DATA USING STRUCTURAL EQUATION MODELING #####
# fit SEM mediation model 
model.xmy = 'X =~ x1 + x2 + x3 + x4   # measurement model
			 Y =~ y1 + y2 + y3 + y4
			 M =~ m1 + m2 + m3 + m4
			 
			 Y ~ c_prime*X + b*M	  # structural model with a, b, and c_prime coefficients labeled
			 M ~ a*X
			 
			 ab := a*b				  # this will compute an estimate and SE of ab, defined as the value of a*b
			 '
fit.xmy = sem(model.xmy, data=data.xmy, std.lv=T)
summary(fit.xmy, fit.measures=T)

# Compare to a model with only the direct effect modeled.  As noted in Hayes (2009), c = c' + ab holds in linear regression but not in SEM, but we'll see that it's very close in this example
model.xy =  'X =~ x1 + x2 + x3 + x4   # measurement model
			 Y =~ y1 + y2 + y3 + y4
			 M =~ m1 + m2 + m3 + m4
			 Y ~ c*X'
fit.xy = sem(model.xy, data=data.xmy, std.lv=T)
summary(fit.xy, fit.measures=T)

# here, c = 0.768, which is almost exactly equal to c_prime+ab (in line with the decomposition of total effect into direct and indirect effects)
coef(fit.xy)["c"]
coef(fit.xmy)["c_prime"] + coef(fit.xmy)["a"]* coef(fit.xmy)["b"]   # (ignore that this is labeled "c_prime", it is actually c_prime+a*b)


##### OBTAIN LATENT VARIABLE ESTIMATES (FACTOR SCORES) USING CFA #####
model.xmy.cfa = 'X =~ x1 + x2 + x3 + x4
				 M =~ m1 + m2 + m3 + m4
				 Y =~ y1 + y2 + y3 + y4'
fit.xmy.cfa = cfa(model.xmy.cfa, data=data.xmy, std.lv=T)
latent_estimates = data.frame(lavPredict(fit.xmy.cfa))

# extract the model path coefficients (a, b, c, c') from the fitted SEM-based mediation model (we'll save these to add to the figure below)
c_prime = coef(fit.xmy)["c_prime"]
a = coef(fit.xmy)["a"]
b = coef(fit.xmy)["b"]
ab=a*b


##### GRAPH A LA FRITZ & MACKINNON (2008) #####
png(paste0(fig.dir, "Mediation_path_coefficients.png"), width=190, height=190, unit="mm", res=1000, pointsize=8)    
layout(matrix(c(1,2,4,3), nrow=2, byrow=T))   # create a 2x2 grid like the one below:
# 12
# 43
# where each plot below will be drawn in the order of the grid above (i.e., bottom-left image drawn last)


### plot 1: show lines only for X=0
plot(latent_estimates$M, latent_estimates$Y, main="A.  Estimated Y and M for X=0", xlab="Latent Rumination (M) Estimate", ylab="Latent Drinking to Cope (Y) Estimate", pch=16, ps=1.25, col=cols.transluscent[1], cex.lab=1.2, cex.axis=1.2)
abline(a=0, b=b, lwd=2)   # add regression lines with intercept 0, with slope equal to b-path coefficient
abline(v=0)   # add vertical lines at 0 (intercept)
abline(h=0)   # add horizontal lines at 0 (intercept)
legend("topleft", legend=c("Y = c'X + bM (when X=0)"), lty=c(1), lwd=2, text.font=3, seg.len=3)   # add a legend

# draw minor tick marks
minorticks.x = seq(par()$xaxp[1], par()$xaxp[2], by=0.25)
minorticks.y = seq(par()$yaxp[1], par()$yaxp[2], by=0.25)
axis(1, minorticks.x, labels=rep("", length(minorticks.x)), tck=-0.01)
axis(2, minorticks.y, labels=rep("", length(minorticks.y)), tck=-0.01)


### plo2 2: show solid lines when X=0 and dashed lines when X=1
plot(latent_estimates$M, latent_estimates$Y, main="B.  Estimated Y and M for X=0 and X=1", xlab="Latent Rumination (M) Estimate", ylab="Latent Drinking to Cope (Y) Estimate", pch=16, ps=1.25, col=cols.transluscent[1], cex.lab=1.2, cex.axis=1.2)
abline(a=0, b=b, lwd=2)   # add regression lines with intercepts of 0 and c_prime, with slopes equal to b-path coefficient
abline(a=c_prime, b=b, lwd=2, lty=2)   
abline(v=0)   # add vertical lines at 0 and a
abline(v=a, lty=2)   
abline(h=0)   # add horizontal lines at 0 and ab+c_prime 
abline(h=ab+c_prime, lty=2)
legend("topleft", legend=c("Y = c'X + bM (when X=1)","Y = c'X + bm (when X=0)"), lty=c(2,1), lwd=2, text.font=3, seg.len=3)

# draw minor tick marks
minorticks.x = seq(par()$xaxp[1], par()$xaxp[2], by=0.25)
minorticks.y = seq(par()$yaxp[1], par()$yaxp[2], by=0.25)
axis(1, minorticks.x, labels=rep("", length(minorticks.x)), tck=-0.01)
axis(2, minorticks.y, labels=rep("", length(minorticks.y)), tck=-0.01)


### plot 3: add arrows explaining a, b, c', ab effects
plot(latent_estimates$M, latent_estimates$Y, main="C.  Estimated Mediation Path Coefficients", xlab="Latent Rumination (M) Estimate", ylab="Latent Drinking to Cope (Y) Estimate", pch=16, ps=1.25, col=cols.transluscent[1], cex.lab=1.2, cex.axis=1.2)
abline(a=0, b=b, lwd=2)   # add regression lines with intercepts of 0 and c_prime, with slopes equal to b-path coefficient
abline(a=c_prime, b=b, lwd=2, lty=2)
abline(v=0)   # add vertical lines at 0 and a
abline(v=a, lty=2)
abline(h=0)   # add horizontal lines at 0 and ab+c_prime 
abline(h=ab+c_prime, lty=2)

arrows(x0=0, x1=a, y0=-2, y1=-2, lwd=2, length=0.06)  # add the "a" arrow and label
text("a", x=a/2, y=-2, pos=1, font=4, cex=1.5)
arrows(x0=2, x1=2, y0=0, y1=ab+c_prime, lwd=2, length=0.06)  # c arrow and label
text("ab+c'", x=2, y=(ab+c_prime)/2, pos=4, font=4, cex=1.5)
arrows(x0=a, x1=a, y0=0, y1=ab, lwd=2, length=0.06)  # ab=c-c' arrow and label
text("ab", x=a, y=ab/2, pos=4, font=4, cex=1.5)
arrows(x0=0, x1=0, y0=0, y1=c_prime, lwd=2, length=0.06)  # c' arrow and label
text("c'", x=0, y=c_prime/2, pos=2, font=4, cex=1.5)

# add legend
legend("topleft", legend=c("Y = c'X + bM (when X=1)","Y = c'X + bm (when X=0)"), lty=c(2,1), lwd=2, text.font=3, seg.len=3)

# draw minor tick marks
minorticks.x = seq(par()$xaxp[1], par()$xaxp[2], by=0.25)
minorticks.y = seq(par()$yaxp[1], par()$yaxp[2], by=0.25)
axis(1, minorticks.x, labels=rep("", length(minorticks.x)), tck=-0.01)
axis(2, minorticks.y, labels=rep("", length(minorticks.y)), tck=-0.01)


##### GRAPH A PATH DIAGRAM ####
# Note that we're customizing our path diagram.  A non-customized path diagram could be generated more simply by using the code: semPaths(fit.xmy)
pathdiagram = semPaths(fit.xmy, what="est", edge.color="black", fade=F, weighted=F, DoNotPlot=T, sizeLat=16, sizeLat2=16, sizeMan=12, sizeMan2=12,  edge.label.cex=1.6, edge.label.position=.5, mar=c(6,6,6,12), asize=3, rotation=2)

pathdiagram$graphAttributes$Edges$lty[pathdiagram$graphAttributes$Edges$lty != 1] = 1    # change dotted lines to solid lines
edgelabels = pathdiagram$graphAttributes$Edges$labels  #this and the next few lines remove leading zeros from factor loading coefficients for easier reading
edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)] = gsub("0[.]", ".", edgelabels[which(pathdiagram$graphAttributes$Edges$curve == 0)])
pathdiagram$graphAttributes$Edges$labels = edgelabels

# we will also make the structural relationships in the mediation model heavier (thicker lines) so that part is easier to see
latentnodes = which(pathdiagram$Arguments$shape == "circle")   # identify which nodes correspond with latent variables
latentedges = which((pathdiagram$Edgelist$from %in% latentnodes) & (pathdiagram$Edgelist$to %in% latentnodes) & (pathdiagram$Edgelist$weight!= 1.00))  # identify which nodes are connecting latent variables, while excluding the latent variances (whose weights = 1.00)
pathdiagram$graphAttributes$Edges$asize[latentedges] = 7
pathdiagram$graphAttributes$Edges$width[latentedges] = 3
pathdiagram$graphAttributes$Edges$label.font[latentedges] = 2

# this will display the path diagram with the parameters that were modified above and saved in the object called "pathdiagram"
plot(pathdiagram)

# add a, b, c' labels
text("(a)", x=-0.05, y=.3, font=3, srt=25, cex=0.9, adj=0.5)
text("(b)", x=.45, y=0, font=3, srt=0, cex=0.9, adj=0.5)
text("(c')", x=-0.05, y=-.3, font=3, srt=-25, cex=0.9, adj=0.5)

# add fit statistics to the path diagram image
fitstats = fitMeasures(fit.xmy, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
fitstring1 = paste0("χ²(",fitstats["df"],")=",sprintf("%.2f",fitstats["chisq"]))
fitstring2 = paste0("p=",sprintf("%.2f",fitstats["pvalue"]))
fitstring3 = paste0("CFI=",sprintf("%.2f",fitstats["cfi"]))
fitstring4 = paste0("TLI=",sprintf("%.2f",fitstats["tli"]))
fitstring5 = paste0("RMSEA=",sprintf("%.2f",fitstats["rmsea"]))
fitstring6 = paste0("SRMR=",sprintf("%.2f",fitstats["srmr"]))
fitstringfull = paste(fitstring1, fitstring2, fitstring3, fitstring4, fitstring5, fitstring6, sep="\n")
text(2.0, 0.7, fitstringfull, adj=c(1,1), family="mono", cex=1, xpd=NA)

dev.off()   # close the PNG file writing connection




##### PANELS OF SCATTER PLOTS (can copy into one image overlaying a path diagram using GIMP or Photoshop) ######
# plot panel 1: X -> M  (grid position 1)
png(paste0(fig.dir, "Mediation_raw_path_a.png"), width=75, height=75, unit="mm", res=1000, pointsize=8)   
plot(y=latent_estimates$M, x=latent_estimates$X, ylab="Latent Ruminative Thinking (M) Estimate", xlab="Latent Depression (X) Estimate", main="Depression (X) → Ruminative Thinking (M)", pch=16, ps=1.25, col=cols[1], cex.lab=1, cex.axis=1, cex.main=1)
abline(a=0, b=coef(fit.xmy)["a"])   # draw regression line
text(2.9, 2.5, "a", font=3, cex=1.25)   # add "a" symbol clarifying what the regression line is
dev.off()


# plot panel 2: M -> Y (grid position 3)
png(paste0(fig.dir, "Mediation_raw_path_b.png"), width=75, height=75, unit="mm", res=1000, pointsize=8)   
plot(y=latent_estimates$Y, x=latent_estimates$M, ylab="Latent Drinking To Cope (Y) Estimate", xlab="Latent Ruminative Thinking (M) Estimate", main="Ruminative Thinking (M) → Drinking to Cope (Y)", pch=16, ps=1.25, col=cols[1], cex.lab=1, cex.axis=1, cex.main=1)
abline(a=0, b=coef(fit.xmy)["b"])  
text(2.9, 1.1, "b", font=3, cex=1.25)
dev.off()


# plot panel 3
png(paste0(fig.dir, "Mediation_raw_path_c.png"), width=75, height=75, unit="mm", res=1000, pointsize=8)   
plot(y=latent_estimates$Y, x=latent_estimates$X, ylab="Latent Drinking To Cope (Y) Estimate", xlab="Latent Ruminative Thinking (X) Estimate", main="Depression (X) → Drinking to Cope (Y)", pch=16, ps=1.25, col=cols[1], cex.lab=1, cex.axis=1, cex.main=1)
abline(a=0, b=coef(fit.xy)["c"], lty=2)   # plot c (total effect) regression line
abline(a=0, b=coef(fit.xmy)["c_prime"], lty=1)   # plot c' (direct effect) regression line
text(2.9, 2.7, "c", font=3, cex=1.25)
text(2.9, 1.4, "c'", font=3, cex=1.25)
#legend("topleft", lty=c(1,2), legend=c("c (total effect)", "c' (direct effect)"), bty="n")
dev.off()

