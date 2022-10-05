#### Bar Graphs of SEM Results as Reported in Addictive Behaviors Journals #####

# This file contains R syntax for creating the bar graph (Figure 1) showing the ways SEM results are presented in addiction journals
# Syntax by Kevin Hallgren (khallgre@uw.edu) 2018


# read data (change directory as needed!)
d = read.csv("C:\\Users\\kevin\\UW Office 365\\OneDrive - UW Office 365\\School\\Atkins Lab\\SEM data viz\\paper syntax\\Data viz in addiction journals.csv")

# compute sums
sums = colSums(d[,c("Tables","PathDiagrams","ModelVisuals","RawDataVisuals")], na.rm=T)

# reverse the order of the table so they present in the desired order in the bar graph
sums = rev(sums)

# create labels that will show up on the bar graph
barlabels = c("Visualizations of\nDisaggregated Data", "Visualizations of\nStatistical Parameter\nEstimates (e.g.,\nregression coefficients)", "Path Diagrams", "Tables")

# Open a port to save the figure in a PNG file.  (Can add "#" symbol to beginning of the lines below to comment this out, which will display the graph on the console instead of saving as PNG file)
fig.dir = "C:\\Users\\kevin\\UW Office 365\\OneDrive - UW Office 365\\School\\Atkins Lab\\SEM data viz\\paper figs\\"    # this is the directory for where the PNG file will be saved
png(paste0(fig.dir, "Barplot of addiction journal figures.png"), width=90, height=80, unit="mm", res=2000, pointsize=8)   

# generate bar plot
par(mar=c(4.1,10.1,2.1,1.1))   # set the figure margins with a large margin on left side for displaying labels -- see ?par for more information
b = barplot(sums, beside=T, axes=F, names.arg="", xlim=c(0,110), col="cyan4", horiz=T, xlab="Number of Occurrences")

# add axis
axis(1, at=c(0,20,40,60,80,100))
text(y=b, x=-1, labels=barlabels, pos=2, xpd=NA, cex=.95)   # use "text" instead of "axis" function to get better control of positioning

# add numbers above bars
text(y=b[1], x=sums[1]+1, labels=sums[1], pos=4, xpd=NA)   # use xpd=NA to prevent this value from getting clipped if it falls outside of plotting region
text(y=b[2], x=sums[2]+1, labels=sums[2], pos=4)
text(y=b[3], x=sums[3]+1, labels=sums[3], pos=4)
text(y=b[4], x=sums[4]+1, labels=sums[4], pos=4)

box()   # add a box on border of the plot

# close the PNG file connection, which will save the figure to the location in fig.dir
dev.off()   


