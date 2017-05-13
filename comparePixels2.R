#2017-05-12 comparePixels2.R
#pixel intensity plots
# to run you need to:
#
# 1) put all the tiff files (red and green channal separately) in a single folder
# Preprocessing steps: I stitched in xuvtools, and saved in .ims format.
# For some reason, the output seemed to have an additional slice added
# in alternate slices. In Fiji it looks black, so i just removed it by doing:
#   Image/Stacks/Tools/Make substack…
# in the following box on the bottom line, choose only frame 1 (bottom line)
#
# Then, for my own clarity, i separated the red and green channels so it is easier to
# keep track of what is from where. To do this i go to:
#   Image/Stacks/Tools/DeInterleave
# I then save the green channel with extension _c1_G.tif and the red channel with _c2_R.tif
#
# 2) place this script and pixelGettingFunctions.R into the same folder
#
# 3) create a tiffList file and complete the data for tiffFileName, wormNum, channal,
# strain,TEVcs. name is created automatically by the script.
#
# 4) set working directory by going to Session/Set Working Directory/To Source File Location
#
# 5) click on the "Source" button
# expected out put will be a file pixelPlots.pdf with various plots of pixel data
# (scatter/boxplots/histograms/density) for raw/log tranformed/ratio/logratio data
# the numerical data for the pixels will also be output as .csv files for each worm

#the first time you run it you need to install the pakcages. Uncomment the section below on
#the first run only:
#install.packages("tiff")
#install.packages("raster")

##setwd("~/Documents/MeisterLab/otherPeopleProjects/Julian/images/2017.05.05.601v604")

library(tiff) #for tiff import
library(raster) #for tiff?
#library(colorspace)
#prepare pallette of non red/green colours
palette(c("blue","purple","orange","pink2","steelblue2","plum2","magenta1","khaki3"))

source("pixelGettingFunctions.R")

#read in file with list of tiff files and experimental info
tiffList<-read.delim("tiffList.txt",stringsAsFactors=FALSE)
tiffList$name<-with(tiffList,paste0("worm",wormNum,"_",sex,"_",strain))

#process tiffs in pairs and process immediately to avoid loading many big objects at once
pixelList=list()
for (f in seq(1,dim(tiffList)[1],by=2)) {
  f=1
  g<-readTIFF(tiffList[f,1],all=TRUE,as.is=TRUE)
  r<-readTIFF(tiffList[f+1,1],all=TRUE,as.is=TRUE)
  #record how many z sections there are
  nlayG<-length(g)
  #set minimal pixel threshold
  th<-4
  allpix=data.frame(z_y_x=character(),G=numeric(),R=numeric(),stringsAsFactors=FALSE)
  for (i in 1:nlayG) {
    both<-getColourPixels(g[[i]],r[[i]],threshold=th,zcoord=i)
    allpix<-rbind(allpix,both)
  }
  pixelList[[tiffList[f,"name"]]]<-allpix
}
#now i have a list of data frames, one for each worm

#open PDF for output
pdf("pixelPlots.pdf",paper="a4",width=8,height=11)

##################### raw data plots #########################
#do smoothed scatter plots for both channels
par(mfrow=c(4,2))
lapply(seq_along(pixelList),function(i) {
  smoothScatter(pixelList[[i]]$G,pixelList[[i]]$R, xlab="green pixel value", ylab="red pixel value",
                main=names(pixelList)[i])
})

#do submsampled scatter plots (randomly selecting 50000 points)
par(mfrow=c(4,2))
lapply(seq_along(pixelList),function(i) {
  plotSubSampleScatter(pixelList[[i]]$G,pixelList[[i]]$R,points=50000, xlab="green pixel value",
                     ylab="red pixel value", main=names(pixelList)[i])
})



######################## log transformed data plots ####################
#log transform the pixel values to improve their distribution and re-plot scatters
lapply(seq_along(pixelList),function(i) {
  pixelList[[i]]$logG<<-log2(pixelList[[i]]$G)
  pixelList[[i]]$logR<<-log2(pixelList[[i]]$R)
})

#do smoothed scatter plots of logged values
par(mfrow=c(4,2))
lapply(seq_along(pixelList),function(i) {
  smoothScatter(pixelList[[i]]$logG,pixelList[[i]]$logR, xlab="log2(green)", ylab="log2(red)",
                main=names(pixelList)[i])
})

#do submsampled scatter plots of logged values (randomly selecting 50000 points)
par(mfrow=c(4,2))
lapply(seq_along(pixelList),function(i) {
  plotSubSampleScatter(pixelList[[i]]$logG,pixelList[[i]]$logR,points=50000, xlab="log2(green)",
                       ylab="log2(red)", main=names(pixelList)[i])
})

#plot histograms of red and green values separately
par(mfrow=c(4,2))
lapply(seq_along(pixelList),function(i) {
  hist(pixelList[[i]]$logG,xlab="log2(green)",main=names(pixelList)[i],col="green")
  hist(pixelList[[i]]$logR,xlab="log2(red)",main=names(pixelList)[i],col="red")
})

par(mfrow=c(2,1))
#do boxplot for each channel separately
forBoxplot<-list()
lapply(seq_along(pixelList),function(i) {
  forBoxplot[[2*i-1]]<<-pixelList[[i]]$logG
  forBoxplot[[2*i]]<<-pixelList[[i]]$logR
})
oldMar<-par()$mar
par(mar=c(7.1,4.1,4.1,2.1))
boxplot(forBoxplot, varwidth=TRUE, notch=TRUE, col=c("green", "red"),
        names=rep(names(pixelList),each=2), cex.axis=1, las=2,
        main="log pixel values")
par(mar=oldMar)

################ ratios ###############
#look at pixel R/G ratios
lapply(seq_along(pixelList),function(i) {
  pixelList[[i]]$RGratio<<-pixelList[[i]]$R/pixelList[[i]]$G
})

#draw histograms of ratios
par(mfrow=c(4,2))
lapply(seq_along(pixelList),function(i) {
  hist(pixelList[[i]]$RGratio, xlab="R/G", main=names(pixelList)[i], col=i, cex.main=1.1)
})

#draw boxplot of ratios
par(mfrow=c(3,1))
forBoxplot<-list()
lapply(seq_along(pixelList),function(i) {
  forBoxplot[[i]]<<-pixelList[[i]]$RGratio
})
boxplot(forBoxplot,names=names(pixelList),col=1:length(pixelList),main="raw ratios")

#draw density plot of ratios
plot(density(pixelList[[1]]$RGratio,adjust=3), col=1, main="distribution of R/G",ylim=c(0,2.5))
legend("topright",names(pixelList), lty=c(1,1), col=1:length(pixelList),
       lwd=c(2.5,2.5), cex=1)
lapply(2:length(pixelList),function(i) {
  lines(density(pixelList[[i]]$RGratio,adjust=3), col=i)
})


###################### log ratios #####################################
lapply(seq_along(pixelList),function(i) {
  pixelList[[i]]$logRatio<<-log2(pixelList[[i]]$RGratio)
})

#draw histograms of log ratios
par(mfrow=c(4,2))
lapply(seq_along(pixelList),function(i) {
  hist(pixelList[[i]]$logRatio, xlab="log2(R/G)", main=names(pixelList)[i], col=i,
       cex.main=1.1)
})

#draw boxplot of log ratios
par(mfrow=c(3,1))
forBoxplot<-list()
lapply(seq_along(pixelList),function(i) {
  forBoxplot[[i]]<<-pixelList[[i]]$logRatio
})
boxplot(forBoxplot,names=names(pixelList),col=1:length(pixelList),main="log ratios")


#draw density plot of log ratios
plot(density(pixelList[[1]]$logRatio,adjust=3), col=1, main="distribution of log(R/G)",
     ylim=c(0,0.7))
legend("topright",names(pixelList), lty=c(1,1), col=1:length(pixelList),
       lwd=c(2.5,2.5), cex=1)
lapply(2:length(pixelList),function(i) {
  lines(density(pixelList[[i]]$logRatio,adjust=3), col=i)
})

dev.off()


#write dataFrames with data to .csv files
lapply(seq_along(pixelList),function(i) {
  write.csv(pixelList[[i]],file=paste0(names(pixelList)[i],".csv"), quote=FALSE,
  row.names=FALSE)
})
