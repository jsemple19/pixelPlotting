#functions for working on pixel data from Tiff files.

#function reduces full Tiff stack to a list of pixels with values above a certain
#threshold (i.e. discards the many black pixels)
# getColourPixels<-function(array1,array2,threshold) {
#   i<-which(unlist(array1)>threshold & unlist(array2)>threshold)
#   return(list(unlist(array1)[i],unlist(array2)[i]))
# }
#a better version extracts z-y-x coordinates for tiff image as well, allows verfication
#of correct value extraction (not taht in fiji a 0 value numbering system is used so numbers
#differ by 1)
getColourPixels<-function(array1,array2,threshold,zcoord) {
  i<-which(unlist(array1)>threshold & unlist(array2)>threshold)
  #i<-which(array1>threshold & array2>threshold)
  if (length(i)>0) {
    y=dim(array1)[1]
    x=dim(array1)[2]
    xcoord<-ceiling(i/y)
    ycoord<-ifelse(i%%y>0,i%%y,y)
    z_y_x=paste(zcoord,ycoord,xcoord,sep="_")
    return(data.frame(z_y_x=z_y_x, G=unlist(array1)[i],R=unlist(array2)[i],
                      stringsAsFactors=FALSE))
  } else {
    return(data.frame(z_y_x=character(),G=numeric(),R=numeric(),
                      stringsAsFactors=FALSE))
  }
}


#function to do a scatter plot with a random subsample of too large a data set
plotSubSampleScatter<-function(vec1,vec2,points=50000,...) {
  i<-sample(1:length(vec1),points)
  ssvec1<-vec1[i]
  ssvec2<-vec2[i]
  plot(ssvec1,ssvec2,pch=16,cex=0.5,col="#0000ff33",...)
}


