## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
library(imagefx)

## ---- eval=FALSE---------------------------------------------------------
#  install.packages('jpeg')

## ----eval=FALSE----------------------------------------------------------
#  library(jpeg)
#  
#  ## identify where the image files are located
#  img.dir <- '/name_of_camera/year/month/day/hour/'
#  
#  ## which image will you read in
#  img.file <- 'sakurajima.jpeg'
#  
#  ## read in the image using functions from the jpeg package
#  ## note the image 'sakurajima' comes preloaded with the imagefx package
#  sakurajima <- readJPEG(paste(img.dir,img.file,sep=""))
#  

## ----fig.height=6,fig.width=7--------------------------------------------
## what are the dimensions of the image
dim(sakurajima)

## plot the image using the imagefx package
image2(sakurajima,asp=1,xlab="image rows",ylab="image columns")


## ---- eval=FALSE,fig.cap="Caption"---------------------------------------
#  ## manually crop the image by clicking on the plot region
#  crop.manual <- crop.image(sakurajima)

## ------------------------------------------------------------------------
## define the corners of the crop region
xleft = 1
xright = 188
ybottom = 1
ytop = 396

## use the above points to automatically crop the image
crop.auto <- crop.image(sakurajima,xleft,ybottom,xright,ytop)

## ----fig.width=7,fig.height=4--------------------------------------------
## isolate the cropped image
img.crop <- crop.auto$img.crop

## Plot original and cropped images
plot.new()
split.screen(c(1,2))
screen(1)
image2(sakurajima,asp=1,xlab="",ylab="", main='Original')
screen(2)
image2(img.crop,asp=1,ylab="",xlab="", main='Cropped')

## ----fig.width=7,fig.height=4--------------------------------------------
## separate the image into its RGB color channels
img.r <- img.crop[,,1]
img.g <- img.crop[,,2]
img.b <- img.crop[,,3]

## subtract the mean from each color channel matrix
img.r.dmean <- img.r - mean(img.r)
img.g.dmean <- img.g - mean(img.g)
img.b.dmean <- img.b - mean(img.b)

## Find the best fit plane (trend) in each color channel matrix
img.r.trend <- fit3d(img.r.dmean)
img.g.trend <- fit3d(img.g.dmean)
img.b.trend <- fit3d(img.b.dmean)

## subtract the fitted plane from each color channel (i.e. detrend)
img.r.dtrend <- img.r.dmean - img.r.trend
img.g.dtrend <- img.g.dmean - img.g.trend
img.b.dtrend <- img.b.dmean - img.b.trend

## plot the red channel detrend and original
plot.new()
split.screen(c(1,2))
screen(1)
image2(img.r,asp=1,xlab="",ylab="",main="Red Channel")
screen(2)
image2(img.r.dtrend,asp=1,xlab="",ylab="",main="Red Channel Detrended")


## ----fig.width=7,fig.height=2,fig.fullwidth=TRUE-------------------------
## define a sigma value for the Gaussian filter
sig=25

## build a Gaussian mask based on this sigma and whose dimensions match the image
gaus <- build.gaus(xdim=nrow(img.r.dtrend),ydim=ncol(img.r.dtrend),sig.x=sig)

## find the pixel value location in each channel that deviates most from the mean.
max.r = which(abs(img.r.dtrend)==max(abs(img.r.dtrend)),arr.ind=TRUE)
max.g = which(abs(img.g.dtrend)==max(abs(img.g.dtrend)),arr.ind=TRUE)
max.b = which(abs(img.b.dtrend)==max(abs(img.b.dtrend)),arr.ind=TRUE)

## define a window size used in the connected component algorithm
win.size=0.05

## extract the blob from each channel
blob.r <- blob.extract(img.r.dtrend,max.r,win.size,gaus)
blob.g <- blob.extract(img.g.dtrend,max.g,win.size,gaus)
blob.b <- blob.extract(img.b.dtrend,max.b,win.size,gaus)


################
#-- PLOTTING --#
################

## note the blob points (blob$xy.coords) must be adjusted according to
## where the origin (0,0) is located in R image plots
blob.coords.r  <- blob.r$xy.coords
blob.coords.r[,1] <- blob.r$xy.coords[,2]
blob.coords.r[,2] <- (blob.r$xy.coords[,1]-nrow(img.r))*-1

blob.coords.g  <- blob.g$xy.coords
blob.coords.g[,1] <- blob.g$xy.coords[,2]
blob.coords.g[,2] <- (blob.g$xy.coords[,1]-nrow(img.g))*-1

blob.coords.b  <- blob.b$xy.coords
blob.coords.b[,1] <- blob.b$xy.coords[,2]
blob.coords.b[,2] <- (blob.b$xy.coords[,1]-nrow(img.b))*-1


close.screen(all.screens=TRUE)
split.screen(c(1,3))

screen(1)
par(mar=c(0,0,2,0))
image2(img.r,asp=1,axes=FALSE)
points(blob.coords.r,col=rgb(1,0,0,alpha=0.05),pch=16,cex=0.3)
title('Red Channel',line=0,font=2,col='red',cex=2)

screen(2)
par(mar=c(0,0,2,0))
image2(img.g,asp=1,axes=FALSE)
points(blob.coords.g,col=rgb(0,1,0,alpha=0.05),pch=16,cex=0.3)
title('Green Channel',line=0,font=2,col='darkgreen',cex=2)

screen(3)
par(mar=c(0,0,2,0))
image2(img.b,asp=1,axes=FALSE)
points(blob.coords.b,col=rgb(0,0,1,alpha=0.05),pch=16,cex=0.3)
title('Blue Channel',line=0,font=2,col='darkblue',cex=2)


## ------------------------------------------------------------------------
## calculate the blob statistics in each RGB channel
blob.stats.r <- calc.blob.stats(img.r.dtrend, blob.r$xy.coords)
blob.stats.g <- calc.blob.stats(img.g.dtrend, blob.g$xy.coords)
blob.stats.b <- calc.blob.stats(img.b.dtrend, blob.b$xy.coords)

print(blob.stats.r)
print(blob.stats.g)
print(blob.stats.b)

## ----eval=FALSE----------------------------------------------------------
#  ## name the directory which holds the image files
#  img.dir <- 'raw_images/year/month/day/hour/'
#  
#  ## list the files in this directory.  Note they should be sorted from oldest to newest
#  img.files <- list.files(img.dir)
#  
#  ## how many statistics will you save.
#  ## this will change if you modify the calc.blob.stats function
#  num.stats=14
#  
#  ## set up matrices to hold all the blob statistics for each RGB channel
#  all.blob.stats.r = matrix(NA,nrow=length(img.files),ncol=num.stats)
#  all.blob.stats.g = matrix(NA,nrow=length(img.files),ncol=num.stats)
#  all.blob.stats.b = matrix(NA,nrow=length(img.files),ncol=num.stats)
#  
#  ## loop over each image and perform the steps outlined above
#  i=1
#  while(i<=length(img.files)) {
#  
#    ## what is the current image file
#    cur.img.file <- paste(img.dir,img.files[i],sep="")
#  
#    ## read in the current image
#    cur.img <-readJPEG(cur.img.file)
#  
#    ## Preprocess the image ...
#    ## Perform blob detection ...
#    ## Calculate blob statistics ...
#  
#    ## save the statistics from the current frame to the all blob statistics matrix
#    all.blob.stats.r[i,] <- blob.stats.r
#    all.blob.stats.g[i,] <- blob.stats.g
#    all.blob.stats.b[i,] <- blob.stats.b
#  
#    ## move onto the next file
#    i=i+1
#  }
#  
#  ## combine the blob statistis associated with each RGB channel into a list
#  all.stats <- list(r=all.blob.stats.r, g=all.blob.stats.g, b=all.blob.stats.b)
#  
#  ## save all the statistics in an appropriate directory
#  save(all.stats,file='blob_stats/year/month/day/hour/blob_stats.RData')

## ----fig.width=7,fig.height=7--------------------------------------------
## pick a single statistic from all the blob statistics 
## in this case, the sum of the red channel blob region
blob.sum <- blob.stats$r[,1]

## PLOTTING ##
## set up the plot
close.screen(all.screens=TRUE)
split.screen(c(2,1))
split.screen(c(1,3),screen=1)

## plot some example images 
screen(3)
par(mar=c(0,0,0,0))
image2(erebus.40,axes=FALSE,xlab="",ylab="",asp=1)
text(40,nrow(erebus.40),labels='t1',pos=1,font=2,col='white')

screen(4)
par(mar=c(0,0,0,0))
image2(erebus.70,axes=FALSE,xlab="",ylab="",asp=1)
text(40,nrow(erebus.70),labels='t2',pos=1,font=2,col='white')

screen(5)
par(mar=c(0,0,0,0))
image2(erebus.90,axes=FALSE,xlab="",ylab="",asp=1)
text(40,nrow(erebus.90),labels='t3',pos=1,font=2,col='white')

## plot raw blob stats
screen(2)
plot(blob.sum,type='o',bg='gray80',xlab="",ylab='',
     pch=21, cex=0.5, ylim=c(min(blob.sum),max(blob.sum)+500),axes=FALSE)

## add a title
title(main='Blob Sum in Red Channel')

## add some axis information
mtext('normalized amplitude',side=2,line=1)
mtext('time (frames)',side=1,line=3)
axis(1)

## add text and lines indicating where the images occur
abline(v=c(40,70,90),lty=2,col='gray80')
text(x=c(40,70,90),y=rep(max(blob.sum)+400,3),labels=c('t1','t2','t3'),pos=2)


## ----fig.width=7,fig.height=5--------------------------------------------
## pick a single statistic from (i.e. the sum of the red channel blob region)
blob.sum <- blob.stats$r[,1]

## choose the window length for the running average operator
win.length = 15

## smooth the blob sum stat according to window length
blob.sum.filt <- run.avg(blob.sum,win.length)

## PLOTTING ##
plot(blob.sum,type='o',bg='gray80',xlab="time (frame)",
     ylab='blob sum in red channel',pch=21,cex=0.5)
lines(blob.sum.filt,col='gray30',lwd=4)
lines(blob.sum.filt,col='red',lwd=2)

## add a legend
legend('topright',legend=c('raw','filtered'),col=c('gray80','red'),
       pch=c(21,NA),lty=c(1,1),pt.bg=c('gray30',NA))


