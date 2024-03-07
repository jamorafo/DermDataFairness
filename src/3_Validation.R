rm(list=ls(all=TRUE))
################################################################################
# #  Paths 
################################################################################
# #  Directory. Type your own source path
setwd("/Volumes/GoogleDrive/My\ Drive/SKINCANCER/src")

docPath      <- "../doc/informes/"
outPath      <- "../output/"
outPathGraph <- "../output/graph/"
inPath       <- "../input/"
srcPath      <- "../src/"

################################################################################
# #  Libraries 
################################################################################

library(jpeg)
library(dplyr)
library(stringr)



################################################################################
# #  Importing 
################################################################################

fileName <- "HAM10000_metadata.RData"
inFile   <- paste(inPath, fileName, sep = "")
load(inFile)

fileName <- "pixel.RData"
outFile   <- paste(outPath, fileName, sep = "")
load(outFile)
dim(pixel)


################################################################################
# #  Randomly selecting 10% of each lesion type and place them in 36 pages to
# #  facilitate the dermatologist verification. A csv file is also created to
# #  be filled out by hand for the dermatologist.
################################################################################
tc <- table(x$dx)
nc <- round(0.1*tc)
sum(nc)

head(x)
nc <- data.frame(nc)
colnames(nc) <- c("dx","nc")

tc <- data.frame(tc)
colnames(tc) <- c("dx","tc")

xnc <- x %>%
  left_join(nc) %>%
  left_join(tc)

set.seed(240216)
xnc$ran <- runif(dim(x)[1])

xnc_sorted <- xnc %>%
  arrange(dx,desc(ran)) %>%
  group_by(dx) %>%
  mutate(ik= rank(-ran)) %>%
  ungroup() %>%
  mutate(select = (ik <= nc) ) %>%
  filter(select) %>%
  mutate(ik_padded = sprintf("%03d", ik), dx_short = substr(dx, 1, 2), 
                             id = str_c(dx_short, ik_padded,sep  = " ")) %>%
  mutate()


## Figures
rr <- 4 # rows
ww <- 7

files <- paste(xnc_sorted$image_id,".jpg",sep="")
pag <- 1
j0 <- 1
length(files)
jn <- j0+rr*ww-1
ctl <- 0
pag_vec <- NULL
while(ctl<2){
  pag_nam <- paste("pag -",sprintf("%02d", pag))
  fileName <- paste(pag_nam,".pdf", sep = "")
  outFile   <- paste(outPath, fileName, sep = "")
  pdf(outFile,width = ww,height = rr)
  par(mfrow=c(rr, ww), mar=c(1, 1.3, 0.8, 1)) # Adjust margins as needed  c(bottom, left, top, right)
   for (ii in j0:jn){
    fileName <- paste("HAM10000_images/",files[ii],sep="")
    inFile   <- paste(inPath, fileName, sep = "")
    img <- readJPEG(inFile)
    dim(img)
    cuts <- c(0.08,.12,0.88,0.92)
    c_p1 <- dim(img)[1]*cuts #= 36  54 396 414
    c_p2 <- dim(img)[2]*cuts #= 48  72 528 552
    
    title <- xnc_sorted$id[ii]
    plot(x=seq(0,600,length=100),y= seq(0,450,length=100) , type='n',
         xlab="width (pixels)",ylab="height (pixels)",las=1,main = title,cex.main=0.7, 
         tcl=-0.1,mgp=c(3,.5,0),yaxt="n",xaxt="n")
    rasterImage(img, 0, 0, 600, 450)
    rect(c_p2[1],c_p1[1],c_p2[2],c_p1[2])
    rect(c_p2[3],c_p1[1],c_p2[4],c_p1[2])
    rect(c_p2[3],c_p1[3],c_p2[4],c_p1[4])
    rect(c_p2[1],c_p1[3],c_p2[2],c_p1[4])
    pag_vec <- c(pag_vec,pag) # pag indicator
  }
  par(mar=c(5, 4, 4, 4) + 0.1)
  text(650,-50,pag_nam,adj=1,xpd=T,cex=0.5)
  dev.off()
  pag <- pag + 1
  j0 <- j0+rr*ww 
  jn <- j0+rr*ww-1
  if (jn>length(files)){
    jn <- length(files)
    ctl <- ctl + 1
  }
}

## Table
xnc_sorted$pag <- pag_vec

fileName <- "20240214_samples.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(xnc_sorted,outFile)

################################################################################
# #  End
################################################################################














