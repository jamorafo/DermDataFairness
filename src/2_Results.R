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
# #  Figure 1:
################################################################################
rr <- 3 # Number of examples per skin lesion type. 
cats <- c("akiec","bcc","bkl","df","mel","nv","vasc")

# # Showing some examples for the sampling method.
fileName <- "Figure_1_sampling_examples.pdf"
outFile   <- paste(outPath, fileName, sep = "")
pdf(outFile,width = length(cats),height = rr)
par(mfcol=c(rr, length(cats)), mar=c(1, 1.3, 1.6, 1)) 
# It shows an example for each type lesion.
for (cc in cats){
  mdata <- x %>%
    filter(dx%in%cc)
  files <- paste(mdata$image_id,".jpg",sep="")
  # # Plotting the first rr images of each skin lesion type
  for (ii in 1:rr){
    fileName <- paste("HAM10000_images/",files[ii],sep="")
    inFile   <- paste(inPath, fileName, sep = "")
    img <- readJPEG(inFile)
    dim(img)
    cuts <- c(0.08,.12,0.88,0.92)
    c_p1 <- dim(img)[1]*cuts #= 36  54 396 414
    c_p2 <- dim(img)[2]*cuts #= 48  72 528 552
    title <- ""
    if(ii==1) title <- cc
    plot(x=seq(0,600,length=100),y= seq(0,450,length=100) , type='n',
         xlab="width (pixels)",ylab="height (pixels)",las=1,main = title,cex=0.5, 
         cex.axis=0.5, tcl=-0.1,mgp=c(3,.5,0))
    rasterImage(img, 0, 0, 600, 450)
    rect(c_p2[1],c_p1[1],c_p2[2],c_p1[2])
    rect(c_p2[3],c_p1[1],c_p2[4],c_p1[2])
    rect(c_p2[3],c_p1[3],c_p2[4],c_p1[4])
    rect(c_p2[1],c_p1[3],c_p2[2],c_p1[4])
  }
}
dev.off()

################################################################################
# #  Applying the Gray World algorithm  with Limiting Correction Factors: 
# #  The Gray World algorithm adjusts the image's RGB channels based on the
# #  average color of the image, aiming to make the average color gray.
# #  We Apply limits to the correction factors in the GWA to prevent extreme 
# #  changes in color balance. Setting thresholds can help maintain closer 
# #  alignment with the original tones.  Specifically, the correction factors 
# #  are constrained to a narrow window between 0.95 and 1.05. 
################################################################################
red <- pixel[,,1]
green <- pixel[,,2]
blue <- pixel[,,3]

mean_r <- mean(red)
mean_g <- mean(green)
mean_b <- mean(blue)

# Compute correction factors with limits
corr_factor <- min(mean_r, mean_g, mean_b) / c(mean_r, mean_g, mean_b)
corr_factor <- pmin(corr_factor, 1.05) # Limiting the correction factor to max 1.05
corr_factor <- pmax(corr_factor, 0.95) # Limiting the correction factor to min 0.95

# Apply correction
red_a <- red *0.95
green_a <- green * corr_factor[2]
blue_a <- blue * corr_factor[3]

# # Calculate the overall mean
mean_gray <- (mean_r + mean_g + mean_b) / 3

pixel_a <- pixel

red_a -> pixel_a[,,1]
green_a -> pixel_a[,,2]
blue_a -> pixel_a[,,3]

#pixel_a[pixel_a>1] <- 1
#pixel_a[pixel_a<0] <- 0

mean_rgb <- (red_a+green_a+blue_a)/3
mean_rgb <- mean_rgb[,1]


# # Skintones for the whole database  

# # Ordering the pixels to plot them
order_var <- order(mean_rgb)
pixels <- pixel_a[
  order_var,,
]

    
fileName <- "Figure_2_skintones.pdf"
outFile   <- paste(outPath, fileName, sep = "")
pdf(outFile,width = 2,height = 6)
par(mar=c(1,2,1,1))
plot(c(0, 100), c(0, 100), type = "n", xlab = "", ylab = "",axes = F)
corners = par("usr")
rasterImage(pixels, 0, 0, 100, 100, interpolate = F)
axis(2, seq(0,100,10),seq(0,100,10)[11:1],las=3,labels=F)
par(xpd = TRUE)
lab_num <- paste(seq(0,100,10)[11:1],"%",sep = "")
text(x = -20, y = seq(0,100,10), lab_num, srt = 270,font=2,cex=0.8)
lines(x=c(100,0), y=c(95,95) , col = "gray", lty = 20, lwd=2)
text(x = -20, y = 95, "5%", srt = 270,font=2,cex=0.7,col = "gray")
dev.off()

library(pdftools)
pdf_rotate(outFile, rotate = 90)


# # Skin tones by type of lesion   
cats <- c("nv","mel","bkl","bcc","akiec","vasc","df")

ids_names <- gsub(".jpg","",files_names)
pixels_cat <- list()
for (cc in cats){
  ids_cats <- x$image_id[x$dx==cc]
  
  ii_names <- which(ids_names%in%ids_cats)
  pixel_cat <- pixel[ii_names,,]
  mean_rgb_cat <- mean_rgb[ii_names]
  order_var <- order(mean_rgb_cat)
  
  pixels_cat[[cc]] <-   pixel_cat[
    order_var,,
  ]
  
  n <- length(ii_names)
  
  pixels_cat[[cc]] <- pixels_cat[[cc]][n:1,,] # for the next plot
}


fileName  <- "Figure_3_skintone_by_lesion_and_barplot.pdf"
outFile   <- paste(outPath, fileName, sep = "")
pdf(outFile, width = 12, height = 7) # Set width and height appropriately

# Define the layout for the plots
layout(matrix(c(1, 2), 1, 2), widths=c(3.4, 2))
par(mar=c(1,4,1,0)) # c(bottom, left, top, right) 
plot(c(0, 84), c(0, 100), type = "n", xlab = "", ylab = "",axes = F)
corners = par("usr")
rasterImage(pixels_cat[[1]], 0, 0, 10, 100, interpolate = F)
rasterImage(pixels_cat[[2]], 12, 0, 22, 100, interpolate = F)
rasterImage(pixels_cat[[3]], 24, 0, 34, 100, interpolate = F)
rasterImage(pixels_cat[[4]], 36, 0, 46, 100, interpolate = F)
rasterImage(pixels_cat[[5]], 48, 0, 58, 100, interpolate = F)
rasterImage(pixels_cat[[6]], 60, 0, 70, 100, interpolate = F)
rasterImage(pixels_cat[[7]], 72, 0, 82, 100, interpolate = F)
axis(2, seq(0,100,10),seq(0,100,10)[11:1],las=3,labels=F)
par(xpd = TRUE)
lab_num <- paste(seq(0,100,10),"%",sep = "")
text(x = -10, y = seq(0,100,10), lab_num, srt = 0,font=2,cex=0.8)
lines(x=c(84,0), y=c(5,5) , col = "gray", lty = 20, lwd=2)
text(x = -10, y = 5, "5%", srt = 0,font=2,cex=0.7,col = "gray")
text(x = c(5,17,29,41,53,65,77), y = -5, cats, srt = 0,font=2,cex=0.8)

par(mar=c(8,1.8,7,3))
hh <- sapply(cats, function(x) dim(pixels_cat[[x]])[1])
hh <- 100*hh/sum(hh)
lab_hh <- paste(format(round(hh,1),1),"%",sep = "")
pp <- barplot(hh,col = 1,axes = F,cex.names = 0.6,font=2,width = 1, axisnames = F)
axis(2, seq(0,60,10),seq(0,100,10)[11:1],las=3,labels=F,tck=-0.02)
par(xpd = TRUE)
text(x = -0.7, y = seq(0,60,10), lab_num[1:7], srt = 0,font=2,cex=0.7)
text(x = pp, y = hh+2, lab_hh , srt = 0,font=2,cex=0.9)
text(pp, -6, names(hh),pos= 3,cex=0.8,font=2)
dev.off()

################################################################################
# #  End
################################################################################
















