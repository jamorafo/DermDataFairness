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

# # HAM10000_metadata.RData is the .RData version of the csv version that you can
# # download from the Harvard repository. It contains auxiliary information for
# # each one of the 10,015 images. The object's name is "x".
fileName <- "HAM10000_metadata.RData"
inFile   <- paste(inPath, fileName, sep = "")
load(inFile)

fileName <- "HAM10000_images/"
inFile   <- paste(inPath, fileName, sep = "")
files_names   <- list.files(inFile)


################################################################################
# #  Pixels sampling
# #  Steps: 
# #  1. A square from each corner image is sampled from each image.
# #  Each sample represents an square. An square has three dimensions: 19 pixels 
# # (width) x 25  pixels (long) x 3 colors (RGB). So, it's not exactly a square. 
# #  2. Then, all 4 squares are joint in one set and the median is computed for
# # each image and each color. The medians are saved in the pixel object. 
################################################################################
n_files <- length(files_names)
pixel   <- array(NA,c(n_files,50,3))

for (ii in 1:n_files){
  fileName <- paste("HAM10000_images/",files_names[ii],sep="")
  inFile   <- paste(inPath, fileName, sep = "")
  img <- readJPEG(inFile)
  dimg <- dim(img)
  # # cuts: It defines the starting and the ending point for each square.
  # # The proportion for pixels is the 4% (= 0.12-0.08 or = 0.92-0.88) for
  # # each dimension.
  cuts <- c(0.08,.12,0.88,0.92)
  c_p1 <- dimg[1]*cuts #= 36  54 396 414
  c_p2 <- dimg[2]*cuts #= 48  72 528 552
  # Pixels location:
  # 450*c(0.08,.12,0.88,0.92) = 36  54 396 414
  # 600*c(0.08,.12,0.88,0.92) = 48  72 528 552
  # ul: upper left, ur: upper right, dl: down left, dr: down right.
  ul <- img[c_p1[1]:c_p1[2],c_p2[1]:c_p2[2],]
  ur <- img[c_p1[1]:c_p1[2],c_p2[3]:c_p2[4],]
  dl <- img[c_p1[3]:c_p1[4],c_p2[1]:c_p2[2],]
  dr <- img[c_p1[3]:c_p1[4],c_p2[3]:c_p2[4],]
  medR <- median(c(ul[,,1],ur[,,1],dl[,,1],dr[,,1]))
  medG <- median(c(ul[,,2],ur[,,2],dl[,,2],dr[,,2]))
  medB <- median(c(ul[,,3],ur[,,3],dl[,,3],dr[,,3]))
  pixel[ii,,1] <- medR
  pixel[ii,,2] <- medG
  pixel[ii,,3] <- medB
  message(paste(ii," out of ",n_files, "images have been processed"))
  message(paste(round(100*ii/n_files,2), "% images have been processed", sep=""))
}


fileName  <- "pixel.RData"
outFile   <- paste(outPath, fileName, sep = "")
save(pixel,files_names,file=outFile)


################################################################################
# #  End
################################################################################


       



