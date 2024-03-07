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
library(tidyr)



################################################################################
# #  Importing 
################################################################################

# # Data from the dermatologist verification
fileName <- "20240214_samples_results.csv"
inFile   <- paste(inPath, fileName, sep = "")
data     <- read.csv(inFile,header = T)



################################################################################
# #  Table 1.  Proportion of images by lesion type according to the number of 
# #  cases where no squares, one square, two squares, three squares, or all four 
# #  squares overlap with either the lesion or the shadow.
################################################################################


table(data$dx,data$n_sombras)
table(data$dx,data$n_lesion)


data_summary <- data %>%
  mutate(n_sq=n_sombras+n_lesion)  

contingency_table <- data_summary %>%
  group_by(dx, n_sq) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  spread(key = n_sq, value = count, fill = 0) %>%
  mutate(Total = rowSums(select(., -dx))) %>% # Add a total column for each 'dx'
  arrange(desc(Total))

totals <- contingency_table %>%
  summarise(across(where(is.numeric), sum)) # Sums only numeric columns


totals$dx <- "Total"

# Binding the totals row to the original contingency table
contingency_table <- bind_rows(contingency_table, totall=totals)


contingency_table <- contingency_table %>%
  mutate(across(`0`:`4`, ~ ./Total))


fileName <- "20240227_samples_table.csv"
outFile   <- paste(outPath, fileName, sep = "")
write.csv(contingency_table,outFile)


################################################################################
# #  Importing 
################################################################################












