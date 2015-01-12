# install.packages("png")
# install.packages("jpeg")
# install.packages("tiff", type = "source")

# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")

library(png)
library(jpeg)
library(tiff)
library(grid)
library(EBImage)

setwd("~/Projects/kaggle/National Data Science Bowl")

files <- list.files(path = "./competition_data/test", full.names = TRUE, recursive = TRUE, pattern = "\\.jpg$")

maxPixel <- 25
imageSize <- maxPixel * maxPixel
numRows <- length(files)
numCols <- 25 * 25 + 6 + 5 + 1 # pixels + shapes + moments + y

extractFeatures <- function(imageFile) {
  filename <- strsplit(imageFile, split = '/', fixed = TRUE)[[1]][4]
  name <- list("image" = filename)
  image <- readImage(imageFile)
  colorMode(image) <- Grayscale
  imageThreshold <- ifelse(image > mean(image), 0.0, 1.0)
  kern <- makeBrush(5, shape = 'box')
  imageDilate <- dilate(imageThreshold, kern)
  labels <- bwlabel(imageDilate)
  imageLabel <- imageThreshold * labels

  moments <- computeFeatures.moment(imageLabel)
  shapes <- computeFeatures.shape(imageLabel)
  maxAreaRow <- which.max(shapes[, 's.area'])[[1]]
  shapeFeatures <- as.list(shapes[maxAreaRow, ])
  momentFeatures <- as.list(moments[maxAreaRow, ])

  sidePixels <- 25
  imageResize <- resize(imageThreshold, sidePixels, sidePixels, filter = "none", output.origin = c(0, 0))
  pixelFeatures <- as.list(imageResize)
  totalPixels <- sidePixels * sidePixels
  names(pixelFeatures) <- lapply(1:totalPixels, function(i){paste0("p", i)})

  features <- append(pixelFeatures, append(shapeFeatures, momentFeatures))
  dataRow <- append(name, features)

  return(dataRow)
}

testCases <- matrix(ncol = numCols, nrow = numRows)

ptm <- proc.time()

print("Processing images...")
for(i in 1:numRows) {
  if(i == 1) {
    colnames(testCases) <- names(extractFeatures(files[i]))
  }
  if(i %% 1000 == 0) {
    cat(format(100.0*i/numRows, digits = 2, nsmall = 2), "%\n")
  }
  testCases[i, ] <- unlist(extractFeatures(files[i]))
}
dim(testCases)
print(proc.time() - ptm)

write.csv(testCases, "./test_cases.csv", col.names = TRUE, row.names = FALSE, quote = FALSE)
