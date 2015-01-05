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
example_file <- "competition_data/train/acantharia_protist/101574.jpg"

im <- readImage(example_file)
display(im, title = "Acantharia Protist", method = 'raster')
print(im)
colorMode(im) <- Grayscale
print(im)

display(im, title = "Original Image", method = 'raster')

imthr <- ifelse(im > mean(im), 0.0, 1.0)
display(imthr, title = "Thresholded Image", method = 'raster')

kern <- makeBrush(5, shape = 'box')
imdilated <- dilate(imthr, kern)
display(imdilated, title = "Dilated Image", method = 'raster')

labels <- bwlabel(imdilated)
imlabels <- imthr * labels
imlabelsgrayscale <- imthr * labels / max(labels) # for plotting only
display(imlabelsgrayscale, title = "Labeled Image", method = 'raster')

moments <- computeFeatures.moment(imlabels)
shapes <- computeFeatures.shape(imlabels)

maxAreaRow <- which.max(shapes[, 's.area'])[[1]]
shapes[maxAreaRow, ]
moments[maxAreaRow, ]

categories <- list.dirs(path = "./competition_data/train", full.names = FALSE, recursive = TRUE)
files <- list.files(path = "./competition_data/train", full.names = TRUE, recursive = TRUE, pattern = "\\.jpg$")

maxPixel <- 25
imageSize <- maxPixel * maxPixel
numRows <- length(files)
numCols <- 25 * 25 + 6 + 5 + 1 # pixels + shapes + moments + y

extractFeatures <- function(imageFile) {
  y <- strsplit(imageFile, split = '/', fixed = TRUE)[[1]][4]
  output <- list("y" = y)
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
  dataRow <- append(features, output)

  return(dataRow)
}

plankton <- matrix(ncol = numCols, nrow = numRows)

ptm <- proc.time()

print("Processing images...")
for(i in 1:numRows) {
  if(i == 1) {
    colnames(plankton) <- names(extractFeatures(files[i]))
  }
  if(i %% 1000 == 0) {
    cat(format(100.0*i/numRows, digits = 2, nsmall = 2), "%\n")
  }
  plankton[i, ] <- unlist(extractFeatures(files[i]))
}
dim(plankton)
print(proc.time() - ptm)

write.csv(plankton, "./plankton.csv", col.names = TRUE, row.names = FALSE, quote = FALSE)
