# install.packages("png")
# install.packages("jpeg")
# install.packages("tiff", type = "source")
# install.packages("raster")
# install.packages("glcm")
# source("http://bioconductor.org/biocLite.R")
# biocLite("EBImage")

library(png)
library(jpeg)
library(tiff)
library(grid)
library(raster)
library(glcm)
library(EBImage)

setwd("~/Projects/kaggle/plankton")

# dataPath = "/Volumes/Gokiburi/Kaggle/augmented/train"
dataPath = "./competition_data/train"

example_file <- paste(dataPath, "acantharia_protist/101574.jpg", sep = "/")
example_file <- paste(dataPath, "acantharia_protist_big_center/13857.jpg", sep = "/")
example_file <- paste(dataPath, "acantharia_protist_halo/4844.jpg", sep = "/")

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

textures <- glcm(raster(im@.Data))
plot(raster(im@.Data))
plot(textures)
plot(textures$glcm_dissimilarity)
sort(unique(textures$glcm_dissimilarity@data@values))

moments <- computeFeatures.moment(imlabels)
shapes <- computeFeatures.shape(imlabels)

maxAreaRow <- which.max(shapes[, 's.area'])[[1]]
shapes[maxAreaRow, ]
moments[maxAreaRow, ]

categories <- list.dirs(path = dataPath, full.names = FALSE, recursive = TRUE)
files <- list.files(path = dataPath, full.names = FALSE, recursive = TRUE, pattern = "\\.jpg$")

maxPixel <- 48
imageSize <- maxPixel * maxPixel
numRows <- length(files)
numCols <- 1 + 1 + 1 + 48 * 48 + 6 + 5 + 1 # file + width + height + pixels + shapes + moments + y

extractFeatures <- function(path, imageName) {
  imageFile <- paste(path, imageName, sep = '/')
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

  sidePixels <- 48
  imageResize <- resize(imageThreshold, sidePixels, sidePixels, filter = "none", output.origin = c(0, 0))
  pixelFeatures <- as.list(imageResize)
  totalPixels <- sidePixels * sidePixels
  names(pixelFeatures) <- lapply(1:totalPixels, function(i){paste0("p", i)})

  attributes <- as.list(c(imageName, dim(image)))
  names(attributes) <- c("file", "width", "height")
  features <- append(pixelFeatures, append(shapeFeatures, momentFeatures))
  dataRow <- append(append(attributes, features), output)

  return(dataRow)
}

plankton <- matrix(ncol = numCols, nrow = numRows)

ptm <- proc.time()

print("Processing train images...")
for(i in 1:numRows) {
  if(i == 1) {
    colnames(plankton) <- names(extractFeatures(dataPath, files[i]))
  }
  if(i %% 1000 == 0) {
    cat(format(100.0*i/numRows, digits = 2, nsmall = 2), "%\n")
  }
  plankton[i, ] <- unlist(extractFeatures(dataPath, files[i]))
}
dim(plankton)
print(proc.time() - ptm)

write.csv(plankton, "./plankton_train_48.csv", row.names = FALSE, quote = FALSE)


# Testing files

testPath = "./competition_data/test"
files <- list.files(path = testPath, full.names = FALSE, recursive = TRUE, pattern = "\\.jpg$")

testCases <- matrix(ncol = numCols, nrow = numRows)

ptm <- proc.time()

print("Processing test images...")
for(i in 1:numRows) {
  if(i == 1) {
    colnames(testCases) <- names(extractFeatures(testPath, files[i]))
  }
  if(i %% 1000 == 0) {
    cat(format(100.0*i/numRows, digits = 2, nsmall = 2), "%\n")
  }
  testCases[i, ] <- unlist(extractFeatures(testPath, files[i]))
}
dim(testCases)
print(proc.time() - ptm)

write.csv(testCases, "./plankton_test_48.csv", row.names = FALSE, quote = FALSE)
