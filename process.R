install.packages("png")
install.packages("jpeg")
install.packages("tiff", type = "source")

source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")

library(png)
library(jpeg)
library(tiff)
library(grid)
library(EBImage)

example_file <- "competition_data/train/acantharia_protist/101574.jpg"

# im <- readJPEG(example_file)
# grid.raster(im)

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
cat('Number of objects: ', max(labels))
imlabelsgrayscale <- imthr * labels / max(labels) # for plotting only
display(imlabelsgrayscale, title = "Labeled Image", method = 'raster')


moment <- computeFeatures.moment(imlabels)
shape <- computeFeatures.shape(imlabels)

