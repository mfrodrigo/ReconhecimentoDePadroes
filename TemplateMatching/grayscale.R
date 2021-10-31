greyscale <- function(image_matrix){
  cor <- rev(gray(5:1/50))
  image(image_matrix[,,2], col=cor)
  return(image)
}