combine_images <- function(image_filepaths, output = "plot") {
  library(png)
  library(grid)
  
  # Create an all-white placeholder image
  placeholder <- matrix(rep(255, 100*100), 100, 100)
  
  # Load the images from the file paths, replacing any that are not found with the placeholder image
  images <- lapply(image_filepaths, function(filepath) {
    if (file.exists(filepath)) {
      readPNG(filepath)
    } else {
      placeholder
    }
  })
  
  # Determine the width and height of the output image
  width <- sum(sapply(images, function(x) dim(x)[1]))
  height <- max(sapply(images, function(x) dim(x)[2]))
  
  # Create a blank image with the desired dimensions
  output_image <- matrix(rep(255, width * height), width, height)
  
  # Place the images side-by-side in the output image
  x_offset <- 0
  lapply(images, function(img) {
    rasterImage(img, x_offset, 0, dim(img)[1], dim(img)[2])
    x_offset <- x_offset + dim(img)[1]
  })
  
  # Plot the output image in the plot window or save it to a file, depending on the value of the output argument
  if (output == "plot") {
    plot(1:10, type = "n")
    grid.raster(output_image, interpolate = FALSE)
  } else if (output == "file") {
    writePNG(output_image, "output.png")
  } else {
    stop("Invalid value for output argument. Must be 'plot' or 'file'.")
  }
}

update_file_extensions <- function(input_dir, extension = "1", preTag = NULL) {
  # Get a list of all the files in the input directory
  file_list <- list.files(input_dir, full.names = TRUE)
  
  # Loop through the files and update the ones with the specified extension
  lapply(file_list, function(oldFileName) {
    if (tools::file_ext(oldFileName) == extension) {
      file.rename(from = oldFileName, to = paste0(tools::file_path_sans_ext(oldFileName), ".png"))
    }
  })
  
  # Step 2: rename
  if (!is.null(preTag)) {
    file_list <- list.files(input_dir, full.names = FALSE)
    lapply(file_list, function(fileName) {
      file.rename(from = file.path(input_dir, fileName),
                  to = file.path(input_dir, paste0("28807", fileName)))
    }
    )
  }
}

imageDir <- "C:/Users/William.Roberts/OneDrive - Unilever/Documents/ic_images"
update_file_extensions(imageDir, preTag = "28807")
image_filepaths <- 

printShelf <- function(itemShelfSDt, imageFileDir) {
  
}