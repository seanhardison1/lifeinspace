#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an array of texture
#'
#' @param width,height area dimensions
#' @param params aesthetic parameters passed from the geom e.g. 'pattern_fill', 
#'        'pattern_frequency' etc.
#' @param legend logical. If the request to create a pattern comes during 
#'        creation of the legend, then this is TRUE, otherwise FALSE
#'
#' @return an RGBA numeric array with dimensions [height, width, 4]
#'
#' @import ambient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_simple <- function(width, height, params, legend) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure the selected pattern is sane.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  choice <- params$pattern_type
  if (is.null(choice) || is.na(choice) || !is.character(choice)) {
    choice <- 'a'
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Choose the values with which to fill the array
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  values <- switch(
    choice,
    a = rep(c(0, 1, 0, 1, 1, 0, 0, 1, 1, 1), each = 3),
    b = rep(c(1, 0, 0, 1, 0.5, 0.5, 1, 1, 0, 0, 0, 0, 0, 0.5), each = 7),
    c = rep(seq(0, 1, 0.05), each = 7),
    rep(c(0, 1, 0, 1, 1, 0, 0, 1, 1, 1), each = 3)
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create an RGBA array of the requested dimensions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  simple_array <- array(values, dim = c(height, width, 4))
  
  simple_array
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create an array of noise using the 'ambient' package
#'
#' @param width,height area dimensions
#' @param params aesthetic parameters passed from the geom e.g. 'pattern_fill', 
#'        'pattern_frequency' etc.
#' @param legend logical. If the request to create a pattern comes during 
#'        creation of the legend, then this is TRUE, otherwise FALSE
#'
#' @return an RGBA numeric array with dimensions [height, width, 4]
#'
#' @import ambient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_pattern_ambient <- function(width, height, params, legend) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The only 2 parameters needed are the 2 ends of the colour scale
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  colour1 <- as.character(params$pattern_fill )
  colour2 <- as.character(params$pattern_fill2)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a ramp function from these 2 colours
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ramp_func <- colorRamp(c(colour1, colour2), alpha = TRUE)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a noise matrix of the requested dimenions using 'ambient'.
  # The contents are normalised to all be in the range [0,1]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  noise_matrix  <- ambient::noise_simplex(dim = c(height, width))
  noise_matrix  <- ambient::normalise(noise_matrix)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use each value in the noise matrix to lookup a colour using the 
  # colour ramp function, then ensure the results are an RGBA array of the
  # correct dimensions.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  colour_matrix <- ramp_func(noise_matrix)/255
  noise_array   <- array(colour_matrix, dim = c(height, width, 4))
  
  
  noise_array
}