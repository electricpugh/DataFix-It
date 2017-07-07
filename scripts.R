# scripts.R

########################################################################################################
# Groom Function: creates a fully-ordered and complete block of data where transmission gaps are filled#
# with NAs and a message is generated if data is found outside the expected transmission intervals     #
########################################################################################################

# Determine all times of data
groom <- function (x, y) {
  # Eliminate rows where no data is present (established by no data in water temperature column)
  x <- x[!is.na(x$WATER_TEMP), ]
  
  # Create vector of times where water quality data is present (to be tested against sequence below)
  datatime <- x$NST_DATI[which(!is.na(x$WATER_TEMP))]
  
  # Generate sequence from start and end of data by interval y and use to fill gaps in x.
  min <- x$NST_DATI[1]
  max <- x$NST_DATI[length(x$NST_DATI)]
  sequence <- seq(min, max, by = y)
  
  # If Water Quality data was recorded outside of expected sequence, display a message
  if(!all(datatime %in% sequence)) {
    message("Some water quality data was recorded outside of the expected measurement interval.")
  }
  
  times <- data.frame(list(NST_DATI = sequence))
  df <- merge(times, x, all = TRUE)
  return(df)
}