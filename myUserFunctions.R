

#---------------------------------------------------------------------------------------------------------------------
#   Load file into R environment
#---------------------------------------------------------------------------------------------------------------------
GetFileData <- function(file.name) {
  # Reads a file from project directory into a dataframe and returns dataframe
  df <- read.csv(file.name, sep = ",",header = TRUE, stringsAsFactors = FALSE, na.strings = c("","NA"));
  return(df);
}

#---------------------------------------------------------------------------------------------------------------------
#   Transform date data into specific date format
#---------------------------------------------------------------------------------------------------------------------

formatDateData <- function(date.column,...) {
  date.column <- as.Date(date.column, format = "%m/%d/%Y");
}

#---------------------------------------------------------------------------------------------------------------------
#   Transform all Date attributes to particular date format
#---------------------------------------------------------------------------------------------------------------------

transformDates <- function(df) {
    df[,grepl("Date" , names(df))] <- lapply(df[,grepl("Date" , names(df))], FUN = formatDateData)
    return(df);
}

#---------------------------------------------------------------------------------------------------------------------
#   Transform $ value data into numeric format
#---------------------------------------------------------------------------------------------------------------------

formatToNumeric <- function(money.column,...) {
  money.column <- gsub('[$]','',money.column); # Remove '$' in column
  money.column <- as.character(money.column);  # Change to character 
  money.column <- as.numeric(gsub(",","",money.column)); # and remove comma
}


#---------------------------------------------------------------------------------------------------------------------
#     Function to plot forecast errors to see if model fits well
#---------------------------------------------------------------------------------------------------------------------



plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}

    


