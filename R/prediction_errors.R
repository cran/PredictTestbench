#' Function working as testbench for comparison of Prediction algorithms
#'
#' @param  dataIn as input time series for testing
#' @param  nextVal as an integer to decide number of values to predict
#' @param  errorParameter as type of error calculation (RMSE, MAE or MAPE)
#' @param  MethodPath as location of function for the proposed imputation method
#' @param  MethodName as name for function for the proposed imputation method
#' @import ggplot2
#' @import forecast
#' @import PSF
#' @importFrom imputeTestbench mape mae rmse
#' @importFrom stats ts
#' @importFrom methods hasArg
#' @return Returns error comparison for imputation methods
#' @export
#' @examples
#' # aa <- prediction_errors(nextVal = 10)
#' # aa

#==================================================================================
# prediction_errors starts here....
#==================================================================================

prediction_errors <- function(dataIn, nextVal, errorParameter, MethodPath, MethodName)
{
  options(warn=-1)
  if(!(hasArg(dataIn)))
  {
    # dataIn <- c(1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5,1:5)
    # dataIn <- nottem
    dataIn <- c(40.6, 40.8, 44.4, 46.7, 54.1, 58.5, 57.7, 56.4, 54.3, 50.5, 42.9, 39.8, 44.2, 39.8, 45.1, 47.0, 54.1, 58.7, 66.3,
                59.9, 57.0, 54.2, 39.7, 42.8, 37.5, 38.7, 39.5, 42.1, 55.7, 57.8, 56.8, 54.3, 54.3, 47.1, 41.8, 41.7, 41.8, 40.1,
                42.9, 45.8, 49.2, 52.7, 64.2, 59.6, 54.4, 49.2, 36.3, 37.6, 39.3, 37.5, 38.3, 45.5, 53.2, 57.7, 60.8, 58.2, 56.4,
                49.8, 44.4, 43.6, 40.0, 40.5, 40.8, 45.1, 53.8, 59.4, 63.5, 61.0, 53.0, 50.0, 38.1, 36.3, 39.2, 43.4, 43.4, 48.9,
                50.6, 56.8, 62.5, 62.0, 57.5, 46.7, 41.6, 39.8, 39.4, 38.5, 45.3, 47.1, 51.7, 55.0, 60.4, 60.5, 54.7, 50.3, 42.3,
                35.2, 40.8, 41.1, 42.8, 47.3, 50.9, 56.4, 62.2, 60.5, 55.4, 50.2, 43.0, 37.3, 34.8, 31.3, 41.0, 43.9, 53.1, 56.9,
                62.5, 60.3, 59.8, 49.2, 42.9, 41.9, 41.6, 37.1, 41.2, 46.9, 51.2, 60.4, 60.1, 61.6, 57.0, 50.9, 43.0, 38.8, 37.1,
                38.4, 38.4, 46.5, 53.5, 58.4, 60.6, 58.2, 53.8, 46.6, 45.5, 40.6, 42.4, 38.4, 40.3, 44.6, 50.9, 57.0, 62.1, 63.5,
                56.3, 47.3, 43.6, 41.8, 36.2, 39.3, 44.5, 48.7, 54.2, 60.8, 65.5, 64.9, 60.1, 50.2, 42.1, 35.8, 39.4, 38.2, 40.4,
                46.9, 53.4, 59.6, 66.5, 60.4, 59.2, 51.2, 42.8, 45.8, 40.0, 42.6, 43.5, 47.1, 50.0, 60.5, 64.6, 64.0, 56.8, 48.6,
                44.2, 36.4, 37.3, 35.0, 44.0, 43.9, 52.7, 58.6, 60.0, 61.1, 58.1, 49.6, 41.6, 41.3, 40.8, 41.0, 38.4, 47.4, 54.1,
                58.6, 61.4, 61.8, 56.3, 50.9, 41.4, 37.1, 42.1, 41.2, 47.3, 46.6, 52.4, 59.0, 59.6, 60.4, 57.0, 50.7, 47.8, 39.2,
                39.4, 40.9, 42.4, 47.8, 52.4, 58.0, 60.7, 61.8, 58.2, 46.7, 46.6, 37.8)
  }

  # if(!is.vector(dataIn))
  # {
  #   dataIn <- dataIn[, 1]
  # }

  testData <- dataIn[1:(length(dataIn)-nextVal)]
  compData <- dataIn[(length(dataIn)-nextVal+1):length(dataIn)]

  # For future reference
  dataIn1 <- testData

  # Set default values
  if(!(hasArg(errorParameter)))
  {
    errorParameter <- 1
  }

  if(!(hasArg(nextVal)))
  {
    nextVal <- 10
  }


  if(!(hasArg(MethodName)))
  {
    MethodName <- "Proposed Method"
  }


  e <- 0
  f <- 0
  e1 <- 0
  f1 <- 0
  enew <- 0
  fnew <- 0

    gh <- NULL
    gh1 <- NULL
    ghnew <- NULL

    dT1 <- Sys.time()
      d <- forecast(auto.arima(dataIn1), nextVal)
      d <- as.numeric(unlist(data.frame(d)[1]))
    dT2 <- Sys.time()
    dT <- dT2 - dT1
    dT <- as.numeric(dT)

    d1T1 <- Sys.time()
      #d1 <- AUTO_PSF(dataIn1, nextVal)$Predicted_Values
      d1 <- psf(data = dataIn1, n.ahead = nextVal)$predictions
    d1T2 <- Sys.time()
    d1T <- d1T2 - d1T1
    d1T <- as.numeric(d1T)

     if((hasArg(MethodPath)))
      {
        # to call functions from provided "MethodPath"
    dnewT1 <- Sys.time()
        dnew <- parse(text = MethodPath)
        dnew <- eval(dnew)
        dnew <- dnew$value(dataIn1)
    dnewT2 <- Sys.time()
    dnewT <- dnewT2 - dnewT1
    dnewT <- as.numeric(dnewT)

        if(errorParameter == 1)
        {
          ghnew <- rmse(compData - dnew)
          parameter <- "RMSE Plot"
        }
        if(errorParameter == 2)
        {
          ghnew <- mae(compData - dnew)
          parameter <- "MAE Plot"
        }
        if(errorParameter == 3)
        {
          ghnew <- imputeTestbench::mape((compData - dnew), compData)
          parameter <- "MAPE Plot"
        }
        if(errorParameter[1] == 4)
        {
          newPar <- parse(text = errorParameter[2])
          newPar <- eval(newPar)
          newPar <- newPar$value(compData, dnew)
          ghnew <- newPar
          parameter <- errorParameter[3]
        }
      }


      if(errorParameter == 1)
      {
        gh <- rmse(compData - d)
        gh1 <- rmse(compData - d1)
        parameter <- "RMSE Plot"
      }
      if(errorParameter == 2)
      {
        gh <- mae(compData - d)
        gh1 <- mae(compData - d1)
        parameter <- "MAE Plot"
      }
      if(errorParameter == 3)
      {
        gh <- imputeTestbench::mape((compData - d), compData)
        gh1 <- imputeTestbench::mape((compData - d1), compData)
        parameter <- "MAPE Plot"
      }
      if(errorParameter[1] == 4)
      {
        newPar <- parse(text = errorParameter[2])
        newPar <- eval(newPar)
        newPar1 <- newPar$value(compData, d)
        gh <- newPar1
        newPar2 <- newPar$value(compData, d1)
        gh1 <- newPar2
        parameter <- errorParameter[3]
      }
  e
  fx <- 1:length(compData)
  ex <- d
  g <- data.frame(fx,ex)
  h <- ggplot(g,aes(fx,ex)) + labs(title = parameter) + xlab("Percent of Missing Values")+ ylab("Error Values") + geom_line(aes(color="ARIMA Method")) + labs(color="Imputing Methods")

  ex <- d1
  g1 <- data.frame(fx,ex)
  h <- h + geom_line(data=g1,aes(color= "PSF Method"))

  if((hasArg(MethodPath)))
  {
    ex <- enew
    fx <- 1:length(compData)
    gnew <- data.frame(fx,ex)

    h <- h + geom_line(data=gnew,aes(color= MethodName))
  }
  options(warn=-1)

  if((hasArg(MethodPath)))
  {

    return(list(Parameter = parameter, Desired_Prediction = compData, ARIMA_Method_Prediction = d, ARIMA_Method_Error = gh, ARIMA_Execution_Time_in_Seconds = dT, PSF_Method_Prediction = d1, PSF_Method_Error = gh1, PSF_Execution_Time_in_Seconds = d1T, Proposed_Method_Prediction = dnew, Proposed_Method_Error = ghnew, Proposed_Method_Execution_Time_in_Seconds = dnewT))
  }
  else{
    return(list(Parameter = parameter, Desired_Prediction = compData, ARIMA_Method_Prediction = d, ARIMA_Method_Error = gh, ARIMA_Execution_Time_in_Seconds = dT, PSF_Method_Prediction = d1, PSF_Method_Error = gh1, PSF_Execution_Time_in_Seconds = d1T))
    }
}
