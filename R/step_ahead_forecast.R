#' Function to calculate the step ahead forecasting for a proposed Prediction method
#'
#' @param dataIn as input time series for testing
#' @param trainedData  as partition point of input data `dataIn`
#' @param MethodPath as as location of function for the proposed imputation method
#' @param errorParameter as type of error calculation (RMSE, MAE or MAPE)
#' @param stepSize as interval at which step by step prediction will be done (Possible values are 1 & 2)
#' @import ggplot2
#' @importFrom stats na.omit
#' @importFrom imputeTestbench mape mae mape
#' @export
#' @return returns the plot for one/two step ahead prediction along with error values decided by `errorParameter`
step_ahead_forecast <- function(dataIn, trainedData, MethodPath, errorParameter, stepSize)
{
  j <- 1
  Predicted <- NULL
  actual <- NULL
  err <- NULL
  index <- NULL
  sd <- NULL
  #stepSize <- 1

  if(!(hasArg(dataIn)))
  {
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
#
#   if(!is.vector(dataIn))
#   {
#     dataIn <- dataIn[, 1]
#   }

  if(!(hasArg(errorParameter)))
  {
    errorParameter <- 1
  }

  if(!(hasArg(stepSize)))
  {
    stepSize <- 1
  }

  if(!(hasArg(trainedData)))
  {
    trainedData <- round(0.75*length(dataIn))
  }

  if(stepSize == 2)
  {
    for(i in seq(trainedData, (length(dataIn)-1), 2))
    {
      testData <- dataIn[1:i]
      testData1 <- dataIn[1:i+1]
      d <- parse(text = MethodPath)
      d <- eval(d)
      d <- d$value(testData)

      Predicted_dummy <- d
      Predicted[j] <- Predicted_dummy[1]
      Predicted[j+1] <- Predicted_dummy[2]
      actual[j] <- dataIn[i+1]
      actual[j+1] <- dataIn[i+2]
      sd[j] <- sd(testData)
      err[j] <- sd[j]
      sd[j+1] <- sd(testData1)
      err[j+1] <- sd[j+1]
      index[j] <- j
      index[j+1] <- j+1
      j <- j + 2
    }
  }

  if(stepSize == 1)
  {
    for(i in trainedData:(length(dataIn)-1))
    {
      testData <- dataIn[1:i]
      d <- parse(text = MethodPath)
      d <- eval(d)
      d <- d$value(testData)

      #Predicted[j] <- pred_for_w(testData, 5, 3, 1)
      Predicted[j] <- d
      actual[j] <- dataIn[i+1]
      sd[j] <- sd(testData)
      err[j] <- sd[j]
      index[j] <- j
      j <- j + 1
    }

  }

  x <- NULL
  Prediction <- NULL
  y2 <- NULL
  y3 <- NULL
  y4 <- NULL

  d <- data.frame(x=index, Prediction = actual, y2= Predicted, y3=(Predicted+err), y4 = (Predicted-err))
  d <- na.omit(d)
  #d1 <- melt(d, id="index")

  jk <- ggplot(d, aes(x)) +
    geom_line(aes(y = Prediction, colour = "Observed Data"), alpha=1, size = 1) + geom_point(aes(y=Prediction)) +
    geom_line(aes(y = y2, colour = "Predicted Data"), alpha=1, size = 1) + geom_point(aes(y=y2)) +
    geom_line(aes(y = y3, colour = "Predicted Data + Std. Deviation"), linetype="dotdash", alpha=0.8) +
    geom_line(aes(y = y4, colour = "Predicted Data - Std. Deviation"), linetype="dotdash", alpha=0.8) +
    scale_color_manual(values= c("black", "blue", "purple", "red"))


  if(errorParameter == 1)
  {
    #ghnew <- rmse(actual - Predicted)
    ghnew <- rmse(d[2] - d[3])
    parameter <- "RMSE Value"
  }
  if(errorParameter == 2)
  {
    #ghnew <- mae(actual - Predicted)
    ghnew <- mae(d[2] - d[3])
    parameter <- "MAE Value"
  }
  if(errorParameter == 3)
  {
    #ghnew <- imputeTestbench::mape((actual - Predicted), actual)
    ghnew <- imputeTestbench::mape((d[2] - d[3]), d[2])
    parameter <- "MAPE Value"
  }
  if(errorParameter[1] == 4)
  {
    newPar <- parse(text = errorParameter[2])
    newPar <- eval(newPar)
    newPar <- newPar$value(d[2], d[3])
    ghnew <- newPar
    parameter <- errorParameter[3]
  }

  existing_method <- list(Plot = jk)

  MethodName <- paste(parameter,"Error", sep = "_")
  existing_method[[paste(parameter)]] <- ghnew

  return(existing_method)
}

