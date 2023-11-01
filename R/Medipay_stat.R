#'Calculation Function
#'
#'\code{Medipay_stat} calculates statistics over all of the DRG codes for average Medicare payments.
#'
#' @param x a string name indicating the statistic to be calculated
#'
#' @return \code{Medipay_stat} returns a sentence containing requested statistic.
#' @export
#'
#' @examples
#' Medipay_stat("mean")


Medipay_stat <- function(x){
  if(x == "mean"){
    mean <- round(mean(DRG_data$`Average Medicare Payments`), 2)
    print(paste("The mean of average Medicare payments is", mean))
  } else if(x == "median"){
    median <- round(median(DRG_data$`Average Medicare Payments`), 2)
    print(paste("The med?ian of average Medicare payments is", median))
  } else if(x == "standard deviation"){
    sd <- round(sd(DRG_data$`Average Medicare Payments`), 2)
    print(paste("The standard deviation of average Medicare payments is", sd))
  } else print("Invalid input, you must choose from mean, median and standard deviation ")
}
