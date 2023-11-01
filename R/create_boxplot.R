#' Boxplot Function
#'
#'\code{create_boxplot} makes a boxplot of payments by DRG code;
#'
#' @param data the data frame "DRG_data"
#' @param payment_type a string name indicating the payment type
#'
#' @return \code{create_boxplot} returns a boxplot; \code{Medipay_stat} returns a sentence containing requested statistic.
#' @export
#'
#'@importFrom ggplot2 ggplot
#'@importFrom ggplot2 geom_boxplot
#'
#' @examples
#' create_boxplot(DRG_data, payment_type = "Average Medicare Payments")

load("data/DRG_data.rda")

create_boxplot <- function(data = DRG_data,
                           payment_type = "Average Medicare Payments") {
  valid_payment_types <- c("Average Medicare Payments",
                           "Average Total Payments",
                           "Average Covered Charges")
  if (!payment_type %in% valid_payment_types) {
    stop("Invalid payment type. Please choose from: 'Average Medicare Payments', 'Average Total Payments', 'Average Covered Charges'")
  }
  data$DRG_Code <- substr(data$`DRG Definition`, 1, 3)

  p <- ggplot(data, aes(x = `DRG Definition`,
                        y = !!sym(payment_type))) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", payment_type), x = "DRG Code", y = payment_type)

  p + theme(axis.text.x = element_text(angle = 90, hjust = 0.5))+
    scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 0, "", x))
}

