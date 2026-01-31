#' Custom function that generates diagnostic plots.
#' @param model A fitted model object of class "lm".
#' @param alpha Defines the opacity of the points (0-1).
#' @param bins  Defines the number of bins in the histogram.
#' @param scaleLocation Logical: should a scale location graph be added?
diagnosticPlots <- function(model, alpha=0.7, bins=10, scaleLocation=FALSE) {
  # First, some error checking, making sure all inputs make sense:
  if (model |> class() != "lm") stop("model must be an lm object")
  if (alpha < 0 | alpha > 1) stop("alpha must be between 0 and 1")
  if (bins <= 0) stop("bins must be a positive number")
  # Summarize residuals, observed, and fitted values in a tibble:
  residualData <-
    dplyr::tibble(
      residuals = residuals(model),
      # The response variable is the first column in the model's model field:
      y = model$model[,1],
      yHat = fitted(model)
    )
  # Generate histogram for assessing normality:
  p1 <-
    ggplot2::ggplot(residualData) +
    ggplot2::aes(x = residuals, y = after_stat(density)) +
    ggplot2::geom_histogram(bins = bins, fill = "steelblue", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Residuals", y = "Density")
  # Generate scatter plot for assessing constant variance:
  p2 <-
    ggplot2::ggplot(residualData) +
    ggplot2::aes(x = yHat, y = residuals) +
    ggplot2::geom_hline(aes(yintercept = 0)) +
    ggplot2::geom_point(color = "steelblue", alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Estimated Values", y = "Residuals")
  # Generate Q-Q plot for assessing normality:
  p3 <-
    ggplot2::ggplot(residualData) +
    # Use standardized residuals
    ggplot2::aes(sample = scale(residuals)) +
    ggplot2::geom_qq_line() +
    ggplot2::geom_qq(color = "steelblue", alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(x= "Theoretical Quantiles", y = "Observed Quantiles")
  # If scaleLocation is TRUE, add a scale-location plot in lower right corner:
  if (scaleLocation) {
    p4 <-
      ggplot2::ggplot(residualData) +
      ggplot2::aes(x = yHat, y = sqrt(abs(residuals))) +
      ggplot2::geom_point(color = "steelblue", alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Estimated Values", y = expression(sqrt("|Residuals|")))

    cowplot::plot_grid(p1, p2, p3, p4, nrow = 2)
    # Otherwise, generate plot with just three panels:
  } else {
    cowplot::plot_grid(p1, p2, p3, nrow = 2)
  }
}
