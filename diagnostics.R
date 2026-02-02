#' Custom function that generates diagnostic plots.
#' @param model A fitted model object of class "lm".
#' @param alpha Defines the opacity of the points (0-1).
#' @param bins  Defines the number of bins in the histogram.
diagnosticPlots <- function(model, alpha = 0.7, bins = 10) {

  # First, make sure the patchwork and ggplot2 packages are installed
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' needed but not installed.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' needed but not installed.")
  }

  # Then some error checking, making sure all inputs make sense
  if (model |> class() != "lm") {
    stop("model must be an lm object")
  }
  if (alpha < 0 | alpha > 1) {
    stop("alpha must be between 0 and 1")
  }
  if (bins <= 0) {
    stop("bins must be a positive number")
  }

  # Summarize residuals, observed, and fitted values in a tibble
  residualData <-
    dplyr::tibble(
      residuals = stats::residuals(model),
      # The response variable is the first column in the model's model field:
      y = model$model[,1],
      yHat = stats::fitted(model)
    )

  # Generate histogram for assessing normality
  p1 <- ggplot2::ggplot(residualData, ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = bins, fill = "steelblue", color = "black") +
    ggplot2::labs(x = "Residuals", y = "Count") +
    ggplot2::theme_bw()

  # Generate Q-Q plot for assessing normality
  p2 <- ggplot2::ggplot(residualData,
                        # Use standardized residuals:
                        ggplot2::aes(sample = scale(residuals))) +
    ggplot2::geom_qq_line() +
    ggplot2::geom_qq(color = "steelblue", alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical Quantiles", y = "Observed Quantiles")

  # Generate scatter plot for assessing constant variance
  p3 <- ggplot2::ggplot(residualData, ggplot2::aes(x = yHat, y = residuals)) +
    ggplot2::geom_hline(aes(yintercept = 0)) +
    ggplot2::geom_point(color = "steelblue", alpha = alpha) +
    ggplot2::labs(x = "Estimated Values", y = "Residuals") +
    ggplot2::theme_bw()

  # Generate scale-location plot
  p4 <- ggplot2::ggplot(residualData,
                        ggplot2::aes(x = yHat, y = sqrt(abs(residuals)))) +
    ggplot2::geom_point(color = "steelblue", alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Estimated Values", y = expression(sqrt("|Residuals|")))

  # Merge plots
  patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2)
}
