# R/knn_assignment_functions.R

utils::globalVariables(c("y"))
#' Split the Optdigits Dataset
#' (Task 1)
#'
#' @param data Data frame.
#' @param seed Random seed.
#' @return List with train, valid, and test sets.
#' @export
split_optdigits_data <- function(data, seed = 12345) {
  set.seed(seed)
  n <- nrow(data)
  n_train <- floor(0.5 * n)
  n_valid <- floor(0.25 * n)

  indices <- sample(1:n)
  train_data <- data[indices[1:n_train], ]
  valid_data <- data[indices[(n_train + 1):(n_train + n_valid)], ]
  test_data <- data[indices[(n_train + n_valid + 1):n], ]

  cat("--- Data Splitting Results ---\n")
  cat(paste("Training set size:", nrow(train_data), "\n"))
  cat(paste("Validation set size:", nrow(valid_data), "\n"))
  cat(paste("Test set size:", nrow(test_data), "\n"))
  cat("------------------------------\n\n")

  return(list(train_data = train_data, valid_data = valid_data, test_data = test_data))
}

#' Evaluate a K=30 Classifier
#' (Task 2)
#'
#' @param train_data Training data.
#' @param test_data Test data.
#' @return List of confusion matrices and errors.
#' @importFrom kknn kknn
#' @importFrom stats fitted
#' @export
evaluate_k_30 <- function(train_data, test_data) {
  # Train & predict on training set
  fit_train <- kknn::kknn(Digit ~ ., train_data, train_data, k = 30, kernel = "rectangular")
  pred_train <- stats::fitted(fit_train)
  cm_train <- table(Actual = train_data$Digit, Predicted = pred_train)
  error_train <- sum(pred_train != train_data$Digit) / nrow(train_data)

  # Train & predict on test set
  fit_test <- kknn::kknn(Digit ~ ., train_data, test_data, k = 30, kernel = "rectangular")
  pred_test <- stats::fitted(fit_test)
  cm_test <- table(Actual = test_data$Digit, Predicted = pred_test)
  error_test <- sum(pred_test != test_data$Digit) / nrow(test_data)

  return(list(
    cm_train = cm_train,
    error_train = error_train,
    cm_test = cm_test,
    error_test = error_test
  ))
}

#' Find Hardest and Easiest Digit Examples
#' (Task 3)
#'
#' @param train_data Training data.
#' @param k Number of neighbors (default 30).
#' @return List with hardest (3) and easiest (2) examples.
#' @importFrom kknn kknn
#' @importFrom utils head tail
#' @export
find_hardest_easiest_8s <- function(train_data, k = 30) {
  indices_8 <- which(train_data$Digit == "8")
  train_8s <- train_data[indices_8, ]

  # Train on specific digit subset
  model_8s <- kknn::kknn(Digit ~ ., train_data, train_8s, k = k)

  # Get probabilities for class "8"
  prob_of_8 <- model_8s$prob[, "8"]

  results_8s <- data.frame(
    original_index = indices_8,
    prob_of_8 = prob_of_8
  )

  # Sort: lowest prob = hardest, highest = easiest
  results_8s_sorted <- results_8s[order(results_8s$prob_of_8), ]

  hardest_indices <- head(results_8s_sorted$original_index, 3)
  easiest_indices <- tail(results_8s_sorted$original_index, 2)

  return(list(
    hardest = train_data[hardest_indices, ],
    easiest = train_data[easiest_indices, ]
  ))
}

#' Plot a Single Digit
#' (Task 3 Helper)
#'
#' @param digit_row Single row from data.
#' @param title Plot title.
#' @return ggplot object.
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradientn scale_y_reverse labs theme_void coord_fixed theme element_text
#' @export
plot_digit <- function(digit_row, title = "") {
  pixel_vector <- as.numeric(digit_row[1:64])
  digit_matrix <- matrix(pixel_vector, nrow = 8, ncol = 8, byrow = TRUE)
  digit_df <- as.data.frame(digit_matrix)
  digit_df$y <- 1:8

  digit_long_df <- tidyr::pivot_longer(digit_df, cols = -y, names_to = "x", values_to = "intensity")
  digit_long_df$x <- as.integer(gsub("V", "", digit_long_df$x))

  plot <- ggplot(digit_long_df, aes(x = .data$x, y = .data$y, fill = .data$intensity)) +
    geom_tile(color = "black") +
    scale_fill_gradientn(colors = c("white", "lightyellow", "darkred"), limits = c(0, 16)) +
    scale_y_reverse() +
    coord_fixed() +
    theme_void() +
    labs(title = title) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))

  return(plot)
}

#' Find Optimal K (Misclassification)
#' (Task 4)
#'
#' @param train_data Training data.
#' @param valid_data Validation data.
#' @param k_values Vector of K values.
#' @return List with results data frame and best K.
#' @importFrom kknn kknn
#' @importFrom stats fitted
#' @export
find_best_k_misclassification <- function(train_data, valid_data, k_values = 1:30) {
  results <- data.frame(
    K = k_values,
    train_error = numeric(length(k_values)),
    valid_error = numeric(length(k_values))
  )

  for (k in k_values) {
    fit_train <- kknn::kknn(Digit ~ ., train_data, train_data, k = k, kernel = "rectangular")
    pred_train <- stats::fitted(fit_train)
    results$train_error[k] <- sum(pred_train != train_data$Digit) / nrow(train_data)

    fit_valid <- kknn::kknn(Digit ~ ., train_data, valid_data, k = k, kernel = "rectangular")
    pred_valid <- stats::fitted(fit_valid)
    results$valid_error[k] <- sum(pred_valid != valid_data$Digit) / nrow(valid_data)
  }

  best_k <- results$K[which.min(results$valid_error)]
  return(list(results_df = results, best_k = best_k))
}

#' Plot KNN Misclassification Error Curve
#' (Task 4 Helper)
#'
#' @param k_results_df Results data frame.
#' @param best_k Optimal K value.
#' @return ggplot object.
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_minimal geom_vline annotate element_text theme scale_x_continuous margin
#' @export
plot_knn_errors <- function(k_results_df, best_k) {
  results_long <- tidyr::pivot_longer(k_results_df,
                                      cols = c("train_error", "valid_error"),
                                      names_to = "Dataset",
                                      values_to = "ErrorRate")

  min_valid_error <- min(k_results_df$valid_error)

  plot_k <- ggplot(results_long, aes(x = .data$K, y = .data$ErrorRate, color = .data$Dataset, group = .data$Dataset)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = best_k, linetype = "dashed", color = "blue", linewidth = 1) +
    annotate("text", x = best_k + 1, y = min_valid_error + 0.05,
             label = paste("Best K =", best_k), color = "brown", size = 5, hjust = 0) +
    labs(title = "K-Nearest Neighbors: Model Complexity",
         subtitle = "Finding the optimal K (Misclassification Error)",
         x = "K (Number of Neighbors)",
         y = "Misclassification Rate") +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 16, hjust = 0.5),
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10, unit = "pt"),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  return(plot_k)
}

#' Find Optimal K (Cross-Entropy)
#' (Task 5)
#'
#' @param train_data Training data.
#' @param valid_data Validation data.
#' @param k_values Vector of K values.
#' @return List with results data frame and best K.
#' @importFrom kknn kknn
#' @export
find_best_k_cross_entropy <- function(train_data, valid_data, k_values = 1:30) {
  results <- data.frame(
    K = k_values,
    valid_cross_entropy = numeric(length(k_values))
  )

  for (k in k_values) {
    fit_valid <- kknn::kknn(Digit ~ ., train_data, valid_data, k = k)

    # Extract prob of the true class for each observation
    true_class_indices <- as.integer(valid_data$Digit)
    n_valid <- nrow(valid_data)
    prob_correct_class <- fit_valid$prob[cbind(1:n_valid, true_class_indices)]

    # Add epsilon to avoid log(0) and compute NLL
    prob_correct_class_safe <- pmax(prob_correct_class, 1e-15)
    results$valid_cross_entropy[k] <- -mean(log(prob_correct_class_safe))
  }

  best_k <- results$K[which.min(results$valid_cross_entropy)]
  return(list(results_df = results, best_k = best_k))
}

#' Plot Confusion Matrix Heatmap
#' (Task 2 Helper)
#'
#' @param cm Confusion matrix.
#' @param title Plot subtitle.
#' @return ggplot object.
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient scale_y_discrete labs theme theme_minimal element_text
#' @importFrom rlang .data
#' @export
plot_confusion_matrix <- function(cm, title = "Confusion Matrix") {
  cm_df <- as.data.frame(cm)

  heatmap_plot <- ggplot(data = cm_df, aes(x = .data$Predicted, y = .data$Actual, fill = .data$Freq)) +
    geom_tile(color = "black") +
    geom_text(aes(label = ifelse(.data$Freq == 0, "", .data$Freq)), color = "black", size = 4) +
    scale_fill_gradient(low = "white", high = "steelblue", name = "Count") +
    scale_y_discrete(limits = rev) +
    labs(
      title = "Confusion Matrix Heatmap",
      subtitle = paste("Data:", title),
      x = "Predicted Digit",
      y = "Actual Digit"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 14, hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12, face = "bold"),
      legend.position = "right"
    )
  return(heatmap_plot)
}

