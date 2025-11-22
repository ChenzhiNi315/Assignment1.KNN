# Assignment1KNN

[![R-CMD-check](https://github.com/ChenzhiNi315/Assignment1.KNN/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ChenzhiNi315/Assignment1.KNN/actions/workflows/R-CMD-check.yaml)

## Description

**Assignment1KNN** is an R package developed for the Machine Learning course assignment. It implements a K-Nearest Neighbors (KNN) classifier to recognize handwritten digits using the `optdigits` dataset (from the UCI Machine Learning Repository).

This package allows you to:
* Split data into training, validation, and test sets.
* Train KNN models with different $K$ values.
* Evaluate models using both **Misclassification Rate** and **Cross-Entropy**.
* Visualize decision complexity, error curves, and confusion matrices.
* Identify "hard" and "easy" to classify digit examples.

## Installation

You can install the development version of this package directly from GitHub using `devtools`:

```r
# install.packages("devtools")
devtools::install_github("ChenzhiNi315/Assignment1.KNN")

## Usage Example
Here is a quick start guide to using the package functions to complete the assignment tasks:

### 1. Load Data and Split

```r
library(Assignment1KNN)

data("optdigits")

# Split into 50% Training, 25% Validation, 25% Test
sets <- split_optdigits_data(optdigits, seed = 12345)
train_data <- sets$train_data
valid_data <- sets$valid_data
test_data  <- sets$test_data
```

### 2. Evaluate a Basic Model (K=30)

```r
# Train K=30 model and get confusion matrices
results_k30 <- evaluate_k_30(train_data, test_data)

# View error rates
print(results_k30$error_test)

# Visualize the Confusion Matrix
plot_confusion_matrix(results_k30$cm_test, title = "Test Set (K=30)")
```

### 3. Find Optimal K (Misclassification vs. Cross-Entropy)

The package provides tools to find the best $K$ by comparing error rates on the validation set.
```r
# Method A: Misclassification Error
res_misclass <- find_best_k_misclassification(train_data, valid_data)
plot_knn_errors(res_misclass$results_df, res_misclass$best_k)

# Method B: Cross-Entropy Error
res_ce <- find_best_k_cross_entropy(train_data, valid_data)
print(paste("Best K by Cross-Entropy:", res_ce$best_k))
```

### 4. Visualize Digits

You can visualize specific digits, such as the hardest-to-classify examples:
```r
# Find hardest '8's
examples <- find_hardest_easiest_8s(train_data, k = 30)
plot_digit(examples$hardest[1, ], title = "Hardest '8'")
```

