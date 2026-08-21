library(testthat)
library(civic.icarm)

# ── civic_fit — auto-detection ───────────────────────────────────────────────
test_that("civic_fit detects binary classification from factor target", {
  df <- tibble::tibble(
    y  = factor(sample(c("yes","no"), 150, replace = TRUE)),
    x1 = rnorm(150), x2 = rnorm(150)
  )
  m <- civic_fit(y ~ x1 + x2, df)
  expect_s3_class(m, "civic_model")
  expect_equal(m$task, "binary")
})

test_that("civic_fit detects multiclass from factor with 3+ levels", {
  m <- civic_fit(Species ~ ., iris)
  expect_s3_class(m, "civic_model")
  expect_equal(m$task, "multiclass")
})

test_that("civic_fit detects regression from numeric target", {
  m <- civic_fit(Sepal.Length ~ Sepal.Width + Petal.Length, iris)
  expect_s3_class(m, "civic_model")
  expect_equal(m$task, "regression")
  expect_equal(m$model, "cart")
})

test_that("civic_fit works with any data frame — mtcars regression", {
  m <- civic_fit(mpg ~ cyl + wt + hp, mtcars, model = "linear")
  expect_s3_class(m, "civic_model")
  expect_equal(m$task, "regression")
  expect_equal(m$model, "linear")
})

test_that("civic_fit stores provenance metadata", {
  m <- civic_fit(Species ~ ., iris, seed = 42L)
  expect_equal(m$seed, 42L)
  expect_equal(m$n_train, 150L)
  expect_true(nchar(m$data_hash) > 10L)
  expect_true(inherits(m$trained_at, "POSIXct"))
})

test_that("civic_fit rejects unknown model for task", {
  expect_error(
    civic_fit(Species ~ ., iris, model = "logistic"),
    regexp = "not available"
  )
})

# ── predict.civic_model ──────────────────────────────────────────────────────
test_that("predict returns factor for binary classification", {
  df <- tibble::tibble(
    y  = factor(sample(c("A","B"), 100, replace = TRUE)),
    x1 = rnorm(100)
  )
  m    <- civic_fit(y ~ x1, df)
  yhat <- predict(m, df, type = "class")
  expect_true(is.factor(yhat))
  expect_true(all(yhat %in% c("A","B")))
})

test_that("predict returns probability matrix for binary", {
  df <- tibble::tibble(
    y  = factor(sample(c("A","B"), 100, replace = TRUE)),
    x1 = rnorm(100)
  )
  m    <- civic_fit(y ~ x1, df)
  prob <- predict(m, df, type = "prob")
  expect_true(is.matrix(prob))
  expect_equal(ncol(prob), 2L)
  expect_true(all(prob >= 0 & prob <= 1))
})

test_that("predict returns numeric for regression", {
  m    <- civic_fit(Sepal.Length ~ ., iris)
  yhat <- predict(m, iris)
  expect_true(is.numeric(yhat))
  expect_equal(length(yhat), 150L)
})

test_that("predict works for multiclass CART", {
  m    <- civic_fit(Species ~ ., iris)
  yhat <- predict(m, iris, type = "class")
  expect_true(is.factor(yhat))
  expect_true(all(levels(yhat) %in% levels(iris$Species)))
})

# ── civic_split ──────────────────────────────────────────────────────────────
test_that("civic_split returns correct proportions", {
  sp <- civic_split(iris, prop = 0.8, seed = 1L)
  expect_equal(nrow(sp$train) + nrow(sp$test), 150L)
  expect_true(nrow(sp$train) >= 115L && nrow(sp$train) <= 125L)
})

test_that("civic_split stratification works for numeric", {
  sp <- civic_split(mtcars, prop = 0.75, stratify = "mpg")
  expect_equal(nrow(sp$train) + nrow(sp$test), 32L)
})

# ── civic_metrics ────────────────────────────────────────────────────────────
test_that("civic_metrics binary classification returns named vector", {
  y    <- factor(c("yes","no","yes","yes","no"))
  yhat <- factor(c("yes","no","no","yes","no"))
  m    <- civic_metrics(y, yhat, positive = "yes")
  expect_named(m, c("accuracy","balanced_acc","f1",
                     "precision","recall","specificity"))
  expect_true(all(m >= 0 & m <= 1))
})

test_that("civic_metrics regression returns mae/rmse/r2", {
  y    <- rnorm(100, 50, 10)
  yhat <- y + rnorm(100, 0, 5)
  m    <- civic_metrics(y, yhat)
  expect_named(m, c("mae","rmse","r2"))
  expect_true(m["mae"] > 0)
})

test_that("civic_metrics multiclass returns macro metrics", {
  m <- civic_fit(Species ~ ., iris)
  yhat <- predict(m, iris, type = "class")
  met  <- civic_metrics(iris$Species, yhat)
  expect_true("accuracy" %in% names(met))
  expect_true("f1" %in% names(met))
})

# ── civic_explain ────────────────────────────────────────────────────────────
test_that("civic_explain returns importance for CART", {
  m  <- civic_fit(Species ~ ., iris)
  ex <- civic_explain(m)
  expect_s3_class(ex, "civic_explainer")
  expect_false(is.null(ex$importance))
  expect_true("feature" %in% names(ex$importance))
})

test_that("civic_explain returns importance for linear regression", {
  m  <- civic_fit(mpg ~ cyl + wt + hp, mtcars, model = "linear")
  ex <- civic_explain(m)
  expect_false(is.null(ex$importance))
  expect_equal(ex$importance_method, "abs_coefficient")
})

# ── civic_fairness ───────────────────────────────────────────────────────────
test_that("civic_fairness returns civic_fairness for binary", {
  m    <- civic_fit(voted ~ age + education, civic_voting)
  fair <- civic_fairness(m, civic_voting, "voted", "gender", "yes")
  expect_s3_class(fair, "civic_fairness")
  expect_true("tpr" %in% names(fair))
  expect_true("dp_ratio" %in% names(fair))
  expect_equal(nrow(fair), length(unique(civic_voting$gender)))
})

test_that("civic_fairness works for regression", {
  m <- civic_fit(Sepal.Length ~ Sepal.Width + Petal.Length, iris)
  f <- civic_fairness(m, iris, "Sepal.Length", "Species")
  expect_s3_class(f, "civic_fairness")
  expect_true("mae" %in% names(f))
  expect_equal(nrow(f), 3L)
})

test_that("civic_fairness works for multiclass", {
  m <- civic_fit(Species ~ ., iris)
  df <- iris; df$grp <- factor(ifelse(df$Sepal.Length > 5.8,"long","short"))
  f <- civic_fairness(m, df, "Species", "grp")
  expect_s3_class(f, "civic_fairness")
  expect_true("acc" %in% names(f))
})

# ── civic_calibrate ──────────────────────────────────────────────────────────
test_that("civic_calibrate returns civic_calibration", {
  m   <- civic_fit(voted ~ age + education, civic_voting)
  cal <- civic_calibrate(m, civic_voting, "voted", "yes")
  expect_s3_class(cal, "civic_calibration")
  expect_true(cal$brier_score >= 0 && cal$brier_score <= 1)
  expect_true(cal$ece >= 0)
})

# ── civic_compare ────────────────────────────────────────────────────────────
test_that("civic_compare returns civic_comparison", {
  sp <- civic_split(iris, stratify = "Species")
  m1 <- civic_fit(Species ~ ., sp$train, model = "cart")
  m2 <- civic_fit(Species ~ ., sp$train, model = "cart", seed = 99L)
  cmp <- civic_compare(list(M1 = m1, M2 = m2),
                        sp$test, outcome = "Species")
  expect_s3_class(cmp, "civic_comparison")
  expect_equal(nrow(cmp), 2L)
  expect_true("accuracy" %in% names(cmp))
})

# ── civic_audit ──────────────────────────────────────────────────────────────
test_that("civic_audit returns valid JSON", {
  m     <- civic_fit(voted ~ age + education, civic_voting)
  trail <- civic_audit(m, analyst = "tester", notes = "unit test")
  expect_type(trail, "character")
  parsed <- jsonlite::fromJSON(trail)
  expect_equal(parsed$analyst, "tester")
  expect_equal(parsed$task, "binary")
  expect_equal(parsed$model, "cart")
})

# ── Plots ────────────────────────────────────────────────────────────────────
test_that("civic_plot_importance returns ggplot", {
  m  <- civic_fit(Species ~ ., iris)
  ex <- civic_explain(m)
  p  <- civic_plot_importance(ex)
  expect_s3_class(p, "ggplot")
})

test_that("civic_plot_fairness returns ggplot", {
  m    <- civic_fit(voted ~ age + education, civic_voting)
  fair <- civic_fairness(m, civic_voting, "voted", "gender", "yes")
  p    <- civic_plot_fairness(fair, metric = "tpr")
  expect_s3_class(p, "ggplot")
})

test_that("civic_plot_confusion returns ggplot", {
  y <- factor(c("a","b","a","b","a"))
  p <- civic_plot_confusion(y, y)
  expect_s3_class(p, "ggplot")
})
