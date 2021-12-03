#'MLR
#'
#'@importFrom stats pf pt
#'
#'@param y a vector, outcomes.
#'
#'@param x a matrix, rows represent each observation, columns represent predictor.
#'
#'@param intercept a logical value, "T" by default, the model has intercept.
#'if it's "F", the model doesn't have intercept.
#'
#'@param y_hat a logical value, "T" by default, output the value of fitted y.
#'if it's "F", fitted value y will not be output.
#'
#'@param SSE a logical value, "F" by default, SSE will not be output
#'if it's "T", output the value of SSE.
#'
#'@param SSY a logical value, "F" by default, SSY will not be output
#'if it's "T", output the value of SSY.
#'
#'@param r_square a logical value, "T" by default, output the value of r square.
#'if it's "F", r square will not be output.
#'
#'@param adjusted_r_square a logical value, "F" by default, adjusted r square will not be output.
#'if it's "T", output the value of adjusted r square.
#'
#'@param t_test a logical value, "F" by default, the results of t-test and p-values will not be output.
#'if it's "T", output the results of t-test and p-values.
#'
#'@param f_test a logical value, "T" by default,output the results of f-test and p-values.
#'if it's "F", the results of t-test and f-values will not be output
#'
#'@param hat_matrix a logical value, "F" by default, the results of hat matrix will not be output.
#'if it's "T", output the results hat matrix.
#'
#'@return the result list
#'
#'@examples
#'MLR(swiss_feritility, swiss_predictors)
#'
#'@export
#'



MLR = function(y,
               x,
               intercept = T,
               y_hat = T,
               SSE = F,
               SSY = F,
               r_square = F,
               adjusted_r_square = F,
               t_test = F,
               f_test = T,
               hat_matrix = F){

  # build the covariates matrix
  x <- as.matrix(x)
  # build the outcomes matrix
  y <- as.matrix(y)
  # find the number of predictors
  p <- ncol(x)
  # find the number of sample size
  n <- nrow(x)

  ## Name the columns without predictor name
    if (length(colnames(x)) == 0){
    predictor_name <- character(p)
    for (i in 1:p) {
      predictor_name[i] <- paste0("predictor_", i, collapse = "")
      colnames(x) <- predictor_name
    }
    }
  if (length(colnames(y)) == 0){
    colnames(y) <- "outcomes"
  }


  #if the model have intercept, add the intercept column
  if (intercept == T) {
    x <- cbind(intercept = rep(1, n), x)
    p <- p + 1
  }

  #create result list
  beta_hat <- solve(t(x) %*% x) %*% t(x) %*% y
  result <- list(beta_hat = beta_hat)
  fitted_value <- x %*% beta_hat
  residuals <- y-fitted_value

  #find the y_hat, the fitted value of y
  if (y_hat == T){
    result$y_hat <- fitted_value
  }

  #find SSE
  if(SSE ==T){
    SSE <- sum(residuals ^ 2)
    result$SSE <- SSE
  }

  #find SSY
  if(SSY == T){
    if (intercept == T) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y ^ 2)
    }
    result$SSY <- SSY
  }

  #find r_square
  if (r_square == T) {
    if (intercept == T) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y^2)
    }
    SSE <- sum(residuals^2)
    r_square <- 1 - SSE/SSY
    result$r_square <- r_square
  }

  #find adjusted_r_square
  if (adjusted_r_square == T) {
    if (intercept == T) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y^2)
    }
    SSE <- sum(residuals^2)
    r_square <- 1 - SSE/SSY
    adjusted_r_square <- 1 - (SSE/(n - p))/(SSY/(n - intercept))
    result$adjusted_r_square  <- adjusted_r_square
  }


  #find t test statistics, df, and p_values
  if (t_test == T) {
    MSE = sum(residuals^2)/(n - p)
    beta_hat_var = solve(t(x) %*% x) * MSE
    t_statistics = (beta_hat) / sqrt(beta_hat_var[(p + 1) * (1:p) - p])
    p_values = 2 * pt(-abs(t_statistics), n - p)
    t_test = list(t_statistics = t_statistics,
                  p_values = p_values,
                  df = n - p)
    result$t_test = t_test
  }

  #find f_statistics, df, and p_value
  if (f_test == T) {
    if (intercept == T) {
      y_bar <- sum(y) / n
      SSY <- sum((y - y_bar) ^ 2)
    } else {  # if the model don't have intercept
      SSY <- sum(y^2)
    }
    SSE <- sum(residuals^2)
    r_square <- 1 - SSE/SSY

    f_statistics = (r_square) / (1 - r_square) * ((n-p)/(p-intercept))
    p_value = 1 - pf(f_statistics, p - intercept, n - p)
    f_test = list(
      f_statistics = f_statistics,
      p_value = p_value,
      df = c(p - intercept, n - p)
    )
    result$f_test = f_test
  }

  #find the hat matrix
  if (hat_matrix == T) {
    hat_matrix = x %*% solve(t(x) %*% x) %*% t(x)
    result$hat_matrix = hat_matrix
  }

  #the return step
  return(result)
}
