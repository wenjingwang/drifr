#'This function decomposite the RIF regression result
#'@export
#'@param formula formula of RIF regression
#'@param data data with varibales
#'@param flag variable denotes the decompositing group, flag is the column name of the
#'variable
#'@param idx value of the grouping variable. idx should be a vector with two dimensions.
#'for example idx = c(1, 2); idx = c(3, 5).
#'@param method the distrubiton statistic to calculate recentered influence funciton, which
#'can be choose from "mean", "quantle", "variance","gini".
#'@param tau quantile when using quantile method
#'@param kernel kernel used for kernel estimating of the dependent variable, selecting
#'from "gaussian", "epanechnikov", "rectangular", "triangular", "biweight",
#'"cosine", "optcosine". Used in quantile method
#'
#'@import oaxaca
#'@details
#'Given are two groups, A and B; an outcome variable, \eqn{Y}; and a
#'set of predictors. For example, think of a group of males and a group
#'of females, (log) wages are the outcome variable, and human capital
#'indicators such as education and work experience as predictors.
#'the distribution statistics (\eqn{\nu}) difference of outcome is,
#'\deqn{R = v(Y_A)-v(Y_B)}
#'where \eqn{v(Y)} denotes the distribution statistics of the outcome
#'variable, such as mean, quantile, variance, gini.
#'Based on the linear model
#'\deqn{RIF(Y_l) = X_{l}^{'}\beta_l+\epsilon_l, l \in (A, B)}
#'where \eqn{X} is a vector containing the predictors and a constant, \eqn{\beta}
#'contains the slope parameters and the intercept, and \eqn{\epsilon} is the error.
#'The outcome difference can be expressed as:
#'\deqn{R = v(Y_A)-v(Y_B) = E(X_A)^{'}\beta_A-E(X_B)^{'}\beta_B}
#'\deqn{R = (E(X_A)-E(X_B))^{'}\beta^{*}+
#'      (E(X_A)^{'}(\beta_A-\beta^{*})+E(X_B)^{'}(\beta{*}-\beta_B))}
#'now we have a twofold decomposition.
#'@author Wenjing Wang \email{Wenjingwang1990@@gmail.com}
#'@keywords RIF
#'@references
#'Firpo S, Fortin N M, Lemieux T(2009). Unconditional quantile regressions.
#'\emph{Econometrica}, 77(3): 953-973.
#'
#'
#'
rif_decomposite <- function(formula, data, flag, idx, method, tau = NULL, kernel = NULL){
  data1 = subset(data, data[[flag]] == idx[1])
  data2 = subset(data, data[[flag]] == idx[2])
  data1[[flag]] = 0
  data2[[flag]] = 1
  data_all <- rbind(data1, data2)
  model1 <- rif_function(formula, data = data1, method, tau = NULL, kernel = NULL)
  model2 <- rif_function(formula, data = data2, method, tau = NULL, kernel = NULL)
  rif <- rbind(model1, model2)
  colnames(rif) <- paste("rif_colname_", 1:ncol(rif), sep = "")
  data_rif <- cbind(rif, data_all)
  val <- list()
  for(i in 1:length(tau)){
    cur_col_name <- colnames(rif)[i]
    right_half <- as.character(formula)[3]
    formula_str <- sprintf("%s ~ %s | %s",cur_col_name,right_half,flag)
    oaxaca_formula <- as.formula(formula_str)
    oaxaca_rst <-  oaxaca(oaxaca_formula,
                          data = data_rif, R = 30)
    rst <- list()
    rst$overall <- oaxaca_rst$twofold$overall[6, 2:5]
    rst$variable <- oaxaca_rst$twofold$variables[[6]][, 2:5]
    rst_key <- paste("decomposite", i, sep = "")
    val[[rst_key]] <- rst
  }
  return(val)
}
