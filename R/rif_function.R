#'This function calculate the recentered influence function of quantile statistic
#'@export
#'@param formula formula of RIF regression
#'@param data data with varibales
#'@param method the distrubiton statistic to calculate recentered influence funciton, which
#'can be choose from "mean", "quantle", "variance","gini".
#'@param tau quantile used in method "quantile".
#'@param kernel kernel used for kernel estimating of the dependent variable, selecting
#'from "gaussian", "epanechnikov", "rectangular", "triangular", "biweight",
#'"cosine", "optcosine", used in method "quantile".
#'
#'@details The \code{rif_quantile} is a function to calculate the recenterd influence function
#'value of different distribution statistics, such as mean, quantile, variance and gini index.
#'The case of mean:
#'The influence function is:
#'\deqn{IF(y;\mu)=lim_{\epsilon \rightarrow} \frac{[(1-\epsilon)\mu+\epsilon y -\mu]}{\epsilon}
#'=y-\mu}
#'and the recentered influence function is:
#'\deqn{RIF(y;\mu)=IF(y;\mu)+\mu=y}
#'The case of quantile:
#'The influence function is:
#'The \eqn{\tau}-th quantile of the distribution \eqn{F} is defined as the functional,
#'\eqn{Q(F, \tau) = inf{y|F(y)\geqslant}\tau}, or as \eqn{q_{\tau}} for short, and its
#'influential function is:
#'\deqn{IF{y;q_{\tau}}=\frac{\tau-I{y \leqslant q_{\tau}}}{f_{Y}(q_{\tau})}}
#'The recentered influential function of the \eqn{\tau^{th}} quantile is
#'\deqn{RIF(y;q_{\tau}) = q_{\tau}+IF(y;q_{\tau})=
#'q_{\tau}+(\tau-I{y \leqslant q_{\tau}})/f_{Y}(q_{\tau)})}
#'The case of variance:
#'The influence function of the variance is well-known to be
#'\deqn{IF(y;\sigma^{2})=(y-\int{z \cdot dF_{Y}(z)})^2-\sigma^2}
#'And the recenterd influence function is:
#'\deqn{RIF(y; \sigma^2)=(y-\int{z \cdot dF_{Y}(z)})^2=(Y-\mu)^2}
#'The case of Gini:
#'The Gini coefficient is defined as:
#'\deqn{\nu^{GC}(F_{Y})=1-2\mu^{-1}R(F_{Y})}
#'where \eqn{R(F_{Y})=\int_{0}^{1} GL(p;F_{Y})dp} with \eqn{p(y)=F_{Y}(y)} and where
#'\eqn{GL(p;F_{Y})} the generalized Lorenz ordinate of \eqn{F_{Y}} is given by
#'\eqn{GL(p;F_{Y})=\int_{-Inf}{F^{-1}(p)zdF_{Y}(z)}}The generalized Lorenz curve tracks the cumulateive total of \eqn{y} divided by total population
#'size against the cumulative distribution function and the generalized Lorenz ordinate can
#'be interpreted as the proportion of earnings going to the 100p% lowest earners.
#'The influence function of the Gini coefficient is:
#'\deqn{IF(y;\nu^{GC})=A_{2}(F_{Y})+B_{2}(F_{Y})y+C_{2}(y;F_{Y})}
#'where,\eqn{A_{2}(F_{Y})=2\mu^{-1}R(F_Y)}, \eqn{B_2(F_Y)=2\mu^{-2}R(F_Y)},
#'\eqn{C_2(y;F_Y)=-2\mu^{-1}[y[1-p(y)]+GL(p(y);F_Y)]}, thus the recentered influence function of
#'Gini is:
#'\deqn{RIF(y;\nu^GC)=1+B_2(F_Y)y+C_2(y;F_Y)}
#'In estimation, the GL coordinates are computed using a series of discrete data points \eqn{y_1,
#'y_2,...y_N}, where observations have been ordered so that \eqn{y_1 \leq y_2 \leq y_3 ...\leq y_N}
#'
#'
#'@import stats
#'@author Wenjing Wang \email{Wenjingwang1990@@gmail.com}
#'@keywords RIF
#'@references
#'Firpo S, Fortin N M, Lemieux T(2009). Unconditional quantile regressions.
#'\emph{Econometrica}, 77(3): 953-973.
#'

rif_function <- function(formula, data, method, tau = NULL, kernel = NULL){
    idx.dep = which(colnames(data) == all.vars(formula)[1])
    missing_y <- which(is.na(data[, idx.dep]))
    data = data[-missing_y, ]  #delete missing value
    y <- data[, idx.dep]
    if(method == "mean"){
      RIF <- y
    }
    if(method == "quantile"){
      if(is.null(kernel)) kernel <- "gaussian"
      if(!(kernel %in% c("gaussian", "epanechnikov", "rectangular", "triangular",
                         "biweight","cosine", "optcosine"))){
        stop('kernel must be one of "gaussian", "epanechnikov", "rectangular",
              "triangular", "biweight","cosine", "optcosine"')
      }
      if(tau < 0 | tau > 1){
        stop('tau must between 0 and 1')
      }
      if(is.null(tau)) tau <- 1:9/10
      indicator <- function(condition) ifelse(condition, 1, 0) #define sign function
      q <- quantile(x = y, probs = tau)
      f <- density(y, kernel = kernel)
      fq <- approx(f$x, f$y, q)$y
      RIF <- matrix(0, ncol = length(tau), nrow = nrow(data))
      for (i in 1:length(tau)){
        RIF[,i] <- q[i] + ((tau[i] - indicator(y < q[i]))/fq[i]) #RIF function expression
      }
    }
   if(method == "variance"){
      mu <- mean(y)
      RIF <- (y - mu)^2
    }
   if(method == "gini"){
      k <- length(y)
      o <- order(y)
      y <- y[o]
      n <- nrow(data)
      p <- cumsum(n)/sum(n)
      L <- cumsum(y)/sum(y)
      p <- c(0, p)
      L <- c(0, L)
      L2 <- L * mean(y)/mean(n)
      integrand <- function(p) {p * L}
      R <- integrate(integrand, lower = 0, upper = 1)
      mu <- mean(y)
      B2 <- 2 * mu^(-2) * R
      C2 <- -2 * mu^(-1)*(y * (1 - p) + L2)
      RIF <- 1 + B2 * y + C2
    }
    RIF
}


