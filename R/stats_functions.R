#' Chi-square function
#'
#' function to perform chi square test with following options:
#' correct=FALSE - don't apply continuity correction when computing the test statistic for 2 by 2 tables:
#' simulate.p.value = true - compute p-values by Monte Carlo simulation.
#'
#'
#' @param table - data for chi.sq to process
#'
#' @return chisq
#' @export
#'
chisq_function <- function(table) {
   chisq <- chisq.test(table, correct=FALSE, simulate.p.value = TRUE)
  return(chisq)
}

#' ggplot table function
#'
#' function to prepare contingency table data for ggplot2 use
#'
#' @param table - data for ggplot to process
#'
#' @return dt1
#' @export
#'
ggplot_prep_function <- function(table) {
  # 1. convert the data as a table
  dt <- as.table(as.matrix(table))

  # 2. Convert table into data frame so ggplot can read it
  dt1 <- as.data.frame(dt)
  return(dt1)
}


#' logistic regression function - for calculating statistics - Task 3 level 2
#'
#' logistic regression function for passing x and y parameters plus the dataset
#'
#' @param y - parameter for glm
#' @param x - parameter for glm
#' @param dataset - dataset for the function
#'
#' @return predictorvariable
#' @export
#'
glm_function<-function(y, x, dataset){
  f <- as.formula(paste(y, x, sep="~"))
  predictorvariable <- glm(formula=f, data=dataset, family = "binomial")
  return(predictorvariable)
}

#' Basic R plot function
#'
#' function for basic R plot with colours and threshold labels
#'
#' @param plot_dataset - dataset for plot
#' @param plot_title - title for plot
#' @param plot_subtitle - sub title for plot
#'
#' @return
#' @export
#'
plot_ROC_function <- function(plot_dataset, plot_title, plot_subtitle) {
  plot(plot_dataset, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
  title(plot_title, sub = plot_subtitle,
        cex.main = 2,   font.main= 4, col.main= "blue",
        cex.sub = 0.75, font.sub = 3, col.sub = "red")
}

#' specificity sensitivity function
#'
#' @param sesp_variable - sensitivity / specificity variable
#' @param predict_variable - predictor variable
#' @param threshold - threshold level
#'
#' @return sensitivity, specificity
#' @export
#'
sesp_func <- function(sesp_variable, predict_variable, threshold) {
  # sensitivity = c()
  # specificity = c()
  # confmat_train = c()
  confmat_train = table(sesp_variable, predict_variable > threshold)
  sensitivity = confmat_train[1 , 1] / sum(confmat_train[ , 1])
  specificity = confmat_train[2 , 2] / sum(confmat_train[ , 2])
  return(list(sensitivity, specificity))
}
