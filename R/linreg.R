
#' Linear Regression Model with Linear Algebra
#' @name linreg
#' @description This function do the linear regression
#' @param formula a linear regression model
#' @param data a dataframe
#' @return returns a RC class
#' @export
#' @importFrom ggplot2 ggplot aes theme geom_point stat_summary labs theme_bw
#' @importFrom methods new
#' @importFrom gridExtra grid.arrange


library(ggplot2)
library(gridExtra)
linreg <- setRefClass("linreg",
                      fields = list(formula = "formula",
                                    data = "data.frame",
                                    reg_coef = "matrix",
                                    fitted_val = "matrix",
                                    res = "matrix", 
                                    dof = "integer",
                                    res_var = "matrix",
                                    var_reg_coef = "numeric",
                                    t_values = "matrix",
                                    p_values = "numeric",
                                    x_matrix = "matrix",
                                    y_matrix = "matrix",
                                    data_name = "character"),
                      
                      methods = list(
                        initialize = function(formula, data){
                          
                          #Set the instance variables
                          .self$formula <<- formula
                          .self$data <<- data
                          
                          #Dataset name (Used in custom print function)
                          .self$data_name <<-  deparse(substitute(data))
                          
                          #CREATE MATRIX FROM DATAFRAME
                          x_matrix <<- model.matrix(formula, data)
                          #Extract the dependent variable
                          y_data <<- data.frame(iris[, (all.vars(.self$formula)[1])])
                          y_matrix <<- as.matrix(y_data)
                          colnames(y_matrix) <<- all.vars(formula)[1]
                          
                          #NECESSARY STATISTICS
                          #Regressions coefficients:
                          .self$reg_coef <<- solve(t(x_matrix) %*% x_matrix) %*% t(x_matrix) %*% y_matrix
                          #The fitted values
                          .self$fitted_val <<- x_matrix %*% reg_coef
                          #The residuals:
                          .self$res <<- y_matrix - fitted_val
                          #The degrees of freedom:
                          .self$dof <<- nrow(iris) - length(reg_coef)
                          #The residual variance:
                          .self$res_var <<- (t(res) %*% res) / dof
                          #The variance of the regression coefficients:
                          sigma_hat <- sqrt(sum(res^2) / dof)
                          .self$var_reg_coef <<- (sigma_hat^2) * diag(solve(t(x_matrix) %*% x_matrix))
                          #The t-values for each coefficient:
                          .self$t_values <<-  reg_coef / sqrt(var_reg_coef)
                          #p-value
                          p_values <- c()
                          for (i in 1:length(t_values)) {
                            p_values[i] <- 2*pt(-abs(t_values[i]), df = dof, lower.tail = TRUE)
                          }
                          .self$p_values <<- p_values
                        },
                        
                        print = function(.self){
                          
                          cat("\nCall:\n")
                          cat("linreg(formula = ", deparse(.self$formula), ", data = ", .self$data_name,")\n", sep = "")
                          output_obj = t(.self$reg_coef)
                          cat("\nCoefficients:\n")
                          print.default(output_obj[1,])
                          #return()
                        },
                        show = function(.self){
                          
                          cat("\nCall:\n")
                          cat("linreg(formula = ", deparse(.self$formula), ", data = ", .self$data_name,")\n", sep = "")
                          output_obj = t(.self$reg_coef)
                          cat("\nCoefficients:\n")
                          print.default(output_obj[1,])
                          #return()
                        },
                        resid = function(){
                          return(.self$res)
                        },  
                        pred = function(){
                          return(.self$fitted_val)
                        },
                        coef = function(){
                          names <- rownames(.self$reg_coef)
                          values <- c(.self$reg_coef)
                          named_vector <- setNames(values, names)
                          return(named_vector)
                        },
                        summary = function(.self){
                          formula_to_char <- as.character(.self$formula)
                          y_data <- data.frame(iris[, (all.vars(.self$formula)[1])])
                          y_matrix <- as.matrix(y_data)
                          res_std_err <- sum(.self$res^2) / (length(y_matrix) - length(.self$reg_coef) - 1)
                          
                          # --- P-values handling method shown during Seminar session on 29/09 by group 12 ---
                          temp_p_vals <- .self$p_values
                          small <- ifelse(temp_p_vals < 2e-16, '<2e-16', round(temp_p_vals,7))
                          sig_code <- .self$p_values
                          sig_code[sig_code < 0.001] <- "***"
                          sig_code[sig_code >= 0.001] <- "**"
                          sig_code[sig_code >= 0.01] <- "*"
                          sig_code[sig_code >= 0.05] <- "."
                          sig_code[sig_code >= 0.1] <- " "
                          # --- --- ---
                          
                          summary_df <- data.frame('Estimate' = round(as.vector(.self$reg_coef),7), 
                                                   'Std. Error' = round(as.vector(sqrt(.self$var_reg_coef)),5), 
                                                   't-values' = round(as.vector(.self$t_values),2), 
                                                   'p-values'= small, 
                                                   "Significance Code" = sig_code)
                          rownames(summary_df) <- rownames(.self$reg_coef)
                          colnames(summary_df) <- c(" ", " ", " ", " ", " ")
                          
                          base::print(summary_df)
                          cat("\n")
                          message <- paste("Residual standard error:", round(sqrt(res_std_err),5), "on", .self$dof, "degrees of freedom")
                          cat(message, "\n")
                          
                          
                        },
                        plot = function(theme = "none"){
                          
                          #------------------------------
                          
                          df_p1 <- data.frame(Fitted = .self$fitted_val, Residuals = .self$res)
                          #---------------PLOT 1---------------------------
                          # Create a residuals vs. fitted values plot using ggplot2
                          p1 <-ggplot(df_p1, aes(x = .self$fitted_val, y = .self$res)) +
                            geom_point() +
                            stat_summary(aes(y = .self$res, group = 1), fun=median, color ="red", geom="line", group=1) +
                            labs(x = "Fitted Values", y = "Residuals", title = "Residuals vs. Fitted Values Plot")
                          
                          #--------------PLOT 2---------------------------
                          standardized_residuals <- .self$res %*% (1 / sqrt(.self$res_var))
                          y_val <- sqrt(abs(standardized_residuals))
                          df_p2 <- data.frame(Fitted = .self$fitted_val, StdRes = y_val)
                          colnames(df_p2) <- c("Fitted", "StdRes")
                          
                          # Create a residuals vs. fitted values plot using ggplot2
                          p2 <- ggplot(df_p2, aes(x = .self$fitted_val, y = y_val)) +
                            geom_point() +
                            stat_summary(aes(y = y_val, group = 1), fun=mean, color ="red", geom="line", group=1) +
                            labs(x = "Fitted Values", y = "Standardized Residuals", title = "Scale-Location")
                          #to show abs and sqrt in axis title, use expression()
                          
                          #-----Theme selection---------
                          if(theme == "liu_light"){
                            p1 <- p1 + ggplot2::theme(
                              text = element_text(size = 12),
                              axis.text = element_text(size = 10, color = "black"),
                              axis.title = element_text(size = 12, color = "black"),
                              plot.title = element_text(size = 14, face = "bold"),
                              plot.background = element_rect(fill = "lightblue"),
                              panel.background = element_rect(fill = "white"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              legend.background = element_rect(fill = "lightblue"),
                              legend.text = element_text(size = 10),
                              legend.title = element_text(size = 12)
                            ) 
                            p2 <- p2 + ggplot2::theme(
                              text = element_text(size = 12),
                              axis.text = element_text(size = 10, color = "black"),
                              axis.title = element_text(size = 12, color = "black"),
                              plot.title = element_text(size = 14, face = "bold"),
                              plot.background = element_rect(fill = "lightblue"),
                              panel.background = element_rect(fill = "white"),
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              legend.background = element_rect(fill = "lightblue"),
                              legend.text = element_text(size = 10),
                              legend.title = element_text(size = 12)
                            )
                          }else {
                            p1 <- p1 + theme_bw()
                            p2 <- p2 + theme_bw()
                            #return(list(p1,p2))
                          }
                          gridExtra::grid.arrange(p1, p2, ncol=2)
                        }
                      )
)