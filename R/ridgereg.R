#' Ridge Regression
#'
#' @title ridgereg
#' 
#' @description ridgereg A ridge regression function
#'
#' @param name description formula formula. 
#' @param data data.frame. 
#' @param lambda numeric. 
#' @field x_matrix matrix. 
#' @field normalized_x_matrix matrix. 
#' @field y_matrix matrix. 
#' @field beta_ridge matrix. 
#' @field prediction matrix. 
#' @field data_name character. 
#'
#' @return a ridgreg object
#' @export 
#'
#' @importFrom methods new

ridgereg <- setRefClass('ridgereg',
                        fields = list(formula = 'formula',
                                      data = 'data.frame',
                                      lambda = 'numeric', 
                                      x_matrix = 'matrix',
                                      normalized_x_matrix = 'matrix',
                                      y_matrix = 'matrix',
                                      beta_ridge = 'matrix',
                                      data_name = 'character'),
                        methods = list(
                          initialize = function(formula, data, lambda){
                            formula <<- formula
                            data <<- data
                            lambda <<- lambda
                            
                            # Convert data frame to matrix
                            x_matrix <<- model.matrix(formula, data)
                            means <- colMeans(x_matrix)
                            sds <- apply(x_matrix, 2, sd)
                            new <- (t(x_matrix[, -1])-means[-1])/sds[-1]
                            normalized <- t(new)
                            normalized_x_matrix <<- cbind(x_matrix[, 1], normalized)
                            
                            y_data <- data.frame(data[, (all.vars(formula)[1])])
                            y_matrix <<- as.matrix(y_data)
                            colnames(y_matrix) <<- all.vars(formula)[1]
                            
                            # Find the beta
                            I <- diag(ncol(normalized_x_matrix))
                            I[1,1] <- 0
                            parant <- (t(normalized_x_matrix) %*% normalized_x_matrix) + (lambda * I)
                            
                            beta <- solve(parant) %*% t(normalized_x_matrix) %*% y_matrix
                            scaled_beta <- t(beta)[-1] / sds[-1] # scale the terms except for intercept
                            intercept <- mean(y_matrix) - sum(scaled_beta * colMeans(x_matrix[,-1])) #find the intercept
                            beta_ridge <<- cbind(intercept, as.matrix(t(scaled_beta)))
                            colnames(beta_ridge)[1] <<- ""
                            rownames(beta_ridge) <<- ""
                            # Find the predictions
                            #prediction <<- x_matrix %*% t(beta_ridge)
                            
                            data_name <<-  deparse(substitute(data))
                          },
                          predict = function(matrix){
                            prediction <- matrix %*% t(beta_ridge)
                            return(prediction)
                          },
                          coef = function(){
                            return(beta_ridge)
                          },
                          show = function(){
                            cat("\nCall:\n")
                            cat("ridgereg(formula = ", deparse(formula), ", data = ", data_name, ", lambda = ", lambda, ")\n", sep = "")
                            cat("\nCoefficients:\n")
                            print.default(ridgereg_model$coef())
                          }
                          
                        ))

#ridgereg_model <- ridgereg$new(Petal.Length~Species, iris, 0.001)
#correct_model <- lm.ridge(Petal.Length~Species, iris)

#$coef()
#coef(correct_model)


#print.default(ridgereg_model)