.__C__linreg <-
new("refClassRepresentation", fieldClasses = list(formula = "formula", 
    data = "data.frame", reg_coef = "matrix", fitted_val = "matrix", 
    res = "matrix", dof = "integer", res_var = "matrix", var_reg_coef = "numeric", 
    t_values = "matrix", p_values = "numeric", x_matrix = "matrix", 
    y_matrix = "matrix", data_name = "character"), fieldPrototypes = <environment>, 
    refMethods = <environment>, refSuperClasses = "envRefClass", 
    slots = list(.xData = structure("environment", package = "methods")), 
    contains = list(envRefClass = new("SClassExtension", subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("envRefClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            class(from) <- "envRefClass"
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            for (what in ".xData") slot(from, what) <- slot(value, 
                what)
            from
        }, simple = TRUE, by = character(0), dataPart = FALSE, 
        distance = 1), .environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure(".environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            {
                class(from) <- ".environment"
                from
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, ".environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), refClass = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refClass") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- as(from, "envRefClass", strict = strict)
            {
                from <- as(from, ".environment", strict = strict)
                from@.xData
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "environment") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = FALSE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3), refObject = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refObject", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- as(from, "envRefClass", TRUE)
            as(.value, "refObject") <- value
            value <- .value
            {
                for (what in ".xData") slot(from, what) <- slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3)), virtual = FALSE, prototype = <S4 object of class NULL>, 
    validity = NULL, access = list(), className = structure("linreg", package = ".GlobalEnv"), 
    package = ".GlobalEnv", subclasses = list(), versionKey = <pointer: 0x00007ff95c885000>, 
    sealed = FALSE)
.__global__ <-
c("formula", "data", "reg_coef", "fitted_val", "res", "dof", 
"res_var", "var_reg_coef", "t_values", "p_values", "x_matrix", 
"y_matrix", "data_name", "plot", "summary", "coef", "pred", "resid", 
"show#envRefClass", "print", "initialize", "field", "trace", 
"getRefClass", "initFields", "copy", "callSuper", ".objectPackage", 
"export", "untrace", "getClass", "show", "usingMethods", ".objectParent", 
"import", ".self")
.requireCachedGenerics <-
structure(list("$", "$<-"), package = c("base", "base"))
