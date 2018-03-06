#' An object created with Trapezoid rule for finding area under a curve
#' 
#' Object of class \code{trapezoid} are created by the \code{integrateIt} function
#'  
#' An object of class 'Trapezoid' has the following slots:
#' \itemize{
#' \item \code{x} the x values
#' \item \code{y} the evaluated f(x) values
#' \item \code{area} estimated area under the curve
#' }
#' 
#' @author Marcus Hallman: \email{hallman@wustl.edu}
#' @aliases trapezoid initialize
#' @rdname trapezoid
#' @export
setClass(Class = "trapezoid",
         representation = representation(
           x = "numeric",
           y = "numeric",
           area = "numeric"
         ),
         prototype = prototype(
           x = c(),
           y = c(),
           area = c()
         ))

#' @export
setMethod("initialize", "trapezoid",
          function(.Object, ...){
            value = callNextMethod()
            return (value)
          })

#' @export
setMethod("print", "trapezoid",
          function(x){
            print (x @ area)
          })