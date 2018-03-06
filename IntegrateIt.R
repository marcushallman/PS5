#' Integrating a function
#'
#' Finds sum of trapezoids or area under parabolas under a curve.
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param rule A character string equal to either "Trap" or "Simpson"
#'
#' @return An object of class Trapezoid or class Simpson containing
#'  \item{x}{The first object input}
#'  \item{y}{The second object input} 
#'  \item{area}{The summed area under the curve}
#' @author Marcus Hallman
#' @note Both methods will give a different answer, because they imperfectly estimate the area 
#' @examples
#' 
#' myX <- c(1, 2) 
#' myY <- c(2, 4) 
#' integrateIt(myX, myY, "Trap")
#' @rdname integrateIt
#' @aliases integrateIt
#' @export
setGeneric(name = "integrateIt",
           def = function(x,y,rule,...){
             standardGeneric("integrateIt")
           })

#' @export
setMethod("integrateIt",
          definition = function(x, y, rule, ...){
            # ensure that x is a numeric
            if (!is.numeric(x)){
              stop ("x must be a numeric vector")
            }
            # ensure that y is a numeric
            if (!is.numeric(y)){
              stop ("y must be a numeric vector")
            }
            # ensure that x and y have the same length
            if (length(x) != length(y)){
              stop ("x and y must be of the same length")
            }
            # ensure that rule is either Trapezoid or Simpson
            if (rule != "Trapezoid" & rule != "Simpson"){
              stop ("rule must be either 'Trapezoid' or 'Simpson'")
            }
    
            # set up uniform length of intervals
            a <- min(x)
            b <- max(x)
            h <- (b-a)/(length(x)-1)
            x_sorted <- sort(x)
            # ensure uniform length of intervals
            for (i in 2:length(x)){
              if (x[i] - h != x[i-1]){
                stop ("must have uniform interval lengths in x")
              }
            }
            # Calculate Trapezoid area
            if (rule=="Trap"){
              areaSum <- sum(2*y)-y[1]-y[length(y)]
              area <- h*areaSum/2
              return (new("trapezoid", x=x, y=y, area=area))
            } else if (rule=="Simpson"){ # Calculate Simpson area
              area_sum <- 0
              for (i in 1:length(y)){
                if (i %% 2 == 0){
                  areaSum <- areaSum+4*y[i]
                } else {
                  areaSum <- areaSum+2*y[i]
                }
              }
              areaSum <- areaSum-y[1]-y[length(y)]
              area <- areaSum*h/3
              return (new("simpson", x=x, y=y, area=area))
            }
          })