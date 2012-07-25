setClass("ConstSingleRule",
         contains="SingleRule",
         representation(constantVal="numeric"),
         S3methods=TRUE)

setMethod("calculateSpecific",signature(x="ConstSingleRule", y="numeric"),
          function(x,y){
            return(x@constantVal)
          })

setClass("DivDoubleRule",contains="DoubleRule",S3methods=TRUE)

setMethod("calculateDoubleSpecific", 
          signature(x="DivDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y%/%z)
          })

setClass("ModuloDoubleRule",contains="DoubleRule",S3methods=TRUE)

setMethod("calculateDoubleSpecific", 
          signature(x="ModuloDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y%%z)
          })

setClass("ExpDoubleRule", contains="DoubleRule",S3methods=TRUE)

setMethod("calculateDoubleSpecific", 
          signature(x="ExpDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y^z)
          })


## Transfer class, so that double rules can be used as single rules
## for example if one of the arguments is a constant
setClass("Double2Single",
         representation = representation(Rule="DoubleRule", firstRule="SingleRule",
                                         secondRule = "SingleRule"),
         contains="SingleRule", S3methods = TRUE)

setMethod("calculateSpecific",signature(x="Double2Single",y="numeric"),
          function(x, y){
            firstArg <- y #previous element of the sequence
            secondArg <- y  
            
            if(!is.null(x@firstRule)){ #if there are some rules nested inside
              firstArg <- calculate(x@firstRule,firstArg) #execute first single-argument rule
            }
            if(!is.null(x@secondRule)){
              secondArg <- calculate(x@secondRule,secondArg) #execute second single-argument rule
            }
            return(calculateDouble(x@Rule,firstArg, secondArg)) #if there are no more nested rules, execute
          })

#### examples

## square the first number, ignore the second
const2 <- new("ConstSingleRule", constantVal = 2)
square2 <- new("ExpDoubleRule", secondRule = const2)
calculateDouble(square2, 3, 5) # 9
calculateDouble(square2, 3, 1) # the same

# now as Single rule
squareDouble <- new("ExpDoubleRule")
squareSingle <- new("Double2Single", Rule = squareDouble, secondRule = const2)
calculate(squareSingle, 3) #9
calculate(squareSingle, 4) #16


## modulo second number
mod <- new("ModuloDoubleRule")
calculateDouble(mod, 13,7)

## divide by 2 rounding down, i.e. integer division
div2Double <- new("DivDoubleRule", secondRule = const2)
calculateDouble(div2Double,5,7) #2
calculateDouble(div2Double,5,77) #also 2, does not depend on 2nd arg
div2Single <- new("Double2Single", Rule = div2Double)
calculate(div2Single, 5) #2
calculate(div2Single, 6) #3

