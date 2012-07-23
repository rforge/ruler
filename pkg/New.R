#-------------------------------------------------------------------------------------------
#------------------------------SingleRules--------------------------------------------------
#-------------------------------------------------------------------------------------------

#VIRTUAL CLASS FOR RULES OPERATING ON SINGLE ARGUMENTS

setClass("SingleRule",  
           representation = representation(previousRule="SingleRule"),
           S3methods=TRUE)


calculateSpecific <- function(x,y){
  return(y)
}


setMethod("calculateSpecific",signature(x="SingleRule", y="numeric"),
          function(x,y){
            return(y)
          })



#[1] RULE 1 - ADDING A CONSTANT 

setClass("AddConstSingleRule",
         contains="SingleRule",
         representation(constantVal="numeric"),
         S3methods=TRUE)

setMethod("calculateSpecific",signature(x="AddConstSingleRule", y="numeric"),
          function(x,y){
            return(x@constantVal+y)
          })


#[2] RULE 2 - MULTIPLYING BY A CONSTANT

setClass("MultConstSingleRule",
         contains="SingleRule",
         representation(constantVal="numeric"),
         S3methods=TRUE)

setMethod("calculateSpecific",signature(x="MultConstSingleRule", y="numeric"),
          function(x,y){
            return(x@constantVal*y)
          })

#[3] SUBSTRACTING A CONSTANT

setClass("SubsConstSingleRule",
         contains="SingleRule",
         representation(constantVal="numeric"),
         S3methods=TRUE)

setMethod("calculateSpecific",signature(x="SubsConstSingleRule",y="numeric"),
          function(x,y){
            return(y-x@constantVal)
          })

#[4] DIGITSUM

setClass("DigSumSingleRule", contains="SingleRule",S3methods=TRUE)

setMethod("calculateSpecific",signature(x="DigSumSingleRule",y="numeric"),
            function(x,y){
              return(sum(digits(y)))
            })




#EXECUTING RULES REFERING TO SINGLE ARGUMENT


calculate <- function(x,y){
  return(y)
}


setMethod("calculate",signature(x="SingleRule", y="numeric"), #both [1] and [2] inherit from class 'SingleRule'
          function(x, y){
            result<-y 
            if(!is.null(x@previousRule)){ # if there are some rules nested inside 'x'
              result <- calculate(x@previousRule,result) 
            }
            return(calculateSpecific(x,result)) # if there are no more nested functions, execute
          })

#-------------------------------------------------------------------------------------------
#------------------------------------------------DoubleRules--------------------------------
#-------------------------------------------------------------------------------------------
#VIRTUAL CLASS FOR RULES OPERATING ON TWO ARGUMENTS

setClass("DoubleRule", representation = representation(firstRule="SingleRule", secondRule="SingleRule"),
         S3methods=TRUE)

calculateDoubleSpecific <- function(x,y,z){stop ("No method to calculate it.")} #throw a mistake


#[1] ADD TWO PREVIOUS EXPRESSIONS

setClass("AddDoubleRule", contains="DoubleRule",S3methods=TRUE)

setMethod("calculateDoubleSpecific",signature(x="AddDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y+z)
          })

#[2] MULTIPLY TWO PREVIOUS EXPRESSIONS 

setClass("MultDoubleRule",contains="DoubleRule",S3methods=TRUE)


setMethod("calculateDoubleSpecific",signature(x="MultDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y*z)
          })

#[3] SUBSTRACT TWO PREVIOUS EXPRESSIONS

setClass("SubsDoubleRule",contains="DoubleRule",S3methods=TRUE)


setMethod("calculateDoubleSpecific",signature(x="SubsDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y-z)
          })






#EXECUTING RULES OPERATING ON TWO ARGUMENTS

calculateDouble <- function(x,y,z){stop ("No function to execute this.")} #throw a mistake, because you should execute just single functions contained by cladd DoubleRule


setMethod("calculateDouble",signature(x="DoubleRule", y="numeric", z="numeric"),
          function(x, y, z){
            firstArg <- y #first element of the sequence
            secondArg <-z #second element of the sequence 
            
            if(!is.null(x@firstRule)){ #if there are some rules nested inside
              firstArg <- calculate(x@firstRule,firstArg) #execute first single-argument rule
            }
            if(!is.null(x@secondRule)){
              secondArg <- calculate(x@secondRule,secondArg) #execute second single-argument rule
            }
            
            return(calculateDoubleSpecific(x,firstArg, secondArg)) #if there are no more nested rules, execute
          })

#-------------------------------------------------------------------------------------------
#--------------------------- examples ------------------------------------------------------
#-------------------------------------------------------------------------------------------

p<-new("AddConstSingleRule", constantVal=6)
q<-new("MultConstSingleRule", constantVal=10, previousRule=p)

calculate(p,4)# 4+6=10
calculate(q,4)# (4+6)*10=100


s<-new("MultDoubleRule", firstRule=p) 
r<-new("AddDoubleRule")

calculateDouble(s,2,2)# (2+6)*2=16
calculateDouble(r,3,2)# 3+2=5

k<-new("SubsConstSingleRule",constantVal=1)
calculate(k,12) #12-1=11

m<-new("SubsDoubleRule", firstRule=k)
calculateDouble(m,10,12) #(10-1)-12=-3

n<-new("SubsDoubleRule")
calculateDouble(n,10,12) #10-12=-2


jjj<-new("AddDigSumDoubleRule")
calculateDouble(jjj,12,13) # (1+2)+13=16


p<-new("DigSumSingleRule")
g<-new("AddDoubleRule", firstRule=p)
calculateDouble(g,12,34) #adding a digitsum to the second word



#-------------------------------------------------------------------------------------------
#----------------------------------generating sequences-------------------------------------
#-------------------------------------------------------------------------------------------

#n - how long should be a sequence
#rule

sequence<-function(rule,n,a){
                            if(class(rule)=="SingleRule"){
                              
                                                          }
                            
                            if(class(rule)=="DoubleleRule"){
                              
                                                            }
                            
                            }
