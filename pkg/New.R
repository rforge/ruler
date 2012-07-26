#-------------------------------------------------------------------------------------------
#------------------------------SingleRules--------------------------------------------------
#-------------------------------------------------------------------------------------------

#VIRTUAL CLASS FOR RULES OPERATING ON SINGLE ARGUMENTS

setClass("SingleRule",  
         representation = representation(previousRule="SingleRule"),
         S3methods=TRUE)


calculateSpecific <- function(x,y,z=NULL){
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
digits <- function(x) {
  if(length(x) > 1 ) {
    lapply(x, digits)
  } else {
    n <- nchar(x)
    rev( x %/% 10^seq(0, length.out=n) %% 10 )
  }
}



setClass("DigSumSingleRule", contains="SingleRule",S3methods=TRUE)

setMethod("calculateSpecific",signature(x="DigSumSingleRule",y="numeric"),
          function(x,y){
            return(sum(digits(y)))
          })

#[5] NEGATIVE 

setClass("NegativeSingleRule", contains="SingleRule",S3methods=TRUE)

setMethod("calculateSpecific",signature(x="NegatieSingleRule",y="numeric"),
          function(x,y){
            return(-y)
          })


#[4] IDENTICAL FUNCTION (input=output) used in random sequence generation
setClass("IdenSingleRule",contains="SingleRule",S3methods=TRUE)

setMethod("calculateSpecific",signature(x="IdenSingleRule",y="numeric"),
          function(x,y){
            return(y)
          })




#EXECUTING RULES REFERING TO SINGLE ARGUMENT


calculate <- function(x,y,z=NULL){
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

# firstRule - operations to be executed at the first element of the numeric sequence (it is an object of class 'SingleRule')
# secondRule - operations to be executed at the second element of the numeric sequence (it is an object of class 'SingleRule')
# nextSingle - operation to be executed at the result of function DoubleRule


setClass("DoubleRule", representation = representation(firstRule="SingleRule", secondRule="SingleRule",nextSingle="SingleRule"),
         S3methods=TRUE)



#[1] ADD TWO PREVIOUS EXPRESSIONS

setClass("AddDoubleRule", contains="DoubleRule",S3methods=TRUE)

setMethod("calculateSpecific",signature(x="AddDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y+z)
          })

#[2] MULTIPLY TWO PREVIOUS EXPRESSIONS 

setClass("MultDoubleRule",contains="DoubleRule",S3methods=TRUE)


setMethod("calculateSpecific",signature(x="MultDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y*z)
          })

#[3] SUBSTRACT TWO PREVIOUS EXPRESSIONS

setClass("SubsDoubleRule",contains="DoubleRule",S3methods=TRUE)


setMethod("calculateSpecific",signature(x="SubsDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y-z)
          })





#EXECUTING RULES OPERATING ON TWO ARGUMENTS

#calculate <- function(x,y,z){stop ("No function to execute this.")} #throw a mistake, because you should execute just single functions contained by cladd DoubleRule


setMethod("calculate",signature(x="DoubleRule", y="numeric", z="numeric"),
          function(x, y, z){
            firstArg <- y #first element of the sequence
            secondArg <-z #second element of the sequence 
            
            if(!is.null(x@firstRule)){ #if there are some rules nested inside
              firstArg <- calculate(x@firstRule,firstArg) #execute first single-argument rule
            }
            
            if(!is.null(x@secondRule)){
              secondArg <- calculate(x@secondRule,secondArg) #execute second single-argument rule
            }
            
            result<-calculateSpecific(x,firstArg, secondArg) #if there are no more nested rules, execute
            
            if(!is.null(x@nextSingle)){
              result<-calculate(x@nextSingle, result)
            }
            return(result)
            
          })

#-------------------------------------------------------------------------------------------
#--------------------------- examples ------------------------------------------------------
#-------------------------------------------------------------------------------------------

p<-new("AddConstSingleRule", constantVal=6)
calculate(p,4)# 4+6=10 

p<-new("AddConstSingleRule", constantVal=6)
q<-new("MultConstSingleRule", constantVal=10, previousRule=p)
calculate(q,4)# (4+6)*10=100


#[A]
n<-new("SubsDoubleRule")
calculate(n,10,12) #10-12=-2

#[B]
p<-new("DigSumSingleRule")
g<-new("AddDoubleRule", firstRule=p)
calculate(g,12,34) # (1+2)+34=37 // take the digitsum of the first argument and add it to the second one 

#[C]
p<-new("DigSumSingleRule")
g<-new("AddDoubleRule", secondRule=p)
calculate(g,12,34)# 12+(3+4)=19 // take the digitsum of the second argument and add it to the first one

#[D]
p<-new("DigSumSingleRule")
g<-new("AddDoubleRule", firstRule=p, secondRule=p)
calculate(g,12,34)# (1+2)+(3+4)=10 // take the digitsum of the second argument,take the digitsum of the first argument and add them up

#[E]
p<-new("DigSumSingleRule")
g<-new("AddDoubleRule", firstRule=p, secondRule=p, nextSingle=p)
calculate(g,12,34) #(1+2)+(3+4)=10, digitSum(10)=1+0=1 // take the digitsum of the second argument,take the digitsum of the first argument. Add those values up. Take the digitSum of the result. 


#OTHER EXAMPLES
r<-new("AddDoubleRule")
calculate(r,3,2)# 3+2=5 // add two arguments 

k<-new("SubsConstSingleRule",constantVal=1)
calculate(k,12) #12-1=11

k<-new("SubsConstSingleRule",constantVal=1)
m<-new("SubsDoubleRule", firstRule=k)
calculate(m,10,12) #(10-1)-12=-3 //substract 1 from the first argument and substract the second argument from the result

s<-new("MultDoubleRule", firstRule=p) 
calculate(s,2,2)# (2+6)*2=16 // multiply two arguments

p<-new("DigSumSingleRule")
s<-new("AddDoubleRule", nextSingle=p)
calculate(s,11,14) #11+14=25 and 2+5=7 // sume two arguments and take the digitsum of the result


p<-new("AddConstSingleRule", constantVal=6)
s<-new("AddDoubleRule", nextSingle=p)
calculate(s,11,14) #(11+14)+6=31 // add two arguments and add 6


#-------------------------------------------------------------------------------------------
#----------------------------------generating sequences-------------------------------------
#-------------------------------------------------------------------------------------------

# #a list of single rules 
# singleRules<-list(list(ruleName="AddConstSingleRule",argumentName=c("previousRule","constantVal"), argumentType= c("SingleRule","numeric")),
# list(ruleName="MultConstSingleRule",argumentName=c("previousRule","constantVal"), argumentType= c("SingleRule","numeric")),
# list(ruleName="SubsConstSingleRule",argumentName=c("previousRule","constantVal"), argumentType= c("SingleRule","numeric")),
# list(ruleName="DigSumSingleRule",argumentName=c("previousRule"), argumentType= c("SingleRule")),
# list(ruleName="NegativeSingleRule",argumentName=c("previousRule"), argumentType= c("SingleRule")))
# 
# 
# #a list of double rules
# doubleRules<-list(list(ruleName="AddDoubleRule",argumentName=c("firstRule","secondRule","nextSingle"), argumentType= c("SingleRule","SingleRule","SingleRule")),
# list(ruleName="MultDoubleRule",argumentName=c("firstRule","secondRule","nextSingle"), argumentType= c("SingleRule","SingleRule","SingleRule")),
# list(ruleName="SubsDoubleRule",argumentName=c("firstRule","secondRule","nextSingle"), argumentType= c("SingleRule","SingleRule","SingleRule")))
# 
# 
# 
# 
# # A FUNCTION TO CREATE A SINGLE RULE 
# # 'a' is an index of a rule on singleRules list (which rule I want to use to create a sequence) - if not specyfied it will be generated
# # 'cv' constant value (default is NULL)
# # 'comb' is telling me whether I want to combine several rules
# 
# #creating a simple singleRule object (with previous rules=NULL)
# createSR<-function(a=NULL,cv=NULL){  
#                               if(!is.null(a) && a>length(singleRules)) stop (paste("The list of SingleRules is shoreter than ",a, ".Please specify 'a' value, which is smaller than or equal to",length(singleRules)))
#                                                     
#                               
#                               
#                               if(is.null(a)) a<-sample(1:length(singleRules),1) #if 'a' is not specyfied, generate it                         
#                                                           
#                               a<-round(a)# 'a' needs to be integer                                                       
#                               
#                               if(length(singleRules[[a]]$argumentName)>1){
#                                                       if(is.null(cv)) cv<-sample(1:100,1)#if constant value is not sdpecyfied and an object needs it - generate it
#                                                       p<-new(singleRules[[a]]$ruleName,constantVal=cv)} else{
#                                                             p<-new(singleRules[[a]]$ruleName) #an object doesn't need a constant value 
#                                                                                                             }       
#                               return(p)
#                                                           
#                               }
# 
# 
# 
# 
# 
# 
# 
# 
# # A FUNCTION TO COMBINE TWO SINGLE RULES
# 
# # 'obj1' is an object of class that inherits from SingleRule
# # 'obj2' is an object of class that inherits from SingleRule. It will be set as previousRule of object 1.
# 
# combineSR<-function(obj1=NULL,obj2=NULL){
#                     if(!is.null(obj1) && !inherits(obj1,"SingleRule")) stop (paste("argument  obj1", obj1, "should be of class 'SingleRule'or inherit from it. It cannot be of class '", class(obj), "'", sep=""))
#                     if(!is.null(obj1) && !inherits(obj2,"SingleRule")) stop (paste("argument  obj2", obj2, "should be of class 'SingleRule'or inherit from it. It cannot be of class '", class(obj), "'", sep=""))
#                     
#                     if(is.null(obj2)) obj2<-createSR() #create a SingleRule object 'obj2' if it was not delivered as an argument
#                     if(is.null(obj1)) obj1<-createSR() #create a SingleRule object 'obj1'if it was not delivered as an argument
#                                             
#                     obj1@previousRule<-obj2
#                     
#                     return(obj1)
#                       }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # # A FUNCTION TO CREATE A DOUBLE RULE (a single one)
# # 
# # #'a' is an index of the rule on doubleRules list 
# # createDR<-function(a=NULL){
# #           if(!is.null(a) && a>length(doubleRules)) stop (paste("The list of doublrRules is shoreter than ",a, ".Please specify 'a' value, which is smaller than or equal to",length(doubleRules)))
# #           if(is.null(a)) {a<-sample (1:length(doubleRules),1)}
# #           a<-round(a) # 'a' needs to be an integer 
# #           
# #           p<-new(doubleRules[[a]]$ruleName)
# #           return(p)
# #                     }
# 
# 
# 
# 
# 
# 
# # A FUNCTION TO COMBINE DOUBLE RULES - it generates all parameters automatically 
# 
# #'fr' firstRule argument of an object of class doubleRule 
# #'sr' secondRule argument of an object of class doubleRule
# #'ns' nextSingle argument of an object of class doubleRule
# combineDR<-function(a=NULL,fr=NULL,sr=NULL,ns=NULL){
#                               if(!is.null(a) && a>length(doubleRules)) stop (paste("The list of doublrRules is shoreter than ",a, ".Please specify 'a' value, which is smaller than or equal to",length(doubleRules)))
#                               if(!inheritsm(fr,"singleRule") && !is.null(fr))stop(paste("'fr' argument must inherit from class singleRule"))
#                               if(!inherits(sr,"singleRule") && !is.null(sr))stop(paste("'sr' argument must inherit from class singleRule"))
#                               if(!inherits(ns,"singleRule") && !is.null(ns))stop(paste("'ns' argument must inherit from class singleRule"))
#                               
#                               
#                               
#                               
#                               
#                               
#                               if(is.null(a)) a<-sample(1:length(doubleRules),1) #generate an index of a doubleRule from the list of doubleRules
#                               a<-doubleRules[[a]]$ruleName
#                               #print(a)
#                               
#                               if(is.null(fr)) fr<-sample(c(k=createSR(),k=new("IdenSingleRule")),1,prob=c(0.5,0.5))# firstRule is chosen from an automatically generated SingleRule or identical rule returning the input
#                                                             
#                               if(is.null(sr)) sr<-sample(c(k=createSR(),k=new("IdenSingleRule")),1,prob=c(0.3,0.7)) #because adding more and more rules makes the rule very difficult I would generate identical function with greater probability
#                               
#                               if(is.null(ns)) ns<-sample(c(k=createSR(),k=new("IdenSingleRule")),1,prob=c(0.3,0.7))
#                                                            
#                               p<-new(a,firstRule=fr$k, secondRule=sr$k,nextSingle=ns$k)
#                               return(p)
#                               
#                                                         }
# 
# 
# 
# 
# #A FUNCTION TO GENERATE NUMERIC SEQUENCE OF DECLARED LENGTH
# # 'n' is the length of the numeric sequence (default value is 6)
# # 'x1', 'x2' are the first elements of the numeric sequence (you don't always need 'x2')
# sequence<-function(x1,x2=NULL,rule,n=6){
#                                   if(inherits(rule,"DoubleRule") && is.null(x2)) stop (" If you want to use a DoubleRoule you need to specify x2")
#                                   if(class(x1)!="numeric" ||(class(x2)!="NULL" && class(x2)!="numeric")) stop ("arguments 'x1', 'x2' must be of type 'numeric'.")
#                                   if(!inherits(rule,"SingleRule") && !inherits(rule,"DoubleRule")) stop ("'rule' argument must inherit from 'SingleRule' or 'DoubleRule' class")
#                                   if(n<3) stop("sequence must be longer than 3")
#                                   
#                                   k<-list()
#                                   k[1]=x1
#                                   
#                                     
#                                                                     
#                                   if(inherits(rule,"SingleRule")){
#                                                                     for(i in 2:n){
#                                                                                    k[i]<-calculate(x=rule,y=k[[i-1]])                                                                
#                                                                                   }
#                                     
#                                                                   }else{
#                                                                         k[2]=x2
#                                                                         for(i in 3:n){
#                                                                                       k[i]<-calculateDouble(x=rule,y=k[[i-2]],z=k[[i-1]])
#                                                                                       }
#                                     
#                                                                         }
#                                   return(k)
#                                                 
#                                         }
# 
# 
# 
# 
# # checking if a vector is in any row of the matrix
# #'mx' is a matrix in which I am searching
# #'vec' is a vector which is being checked
# # result TRUE means that there is already such vector in the matrix
# 
# duplicate<-function(mx,vec){
#                           return(any(apply(mx, 1, function(x, want) isTRUE(all.equal(x, want)), vec)))
#                           }
# 
# 
# 
# 
# 
# #CHECKING IF THE SEQUENCE IS NOT CONSTANT
# # it returns '0' when teh sequence is constant and '1' when the sequence is not constant
# # a function examines three last elements of a sequence, so even sequences like 27,9,9,9,9 ... are excluded
# 
# conCheck<-function(seq){
#   if(class(seq)!="list") stop("sequence must be of type 'list'")
#   
#   m<-length(seq)
#   
#   if(identical(seq[m],seq[m-1]) && identical(seq[m],seq[m-2]) ) {return(0)} else {return(1)}
#                         }
# 
# 
# 
# 
# 
# 
# # checking whether the sequence is not constant, numbers are not greater than 1000 or no lesser than -1000
# check<-function(seqlen,items){
#         x1<-as.numeric(sample(1:100,1)) #generate the first element of numeric sequence
#         x2<-as.numeric(sample(1:100,1)) # generate the second element of numeric sequence
#         m<-sample(c(1,2,3),1) #if m=1 I will create a singleRule, if m=2 rule will be a combination of singleRules, if m=3 rule is a doubleRule
#           
#         if(m==1){rule<-createSR()} else{
#           if(m==2){rule<-combineSR(createSR(),createSR())} else {rule<-combineDR()}}
#   
#               
#         result<-sequence(x1,x2,rule,n=seqlen)
#           
#         if(conCheck(result)==0 || result[length(result)]>1000 || result[length(result)]< -1000||duplicate(mx=items,vec=result)){check(seqlen,items)} else{return(result)}
#   
#                   }
# 
# 
# 
# 
# # AUTOMATIC TEST GENERATION
# # random 
# # 'seqlen' specyfies how long should a single sequence be
# # 'testlen' specyfies how many sequences (item positions) are there to be in a test
# automaticTest<-function(testlen,seqlen=6){
#                                           items<-matrix(NA,testlen,seqlen) #I will store generated items in a matrix
#                                           rules<- list() # I will keep the rules on a list
#                                                                                     
#                                           for(i in 1:testlen){
#                                                     items[i,]<- unlist(check(seqlen,items))                                                    
#                                                              }                                                                                       
#                                                                                   
#                                           return(items)
#                                           }
# 
# 
# 
# 
# # p<-new("DigSumSingleRule")
# # g<-new("AddDoubleRule", firstRule=p, secondRule=p, nextSingle=p)
# 
# 
#   
# #getting rule names
# grn<-function(x){
#   
#                   k<-list()
#                   k[1]<-class(x)[1] #writing a name of class of the object
#   
#                   if(inherits(x,"singleRule")){ #if x is a singleRule
#                                               k[2]<-x@constantVal
#                                               if(class(x@previousRule)[1] !="SingleRule"){b<-class(x@previousRule)[1]
#                                                                                           p<-list(k,b)
#                                                                                           gsrn(x)}
#                                               } 
#                   
#                   if(inherits(x,"doubleRule")){ #if x is a doubleRule
#                                         
#                                         if(class(x@firstRule)[1] !="SingleRule") #if firstRule is specyfied
#                                           {b<-class(x@firstRule)
#                                            p<-list(k,firstRule=b)
#                                            gsrn(x)
#                                            }
#                                         
#                                         if(class(x@secondRule)[1] !="SingleRule") #if secondRule is specyfied
#                                         {b<-class(x@secondRule)
#                                          p<-list(k,secondRule=b)
#                                          gsrn(x)
#                                         }
#                                         
#                                         
#                                           if(class(x@nextSingle)[1] !="SingleRule") #if nextSingle is specyfied
#                                         {b<-class(x@nextSungle)
#                                          p<-list(k,nextSingle=b)
#                                          gsrn(x)
#                                         }else{p<-k;return(p)}
#                                         
#                                         
#                                               }}
#                   
# 
