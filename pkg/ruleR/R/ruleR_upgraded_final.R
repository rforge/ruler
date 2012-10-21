#-------------------------------------------------------------------------------------------
#------------------------------SingleRules--------------------------------------------------
#-------------------------------------------------------------------------------------------

#VIRTUAL CLASS FOR RULES OPERATING ON SINGLE ARGUMENTS

setClass("SingleRule",  
         representation = representation(previousRule="SingleRule",description="character"),
         S3methods=TRUE)


calculateSpecific <- function(x,y,z=NULL){
  return(y)
}


setMethod("calculateSpecific",signature(x="SingleRule", y="numeric"),
          function(x,y){
            return(y)
          })




#[0] IDENTICAL FUNCTION (input=output) used in random sequence generation
setClass("IdenSingleRule",contains="SingleRule",S3methods=TRUE)

setMethod("calculateSpecific",signature(x="IdenSingleRule",y="numeric"),
          function(x,y){
            return(y)
          })


#[1] RULE 1 - ADDING A CONSTANT 

setClass("AddConstSingleRule",
         contains="SingleRule",
         representation(constantVal="numeric"),
         prototype(previousRule=new("IdenSingleRule"),description="Add"),
         S3methods=TRUE)

setMethod("calculateSpecific",signature(x="AddConstSingleRule", y="numeric"),
          function(x,y){
            return(x@constantVal+y)
          })


#[2] RULE 2 - MULTIPLYING BY A CONSTANT

setClass("MultConstSingleRule",
         contains="SingleRule",
         representation(constantVal="numeric"),
         prototype(previousRule=new("IdenSingleRule"), description="Multiply by "),
         S3methods=TRUE)

setMethod("calculateSpecific",signature(x="MultConstSingleRule", y="numeric"),
          function(x,y){
            return(x@constantVal*y)
          })



#[4] DIGITSUM
digits <- function(x) {
    x<-abs(x)
    if(length(x) > 1 ) {
    lapply(x, digits)
  } else {
    n <- nchar(x)
    rev( x %/% 10^seq(0, length.out=n) %% 10 )
  }
}


setClass("DigSumSingleRule",
         contains="SingleRule",
         representation(description="character"),
         prototype(description="Take the sum of digits ",previousRule=new("IdenSingleRule")),
         S3methods=TRUE)

setMethod("calculateSpecific",signature(x="DigSumSingleRule",y="numeric"),
          function(x,y){
            if(length(y) == 1){return(sum(digits(y)))   ## only for one argument
                               }else{
            return(unlist(lapply(digits(y),sum)))} ## properly vectorized
            
          })

#[5] NEGATIVE 
# 
# setClass("NegativeSingleRule", contains="SingleRule",S3methods=TRUE)
# 
# setMethod("calculateSpecific",signature(x="NegativeSingleRule",y="numeric"),
#           function(x,y){
#             return(-y)
#           })




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


setClass("DoubleRule", 
         representation = representation(firstRule="SingleRule", secondRule="SingleRule",nextSingle="SingleRule"),
         prototype(firstRule=new("IdenSingleRule"),secondRule=new("IdenSingleRule"),nextSingle=new("IdenSingleRule")),
         S3methods=TRUE)



#[1] ADD TWO PREVIOUS EXPRESSIONS

setClass("AddDoubleRule",
         contains="DoubleRule",
         representation(description="character"),
         prototype(firstRule=new("IdenSingleRule"),secondRule=new("IdenSingleRule"),nextSingle=new("IdenSingleRule"), description="Add two previous elements"),
          S3methods=TRUE)

setMethod("calculateSpecific",
          signature(x="AddDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y+z)
          })

#[2] MULTIPLY TWO PREVIOUS EXPRESSIONS 

setClass("MultDoubleRule",
         contains="DoubleRule",  
         representation(description="character"),
         prototype(firstRule=new("IdenSingleRule"),secondRule=new("IdenSingleRule"),nextSingle=new("IdenSingleRule"), description="Multiply two previous elements"),
         S3methods=TRUE)


setMethod("calculateSpecific",signature(x="MultDoubleRule", y="numeric", z="numeric"),
          function(x,y,z){
            return(y*z)
          })

# #[3] SUBSTRACT TWO PREVIOUS EXPRESSIONS
# 
# setClass("SubsDoubleRule",contains="DoubleRule",S3methods=TRUE)
# 
# 
# setMethod("calculateSpecific",signature(x="SubsDoubleRule", y="numeric", z="numeric"),
#           function(x,y,z){
#             return(y-z)
#           })

#[4] DIVIDING TWO NUMBERS (Philipp)
#prevent from dividing by zero !!!
# 
# setClass("DivDoubleRule",contains="DoubleRule",S3methods=TRUE)
# 
# setMethod("calculateSpecific", 
#           signature(x="DivDoubleRule", y="numeric", z="numeric"),
#           function(x,y,z){
#             if(z!=0){return(y%/%z)
#                      }else{return(0)}})
# 
# #[5] MODULO (Philipp)
# 
# setClass("ModuloDoubleRule",contains="DoubleRule",S3methods=TRUE)
# 
# setMethod("calculateSpecific", 
#           signature(x="ModuloDoubleRule", y="numeric", z="numeric"),
#           function(x,y,z){
#             return(y%%z)
#           })

# #[6] EXPONENTIAL FUNCTION (Philipp)
# setClass("ExpDoubleRule", contains="DoubleRule",S3methods=TRUE)
# 
# setMethod("calculateSpecific", 
#           signature(x="ExpDoubleRule", y="numeric", z="numeric"),
#           function(x,y,z){
#             return(y^z)
#           })



#EXECUTING RULES OPERATING ON TWO ARGUMENTS


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

# ---------------------------INTERTWINED RULES ----------------------------------------------------------
# -------------- different fules for odd and different for even element of numeric sequence -------------
#--------------------------------------------------------------------------------------------------------

setClass("IntertwinedRule",  
         representation = representation(odd_rule="SingleRule",even_rule="SingleRule"),
         S3methods=TRUE)


#--------------------------------------------------------------------------------------------------------------------
#----------------------------------------------------generating sequences--------------------------------------------
#--------------------------------------------------------------------------------------------------------------------

#a list of single rules 
singleRules<-list("IdenSingleRule","AddConstSingleRule","MultConstSingleRule","DigSumSingleRule")#,"NegativeSingleRule")


#a list of double rules
doubleRules<-list("AddDoubleRule","MultDoubleRule")#,"DivDoubleRule","ModuloDoubleRule")#"ExpDoubleRule")



#A FUNCTION TO CREATE SINGLE RULES
# 'a1' is an index from table SingleRule (default a=NULL) //if 'a' is NULL it will be generated
# 'n' how many rules are to be nested (default=NULL menas that I want to generate it automatically let's say from c(0,1,2) 
# n=0 would mean that I want to create just one rule with nothing nested inside)
# 'cv1' is a constant value
# '...' if I would like to add some rules nested I can provide their parameters cv must be always supplied #9even if the function doesn't require that

createSR<-function(a1=NULL,cv1=NULL,n=NULL,...){
  p<-list(...)#arguments for nesting other functions
  p<-unlist(p)
  
  
  if(is.null(a1)) {a1<-sample(2:length(singleRules),1)} #generate 'a' if no is supplied (we don't want to generate a=1 because it is identical function)
  if(is.null(cv1)) {cv1<-sample(-100:100,1)} # generate a constant value if no is supplied
  if(is.null(n)){n<-sample(c(0,1,2),1,prob=c(3/6,2/6,1/6)) #nesting more than two rules would be impossible to guess
                 k<-1:length(singleRules) #preventing nesting the same rules of the same class together
                 r<-sample(k[-a1],n,replace=FALSE)#generating rules to be nested
                 co<-sample(1:100,n) # generating constant values for nested rules
                 p<-as.vector(rbind(r,co))
                
                 } # generate 'n' if it is set as null with different probabilities
  

  if("constantVal"%in%slotNames(singleRules[[a1]])){m<-new(singleRules[[a1]],constantVal=cv1,previousRule=new("IdenSingleRule"))
  }else{m<-new(singleRules[[a1]],previousRule=new("IdenSingleRule"))}
  
  if(n!=0) {k<-createSR(p[[1]],p[[2]],n-1,p[-c(1,2)])
            m@previousRule<-k}#else{m@previousRule<-new("IdenSingleRule")}
  
  return(m)                                                     
}



# 'a' is index from a list of DoubleRules
#'fr' firstRule argument of an object of class doubleRule 
#'sr' secondRule argument of an object of class doubleRule
#'ns' nextSingle argument of an object of class doubleRule
#'
createDR<-function(a=NULL,fr=NULL,sr=NULL,ns=NULL){
                    if(!is.null(a) && a>length(doubleRules)) stop (paste("The list of doubleRules is shorter than ",a, ".Please specify 'a' value, which is smaller than or equal to",length(doubleRules)))
                    if(!inherits(fr,"SingleRule") && !is.null(fr))stop(paste("'fr' argument must inherit from class singleRule"))
                    if(!inherits(sr,"SingleRule") && !is.null(sr))stop(paste("'sr' argument must inherit from class singleRule"))
                    if(!inherits(ns,"SingleRule") && !is.null(ns))stop(paste("'ns' argument must inherit from class singleRule"))
                                              
                    if(is.null(fr)) fr<-sample(c(createSR(),new("IdenSingleRule")),1,prob=c(0.5,0.5))[[1]]# firstRule is chosen from an automatically generated SingleRule or identical rule returning the input
                                                            
                    if(is.null(sr)) {sr<-sample(c(createSR(),new("IdenSingleRule")),1,prob=c(0.3,0.7))[[1]] #because adding more and more rules makes the rule very difficult I would generate identical function with greater probability
                                     }
                              
                    if(is.null(ns)) ns<-sample(c(createSR(),new("IdenSingleRule")),1,prob=c(0.3,0.7))[[1]]
                                              
                    if(is.null(a)) a<-sample(1:length(doubleRules),1) #generate an index of a doubleRule from the list of doubleRules
                    
                    a<-doubleRules[[a]]
                                                         
                    
                    p<-new(a,firstRule=fr, secondRule=sr,nextSingle=ns)
                    
                    return(p)
                              
                                                  }
 
 
#A FUNCTION TO GENERATE NUMERIC SEQUENCE OF DECLARED LENGTH
# 'seqlen' is the length of the numeric sequence (default value is 6)
# 'start' - range from which starting values are generated


sequenceR<-function(start,rule,seqlen)
  {
  return(a)}



setMethod("sequenceR",signature(start="vector",rule="SingleRule",seqlen="numeric"),
          function(start,rule,seqlen){
                      
            
            if(length(start)==1){ #generating starting elements of numeric sequence
              x1<-start;x2<-start
            }else{
                start<-sample(start,2)
                x1<-start[1]
                x2<-start[2]
                            
            }
            
            k<-list()
            k[1]=x1
            
            
            for(i in 2:seqlen){
              k[i]<-calculate(x=rule,y=k[[i-1]])                                                                
            }
            return(list(k,rule))
          })


setMethod("sequenceR",signature(start="vector",rule="DoubleRule",seqlen="numeric"),
          function(start,rule,seqlen){
                        
            
            if(length(start)==1){ #generating starting elements of numeric sequence
              x1<-start;x2<-start
            }else{
              
                start<-sample(start,2)
                x1<-start[1]
                x2<-start[2]
             
            }
            
                        
            k<-list()
            k[1]=x1
            k[2]=x2
            
            for(i in 3:seqlen){
              k[i]<-calculate(x=rule,y=k[[i-2]],z=k[[i-1]])
            }
                      
            return(list(k,rule))
          })



setMethod("sequenceR",signature(start="vector",rule="IntertwinedRule",seqlen="numeric"),
          function(start,rule,seqlen){         
                    
            
            odd_list<-sequenceR(start=start,rule=rule@odd_rule,seqlen=seqlen%/%2)[[1]]
            
            even_list<-sequenceR(start=start,rule=rule@even_rule,seqlen=seqlen%/%2)[[1]]
                        
            k<-unlist(mapply(c,odd_list, even_list, SIMPLIFY=FALSE))
            if(seqlen%%2==1)k<-c(k,calculate(rule@odd_rule,k[[length(k)-1]]))#if sequence length is an odd number

            
            
            return(list(as.list(k),rule))
          })







 
# checking if a vector is in any row of the matrix
#'mx' is a matrix in which I am searching
#'vec' is a vector which is being checked
# result TRUE means that there is already such vector in the matrix

duplicate<-function(mx,vec){
                          return(any(apply(mx, 1, function(x, want) isTRUE(all.equal(x, want)), vec)))
                          }





#CHECKING IF THE SEQUENCE IS NOT CONSTANT
# it returns '0' when teh sequence is constant and '1' when the sequence is not constant
# a function examines three last elements of a sequence, so even sequences like 27,9,9,9,9 ... are excluded

conCheck<-function(x){
    
  if(length(x)%%2==1){m<-length(x)-1}else{m=length(x)}
  
  for(j in 1:(m/2-1)){
                  if(identical(x[c((m-j):m)],x[c((m-2*j-1):(m-j-1))])){return(0);break
                  }else{if(x[[m]]==x[[m-1]])return(0);break}
                      }
  
                    return(1)}




#---------------------------------------------------------------------------
#------function to print objects of class SingleRule and DoubleRule---------
# #---------------------------------------------------------------------------
# 
# print.SingleRule<-function(x){
#                                 pr<-function(x){
#                                                 cat(paste("\nname:", class(x)[1]))
#                                                 if("constantVal"%in%slotNames(x)) {cat(paste(", constant value: ", x@constantVal))} 
#                                                 }
#                                 pr(x)
#                                 
#                                 if(!class(x@previousRule)=="SingleRule"){x<-x@previousRule; print(x)}
#                                 
#                               }
# 
# 
# 
# print.DoubleRule<-function(x){
#                               x1=x
#                                              
#                                                              
#                                 cat(class(x1)[1])
#                                 
#                                 if("firstRule"%in%slotNames(x)){cat("\n FIRST RULE:"); x<-x@firstRule;print.SingleRule(x)}
#                                 x=x1
#                                 if("secondRule"%in%slotNames(x)){cat("\n SECOND RULE:"); x<-x@secondRule;print.SingleRule(x)}
#                                 x=x1
#                                 if("nextSingle"%in%slotNames(x)){cat("\n NEXT SINGLE:"); x<-x@nextSingle;print.SingleRule(x)}
#                             
#                                 }


#-----------------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------NEW APPROACH---------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------------------------------------------------------

setClassUnion(name="Rule", members=c("SingleRule","DoubleRule"))
setClassUnion(name="vecORnull", members=c("vector","NULL"))         
         
         
# 'range' of constant value user want to use (it can be a vector (ex. sequence with min value, max value and step)) 

setClassUnion(name="Rule", members=c("SingleRule","DoubleRule"))
setClassUnion(name="vecORnull", members=c("vector","NULL"))


setClass("DictionaryRule",representation(rule="Rule",range="vecORnull"),S3methods=TRUE)




#make a list of objects of class DirectRules and send the list to createItems function and name it as you wish

#'range' for constant Value parameter// can be a vector ie. seq(3,5,by=.1) or a single numeric value
#'index' - index of the rule in the basic rules list - it can be a list or a simple element

createDictRule<-function(index,range=c(-15:-1,1:15)){
                                
                                if(inherits(index,"SingleRule")){rule<-index;if("constantVal"%in%slotNames(index)){range=index@constantVal}else{range=NULL}}
                                if(inherits(index,"DoubleRule")){rule<-index;range=NULL}
                                if(class(index)=="numeric"){#you specify rule from the table c(singleRules,doubleRules)
  
                                      if(index>length(c(singleRules,doubleRules)))stop("supplied index is larger than available list of rules")
                                      
                                      if(index<=length(singleRules)){
                                          rule<-createSR(a1=index,cv1=0,n=0,previousRule=new("IdenSingleRule")) #create a singleRule
                                          }else{rule<-createDR(a=(index-length(singleRules)),
                                                               fr=new("IdenSingleRule"),
                                                               sr=new("IdenSingleRule"),
                                                               ns=new("IdenSingleRule"))
                                                range=NULL}#create a double rule basing on index value
                                      
                                }                                                                     
                                    range=as.vector(range)
                                    b<-new("DictionaryRule", rule=rule, range=range)          
                                    return(b)
                                      }




#FUNKCJE POMOCNICZE

#GENERATE CV - function generating a constant value out of a given range
#function generatin constantVal for singleRules from specyfied range
#'x' an object of class DictionaryRule
generate_cv<-function(x){
  if(length(x@range)==1)x@range<-rep(x@range,2)
  cv<-sample(x@range,1)
  return(cv)}#generating a constant Value for the first rule (if it needs one)




#NEST - function nesting rules
#'chain' - how many rules I am allowed to nest
#'dict_single' - a list of available singleRules
#'a' exterior rule - object of class SingleRule

nest<-function(chain,dict_single,a){
    
  # if you want to nest more functions than there are available in dict_single list
  if(chain>length(dict_single))chain<-length(dict_single)

  if(chain!=0){    
    p<-sample(1:length(dict_single),1)  
    k<-dict_single[[p]]@rule
    if("constantVal"%in%slotNames(k))k@constantVal<-generate_cv(dict_single[[p]])
    a@previousRule<-k
    chain=chain-1
    dict_single<-dict_single[-p] #there is no point on nesting several same singleRules
        
    k<-nest(chain,dict_single,k)
    a@previousRule<-k
            }
  return(a)
}


#SPLIT DICT - function getting a list of SingleRules
#a function to split itemDictionary into 
#two list - first one containing only SingleRules and the second only doubleRules
#'type'=1 singlerules, type=2 doubleRules
split_dict<-function(itemDictionary,type){
    
    lista<-list()
  
    if(type==1){b<-"SingleRule"}else{b<-"DoubleRule"}
    for(i in 1:length(itemDictionary)){
            if(inherits(itemDictionary[[i]]@rule,b))lista<-c(lista,itemDictionary[[i]])
             
     }
  return(lista)
}



#GENERATE RULE - function returning a rule form itemDictionary
#'items' list of already generated items
#'itemDictionary' list of objects of class "DictionaryRule" allowed in creating numeric sequence
#'chain' showing how many rules I can combine
#'del' which slots in doubleRules can be modyfied(1-only first,2-only second, 3-only next,c(1,2,3) all)
#'start' - range for starting values that user can specify (it can be a single value or a vector specifying the range)

generateRule<-function(itemDictionary,chain,del){
  if(class(itemDictionary)!="list")stop("itemDictionary must be of class 'list'")
  
  h<-sample(1:length(itemDictionary),1)#generate index of a rule of itemDictionary list
  a<-itemDictionary[[h]]@rule #craeting the outer singleRule

  
    #if we are dealing with single rules
 if(inherits(a,"SingleRule")){ #if the first generated rule is of class SingleRule
            
            if("constantVal"%in%slotNames(a)){
            cv<-generate_cv(itemDictionary[[h]])
            a@constantVal<-cv #generating a constant value for the outer rule
            }
            itemDictionary<-itemDictionary[-h]#nesting the same singleRules doesn't make sense so remove the rule from the list
   
   
   if(length(itemDictionary)>0){dict_single<-split_dict(itemDictionary,1)
                               a<-nest(chain,dict_single,a)
                              }else{dict_single<-NULL
                                    a<-a}
   
   
   
     
  }else{#if we are dealing with Double Rules
    
   chain0<-chain
   dict_single0<-split_dict(itemDictionary,1)
      
   if(1%in%del){chain<-chain0
                dict_single<-dict_single0
                h<-sample(1:length(dict_single),1)#generate index of a rule of itemDictionary list
                b<-dict_single[[h]]@rule #first nested function
                if("constantVal"%in%slotNames(b))b@constantVal<-generate_cv(dict_single[[h]])
                dict_single<-dict_single[-h]
                chain=chain-1
                if(chain>=1){a@firstRule<-nest(chain,dict_single,b)}else{a@firstRule<-b}
                }
                                    
                
   
   if(2%in%del){chain<-chain0
                dict_single<-dict_single0
                h<-sample(1:length(dict_single),1)#generate index of a rule of itemDictionary list
                b<-dict_single[[h]]@rule #first nested function
                if("constantVal"%in%slotNames(b))b@constantVal<-generate_cv(dict_single[[h]])
                dict_single<-dict_single[-h]
                chain=chain-1
                if(chain>=1){a@secondRule<-nest(chain,dict_single,b)}else{a@secondRule<-b}                
                }
   
   if(3%in%del){chain<-chain0
                dict_single<-dict_single0
                h<-sample(1:length(dict_single),1)#generate index of a rule of itemDictionary list
                b<-dict_single[[h]]@rule #first nested function
                if("constantVal"%in%slotNames(b))b@constantVal<-generate_cv(dict_single[[h]])
                dict_single<-dict_single[-h]
                chain=chain-1
                if(chain>=1){a@nextSingle<-nest(chain,dict_single,b)}else{a@nextSingle<-b}                
                }
   
   
   
    }
  return(a)
}



#CHECK - function checking if generated sequence is constant, or has elements greater/smaller than the limit
#'result' - numeric sequence that I want to check  
#function would check for duplicates, constant/cyclic 
#I compare result with elements of list items that have the same length as 'result'
# 'element_range' maximum and minimum values of elements of the numeric sequence

check<-function(items,result,element_range){
  if(class(items)!="list")stop("items must be a list")
  max_val<-max(element_range)
  min_val<-min(element_range)
    
  #getting elements of list 'items' that have the same length as 'result'
  to_compare<-list()
  
  for(i in 1:length(items)){
    if(length(items[[i]])==length(result))to_compare<-c(to_compare,items[i])
  }
  
  if(length(to_compare)!=0){to_compare<-matrix(unlist(to_compare),length(to_compare),length(result),byrow=TRUE)
                           }else{to_compare<-matrix(NA,1,length(result))}
  
 
  
  if(any(is.na(result))|| max(unlist(result))>max_val || min(unlist(result))< min_val||conCheck(result)==0 ||duplicate(to_compare,unlist(result))){
    return(0)}else{return(1)}
   
}





# VALIDATION FUNCTION - function saving new items on a list if they are unique and not constant etc.
#'seqlen' - length of numeric sequence
#items' list of already generated items
#'itemDictionary' objects of class "DictionaryRule" allowed in creating numeric sequence
#'chain' showing how many rules I can combine
#'del' which slots in doubleRules can be modyfied(1-only first,2-only second, 3-only next,c(1,2,3) all)
#'start' - range for starting values that user can specify (it can be a single value or a vector specifying the range)
#'number'-iteration counter


val<-function(items,rules,itemDictionary,chain,del,start,element_range,seqlen,number){
  
  number=number
   
  rule<-generateRule(itemDictionary,chain,del)
  result<-sequenceR(start=start,rule=rule,seqlen=seqlen)[[1]]
  
  
  validation<-check(items,result,element_range)
 
  
  
if(validation==0){#if the sequence is not unique, tru generating another one (but max. for three times)
    
    
                  if(number<4) {number=number+1 
                                val(items=items,rules=rules,itemDictionary=itemDictionary,chain=chain,del=del,start=start,element_range=element_range,seqlen=seqlen,number)
                  }else{item<-rep(NA,seqlen)
                        rule<-new("IdenSingleRule")
                        
                        k<-list(item,rule)
                        return(k)
                        break                       
                        
                        } # if you failed to generate a unique sequence in 4 attempts- return zero sequence
  }else{item<<-unlist(result)
                
        rule<-rule
      
        k<-list(item,rule)
        return(k)
        break
        }


}


  
    #'n'- how many items would you like to create
    #'seqlen' - how long would you like your items to be (you can specify different length for every item)
    #'dictionary' - dictionary of rules user would like to use in generating new sequences
    #'chain' - how many rules you want to combine - it can be a range, a list of ranges or a single value
    #'del'- how many of DoubleRule slots I can change
    #'start' - range for starting values that user can specify (it can be a single value or a vector specifying the range)
    #'exact' - which dictionary rules can be applied
    #'element_range' - the range of elements - should be a vector of length 2 specyfying max and min approved element
    
basicDictionary<-list(createDictRule(2),createDictRule(3),createDictRule(4),createDictRule(5),createDictRule(6))#,createDictRule(7))#,createDictRule(8),createDictRule(9)) 
     


createTest<-function(n,dictionary=basicDictionary,start=1:100,chain=2,del=c(1,2,3),seqlen=6,exact=NULL,element_range=c(-1000,1000)){
        if(class(dictionary)!="list")stop("dictionary must be a list")
        
        if(is.null(exact)){exact0<-rep(list(1:length(dictionary)),length.out=n)
        }else{if(class(exact)=="list"){exact0<-exact}else{exact0<-rep(list(exact),length.out=n)}}
        
        if(class(start)=="list"){start0<-start}else{start0<-rep(list(start), length.out=n)}
        if(class(chain)=="list"){chain0<-chain}else{chain0<-rep(list(chain), length.out=n)}
        if(class(del)=="list"){del0<-del}else{del0<-rep(list(del), length.out=n)}
        if(class(seqlen)=="list"){seqlen0<-seqlen}else{seqlen0<-rep(list(seqlen), length.out=n)}
        if(class(element_range)=="list"){element_range0<-element_range}else{element_range0<-rep(list(element_range), length.out=n)}
   
        
        items<-list(NA) #I will store generated items in a matrix
        rules<- list(NA)
                
         for(i in 1:n){
                  exact<-exact0[[i]]
                  
                  start<-start0[[i]]
                  chain<-chain0[[i]]
                  del<-del0[[i]]
                  seqlen<-seqlen0[[i]]
                  element_range<-element_range0[[i]]
                                                                         
                  itemDictionary<-dictionary[exact] #list of dictionary rules allowed in generating an item
                  
                                  
                  m<-val(items=items,rules=rules,itemDictionary,chain,del,start,element_range,seqlen,number=0)
                                    
                  items[[length(items)+1]]<-m[[1]]
                  rules[[length(rules)+1]]<-m[[2]]
                                    
                              }
        return(list(items[-1],rules[-1]))
                                  }

#=====PRINTING RULES=================================================================================================================

setMethod("print",signature(x="SingleRule"), #both [1] and [2] inherit from class 'SingleRule'
          function(x){
            k<-list()
            
            desc_list<-function(x){ 
              if(!inherits(x,"IdenSingleRule")){
                                                if("constantVal"%in%slotNames(x)){k<<-c(k,paste(x@description,x@constantVal)); x<-x@previousRule;desc_list(x)
                                                                                  }else {k<<-c(k,x@description); x<-x@previousRule;desc_list(x)}
                                                }
              
              
                                  }
              
                                
            
            
            
            desc_list(x)
            
            for(i in length(k):1){cat(paste(k[[i]],"\n"))}
                    
          })



setMethod("print",signature(x="DoubleRule"), #both [1] and [2] inherit from class 'SingleRule'
          function(x){
            if(!inherits(x@firstRule,"IdenSingleRule"))cat("\nRULES APPLIED TO THE FIRST ELEMENT:\n");print(x@firstRule)
            if(!inherits(x@secondRule,"IdenSingleRule"))cat("\nRULES APPLIED TO THE SECOND ELEMENT:\n");print(x@secondRule)
            cat(paste("\n",x@description,"\n",sep=""))
            if(!inherits(x@nextSingle,"IdenSingleRule"))cat("\nRULES APPLIED TO THE RESULT OF PREVIOUS OPERATIONS:\n");print(x@nextSingle)
            
          })




setMethod("print",signature(x="IntertwinedRule"),
          function(x){
            cat("\nRULES APPLIED TO 1st,3rd,5th ... ELEMENT:\n");print(x@oddRule)
            cat("\nRULES APPLIED TO 2nd,4th,6th ...ELEMENT:\n");print(x@evenRule)
            
          })



