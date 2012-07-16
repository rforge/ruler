#------------------DEFINIG CLASSES--------------------------------------------------------------------------------
#[1]
#a class for creating starting items
setClass("Item", representation(first="numeric", second="numeric"),S3methods=TRUE)


#a function creating object of class "Item"
createItem<-function(first,second=0){
                                    name<-new("Item", first=first, second=second)
                                    return(name)
                                     }



#[2]
#creating a class describing the rules (S4 class)
setClass("Rule", representation(ruleName="character",ruleBody="function"),S3methods=TRUE)


createRule<-function(insertedRule){ #a function creating objects of class 'Rule'
                                if(length(formals(insertedRule))!=2) stop (paste("A function needs to have two arguments instead of",length(formals(insertedRule))))# a function needs to have two arguments
                                
                                #the rule needs to have two arguments x - for class Item and y of class numeric !!!!!
                                #the rule has to return objects of class "Item" !!!!!
                                name<-new("Rule", ruleName=as.character(match.call(insertedRule)$x),ruleBody=insertedRule )
                                return(name)
                                }

#[3]
#creating class for generated sequence
#'findex' is the index of table 'functions' where a rule is stored
# 'co' is constant; 'seq' is number sequence; 'item' is the starting item
# 
# setClass("Sequence", representation(seq="numeric", item="Item", findex="numeric", co="numeric"))



#-------------------------------------------------------------------------------
# DIFFERENT RULES
#-------------------------------------------------------------------------------

#ADDING A CONSTANT #1
addConst<-function(x,y){ 
                    if(class(x)!="Item") stop("x argument must be an object of Item class")
                    if(class(y)!="numeric") stop ("'y' argument must be of type numeric")
                    stopifnot(y > 0)
                                        
                    a<-new("Item", first=x@first+y)
                    return(a)
                      }


#SUBSTRACTING A CONSTANT #2
subsConst<-function(x,y){
                        if(class(x)!="Item") stop("'x' argument must be an object of Item class")
                        if(class(y)!="numeric") stop ("'y' argument must be of type numeric")
                        if(x@first<y)stop (paste(deparse(substitute(x)), "<", deparse(substitute(y))," ;sorry - no negative numbers allowed"))
                        stopifnot(y > 0) # prevents from generatng constant sequence
                        
                        a<-new("Item", first=x@first-y)
                        return(a)
                        }



#ADD TWO PREVIOUS NUMBERS #3
add<-function(x,y){
                  if(class(x)!="Item") stop("x argument must be an object of Item class")
                  
                  a<-new("Item", first=x@second, second=(x@first+x@second))
                  return(a)                  
                  }

#transforming number into digits (from: https://stat.ethz.ch/pipermail/r-help/2011-March/270786.html )

digits <- function(x) {
                      if(length(x) > 1 ) {
                                          lapply(x, digits)
                                          } else {
                                                  n <- nchar(x)
                                                  rev( x %/% 10^seq(0, length.out=n) %% 10 )
                                                  }
                      }



#ADD A DIGITSUM TO A NUMBER  #4

digSum1<-function(x,y=0){
                        if(class(x)!="Item") stop ("'x' argument must be an object of Item class")
                        if(x@first<10) stop("first element of an object is less than 10")

  
                        m=digits(x@first) #a number transformed into digits
                        suma=sum(m[c(1:length(m))]) #sum of digits
      
                        d<-new("Item", first=x@first+suma)
                        return(d)
                          }



#ADD A DIGITSUM OF TWO PREVIOUS NUMBERS TO A FIRST NUMBER #5

digSum2<-function(x,y=0){
                        if(class(x)!="Item") stop ("'x' argument must be an object of Item class")
                        if(x@first<10) stop("first element of an object is less than 10")
                        if(x@second<10) stop("second element of an object is less than 10")
  
  
                        m=digits(x@first) #a number transformed into digits
                        suma1=sum(m[c(1:length(m))]) #sum of digits
  
  
                        n=digits(x@second) #a number transformed into digits
                        suma2=sum(n[c(1:length(n))]) #sum of digits
  
    
                        g<-new("Item", first=x@second, second=x@first+suma1+suma2)
                        return(g)
  
                        }



#ADD A DIGITSUM OF TWO PREVIOUS NUMBERS TO A SECOND NUMBER #6

digSum3<-function(x,y=0){
                      if(class(x)!="Item") stop ("'x' argument must be an object of Item class")
                      if(x@first<10) stop("first element of an object is less than 10")
                      
  
                      m=digits(x@first) #a number transformed into digits
                      suma1=sum(m[c(1:length(m))]) #sum of digits
  
  
                      n=digits(x@second) #a number transformed into digits
                      suma2=sum(n[c(1:length(n))]) #sum of digits
  
  
                      e<-new("Item", first=x@second)
                      e@second=e@first+suma1+suma2
                      return(e)
  
                        }



#-------------------------------------------------------------------------------
# GENERATING NUMERIC SEQUENCE
#-------------------------------------------------------------------------------

# a function to generate a number sequence.
# I will print only attributes 'first' for objects of class 'Item'
# I used function 'round' to 

#przy generacji ciagu bede wyswietlac tylko parametry 'first' kolejnych elementow listy
#przy generowaniu ciagow o zadanej dlugosci zaokraglaj wartosci length funkcja 'round'

generate<-function(x,rule,const=0,length=6){
                        if(class(x)!="Item") stop("'x' argument must be an object of class Item")
                        if(class(rule)!="Rule") stop("'rule' argument must be of class 'Rule'")
                        if(class(length)!="numeric") stop("'length' argument must be numeric")
                        if(length<4||length > 10) stop("'length' argument greater than 10 or smaller than 3")
                        
                              
                        y<-const #not every function uses 'constant' parameter (if they dont this parameter is set 0)
                        
                        k<-list()  # a list containing objects of class "Item" necessary to generate a sequence
                        m<-list() # a list containing only numbers for generating the sequence
                        k[[1]]=x
                        m[1]=k[[1]]@first
                                                          
                        
                        #print(k[[1]]@first)
                                                                                  
                                                         
                        for (i in 2:(round(length,0))){
                                                        k[[i]]=rule@ruleBody(k[[i-1]],y)
                                                        m[i]=(k[[i]]@first)
                                                        
                                                       }         
                                                            
                       
                        return(unlist(m))
                                                                                                                     
                                           }


#--------------------------------------------------------------------------------
# NO REPETITIONS
#--------------------------------------------------------------------------------

# transforming sequence into integer
as.numeric(unlist(k)) # where k is a list generated by function generate




#---------------------------------------------------------------------------------
# CHECKING THE UNIQUENESS OF RULES
#---------------------------------------------------------------------------------

#A LIST OF RULES 


# checkList is a list in which I store objects of class 'Rule'- there are 6 defult rules there.
# It stores rulenames [[1]], rylebody [[2]], and [[3]]sample sequence generated for object of class 'Item' first=17, second=20 and constant=1                            
# A function checkRule checks whether an object of class 'Rule' is already on the list 

                                
functions=list(createRule(addConst), createRule(subsConst), createRule(add), createRule(digSum1), createRule(digSum2),createRule(digSum3)) #the default list of rules




createCL<-function(){    # a function creating a checklist
  
                        fnames<-list()
                        body<-list()
                        checkGen<-list()

                        checkList<<-list(fnames,body,checkGen)
                        
                        for(i in 1:length(functions)){
                                                      checkList[[1]][i]<-functions[[i]]@ruleName
                                                      checkList[[2]][[i]]<-functions[[i]]@ruleBody
                                                      checkList[[3]][[i]]<-generate(createItem(17,20),functions[[i]],1)
                                                      }
                        
                                                
                        return(checkList)
                        
                        }




checkRule<-function(rule){
                          if(class(rule)!="Rule")stop("'rule' argument must be an object of class Rule")
                          
                          cl<-createCL() # an up-to-date checkList
                          
                                                    
                          z=0 #z=0 means that there are no conflicts
                          
                          
                          if(rule@ruleName%in%unlist(cl[[1]])) {z=1 #checking the uniquness of rule name
                                                                return(list(z,"your rule has the same name as an existing rule"))
                                                                break 
                                                                }
                          
                          for(i in 1:length(cl[[2]])){
                                                      if(identical(generate(createItem(17,20),rule,1), cl[[3]][[i]])){
                                                                                                z=1
                                                                                                return(list(z,"your rule gives the same number sequence as an existing rule"))
                                                                                                break 
                                                                                                  }
                                                      
                                                      if(identical(rule@ruleBody, cl[[2]][[i]])) {
                                                                                                  z=1
                                                                                                  return(list(z,"your rule has the same body as an existing rule"))
                                                                                                  break 
                                                                                                  }
                                                      
                                                      }
                          return(z=0)
                          }




addRule<-function(rule){
                        m<-checkRule(rule)
                        if(m[[1]]==0) {# it means that NO conflicts have occured
                                      functions[[length(functions)+1]]<<-rule
                                      print(paste("Your rule named", rule@ruleName, "successfully added to rule list at [[",length(functions),"]]"))
                                      } else {
                                              print(paste("Sorry, the following conflict occured: '",m[[2]],"'. Your rule '",rule@ruleName, "'could not be added to the lust of rules"), sep="")
                                              }
                        
                        
                        }                                
                                

              

                              
#------------------------------------------------- checking the uniquness of generated sequences -------------------

# #Now I will create a matrix to store the generated number sequences in format created by storageTransf function
# #After generating a new sequence the program will check whether such combination already exists in this matrix.
# #If it does - there will be an error.
# 
# 
# uniqGen<-function(nseq){ #nseq is a new sequence produced by function 'generate'
#                     testItems<-matrix("020010030040070110",1,1) # a matrix containing generated number sequences(initial matrix has one column and one row)
#                     k=storageTransf(nseq)
#                     if(!k%in%testItems) { testItems=rbind(NA,testItems)#if new sequence is not present in a matrix, add it to the matrix in the last
#                                           testItems[nrow(testItems),1]=k #saving new generation in the last row of the matrix
#                                         
#                                         } else stop ("Such generation already exists") 
#                     return(testItems)
#                     
#                     }




# this function need exception handling, because it sometimes works and sometimes it doesn't
# check whether the number sequence is not constant!


randSeq<-function(){  f<-sample(c(1:99), 1,replace = T) # argument 'first' for object from class "Item" generated from between 1 and 99. All numers are chosen at the same probability
                      s<-sample(c(1:99,0), 1, prob = c(rep(0.3/99,99),0.7), replace = T) # argument 'second' for object from class "Item" generated from between . Zero should be chosen with greater probability
                      co<-sample(c(1:50,0), 1,prob = c(rep(0.5/50,50),0.5), replace = T) # generating a constant for functions that would use it 
                      #print(paste("constant: ",co))
                      
                      iii<-createItem(first=f,second=s) #creating an item named 'iii' where first=f and second=s
                      #print(iii)
                      
                      fun<-sample(1:length(functions),1) #generating function from the list of functions and evaluating it by its name
                      #print(fun)
                      
                      #seq<-generate(iii,functions[[fun]],co)
                      
                      tryCatch({ result=list(seq=generate(iii,functions[[fun]],co),item=iii,findex=fun,constant=co)
                                 return(result)
                                 }, error=function(e){randSeq()} ) #catching the errors
                      
                                  
                      }





noise<-function(seq,n){ #generating wrong answers (to choose), 'n' specyfies how many answers you want to create
                      #if(class(rule)!="Sequence")stop("'seq' argument must be an object of class Sequence")
                      
                       k<-c(findex)
                       m=n # numerator
                       
                       functions=functions[-seq$k] #all the rules exept the one used to generate a sequence
                       
                       print(functions)
                       b<-sample(1:length(functions),1)
                       print(b)
                                      
                       
                       
                                          tryCatch({ result=generate(seq$item,functions[[b]],seq$constant)
                                          return(result[length(result)])
                                       }, error=function(e){noise(seq)} ) #catching the errors
                                        
                                            
                      }  


# creating a session (generating several number sequences, saving them in a matrix and checking whether such a sequence already exists)
# n - means how many sequences you want to generate

