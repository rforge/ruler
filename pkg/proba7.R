
#a class for creating sequence
setClass("Item", representation(first="numeric", second="numeric"))





#a function creating object of class "Item"
createItem<-function(first,second=0){
                                    name<-new("Item", first=first, second=second)
                                    return(name)
                                    }




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


#przy generacji ciagu bede wyswietlac tylko parametry 'first' kolejnych elementow listy
# przy generowaniu ciagow o zadanej dlugosci zaokraglaj wartosci length funkcja 'round'

generate<-function(x,rule,const=0,length=6){
                        if(class(x)!="Item") stop("'x' argument must be an object of class Item")
                        if(class(rule)!="function") stop("'rule' argument must be a function")
                        if(class(length)!="numeric") stop("'length' argument must be numeric")
                        if(length<4||length > 10) stop("'length' argument greater than 10 or smaller than 3")
                        
                                
                        y=const #not every function uses 'constant' parameter (if theu dont this parameter is set 0)
                        
                        k<-list()  # a list containing objects of class "Item" necessary to generate a sequence
                        k[1]=x
                        
                        m <-list() # a list containing generated sequence (numeric values)
                        m[1]=x@first
                        
                        #print(x@first)
                                                                                  
                                                         
                        for (i in 2:(round(length,0))){
                                                         k[i]=rule(k[[i-1]],y)
                                                         #print(k[[i]]@first)
                                                         m[i]=k[[i]]@first
                                                       }         
                                                            
                        #return(k)
                        return(m)
                                                                                             
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


#new functions created by user should contain parameters referring to (x,y)
#returning objects of class "Item" or take objects of class "Item" as an argument


#a list containing names of functions - YOU SHOULD MAKE IT DYNAMIC RATHER THAN STATIC
rulenames<-matrix(c("addConst","subsConst","add","digSum1","digSum2","digSum3"),6,1) # default matrix of rules (6 default rules present) that can be expanded by a user


newRule<-function(rule){if(class(rule)!="function") stop("'rule' argument must be a function")
                        
                        
                            m<-as.character(match.call(rule)$x) #gettig the name of function from parameter 'rule'
                            if(m%in%rulenames[,1]){stop (paste(" functioned named",m, "already exists. For details print 'rulenames' matrix"))} else{
                            rulenames<<-rbind(rulenames,m)
                            print(paste(m,"succesfully added to the matrix of functions"))
                                                                                                                                                    }
                        }

                        
             
#------------------------------------------------- checking the uniquness of generated sequences ---------------------------------------------------


#in order to check the uniquness of generations (in order to avoid presenting the person the same number sequences)
# I would like to store all the sequences in a matrix. In order to be easily compared I assume all the sequences
#are of the same length. Each number in a sequence will be represented by three digits i.e.  1 will be saved as 001
# Otherwise it would be impossible to distinguish between similiar sequences. ie. 3-32-21 and 33-2-21 are different #sequences but both would be saved as 33221. So I am planning to save them in the following manner: 003032021 and 033002021.


storageTransf<-function(glist){
                                    tabela=matrix(NA,1,length(glist))
                                    for(i in 1:length(glist)){ #where kkk is a list with numeric sequence produced by function 'generate'
                                                              lg=length(digits(glist[[i]])) #length of an element of list 
                                                              if(lg>3)stop (paste("element ",i, "of the generated sequence is greater than 999" ))
                                                              if(lg==3){k=paste(glist[[i]])}            else{
                                                              if(lg<3)k=paste(paste(rep(0,3-lg),collapse=""),glist[[i]], sep="")} #glueing together zeros and a number from the list
                                                              tabela[1,i]=k
                                                              }
                                    
                                    return(paste(unlist(tabela),collapse=""))
                                    }





#Now I will create a matrix to store the generated number sequences in format created by storageTransf function
#After generating a new sequence the program will check whether such combination already exists in this matrix.
#If it does - there will be an error.


uniqGen<-function(nseq){ #nseq is a new sequence produced by function 'generate'
                    testItems<-matrix("020010030040070110",1,1) # a matrix containing generated number sequences(initial matrix has one column and one row)
                    k=storageTransf(nseq)
                    if(!k%in%testItems) { testItems=rbind(NA,testItems)#if new sequence is not present in a matrix, add it to the matrix in the last
                                          testItems[nrow(testItems),1]=k #saving new generation in the last row of the matrix
                                        
                                        } else stop ("Such generation already exists") 
                    return(testItems)
                    
                    }




# this function need exception handling, because it sometimes works and sometimes it doesn't
# check whether the number sequence is not constant!

randSeq<-function(){  f<-sample(c(1:99), 1,replace = T) # argument 'first' for object from class "Item" generated from between 1 and 99. All numers are chosen at the same probability
                      s<-sample(c(1:99,0), 1, prob = c(rep(0.3/99,99),0.7), replace = T) # argument 'second' for object from class "Item" generated from between . Zero should be chosen with greater probability
                      co<-sample(c(1:50,0), 1,prob = c(rep(0.3/50,50),0.7), replace = T) # generating a constant for functions that would use it 
                      print(co)
                      
                      iii<-createItem(first=f,second=s) #creating an item named 'iii' where first=f and second=s
                      print(iii)
                      
                      fun<-sample(rulenames,1) #generating function from the list of functions and evaluating it by its name
                      print(fun)
                      
                      seq<-generate(iii,eval(parse(text=fun),co))
                      
                      
                      return(seq)
                      
                      }

#Error in eval(parse(text = fun), 22) : not that many frames on the stack   ???
# checking the uniqueness of rules







