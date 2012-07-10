
#a class for creating sequence
setClass("Item", representation(first="numeric", second="numeric"))





#a function creating object of class "Item"
createItem<-function(first,second=0){
                                    name<-new("Item", first=first, second=second)
                                    return(name)
                                    }




#------------------------------------------------------------------------------
# DIFFERENT RULES
#------------------------------------------------------------------------------

#ADDING A CONSTANT #1
addConst<-function(x,y){ 
                    if(class(x)!="Item") stop("x argument must be an object of Item class")
                    if(class(y)!="numeric") stop ("'y' argument must be of type numeric")  
                    a<-new("Item", first=x@first+y)
                    return(a)
                      }


#SUBSTRACTING A CONSTANT #2
subsConst<-function(x,y){
                        if(class(x)!="Item") stop("'x' argument must be an object of Item class")
                        if(class(y)!="numeric") stop ("'y' argument must be of type numeric")
                        if(x@first<y)stop (paste(deparse(substitute(x)), "<", deparse(substitute(y))," ;sorry - no negative numbers allowed"))
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
                        if(class(x)!="Item") stop("'x' argument must be an object of Item class")
                        if(class(rule)!="function") stop("'rule' argument must be a function")
                        if(class(length)!="numeric") stop("'length' argument must be numeric")
                        if(length<4||length > 10) stop("'length' argument greater than 10 or smaller than 3")
                        
                                
                        y=const
                        
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





#function generating a sequence - I'll need a tree structure for that ?
Itemseq<-function(){
  
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

# rules<-list()
# rules<-c(addConst,subsConst,add,digSum1,digSum2,digSum3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) # I allow user to create (his/her own 10 functions) 


#new functions created by user shold contain parameters referring to (x,y)
#returning objects of class "Item"


#a list containing names of functions
rulenames<-list("addConst","subsConst","add","digSum1","digSum2","digSum3", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA) # I allow user to create (his/her own 10 functions) 
package <- as.character(substitute(package)) #gettig function names



#A FUNCTIONS ADDING A METHOD TO THE LIST OF METHODS 
#dodaj sprawdzanie - ?eby funkcje na li?cie si? nie powtarza?y
# newRule<-function(rule){if(class(rule)!="function") stop("'rule' argument must be a function")
#                         
#                         
#                         for(i in 6:length(rules)) #finding first free cell in a list
#                                                 {
#                                                 if(identical(rules[[i]],NA)){
#                                                                             #if(duplicated(rules[[c(1:(i-1))]]) ) stop (paste(" functioned named",deparse(substitute(rule), "already exists")))
#                                                                                                
#                                                                              rules[[i]]<<-rule
#                                                                              print(paste(deparse(substitute(rule))," added to 'rules' list with a number",i))
#                                                                              break 
#                                                                              }
#                                                 
#                                                   }                      
#                         
#                         }


newRule<-function(rule){if(class(rule)!="function") stop("'rule' argument must be a function")
                        
                        
                        m<-as.character(substitute(rule)) #gettig function names
                        
                        for(i in 6:length(rulenames)) #finding first free cell in a list
                        {
                          if(identical(rulenames[[i]],NA)){
                            #if(duplicated(rulenames[[c(1:(i-1))]]) ) stop (paste(" functioned named",deparse(substitute(rule), "already exists")))
                            
                            rulenames[[i]]<<-m
                            print(paste(deparse(substitute(rule))," added to 'rulenames' list with a number",i))
                            break 
                          }
                          
                        }                      
                        
}




#automatically chosing the rule from the list

choseRule<-function(){
                      for(i in length(rules):1){
                                              if(identical(rules[[i]],NA)==F)
                                              return(i) #i is the last element of the rules list not equal NULL
                                              
                                              x1<- runif(1, 0,i) # it starts from 0because I want the first expression also tobe generated
                                              
                                              break
                                              }
                      return(round(x1))
                      
                      }


# PRZESZUKIWANIE, CZY NIE MA TAKICH SAMYCH ELEMENT?W - W DRZEWIE -
#NP. ZAPISYWANIE GENERACJI W POSTACI DRZEWA
wo caveats:
  
  1.  It hasn't been checked yet.  There may be bugs, inefficiencies etc.

2.  It does search and insert, not delete.

Norm Matloff

# routines to create trees and insert items into them are included
# below; a deletion routine is left to the reader as an exercise

# storage is in a matrix, say m, one row per node of the tree; a link i
# in the tree means the vector m[i,] = (u,v,w); u and v are the left and
# right links, and w is the stored value; null links have the value NA;
# the matrix is referred to as the list (m,nxt,inc), where m is the
# matrix, nxt is the next empty row to be used, and inc is the number of
# rows of expansion to be allocated when the matrix becomes full

# initializes a storage matrix, with initial stored value firstval
newtree <- function(firstval,inc) {
m <- matrix(rep(NA,inc*3),nrow=inc,ncol=3)
m[1,3] <- firstval
return(list(mat=m,nxt=2,inc=inc))
}

# inserts newval into nonempty tree whose head is index hdidx in the
# storage space treeloc; note that return value must be reassigned to
# tree; inc is as in newtree() above
ins <- function(hdidx,tree,newval,inc) {
tr <- tree
# check for room to add a new element
tr$nxt <- tr$nxt + 1
if (tr$nxt > nrow(tr$mat))
tr$mat <- rbind(tr$mat,matrix(rep(NA,inc*3),nrow=inc,ncol=3))
newidx <- tr$nxt  # where we'll put the new tree node
tr$mat[newidx,3] <- newval
idx <- hdidx  # marks our current place in the tree
node <- tr$mat[idx,]
nodeval <- node[3]
while (TRUE) {
  # which direction to descend, left or right?
  if (newval <= nodeval) dir <- 1 else dir <- 2
  # descend
  # null link?
  if (is.na(node[dir])) {
    tr$mat[idx,dir] <- newidx
    break
  } else {
    idx <- node[dir]
    node <- tr$mat[idx,]
    nodeval <- node[3]
  }
}
return(tr)
}

# print sorted tree via inorder traversal
printtree <- function(hdidx,tree) {
  left <- tree$mat[hdidx,1]
  if (!is.na(left)) printtree(left,tree)
  print(tree$mat[hdidx,3])
  right <- tree$mat[hdidx,2]
  if (!is.na(right)) printtree(right,tree)
} 
