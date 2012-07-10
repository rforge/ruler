
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

                        
                 
                          
                      

#----------------------------------------------- generating radom sequences with random rules--------------------------


#automatically creating objects of class "Item"



# if any error appears make it drop it and generate another sequence (so that the final ) 

createSequence<-function(n=6){          #a function to creates n sequences (default number of sequences is 6)
                              for(i in 1:n){
                                            f<-sample(c(1:99), 1,replace = T) # argument 'first' for object from class "Item" generated from between 1 and 99. All numers are chosen at the same probability
                                            s<-sample(c(1:99,0), 1, prob = c(rep(0.3/99,99),0.7), replace = T) # argument 'second' for object from class "Item" generated from between . Zero should be chosen with greater probability
                                
                                            co<-sample(c(1:50,0), 1,prob = c(rep(0.3/50,50),0.7), replace = T) # generating a constant for functions that would use it 
                                
                                            iii<-createItem(first=f,second=s) #creating an item named 'iii' where first=f and second=s
                                                                                                
                                            fun<-sample(rulenames,1) #generating function from the list of functions and evaluating it by its name 
                                
                                            seq1<-generate(iii,eval(parse(text=fun),co)) #generating the random sequence
                                            
                                            seq2<-paste(unlist(seq1),collapse="") #transorming a sequence into a single number (that can be stored in a tree)
                                            seq3<-as.numeric(seq2) # in this form I will save the sequence in a tree
                                            
                                            #cat(paste("generation",i,"is \n",seq1,"\n Item", iii, "\n rule", fun))
                                            }
                              
                              cat(paste("generation",i,"is \n",seq1,"\n Item", iii, "\n rule", fun))
                             }












#----------------------------a tree---------------------------------------------------------------------------------
# a tree structure used to store generated sequences (checking whether there are no sequences that are the same)
# based on the art of programming book


# function creating storage matrix - being given the first value, and pointing 
# there are three columns of the matrix - first row is , second row is, third row is the inserted value

newtree <- function(firstval,inc) {
                                  m <- matrix(rep(NA,inc*3),nrow=inc,ncol=3)
                                  m[1,3] <- firstval
                                  return(list(mat=m,nxt=2,inc=inc))
                                  }




# inserts newval into the subtree of tr, with the subtree's root being
# at index hdidx; note that return value must be reassigned to tr by the
# caller (including ins() itself, due to recursion)

  ins <- function(hdidx,tr,newval) {   
              dir <- if (newval <= tr$mat[hdidx,3]) 1 else 2 # which direction will this new node go, left or right?
                                                  # if null link in that direction, place the new node here, otherwise
                                                  # recurse
                     if (is.na(tr$mat[hdidx,dir])) {
                                          newidx <- tr$nxt # where new node goes
                                          
                                          if (tr$nxt == nrow(tr$mat) + 1) {# check for room to add a new element
                                                                          tr$mat <- rbind(tr$mat, matrix(rep(NA,tr$inc*3),nrow=tr$inc,ncol=3))
                                                                          }
     
                                          tr$mat[newidx,3] <- newval # insert new tree node
     
                                          tr$mat[hdidx,dir] <- newidx# link to the new node
                                          tr$nxt <- tr$nxt + 1 # ready for next insert
                                          return(tr)
                                                    } else tr <- ins(tr$mat[hdidx,dir],tr,newval)
                                    }



































#automatically generating the sequence with generated starting items and rules 


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
