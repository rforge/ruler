require("grid")
require("gridExtra") # not necessarily - you can just copy parameter result to draw shapes

#---------------------------------------BASIC SHAPES ------------------------------------------------------------------

#FOREGROUND

rectangle<-rectGrob(name="rectangle",width = unit(0.65, "npc"), height = unit(0.65, "npc"),gp=gpar(fill="transparent"))
#circle<-circleGrob(name="circle",r=0.4)
triangle<-polygonGrob(name="triangle",x=c(0.2, 0.5, 0.8), y=c(0.2, 0.8, 0.2),gp=gpar(fill="transparent"))
pentagon<-polygonGrob(name="pentagon",y=c(polygon.regular(5)[,1])*0.40+0.45,x=c(polygon.regular(5)[,2])*0.40+0.5,gp=gpar(fill="transparent"))
star20<-polygonGrob(name="star20",x=c(0.5000000, 0.3763932, 0.7351141, 0.1763932, 0.8804226, 0.1000000, 0.8804226, 0.1763932, 0.7351141, 0.3763932, 0.5000000, 0.6236068, 0.2648859, 0.8236068,0.1195774, 0.9000000, 0.1195774, 0.8236068, 0.2648859, 0.6236068, 0.5000000),y=c(0.9000000, 0.1195774, 0.8236068, 0.2648859, 0.6236068, 0.5000000, 0.3763932, 0.7351141, 0.1763932, 0.8804226, 0.1000000, 0.8804226, 0.1763932, 0.7351141, 0.3763932, 0.5000000, 0.6236068, 0.2648859, 0.8236068, 0.1195774, 0.9000000),gp=gpar("alpha"=1, fill="transparent"))                    

#grid.roundrect()

foreGr<-list(rectangle,triangle,pentagon,star20) #first plan shapes

#BACKGROUND

vertical6<-polylineGrob(name="line6v",x = unit(rep(seq(0,1,0.2),each=2), "npc"),
                        y = unit(rep(c(0,1),6), "npc"),id.lengths=rep(2,6)) # 6vertical lines
cross<-polylineGrob(x=unit(c(0.5,0.5,0,1),"npc"),y=unit(c(0,1,0.5,0.5),"npc"),id.lengths=c(2,2),name="cross")

blank<-nullGrob(name="blank")

backGr<-list(vertical6,cross)#background shapes




#--------------------------------GENERATING VIEWPORTS--------------------------------------------------------------------------------------
# a grid can be of any size but it has to be a square
# FUNCTION FOR CREATING LIST OF VIEWPORTS 
# from top do bottom, from left to right 
# s'size' is a size of a matrix
# possible nesting viewpoints


createVp<-function(size){ 
  vlist<-vector(mode="list",length=0)
  v0<-viewport(layout=grid.layout(size, size, respect= diag(size)),name="vp00")
  
  
  for(i in 1:size){
    rowlist<-vector(mode="list",length=size)
    pushViewport(v0) #general viewport for whole grid
    
    for(j in 1:size){
      rowlist[[j]]<-vpStack(v0,viewport(layout.pos.col=i,layout.pos.row=j,clip="on", name=paste("vp",i,j,sep="")))                                          
    }
    
    vlist<-c(vlist,rowlist)
  }
  return(vlist) # a list of viewpoints for samll grids
}


#------------------------------- GENERATING PICTURES---------------------------------------------------------------------------------
#function returning indexes of m matrix of objects forming columns and rows
#'size' size of a matrix
index<-function(size){
  imat<-matrix(NA,size,size)
  for(i in 1:size){
    for(j in 1:size){
      imat[i,j]<-i+(j-1)*size
    }
  }
  return(imat)
}



#function thet gives indexes of pictures 
dir_index<-function(direction,which,size){
                                          imat<-index(size)#a matrix of all indexes
                                          if(direction==1){ #columns
                                                          k<-imat[,which]
                                                          }
  
                                          if(direction==2){ #rows
                                                          k<-imat[which,]
                                                          }
                                                              
                                          return(k)
                                          }


#FUNCTION FOR FILLING IN SHAPE MATRIX
#'mtofill' matrix to be filled with shapes
#'size' of a matrix
#'direction' 1 is for columns, 2 is for rows // where to fix shapes
fill_in_matrix<-function(direction,mtofill,size){
                                          fg<-vector(mode="list", length=size^2) #a list of foreground shapes
                                          bg<-vector(mode="list", length=size^2)#a list of background shapes                                          
                                          if(mtofill==2){k=foreGr; mtofill=fg}else{k=backGr;mtofill=bg}
                                                                                  
                                                
                                          for(i in 1:size){                                                             
                                                        p<-k[[sample(1:length(k),1)]]#generating a picture for foreground
                                                                                                        
                                                        if(!is.null(direction)){
                                                              v<-dir_index(direction=direction,which=i,size=size)#getting indexes of pictures forming a specyfied row/column                                                              
                                                              for(j in v) mtofill[[j]]<-p                        
                                                        }else{ for(j in 1:size^2)mtofill[[j]]<-k[[sample(1:length(k),1)]]}
                                                                         }
                                          
                                            return(mtofill)
                                                                }
                                       
                                                  




#'size' of the matrix
#'f' f=1 fixed foreground in columns, f=2 fixed foreground in rows, f=NULL - foreground random
#'b' b=1 fixed background in columns, b=2 fixed background in rows, b=NULL - background random

firstMatrix<-function(f=NULL,b=NULL,size=3){
      m<-createVp(size=size) #viewports where to draw small pictures
      
      pic<-vector(mode="list", length=size^2) # a list of pictures (consisting of foregropund and background)
      
      
      fg<-fill_in_matrix(direction=f,mtofill=2,size=size)
      bg<-fill_in_matrix(direction=b,mtofill=1,size=size)
      
      
      for(i in 1:size^2){
                        foreground<-gTree(children=gList(fg[[i]]),name=paste("foreground",i,sep=""))
                        background<-gTree(children=gList(bg[[i]]),name=paste("background",i,sep="i"))
                        pic[[i]]<-gTree(children=gList(foreground,background), name=paste("picture",i,sep=""))
                        }
      
      return(list(viewports=m,pictures=pic))
                                  }




#-------------------------------------------------OBJECTS-----------------------------------------------------------

setOldClass("grob") # allows me to use S3 grob objects as slot of S4 class
setOldClass("gTree") # allows me to use S3 gTree objects as slot of S4 class



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


#------------------------------------------RULES-----------------------------------------------------
#ROTATION RULE [1]


#figure=1 background, fig=2 foreground
#'progression' do you want to apply progressive rule (if !0 than rotation will be progressive)


setClass("Rotation",
         contains="SingleRule",
         representation(parVal="numeric",figure="numeric",progression="numeric"), 
         S3methods=TRUE)


setMethod("calculateSpecific",signature(x="Rotation", y="gTree"),
          function(x,y){
            if(x@figure==2){fg<-getGrob(y,childNames(y)[2])
                            fg<-editGrob(fg,vp=viewport(angle=x@parVal))
                            bkg<-getGrob(y,childNames(y)[1])
                            
            }else{
              bkg<-getGrob(y,childNames(y)[1])
              bkg<-editGrob(bkg,vp=viewport(angle=x@parVal))
              fg<-getGrob(y,childNames(y)[2])
              
            }
            
            return(gTree(children=gList(bkg,fg)))
          })


#LWD RULE [2]
setClass("ChangeLwd",
         contains="SingleRule",
         representation(parVal="numeric",figure="numeric",progression="numeric"), #figure=1 background, fig=2 foreground
         S3methods=TRUE)


setMethod("calculateSpecific",signature(x="ChangeLwd", y="gTree"),
          function(x,y){
            
            if(x@figure==2){fg<-getGrob(y,childNames(y)[2])
                            fg<-editGrob(fg,gp=gpar(lwd=x@parVal))
                            bkg<-getGrob(y,childNames(y)[1])
                            
            }else{
              bkg<-getGrob(y,childNames(y)[1])
              bkg<-editGrob(bkg,gp=gpar(lwd=x@parVal))
              fg<-getGrob(y,childNames(y)[2])
              }
           return(gTree(children=gList(bkg,fg)))
            
          })



#LTY RULE [3]
setClass("ChangeLty",
         contains="SingleRule",
         representation(parVal="numeric",figure="numeric"), #figure=1 background, fig=2 foreground
         S3methods=TRUE)


setMethod("calculateSpecific",signature(x="ChangeLty", y="gTree"),
          function(x,y){
            if(x@figure==2){fg<-getGrob(y,childNames(y)[2])
                            fg<-editGrob(fg,gp=gpar(lty=x@parVal))
                            bkg<-getGrob(y,childNames(y)[1])
            }else{
              bkg<-getGrob(y,childNames(y)[1])
              bkg<-editGrob(bkg,gp=gpar(lty=x@parVal))
              fg<-getGrob(y,childNames(y)[2])}
            
            return(gTree(children=gList(bkg,fg)))
          })


#COLOR RULE[4]
setClass("ChangeColor",
         contains="SingleRule",
         representation(parVal="numeric",figure="numeric"), #figure=1 background, fig=2 foreground
         S3methods=TRUE)



setMethod("calculateSpecific",signature(x="ChangeColor", y="gTree"),
          function(x,y){
            if(x@figure==2){fg<-getGrob(y,childNames(y)[2])
                            fg<-editGrob(fg,gp=gpar(col=x@parVal))
                            bkg<-getGrob(y,childNames(y)[1])
            }else{
              bkg<-getGrob(y,childNames(y)[1])
              bkg<-editGrob(bkg,gp=gpar(col=x@parVal))
              fg<-getGrob(y,childNames(y)[2])}
            
            return(gTree(children=gList(bkg,fg)))
          })

#FILL RULE[5]  - ONLY FOR FOREGROUND
setClass("ChangeFill",
         contains="SingleRule",
         representation(parVal="character",figure="numeric"), #figure=1 background, fig=2 foreground
         S3methods=TRUE)



setMethod("calculateSpecific",signature(x="ChangeFill", y="gTree"),
          function(x,y){
            fg<-getGrob(y,childNames(y)[2])
            fg<-editGrob(fg,gp=gpar(fill=x@parVal))
            bkg<-getGrob(y,childNames(y)[1])
            
            
            return(gTree(children=gList(bkg,fg)))
          })


#IDENTICAL RULE[6]
setClass("IdenticalPicture",
         contains="SingleRule",
         S3methods=TRUE)


setMethod("calculateSpecific",signature(x="IdenticalPicture", y="gTree"),
          function(x,y){return(y)})


#EXECUTING RULES REFERING TO SINGLE ARGUMENT

calculate <- function(x,y,z=NULL){
  return(y)
}


setMethod("calculate",signature(x="SingleRule", y="gTree"), #both [1] and [2] inherit from class 'SingleRule'
          function(x, y){
            result<-y 
            if(!is.null(x@previousRule)){ # if there are some rules nested inside 'x'
              result <- calculate(x@previousRule,result) 
            }
            return(calculateSpecific(x,result)) # if there are no more nested functions, execute
          })


#-----------------------DOUBLE RULES ----------------------------------------------------------------

setClass("DoubleRule", representation = representation(firstRule="SingleRule", secondRule="SingleRule",nextSingle="SingleRule"),
         S3methods=TRUE)


#----sum two pictures [1]------------------------------------------------
setClass("AddTwoPictures", contains="DoubleRule",S3methods=TRUE)


setMethod("calculateSpecific",signature(x="AddTwoPictures", y="gTree", z="gTree"),
          function(x,y,z){
            fg1<-getGrob(y, childNames(y)[2])
            bkg1<-getGrob(y, childNames(y)[1])
            fg2<-getGrob(z, childNames(z)[2])
            bkg2<-getGrob(z, childNames(z)[1])
            fg<-gList(fg1,fg2)
            bkg<-gList(bkg1,bkg2)
            foreground<-gTree(children=fg)
            background<-gTree(children=bkg)
            return(gTree(children=gList(background,foreground)))
          })



#EXECUTING RULES REFERING TO DOUBLE  ARGUMENT
setMethod("calculate",signature(x="DoubleRule", y="gTree", z="gTree"),
          function(x, y, z){
            firstArg <- y #first element of the matrix
            secondArg <-z #second element of the matrix 
            
            
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

#------------------------------------------------APPLICATION OF RULES-----------------------------------------------

#function creating an empty matrix in which rules for every row and column will be stored
schedule<-function(size){ 
  
  index_m<-index(size) #which pictures are to be changed
  rowRules<-vector(mode="list", length=size)
  row_index<-vector(mode="list", length=size)#indices of pictures in rows
  for(i in 1:size){row_index[[i]]<-index_m[i,]}
  rowRules<-list(rowRules=rowRules,row_index=row_index)
  
  
  colRules<-vector(mode="list",length=size)
  col_index<-vector(mode="list", length=size)# indices of pictures in columns
  for(i in 1:size){col_index[[i]]<-index_m[,i]}
  colRules<-list(colRules=colRules,col_index=col_index)
  
  colrowRules<-list(column=colRules,rows=rowRules)
  
  return(colrowRules)                        
}


setClassUnion("GrRules", members=c("SingleRule", "DoubleRule"))


#a class specyfying rules for columns and rows
#'rule' can be either an object of class 'SingleRule' or 'DoubleRule'
#'direction' 1=column, 2=row
#'which' which row/column to affect by a rule 

setClass("ColRowRule", representation = representation(direction="numeric", which="numeric",rule="GrRules"), S3methods=TRUE)
setClass("MatrixRulesList", representation = representation(rulelist="list"), S3methods=TRUE)

#function returning an object of class MatrixRulesList" (which basically is a list of objects of class "ColRowRule")
defineMatrixRules<-function(...){
  arglist<-list(...)
  rlist<-list()#an empty list on which objects of class ColRowRule will be stored
  k=1
  for(i in 1:length(arglist)){
    if(inherits(arglist[[i]],"ColRowRule"))rlist[[k]]<-arglist[[i]]
    k=k+1                                                             
  }
  
  return(new("MatrixRulesList",rulelist=rlist))
}




applyMatrixRules<-function(f,b,size,rulelist){}

setClassUnion("nullORnumeric", members=c("NULL", "numeric"))

setMethod("applyMatrixRules",signature(f="nullORnumeric",b="nullORnumeric",size="numeric", rulelist="MatrixRulesList"),
          function(f,b,size,rulelist){
            m<-firstMatrix(f,b,size) #list of pictures and their viepoints(generated randomly)
            rules_to_apply<-schedule(size) # a list of rules to apply to particular rowsand columns (as default rule is NULL)
            
            rl<-rulelist@rulelist # getting the rulelist slot - this is my list of defined rules
            
            
            for(i in 1:length(rl)){ #modifying rules_to_apply list by rules defined in ruleList
              direction=rl[[i]]@direction
              which=rl[[i]]@which
              rule=rl[[i]]@rule
              
              
              rules_to_apply[[direction]][[1]][[which]]<-rule                                           
            }
            
            
            #executing rules for columns
            for(i in 1:length(rules_to_apply[[1]][[1]])){
              ind<-rules_to_apply$column$col_index[[i]]
              r<-rules_to_apply$column$colRules[[i]]
              if(!is.null(r)){
                
                        if(inherits(r,"SingleRule")){
                                for(j in ind) m[[2]][[j]]<-calculate(x=r,y=m[[2]][[j]])
                                       #if("progression"%in%slotNames(r))r@parVal<-r@parVal+r@progression}
                                                    }
                        
                        if(inherits(r,"DoubleRule")){
                              for(j in ind){ m[[2]][[j]]<-calculate(x=r,y=m[[2]][[j-size]],z=m[[2]][[j-2*size]])
                                            if("progression"%in%slotNames(r))r@parVal<-r@parVal+r@progression}
                                          }
                              }
                        
                          }
            
            #executing rules for rows
            for(i in 1:length(rules_to_apply[[2]][[1]])){
              
              ind<-rules_to_apply$rows$row_index[[i]]
              r<-rules_to_apply$rows$rowRules[[i]]
              if(!is.null(r)){
                
                
                        if(inherits(r,"SingleRule")){
                            for(j in ind) {m[[2]][[j]]<-calculate(x=r,y=m[[2]][[j]])
                                           if("progression"%in%slotNames(r))r@parVal<-r@parVal+r@progression}
                              }
                        
                        if(inherits(r,"DoubleRule")){
                          for(j in ind) {m[[2]][[j]]<-calculate(x=r,y=m[[2]][[j-1]],z=m[[2]][[j-2]])
                                        if("progression"%in%slotNames(r))r@parVal<-r@parVal+r@progression}
                          }
                        
                        
                        }
            }
            
            
            
            return(m)
            
            
            
          })



#------------------------------------DRAWING MATRIX-------------------------------------------------------------------
#'|1||4||7|
#'|2||3||8|
#'|3||6||9|

#size=3

draw_matrix<-function (m){
                      size=sqrt(length(m[[2]]))
                      for(i in 1:size^2){
                      #m<-firstMatrix(size)
                      pushViewport(m[[1]][[i]])
                      grid.draw(m[[2]][[i]])
                      popViewport()
                                        }
                          }


#=====================================================================================
#========EXAMPLES OF USAGE ===========================================================
#=====================================================================================


#STEP 1 - create some basic rules
#STEP 2 - combine the rules if neccessary
#STEP 3 - create an object of RowColRule
#STEP 4 - make a list of rules you want to apply to your matrix
#STEP 5 - use the RuleList object to change the pre-generated matrix
#STEP 6 - draw Raven like matrix


## EXAMPLE 1 

#[1]
b<-new("ChangeColor", parVal=4,figure=2) #a rule changing the color of the figure in the foreground to blue
c<-new("Rotation", parVal=-30, figure=1,progression=0, previousRule=b) # a rule rotating a backround & changing width and color of the foreground figure
d<-new("ChangeLwd", parVal=1,figure=2,progression=5,previousRule=c) # a rule changing the width of lines of figure in foreground and its color to blue

d_row3<-new("ColRowRule",direction=2,which=3,rule=d) #rule 'd' will be applied to third row

#[2]
e<-new("ChangeColor", parVal=7,figure=1)#changing color of the foreground
f<-new("ChangeLty", parVal=4,figure=2,previousRule=e)#changing "lty" parameter

f_row2<-new("ColRowRule",direction=2,which=2,rule=f) #rule 'f' will be applied to second row

#[3]
g<-new("ChangeColor",parVal=5,figure=2,previousRule=d)
g_row1<-new("ColRowRule",direction=2,which=1,rule=g)#rule 'f' will be applied to second row

#[4]
h <-new("ChangeLty", parVal=4,figure=2)
h_col3<-new("ColRowRule",direction=1,which=3,rule=h)


bbb<-defineMatrixRules(d_row3,f_row2,g_row1,h_col3)#creating a list of rules to apply
m<-applyMatrixRules(f=1,b=2,size=3, rulelist=bbb)#fixing foreground in columns and background in rows

draw_matrix(m)

grid.newpage()


##EXAMPLE 2

h<-new("AddTwoPictures") #basic doubleRule for adding two previous pictures

h_col3<-new("ColRowRule",direction=1,which=3,rule=h)# I want to use this rule in third column
h_row3<-new("ColRowRule",direction=2,which=3,rule=h)# I also want to use this rule in thord row

bbb<-defineMatrixRules(h_col3,h_row3)#creating a list of rules to apply
m<-applyMatrixRules(f=NULL,b=NULL,size=3, rulelist=bbb) # setting shapes (background and foreground) in rows and columns as random

draw_matrix(m)
grid.newpage()



##EXAMPLE 3


h<-new("AddTwoPictures",nextSingle=f) #basic doubleRule for adding two previous pictures
h_col3<-new("ColRowRule",direction=1,which=3,rule=h)# I want to use this rule in third column
h_row3<-new("ColRowRule",direction=2,which=3,rule=h)# I also want to use this rule in thord row

bbb<-defineMatrixRules(h_col3,h_row3)#creating a list of rules to apply
m<-applyMatrixRules(f=NULL,b=NULL,size=3, rulelist=bbb) # setting shapes (background and foreground) in rows and columns as random

draw_matrix(m)
