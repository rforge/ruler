#-------------------------------------------------------------------------------------------------------------

require("grid")

# a grid can be of any size but it has to be a square
# FUNCTION FOR CREATING LIST OF VIEWPORTS 
# from top do bottom, from left to right 
# s'size' is a size of a matrix
# possible nesting viewpoints

createVp<-function(size=3){ 
                          vlist<-vector(mode="list",length=0)
                          v0<-viewport(layout=grid.layout(size, size, respect= diag(size)),name="vp00")
                          
                          for(i in 1:size){
                                          rowlist<-vector(mode="list",length=size)
                                          pushViewport(v0) #general viewport for whole grid
                                          for(j in 1:size){
                                                          rowlist[[j]]<-vpStack(v0,viewport(layout.pos.col=i,layout.pos.row=j, name=paste("vp",i,j,sep="")))                                               
                                                          }
                                          vlist<-c(vlist,rowlist)
                                          }
                          return(vlist) # a list of viewpoints for samll grids
                          }






m<-createVp()



for(i in 1:length(m)){
                      pushViewport(m[[i]])
                      
                      foreground<- foreGr[[sample(1:length(foreGr),1)]] #figure in the foreground
                      background<-backGr[[sample(1:length(backGr),1)]] #figure in the background
                      picture<-gTree(children=gList(background,foreground)) #object consisting of foregroung and background
                      
                      #picture <- editGrob(picture,gPath(childNames(picture)[[2]]), gp = gpar(lwd =4)) #//here should be a function changing parameters
                      
                      grid.draw(picture)
                      popViewport()
                                            
                      }





#---------------------------------------BASIC SHAPES ------------------------------------------------------------------

#FOREGROUND

rectangle<-rectGrob(name="rectangle",width = unit(0.8, "npc"), height = unit(0.8, "npc"))
circle<-circleGrob(name="circle",r=0.4)
triangle<-polygonGrob(name="triangle",x=c(0.1, 0.5, 0.9), y=c(0.1, 0.9, 0.1))
pentagon<-polygonGrob(name="pentagon",y=c(polygon.regular(5)[,1])*0.65+0.45,x=c(polygon.regular(5)[,2])*0.65+0.5)
star20<-polygonGrob(name="star20",x=c(0.5000000, 0.3763932, 0.7351141, 0.1763932, 0.8804226, 0.1000000, 0.8804226, 0.1763932, 0.7351141, 0.3763932, 0.5000000, 0.6236068, 0.2648859, 0.8236068,0.1195774, 0.9000000, 0.1195774, 0.8236068, 0.2648859, 0.6236068, 0.5000000),y=c(0.9000000, 0.1195774, 0.8236068, 0.2648859, 0.6236068, 0.5000000, 0.3763932, 0.7351141, 0.1763932, 0.8804226, 0.1000000, 0.8804226, 0.1763932, 0.7351141, 0.3763932, 0.5000000, 0.6236068, 0.2648859, 0.8236068, 0.1195774, 0.9000000),gp=gpar("alpha"=1))                    

foreGr<-list(rectangle,circle,triangle,pentagon,star20) #first plan shapes

#BACKGROUND

vertical6<-polylineGrob(name="line6v",x = unit(rep(seq(0,1,0.2),each=2), "npc"),
           y = unit(rep(c(0,1),6), "npc"),id.lengths=rep(2,6)) # 6vertical lines


backGr<-list(vertical6)#background shapes


#----------------------------------------------------------------------------------------------------------------

# 
# #generating basic grapical parameters
# 
# 
# 
# #rule1 - SUM OF TWO PREVIOUS ELEMENTS
# 
# #rule2 - EXQUISITE SUM OF TWO PREVIOUS ELEMENTS
# 
# generateMatrix <-function(){
#                               
#                               basicGrid(rectGrob()) # creating an empty 3*3 matrix
#                               for(i in 1:2){
#                                             for(j in 1:2){
#                                                           pushViewport(viewport(layout.pos.col=i,layout.pos.row=j))
#                                                           m<-sample(1:length(foreGr),1)#generating the shape
#                                                           n<- sample(1:length(backGr),1)
#                                                           grid.draw(editGrob(backGr[[n]],gp=gpar(alpha=sample(c(0,1),1),lwd=sample(c(2,4),1),lty=sample(1:6,1))))
#                                                           
#                                                           
#                                                           editGrob(foreGr[[m]])
#                                                           grid.draw(foreGr[[m]])
#                                                           popViewport()
#                                                           }
#                                             }
#                               
#                               }
# 
# 








