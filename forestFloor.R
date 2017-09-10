#####Forest floor####

library(forestFloor)
library(randomForest)
forFF = randomForest(Q,q,ntree=800, keep.inbag=T)
ff = forestFloor(forFF,Q,binary_reg = T,calc_np=T)
#print forestFloor
print(ff)
Col = fcol(ff,cols=1,outlier.lim = 2.5)
#plot partial functions of most important variables 
plot(ff,col=Col,plot_GOF = T)


#print forestFloor
print(ff) 

#by applying different colourgradient, interactions reveal themself 
Col = fcol(ff,3)
plot(ff,col=Col) 

#in 3D the interaction between X3 and X reveals itself completely
show3d(ff,3:4,col=Col,plot.rgl=list(size=5)) 

#although no interaction, a joined additive effect of X1 and X2
Col = fcol(ff,1:2,X.m=FALSE,RGB=TRUE) #colour by FC-component FC1 and FC2 summed
plot(ff,col=Col) 
show3d(ff,1:2,col=Col,plot.rgl=list(size=5)) 

#...or two-way gradient is formed from FC-component X1 and X2.
Col = fcol(ff,1:2,X.matrix=TRUE,alpha=0.8) 
plot(ff,col=Col) 
show3d(ff,1:2,col=Col,plot.rgl=list(size=5))
######



library(rgl)
show3d(ff,c(1,3),3,col=Col) 


show3d(ff,c(1,5),5,col=Col,plot_GOF = T) library(rgl); rgl.snapshot("3dPressure.png") 