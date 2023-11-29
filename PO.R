#Generate data with a binary variable and continuous
# no confounding 

library(bnlearn)

n<-1000
treat<-rbinom(n,1,0.5)

#Mediators that are counterfactuals

M1<-rnorm(n,3,1)
M0<-rnorm(n,2,1)

# Mediators that are counterfactuals that are deterministic functions of the mediators above

V1<-ifelse(M1<3,0,2)
V0<-ifelse(M0<2,0,1)

#Average treatment effect is equal to total effect of 2+0.5 = 2.5

Y1<-4+V1+rnorm(n,0,1)
Y0<-2+V0+rnorm(n,0,1)  

mean(Y1)-mean(Y0)

#Observed variables
Y<-ifelse(treat==1,Y1,Y0)
M<-ifelse(treat==1,M1,M0)
V<-ifelse(treat==1,V1,V0)

#Conditional independence properties of the counterfactuals cannot be depicted in a SWIG
#Y(t) indep V(t)|M(t)

####################################
#Drawing a DAG from the observed data

df<-as.data.frame(cbind(treat,M,V,Y))

cpdag = pc.stable(df)

plot(cpdag)

ci.test(Y,M,V)

#Drawing a DAG from the unobserved potential outcomes and the treatment variable (corresponding to the swig) 
# not a valid swig due to the deterministic function V1 of M1!

df2<-as.data.frame(cbind(treat,M1,V1,Y1))

cpdag2 = pc.stable(df2)

plot(cpdag2)

ci.test(Y1,M1,V1)

