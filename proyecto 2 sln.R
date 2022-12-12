# 10000 datos en dimension 5. Estos datos son covariables que determinan
# la distribucion gamma asociada a la severidad correspondiente
n=10000;d=5
z=matrix(rnorm(n*d),n,d)  
D=diag(c(2,5,3,3,2))  
xorig=t(D%*%t(z)) 

for(i in 1:n)xorig[i,]=xorig[i,]+D%*%rep(5,5)

xextend=cbind(xorig,rep(1,n)) 

alfa=c(2,1,1,0.5,3,-30)

beta=c(3,5,4,7,4,-160)

delta=c(-0.2,-0.1,0.2,0.3,-0.2,-2.2)

param=t(rbind(alfa,beta,delta)%*%t(xextend))

sev=rgamma(n,shape=param[,1],scale=param[,2])

#las pi
proba=exp(param[,3])/(1+exp(param[,3]))  

#indicadoras de siniestro
unos=rbinom(n,1,proba)
#p??rdida agregada
S=sum(unos*sev)

############# Ahora hacemos 1000 repeticiones #######
#1. distribucion empirica de S
perdidas=rep(0,1000) #1,000 valores de S para la distribucion emp????rica de a
for(j in 1:1000){ 
  n=10000;d=5
  z=matrix(rnorm(n*d),n,d)  
  D=diag(c(2,5,3,3,2))  
  xorig=t(D%*%t(z)) 
  for(i in 1:n)xorig[i,]=xorig[i,]+D%*%rep(5,5)
  xextend=cbind(xorig,rep(1,n)) 
  alfa=c(2,1,1,0.5,3,-30)
  beta=c(3,5,4,7,4,-160)
  delta=c(-0.2,-0.1,0.2,0.3,-0.2,-2.2)
  param=t(rbind(alfa,beta,delta)%*%t(xextend))
  sev=rgamma(n,shape=param[,1],scale=param[,2])
  
  #las pi
  proba=exp(param[,3])/(1+exp(param[,3]))  
  #indicadoras de siniestro
  unos=rbinom(n,1,proba)
  #p??rdida agregada
  perdidas[j]=sum(unos*sev)
}

#2.clustering de k-medias
#I. k= piso ln n
k=floor(log(n))
algoritmo_kmeans=kmeans(xorig, k, iter.max = 10, nstart = 1)
    #a. pi_j
    algoritmo_kmeans['size']
    tamanos_clusters=as.numeric(unlist(algoritmo_kmeans['size']))
    #c(2756,2316,2566,2362)
    pjs=tamanos_clusters/n
    sum(pjs)
    
    #b. q_j
    #inidicadoras de siniestros eran unos
    cluster=as.numeric(unlist(algoritmo_kmeans['cluster']))
    #cluster*unos
    num_siniestros_clusters=c()
    for (j in 1:k){
      num_siniestros_clusters[j]=sum(cluster*unos==j)
    }
    qjs=num_siniestros_clusters/tamanos_clusters
    #c. medias y varianzas
    #siniestros ocurridos en cada cluster
    siniestros_clusters=unos*sev
    #ahora clasificamos
    medias_siniestros=c()
    varianzas_siniestros=c()
    for (i in 1:k){
      medias_siniestros[i]=mean(siniestros_clusters[cluster == i & siniestros_clusters>0])
      varianzas_siniestros[i]=(sd(siniestros_clusters[cluster == i & siniestros_clusters>0]))^2
    }
    #mean(siniestros_clusters[cluster == 1 & siniestros_clusters>0])
    #mean(siniestros_clusters[cluster == 2  & siniestros_clusters>0 ])
    #mean(siniestros_clusters[cluster == 3  & siniestros_clusters>0])
    #mean(siniestros_clusters[cluster == 4  & siniestros_clusters>0])
    #d.Aj y Bj
    #usamos los momentos
    gamma_theta=varianzas_siniestros/ medias_siniestros
    gamma_alpha= medias_siniestros/gamma_theta

#II. k= 0.5 sqrt(n)
k=0.5*(sqrt(n))
    #repetir el proceso anterior
    algoritmo_kmeans=kmeans(xorig, k, iter.max = 10, nstart = 1)
    #a. pi_j
    algoritmo_kmeans['size']
    tamanos_clusters=as.numeric(unlist(algoritmo_kmeans['size']))
    #c(2756,2316,2566,2362)
    pjs=tamanos_clusters/n
    sum(pjs)
    
    #b. q_j
    #inidicadoras de siniestros eran unos
    cluster=as.numeric(unlist(algoritmo_kmeans['cluster']))
    #cluster*unos
    num_siniestros_clusters=c()
    for (j in 1:k){
      num_siniestros_clusters[j]=sum(cluster*unos==j)
    }
    qjs=num_siniestros_clusters/tamanos_clusters
    #c. medias y varianzas
    #siniestros ocurridos en cada cluster
    siniestros_clusters=unos*sev
    #ahora clasificamos
    medias_siniestros=c()
    varianzas_siniestros=c()
    for (i in 1:k){
      medias_siniestros[i]=mean(siniestros_clusters[cluster == i & siniestros_clusters>0])
      varianzas_siniestros[i]=(sd(siniestros_clusters[cluster == i & siniestros_clusters>0]))^2
    }
    
    #d.Aj y Bj
    #usamos los momentos
    gamma_theta=varianzas_siniestros/ medias_siniestros
    gamma_alpha= medias_siniestros/gamma_theta

#III.  para los valores intermedios tomamos (50-4)/4 y vamos sumando aproximadamente estas distancias
k= 15
    algoritmo_kmeans=kmeans(xorig, k, iter.max = 10, nstart = 1)
    #a. pi_j
    algoritmo_kmeans['size']
    tamanos_clusters=as.numeric(unlist(algoritmo_kmeans['size']))
    #c(2756,2316,2566,2362)
    pjs=tamanos_clusters/n
    sum(pjs)
    
    #b. q_j
    #inidicadoras de siniestros eran unos
    cluster=as.numeric(unlist(algoritmo_kmeans['cluster']))
    #cluster*unos
    num_siniestros_clusters=c()
    for (j in 1:k){
      num_siniestros_clusters[j]=sum(cluster*unos==j)
    }
    qjs=num_siniestros_clusters/tamanos_clusters
    #c. medias y varianzas
    #siniestros ocurridos en cada cluster
    siniestros_clusters=unos*sev
    #ahora clasificamos
    medias_siniestros=c()
    varianzas_siniestros=c()
    for (i in 1:k){
      medias_siniestros[i]=mean(siniestros_clusters[cluster == i & siniestros_clusters>0])
      varianzas_siniestros[i]=(sd(siniestros_clusters[cluster == i & siniestros_clusters>0]))^2
    }
    
    #d.Aj y Bj
    #usamos los momentos
    gamma_theta=varianzas_siniestros/ medias_siniestros
    gamma_alpha= medias_siniestros/gamma_theta
#IV
k= 26
    algoritmo_kmeans=kmeans(xorig, k, iter.max = 10, nstart = 1)
    #a. pi_j
    algoritmo_kmeans['size']
    tamanos_clusters=as.numeric(unlist(algoritmo_kmeans['size']))
    #c(2756,2316,2566,2362)
    pjs=tamanos_clusters/n
    sum(pjs)
    
    #b. q_j
    #inidicadoras de siniestros eran unos
    cluster=as.numeric(unlist(algoritmo_kmeans['cluster']))
    #cluster*unos
    num_siniestros_clusters=c()
    for (j in 1:k){
      num_siniestros_clusters[j]=sum(cluster*unos==j)
    }
    qjs=num_siniestros_clusters/tamanos_clusters
    #c. medias y varianzas
    #siniestros ocurridos en cada cluster
    siniestros_clusters=unos*sev
    #ahora clasificamos
    medias_siniestros=c()
    varianzas_siniestros=c()
    for (i in 1:k){
      medias_siniestros[i]=mean(siniestros_clusters[cluster == i & siniestros_clusters>0])
      varianzas_siniestros[i]=(sd(siniestros_clusters[cluster == i & siniestros_clusters>0]))^2
    }
    
    #d.Aj y Bj
    #usamos los momentos
    gamma_theta=varianzas_siniestros/ medias_siniestros
    gamma_alpha= medias_siniestros/gamma_theta

#V
k=37
    algoritmo_kmeans=kmeans(xorig, k, iter.max = 10, nstart = 1)
    #a. pi_j
    algoritmo_kmeans['size']
    tamanos_clusters=as.numeric(unlist(algoritmo_kmeans['size']))
    #c(2756,2316,2566,2362)
    pjs=tamanos_clusters/n
    sum(pjs)
    
    #b. q_j
    #inidicadoras de siniestros eran unos
    cluster=as.numeric(unlist(algoritmo_kmeans['cluster']))
    #cluster*unos
    num_siniestros_clusters=c()
    for (j in 1:k){
      num_siniestros_clusters[j]=sum(cluster*unos==j)
    }
    qjs=num_siniestros_clusters/tamanos_clusters
    #c. medias y varianzas
    #siniestros ocurridos en cada cluster
    siniestros_clusters=unos*sev
    #ahora clasificamos
    medias_siniestros=c()
    varianzas_siniestros=c()
    for (i in 1:k){
      medias_siniestros[i]=mean(siniestros_clusters[cluster == i & siniestros_clusters>0])
      varianzas_siniestros[i]=(sd(siniestros_clusters[cluster == i & siniestros_clusters>0]))^2
    }
    
    #d.Aj y Bj
    #usamos los momentos
    gamma_theta=varianzas_siniestros/ medias_siniestros
    gamma_alpha= medias_siniestros/gamma_theta
