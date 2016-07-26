library(igraph)
library(igraphdata)
library(plyr)
library(mapplots)
library(maps)
library(mapdata)
library(maptools)
library(rworldmap)
library(rworldxtra)
library(geosphere)
library(choroplethr)
library(choroplethrMaps)
library(ggmap)
library(ggplot2)
library(vegan)
library(MUCflights)
library(raster)
library(rgdal)
library(rgeos)
library(raster)
library(data.table)
library(devtools)
library(PBSmapping)
library(SOMbrero)
library(cluster)
library(PBSmapping)
library(reshape2)
library(RColorBrewer)
library(rgl)
data(USairports)

plot(USairports, vertex.size = 0)

sort(degree(USairports), decreasing=T)

# Les vols
USairport.edges <- get.data.frame(USairports, what="e")
USairport.edges[1:10,]

# Les aeroports
USairport.vertices <- get.data.frame(USairports, what="v")
USairport.vertices[1:10,]

# Distance des plus courts chemins sur la plus grande composante connexe
is.connected(USairports)
clusters.USairport <- clusters(USairports)
USairport.connex <- subgraph(USairports,
                             v=which(clusters.USairport$membership==1))

plot(USairport.connex, vertex.size = 0)

dist.USairport <- shortest.paths(USairport.connex)



# Le nombre de compagnie ayant opéré sur la période
compagnies_nbre<-aggregate(USairport.edges[,c("Departures","Seats","Passengers","Distance")],
                      by=list(USairport.edges$Carrier), "sum")
paste(nrow(compagnies_nbre))
# il y a 118 compagnies qui ont opér sur le territoire amériacan 
paste(sum(compagnies_nbre$Departures))#Nombre de vols effectué
paste(sum(compagnies_nbre$Passengers))#Nombre de passagers transportés


######### les type d'avions par compagnie######

compagnies<-aggregate(USairport.edges[,c("Departures","Seats","Passengers","Distance")],
                      by=list(USairport.edges$Carrier,USairport.edges$Aircraft), "sum")

# Type d'avion par compagnies
comp_aircraft<-count(compagnies, "Group.1")

#slices <- comp_aircraft$freq
#lbls <- comp_aircraft$Group.1
#pie(slices, labels = lbls, main="Pie Chart")

comp_aircraft<-comp_aircraft[order(-comp_aircraft$freq),]
comp_aircraft[1:10,]
## les compagnie avec un grand nombre de type d'avion sont delta ailines 
## Continental Air Lines US et Airways Inc Avjet Corporation



## LE nombre de vol par compagnie (départures)
compagnies<-aggregate(compagnies[,c("Departures","Seats","Passengers","Distance")],
                      by=list(compagnies$Group.1), "sum")
compagnies<-compagnies[order(-compagnies$Departures),]
# Le nombre de vol des 10 plus grandes compagnie
compagnies[1:10,c(1,2)]
# LE nombre de passager
compagnies<-compagnies[order(-compagnies$Passengers),]
compagnies[1:10,c(1,4)]

a<-data.frame(compagnies[1:10,c(1,4)])

# LE nombre de miles
compagnies<-compagnies[order(-compagnies$Distance),]
compagnies[1:10,c(1,5)]
a<-data.frame(compagnies[1:10,c(1,5)])
compagnies$ratio<-(compagnies$Passengers/compagnies$Seats)*100

a<-data.frame(compagnies[1:10,c(1,5)])

## Récupération des coordonnés des aéroport dans un source externe

airports<- read.csv("http://datasets.flowingdata.com/tuts/maparcs/airports.csv", header=TRUE)
air = airports[which(airports$country == "USA"),] 
#air<-air[,c(5,7,8)]
#colnames(air)[1] <- "from"
#total <- merge(USairport.edges,air,by="from")
#colnames(air)[1] <- "to"
#colnames(air)[2] <- "lato"
#colnames(air)[3] <- "lonto"
#total <- merge(total,air,by="to")
xlim <- c(-171.738281, -56.601563)
ylim <- c(12.039321, 71.856229)


                    ###############################################
                    #Visualisation du traffic aérien des cinq     # 
                    #            granded compagnies               #
                    ###############################################


# Graph pas trop lisible on le colore selon le nombre de departs
## Representations des lignes aériennes pour les 5 grandes compagnies

pal <- colorRampPalette(c("#f2f2f2", "darkred"))
colors <- pal(50)

graph<- function(comp)
{

map("world", col="darkseagreen", fill=TRUE, bg="#000000", lwd=0.05, xlim=xlim, ylim=ylim)
fsub <- USairport.edges[USairport.edges$Carrier == comp,]
maxcnt <- max(fsub$Departures)
for (j in 1:length(fsub$Carrier)) {
  air1 <- airports[airports$iata == fsub[j,]$from,]
  air2 <- airports[airports$iata == fsub[j,]$to,]
  
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE, breakAtDateLine=TRUE)
  colindex <- round( (fsub[j,]$Departures/maxcnt) * length(colors) )
  
  lines(inter, col=colors[colindex], lwd=0.8)
}

}

graph("Southwest Airlines Co.")
graph("Delta Air Lines Inc.")
graph("SkyWest Airlines Inc.")
graph("American Airlines Inc.")
graph("American Eagle Airlines Inc.")
graph("US Airways Inc.")





## Grandes aéroports de départs
departs<-aggregate(USairport.edges[,c("Departures","Seats","Passengers","Distance")],
                     by=list(USairport.edges$from), "sum")
colnames(departs)[1] <- "iata"
departs<- merge(departs,airports,by="iata",all.x=TRUE)

#Les grans hubs
departs<-departs[order(-departs$Departures),]
departs[1:10,c(1,2)]
a<-data.frame(departs[1:10,c(1,2)])
departs<-departs[order(-departs$Passengers),]
departs[1:10,c(1,4)]
a<-data.frame(departs[1:10,c(1,4)])

# LE nombre d'aéroport desservi par aéroport de départ

departs2<-aggregate(USairport.edges[,c("Departures","Seats","Passengers","Distance")],
                   by=list(USairport.edges$from, USairport.edges$to), "sum")
aero_dep<-count(departs2, "Group.1")
aero_dep<-aero_dep[order(-aero_dep$freq),]
aero_dep[1:10,]

# récupération de la carte des USA
usa <- map_data("usa")
#On ajoute le canada et Mexico pour tenir compte de l'Alaska et des iles
usa<- subset(usa,usa$region %in% c("USA", "Canada", "Mexico")) 

# dessin de la carte et ajout des points
worldmap = map_data("world")
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
worldmap = clipPolys(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)

statemap = map_data("state")
setnames(statemap, c("X","Y","PID","POS","region","subregion"))
usa = clipPolys(statemap, xlim=xlim,ylim=ylim, keepExtra=TRUE)
#Aéroports de grand départs
ggplot() +
  coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=worldmap,aes(X,Y,group=PID),
               fill = "darkseagreen",color="grey50") +
  geom_polygon(data=usa,aes(X,Y,group=PID),
               fill = "darkseagreen",color="grey50") +
  labs(y="",x="") +
  theme_bw()+
  geom_point(data = departs, aes(x = long, y = lat, size =Departures), color = "red") 

# aéroports d'affluence
ggplot() +
  coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=worldmap,aes(X,Y,group=PID),
               fill = "darkseagreen",color="grey50") +
  geom_polygon(data=usa,aes(X,Y,group=PID),
               fill = "darkseagreen",color="grey50") +
  labs(y="",x="") +
  theme_bw()+
  geom_point(data = departs, aes(x = long, y = lat, size = Passengers), color = "blue") 



                ###############################################
                #Visualisation du traffic aérien au départ des# 
                #               grands aéroports              #
                ###############################################




## Mouvements selon les grands aéroports
pal <- colorRampPalette(c("#f2f2f2", "blue"))
colors <- pal(20)

## Mouvements au départ de l'aéroport d'ATLANTA

graph1<- function(aero)
{

map("world", col="darkseagreen", fill=TRUE, bg="#000000", lwd=0.05, xlim=xlim, ylim=ylim)
fsub <- USairport.edges[USairport.edges$from == aero,]
maxcnt <- max(fsub$Departures)
for (j in 1:length(fsub$from)) {
  air1 <- airports[airports$iata == fsub[j,]$from,]
  air2 <- airports[airports$iata == fsub[j,]$to,]
  
  inter <- gcIntermediate(c(air1[1,]$long, air1[1,]$lat), c(air2[1,]$long, air2[1,]$lat), n=100, addStartEnd=TRUE, breakAtDateLine=TRUE)
  colindex <- round( (fsub[j,]$Departures/maxcnt) * length(colors) )
  
  lines(inter, col=colors[colindex], lwd=0.5)
}
}

graph1("ATL")
graph1("ORD")
graph1("DEN")
graph1("DFW")


                  ###############################################
                  #Visualisation multidimensionnelle des données# 
                  #         par projection non linéaire         #
                  ###############################################




#Récupération de la partie connexe du graph
USairport.connex.edges <- get.data.frame(USairport.connex, what="e")
USairport.connex.edges[1:10,]

# Les aeroports
USairport.connex.vertices <- get.data.frame(USairport.connex, what="v")
USairport.connex.vertices[1:10,]


#mds sur le graph
aeroport.mds <- cmdscale(dist.USairport, k = 3, eig = T)
scatterplot3d(aeroport.mds$points, main="Visualisation en 3 dimension", ,xlab="",ylab="",zlab="",
              highlight.3d=TRUE)

aeroport.mds <- cmdscale(dist.USairport, k = 2, eig = T)
plot(aeroport.mds$points,type="n",xlab="",ylab="",axes=T)
text(aeroport.mds$points,labels="o", main="Projection par MDS")

plot(aeroport.mds$points,type="n",xlab="",ylab="",axes=T)
text(aeroport.mds$points,labels=row.names(dist.USairport))
length(aeroport.mds$eig)
aeroport.mds$eig[1:10]

#sammon map
aeroport.nlm<-sammon(d=dist.USairport, y = jitter(cmdscale(dist.USairport), 2))
plot(aeroport.nlm$points,type="n",xlab="",ylab="",axes=T)
text(aeroport.nlm$points,labels=row.names(dist.USairport) )


#Isomap
aeroport.iso <- isomap(dist.USairport,ndim=2,k=5)
plot(aeroport.iso$points,type="n",xlab="",ylab="",axes=T)
text(aeroport.iso$points,labels=row.names(dist.USairport) )

#Comparaison des projections

# Distance sur la projection MDS avec un voisinage de 3
aeroport.dist.mds <- as.matrix(dist(aeroport.mds$points,method="euclidean",diag=T,upper=T))
MRRE(dist.USairport , aeroport.dist.mds , 3)
# Distance sur la projection Sammon
aeroport.dist.nlm <- as.matrix(dist(aeroport.nlm$points,method="euclidean",diag=T,upper=T))
MRRE(dist.USairport , aeroport.dist.nlm, 3)
# Distance sur la projection Isomap
aeroport.dist.iso  <- as.matrix(dist(aeroport.iso$points,method="euclidean",diag=T,upper=T))
MRRE(dist.USairport, aeroport.dist.iso, 3)



                    ###############################
                    # Classification des aéroports# 
                    ###############################



#Récupération des coordonnées du MDS
aeroport<-as.data.frame(aeroport.mds$points)

# Normalisation
vect.norm <- function(vect)
{
  vect <- (vect-mean(vect))/sd(vect)
  return(vect)
}

aeroport.norm<-apply(aeroport,2,vect.norm)

#CAH sur les coordonnées récupérés
aeroport.hc <- hclust(dist(aeroport.norm,method="euclidean")^2,method="ward.D")
plot(aeroport.hc)
plot(sort(aeroport.hc$height, decreasing=T)[1:15])
R2 <- 1-cumsum(aeroport.hc$height/sum(aeroport.hc$height))
plot(sort(R2)[1:15])
#Découpage en quatre classes
aeroport.clust <-cutree(aeroport.hc,k=4)

# Annova sur la classification
aeroport.aov <- aov(aeroport.clust~aeroport[,1]+aeroport[,2])
summary(aeroport.aov)

plot(aeroport.mds$points,type="p",xlab="",ylab="",axes=T, col=aeroport.clust,pch=8)

# Récupération des aéroports avec leur classe
classe<-as.data.frame(aeroport.clust)
classe$from=row.names(classe)


# Récupération des aéroports de départs à partir des arretes du graph
aeroport.connex<-aggregate(USairport.connex.edges[,c("Departures","Seats","Passengers","Distance")],
                             by=list(USairport.connex.edges$from), "sum")

aeroport.connex<- rename(aeroport.connex, c(Group.1="from"))

total <- merge(aeroport.connex,classe,by="from")
aeroport.aov <- aov(total[,6]~total[,2]+total[,4]+total[,5])
summary(aeroport.aov)

#Analyse des classes par les départs, les passagers et les distances
table(total$aeroport.clust)
a<-as.data.frame(total[total$aeroport.clust==1,])
a<-a[order(-a$Departures),]
a[1:10,]

a<-as.data.frame(total[total$aeroport.clust==2,])
a<-a[order(-a$Departures),]
a[1:10,]
a<-a[order(-a$Passengers),]
a[1:10,]
summary(a)
summary(total[total$aeroport.clust==2,])
summary(total[total$aeroport.clust==3,])
summary(total[total$aeroport.clust==4,])

by(total$Departures, total$aeroport.clust, mean, na.rm=TRUE)

by(total$Passengers, total$aeroport.clust, mean, na.rm=TRUE)

by(total$Distance, total$aeroport.clust, mean, na.rm=TRUE)



#Carte auto adaptatives de Kohonen

set
dist.USairport.som <- trainSOM(x.data = dist.USairport, type = "relational", nb.save = 50)
plot(dist.USairport.som, what = "energy")
plot(dist.USairport.som)

plot(dist.USairport.som, what = "obs", type = "names",scale=c(1,0.7))
plot(dist.USairport.som, what = "add", type = "graph", var = USairport.connex)
plot(dist.USairport.som, what = "prototypes", type = "lines")
plot(dist.USairport.som, what = "prototypes", type = "radar")
plot(dist.USairport.som, what = "prototypes", type = "poly.dist", print.title = TRUE)
plot(dist.USairport.som, what = "prototypes", type = "smooth.dist")
plot(dist.USairport.som, vertex.label.color = rainbow(81)[dist.USairport.som$clustering], vertex.size = 0)
legend(x = "left", legend = 1:81, col = rainbow(81), pch = 19)
plot(superClass(dist.USairport.som))
sc.airport <- superClass(dist.USairport.som, k = 3)
summary(sc.airport)

plot(sc.airport)
plot(sc.airport, type = "hitmap", plot.legend = TRUE)
plot(sc.airport, type = "grid", plot.legend = TRUE)

projectIGraph(sc.airport,USairport.connex)
par(mar=rep(0,4))
plot(sc.airport,type="projgraph",variable=USairport.connex,s.radius=2)

plot(USairport.connex, vertex.size = 0, 
     vertex.label.color = brewer.pal(4, "Set2")[sc.airport$cluster[dist.USairport.som$clustering]])
legend(x = "left", legend = paste("SC", 1:4), col = brewer.pal(4, "Set2"), pch = 19)

a<-as.data.frame(dist.USairport.som$clustering)
a$clustering2<-sc.airport$cluster[dist.USairport.som$clustering]
a<-a[order(-a$clustering2),]

a$from=row.names(a)

total2 <- merge(total,a,by="from")
summary(total2[total2$clustering2==1,])
summary(total2[total2$clustering2==2,])
summary(total2[total2$clustering2==3,])

plot(aeroport.mds$points,type="p",xlab="",ylab="",axes=T, col=a$clustering2,pch=8)





compagnie.connex<-aggregate(USairport.connex.edges[,c("Departures","Seats","Passengers","Distance")],
                            by=list(USairport.connex.edges$Carrier), "sum")
compagnie.connex<- rename(compagnie.connex, c(Group.1="compagnie"))

data_train <- compagnie.connex
row.names(data_train)=data_train$compagnie
data_train <- data_train[,-c(1,3)]

data_train_matrix <- as.matrix(scale(data_train))

# Paramétrage de la topologie

som_grid <- somgrid(xdim = 6, ydim=6, topo="hexagonal")

#Définition des options,
# taux d'apprentissage et voisinage des noeuds
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=200, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE,
                 n.hood="circular" )

plot(som_model, type="changes")

plot(som_model, type="count")

plot(som_model, type="dist.neighbours")

plot(som_model, type="codes")

coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}
par(mfrow=c(2,2))
plot(som_model, type = "property", property = som_model$codes[,1], 
     main="nombre de vols", palette.name=coolBlueHotRed)
plot(som_model, type = "property", property = som_model$codes[,2], 
     main="Passagers", palette.name=coolBlueHotRed)

plot(som_model, type = "property", property = som_model$codes[,3], 
     main="Distance", palette.name=coolBlueHotRed)
var <- 2 #var a ploter
var1<-1
var2<-3
par(mfrow=c(2,2))
var_unscaled <- aggregate(as.numeric(data_train[,var]), 
                          by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var],
     palette.name=coolBlueHotRed)
var_unscaled <- aggregate(as.numeric(data_train[,var1]), 
                          by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var1],
     palette.name=coolBlueHotRed)

var_unscaled <- aggregate(as.numeric(data_train[,var2]), 
                          by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var2],
     palette.name=coolBlueHotRed)

par(mfrow=c(1,1))
mydata <- som_model$codes 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
}
plot(wss)

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd',
                    '#8c564b', '#e377c2')
## CAH sur les noeuds
som_cluster <- cutree(hclust(dist(som_model$codes)), 3)
# plot these results:
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)

data_train$cluster<-som_cluster[som_model$unit.classif]








