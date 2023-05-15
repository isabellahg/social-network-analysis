setwd("C:/Users/isabe/Documents/entregable3")
nodes <- read.csv("nodos.csv", header = T)
links <- read.csv("enlaces.csv", header = T)

library(igraph)

cliques <- function(graph, min_size=3) {
  print("## Cliques")
  # número de cliques mayores de 3 
  num_cliques_3_or_larger <- count_max_cliques(net, min=3)
  print(paste("Número de cliques de tamaño 3 o mayor:", num_cliques_3_or_larger))
  cliques_3_or_larger <- max_cliques(net, min=3)
  # Clique de mayor tamaño
  max_size <- max(sapply(cliques_3_or_larger, length))
  cliques_max_size <- cliques_3_or_larger[sapply(cliques_3_or_larger, length) == max_size]
  
  
  # número de cliques
  print(paste("Numero de cliques de mayor tamaño", length(cliques_max_size)))
  
  # Nodos de los cliques de mayor tamaño
  
  print("### Nodos de los cliques de mayor tamaño")
  for (i in seq_along(cliques_max_size)) {
    print(paste("Nodos del Clique", i, ":", paste(cliques_max_size[[i]], collapse=", ")))
  }
  
  print("### Nodos de todos los cliques de tamaño 3 o mayor")
  for (i in seq_along(cliques_3_or_larger)) {
    print(paste("Nodos del Clique", i, ":", paste(cliques_3_or_larger[[i]], collapse=", ")))
  }
  
}

kcores <- function(net, min_size=3) {
  print("## K Cores")
  
  #Incorpor la informción dentro del grafo
  V(net)$outdegree <- degree(net, mode="out")
  V(net)$indegree <- degree(net, mode="in")
  V(net)$degree <- degree(net, mode="all")
  
  
  kcore <- coreness(net)
  #Añadir la propiedad kcore a los nodos de la red
  V(net)$kcore <- kcore   

  
  kcore <- coreness(net.sym)
  
  #Mostrar los cores. Los nombres de los vértices está arriba y el core al que pertenece abajo
  print(kcore)
  
  #Añadir la propiedad kcore a los nodos de la red
  V(net.sym)$kcore <- kcore     
  
  #print(V(net.sym)$kcore)
  
  #Mostrar el gráfico donde el color de los nodos está en función del
  #kcore
  plot(net.sym, edge.arrow.size=.2, edge.arrow.width=0.6, edge.curved=0,
       vertex.color=V(net.sym)$kcore, vertex.frame.color="#555555",
       vertex.label=V(net)$label, vertex.label.color="black",
       vertex.label.cex=.5, vertex.size=V(net)$degree/2, layout=layout.fruchterman.reingold,
       edge.width = E(net.sym)$weight/3)
  
  num_kcores <- length(unique(kcore))
  print(paste("Número de kcores", num_kcores))
  
  max_deg <- max(kcore)
  print(paste("Mayor grado k: ", max_deg))
  node_count <- sum(kcore == max_deg)
  print(paste("Cantidad de nodos del kcore de grado k más alto: ", node_count))
  
}




net <- graph.data.frame(d=links,vertices=nodes,directed=F)
cliques(net)
kcores(net)




  



