library(sna)
library(tsna)
library(ndtv)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

edges <- read.csv("data/edge.csv")
#edges <- edges[, c(2, 1)]

node_attr <- read.csv(
  "data/node.csv",
  stringsAsFactors = FALSE
)

thenetwork <- network(
  edges[, c(1:2)],
  vertex.attr = node_attr,
  vertex.attrnames = c("vertex.id", "name", "gender", "channel"),
  directed = TRUE,
  bipartite = FALSE
)

plot(thenetwork)

dy_Nodes <- read.csv("data/node_dy.csv")
dy_Edges <- read.csv("data/edge_dy.csv")

#dy_Nodes <- dy_Nodes[, c(1:3, 6)]
dy_Edges <- dy_Edges[, c(3, 4, 1, 2, 5:8)]

#dy_Nodes$vertex.id <- as.numeric(dy_Nodes$vertex.id)

dynamicCollabs <- networkDynamic(
  thenetwork,
  edge.spells = dy_Edges,
  vertex.spells = dy_Nodes
)


network.dynamic.check(dynamicCollabs)

plot(dynamicCollabs)

g <- graph.adjacency(edges, mode = "undirected")

g2<-V(g)[degree(g)<1]
g<-delete.vertices(g,g2)

reconcile.vertex.activity(dynamicCollabs, mode = "match.to.edges")

#filmstrip(dynamicCollabs, displaylabels = FALSE)

compute.animation(
  dynamicCollabs,
  #verbose = FALSE,
  #seed.coords = (100),
  animation.mode = "MDSJ",
  slice.par = list(
    start = 5,
    end = 75,
    interval = 10,
    aggregate.dur = 5,
    rule = "latest"
  )
)

render.d3movie(
  dynamicCollabs,
  usearrows = TRUE,
  #render.par = list(tween.frames = 10, show.time = FALSE),
  #plot.par = list(mar = c(0, 0, 0, 0)),
  displaylabels = TRUE,
  label = paste(dynamicCollabs%v%"vertex.id",
                dynamicCollabs%v%"name", "途徑：", dynamicCollabs%v%"channel"),
  # This slice function makes the labels work
)


edge.tooltip = function(slice) {
  paste(
    "<b>傳染途徑:</b>", (slice %v% "channel")
  )
}


PHStaticEdges <- read.csv(file.choose())
PHVertexAttributes <- read.csv(
  file.choose(),
  stringsAsFactors = FALSE
)

thenetwork <- network(
  PHStaticEdges,
  vertex.attr = PHVertexAttributes,
  vertex.attrnames = c("vertex.id", "name", "region"),
  directed = FALSE,
  bipartite = FALSE
)
plot(thenetwork)

PHDynamicNodes <- read.csv(file.choose())
PHDynamicEdges <- read.csv(file.choose())

dynamicCollabs <- networkDynamic(
  thenetwork,
  edge.spells = PHDynamicEdges,
  vertex.spells = PHDynamicNodes
)
network.dynamic.check(dynamicCollabs)

plot(dynamicCollabs)

filmstrip(dynamicCollabs, displaylabels = FALSE)

compute.animation(
  dynamicCollabs,
  animation.mode = "MDSJ",
  slice.par = list(
    start = 1260,
    end = 1300,
    interval = 1,
    aggregate.dur = 20,
    rule = "any"
  )
)
render.d3movie(
  dynamicCollabs,
  displaylabels = FALSE,
  # This slice function makes the labels work
  vertex.tooltip = function(slice) {
    paste(
      "<b>Name:</b>", (slice %v% "name"),
      "<br>",
      "<b>Region:</b>", (slice %v% "region")
    )
  }
)


wheel <- network.initialize(10)
add.edges.active(wheel,tail=1:9,head=c(2:9,1),onset=1:9, terminus=11)
add.edges.active(wheel,tail=10,head=c(1:9),onset=10, terminus=12)
df<-matrix(c(-0.99603723, 2.798261858,
             -0.10480299, 1.754082668,
             0.64294818, 0.679889124,
             1.19137571, 0.042572219,
             1.47703967, 0.250050715,
             1.49393321, 1.523045819,
             1.2319355, 3.772612788,
             0.72715205, 6.634426198,
             0.01328487, 9.529656458,
             -1.49393321, 0.662555779),ncol=2,byrow=TRUE)
set.vertex.attribute(wheel,'x',df[,1])
set.vertex.attribute(wheel,'y',df[,2])

compute.animation(wheel,animation.mode = 'useAttribute')
render.d3movie(wheel)
