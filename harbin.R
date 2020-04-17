library(sna)
library(tsna)
library(ndtv)
library(dplyr)
library(tidyr)
library(readxl)
library(writexl)

harbin <- read.csv("data/harbin_sn.csv", stringsAsFactors = FALSE)

edges <- harbin %>%
  select(one_of("tail_no", "head_no"))

edges <- edges[-1,]

colnames(edges) <- c("tail", "head")

#write.csv(edges, "edges.csv", row.names = FALSE)

#edges <- read.csv("edges.csv", stringsAsFactors = FALSE)

nodes <- harbin %>%
  select(1:5, 8)

nodes_tail <- nodes %>% 
  select(one_of("tail_no")) %>%
  distinct()

nodes_head <- nodes %>%
  select(one_of("head_no")) %>%
  distinct()

setdiff(nodes_tail$tail_no, nodes_head$head_no)

duplicated(nodes$head_no)

nodes <- nodes[duplicated(nodes$head_no) == FALSE, 3:6]

colnames(nodes) <- c("vertex.id", "name", "gender", "channel")

#write.csv(nodes, "nodes.csv", row.names = FALSE)

#nodes <- read.csv("nodes.csv", stringsAsFactors = FALSE)

harbin_network <- network(
  edges,
  vertex.attr = nodes,
  vertex.attrnames = c("vertex.id", "name", "gender", "channel"),
  directed = TRUE,
  bipartite = FALSE
)

plot_position <- plot(harbin_network)



nodes_dy <- harbin[duplicated(harbin$head_no) == FALSE, c(3, 6)]

nodes_dy$end <- c("2020/4/10")
nodes_dy$contact_date[1] <- "2020/3/19"

nodes_dy$contact_date <- as.POSIXct(nodes_dy$contact_date)
nodes_dy$end <- as.POSIXct(nodes_dy$end)

nodes_dy$onset <- ((nodes_dy$contact_date - as.POSIXct("2020-03-19")) / (24 * 60 * 60)) + 1
nodes_dy$terminus <- (as.POSIXct("2020-04-10") - as.POSIXct("2020-03-19"))

nodes_dy <- nodes_dy[, c(4, 5, 1)]

colnames(nodes_dy)[3] <- c("vertex.id")

write.csv(nodes_dy, "nodes_dy.csv", row.names = FALSE)

nodes_dy <- read.csv("nodes_dy.csv", stringsAsFactors = FALSE)



edges_dy <- harbin[, c(1, 3, 6)]

edges_dy$end <- c("2020/4/10")
edges_dy <- edges_dy[-1,]

edges_dy$contact_date <- as.POSIXct(edges_dy$contact_date)
edges_dy$end <- as.POSIXct(edges_dy$end)

edges_dy$onset <- ((edges_dy$contact_date - as.POSIXct("2020-03-19")) / (24 * 60 * 60)) + 1
edges_dy$terminus <- (as.POSIXct("2020-04-10") - as.POSIXct("2020-03-19"))

edges_dy <- edges_dy[, c(5, 6, 1, 2)]

colnames(edges_dy)[3:4] <- c("tail",	"head")

write.csv(edges_dy, "edges_dy.csv", row.names = FALSE)

edges_dy <- read.csv("edges_dy.csv", stringsAsFactors = FALSE)

harbin_dynamic <- networkDynamic(
  harbin_network,
  edge.spells = edges_dy,
  vertex.spells = nodes_dy
)


network::set.vertex.attribute(harbin_dynamic, 'x', plot_position[,1])
network::set.vertex.attribute(harbin_dynamic, 'y', plot_position[,2])


network.dynamic.check(harbin_dynamic)

plot(harbin_dynamic)

reconcile.vertex.activity(harbin_dynamic, mode = "match.to.edges")

#filmstrip(dynamicCollabs, displaylabels = FALSE)

compute.animation(
  harbin_dynamic,
  #verbose = FALSE,
  #seed.coords = (100),
  animation.mode = "useAttribute",
  slice.par = list(
    start = 0,
    end = 23,
    interval = 2,
    aggregate.dur = 1,
    rule = "latest"
  )
)

render.d3movie(
  harbin_dynamic,
  usearrows = TRUE,
  #render.par = list(tween.frames = 10, show.time = FALSE),
  #plot.par = list(mar = c(0, 0, 0, 0)),
  displaylabels = TRUE,
  label = paste(harbin_dynamic%v%"vertex.id",
                harbin_dynamic%v%"name", "途徑：", harbin_dynamic%v%"channel")
  # This slice function makes the labels work
)

plot(tEdgeFormation(harbin_dynamic, time.interval = 1))


dynamicBetweenness <- tSnaStats(
  harbin_dynamic,
  snafun = "centralization",
  start = 0,
  end = 23,
  time.interval = 2,
  aggregate.dur = 1,
  FUN = "infocent"
)

plot(dynamicBetweenness)

harbin_anal <- data.frame(nodes, infocent = infocent(harbin_network), degree = sna::degree(harbin_network), betweenness = flowbet(harbin_network))

harbin_anal <- arrange(harbin_anal, desc(degree))

harbin_anal

eccentricity(harbin_network)


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

plot_position <- plot(thenetwork)



# attach your *static* coordinates to the network

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


network::set.vertex.attribute(dynamicCollabs, 'x', plot_position[,1])
network::set.vertex.attribute(dynamicCollabs, 'y', plot_position[,2])


network.dynamic.check(dynamicCollabs)

plot(dynamicCollabs)

reconcile.vertex.activity(dynamicCollabs, mode = "match.to.edges")

#filmstrip(dynamicCollabs, displaylabels = FALSE)

compute.animation(
  dynamicCollabs,
  #verbose = FALSE,
  #seed.coords = (100),
  animation.mode = "useAttribute",
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





PHStaticEdges <- read.csv("demo/TNAWR_StaticEdgelist.csv")
PHVertexAttributes <- read.csv(
  "demo/TNAWR_VertexAttributes.csv",
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

PHDynamicNodes <- read.csv("demo/TNAWR_DynamicNodes.csv")
PHDynamicEdges <- read.csv("demo/TNAWR_DynamicEdges.csv")

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
network::set.vertex.attribute(wheel,'x',df[,1])
network::set.vertex.attribute(wheel,'y',df[,2])

compute.animation(wheel,animation.mode = 'useAttribute')
render.d3movie(wheel)




nodes_dy$onset <- as.POSIXct(nodes_dy$onset)
nodes_dy$terminus <- as.POSIXct(nodes_dy$terminus)

nodes_dy$onset <- as.character(nodes_dy$onset)
nodes_dy$terminus <- as.character(nodes_dy$terminus)

nodes_dy$onset <- gsub("-", "", nodes_dy$onset)
nodes_dy$terminus <- gsub("-", "", nodes_dy$terminus)

nodes_dy$onset <- as.numeric(nodes_dy$onset)
nodes_dy$terminus <- as.numeric(nodes_dy$terminus)

nodes_dy$vertex.id <- as.numeric(nodes_dy$vertex.id)

#nodes_dy$onset.censored <- "FALSE"
#nodes_dy$terminus.censored <- "FALSE"
#nodes_dy$duration <- nodes_dy$terminus - nodes_dy$onset



edges_dy <- edges_dy[-1,]

edges_dy$onset <- as.POSIXct(edges_dy$onset)
edges_dy$terminus <- as.POSIXct(edges_dy$terminus)

edges_dy$onset <- as.character(edges_dy$onset)
edges_dy$terminus <- as.character(edges_dy$terminus)

edges_dy$onset <- gsub("-", "", edges_dy$onset)
edges_dy$terminus <- gsub("-", "", edges_dy$terminus)

edges_dy$onset <- as.numeric(edges_dy$onset)
edges_dy$terminus <- as.numeric(edges_dy$terminus)

edges_dy <- edges_dy[, c(3, 4, 1, 2)]

#edges_dy$onset.censored <- "FALSE"
#edges_dy$terminus.censored <- "FALSE"
#edges_dy$duration <- edges_dy$terminus - edges_dy$onset
#edges_dy$edge.id <- seq(1:43)s