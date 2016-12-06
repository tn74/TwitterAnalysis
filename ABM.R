library(igraph)
library(networkD3)
library(dplyr)
library(magrittr)
library(fGarch)
library(ggplot2)


# First function called, creates the nodes of the network
createGraph = function(nodes = 10, distribution){
  eg = make_empty_graph(nodes) %>%
    set_vertex_attr("knownState",V(.), FALSE) %>%
    set_vertex_attr("color", V(.), "red")
  for (i in 1:length(V(eg))){
    print('hello')
    eg = set_vertex_attr(eg, "support", i, sample(distribution$col,1))
  }
  return(eg)
}

# Create a number of edges stemming from a given node 
createEdges = function(g, nodeIndex, numConnections){
  numNodes = length(V(g))
  if (numNodes<numConnections){
    stop("More connections than nodes!")
  }
  toConn = matrix(sample.int(numNodes, size = numConnections, replace = FALSE))
  addConnMat = c(rep(nodeIndex,numConnections*2))
  for (i in 1:numConnections){
    addConnMat[2*i] = toConn[i]
  }
  g = add_edges(g, addConnMat ) 
}

# Sets a node to be "known" and changes the fill color
becomeKnown = function(g, nodeIndex){
  g = set_vertex_attr(g,"knownState",nodeIndex, TRUE) %>%
    set_vertex_attr("color", nodeIndex, "green")
  return(g)
}


# Plots graphs as we wish
plotGraph = function (g){
  plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1)
}

#Parameters
people = 1000   #Number of people in the network
avgCon = 20     #Average number of followers on twitter
sdCon = 5       #Standard deviation of followers on twitter


distSupport = data.frame(col = rnorm(1000), mean = 0, sd = 0.02)
distConnection = data.frame(col = round(rsnorm(1000, mean = avgCon, sd = sdCon, xi = 20)))


ggplot(distConnection, aes(x=col)) +geom_histogram()
graphObj = createGraph(people,distSupport)
graphObj = becomeKnown(graphObj,10)
for (i in 1:length(V(graphObj))){
  graphObj = createEdges(graphObj,i,sample(distConnection$col,1))
}
#plot(graphObj)
#convertToDF(graphObj)
attr = get.vertex.attribute(graphObj,index = V(graphObj))












# wc <- cluster_walktrap(graphObj$known)
# members <- membership(wc)
# graphObj_d3 = igraph_to_networkD3(graphObj, group = members)
# forceNetwork(Links = graphObj_d3$links, Nodes = graphObj_d3$nodes,
#              Source = 'source', Target = 'target',
#              NodeID = 'name', Group = 'group')
