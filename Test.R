library(igraph)
library(networkD3)
library(dplyr)
library(magrittr)


# First function called, creates the nodes of the network
createGraph = function(nodes = 10){
  eg = make_empty_graph(nodes) 
  eg = set_vertex_attr(eg, "knownState",V(eg), FALSE) 
  eg = set_vertex_attr(eg, "color", V(eg), "red")
  return(eg)
}

# Create a number of edges stemming from a given node 
createEdges = function(g, nodeIndex, numConnections){
  numNodes = length(V(g))
  print(numNodes)
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
    set_vertex_attr("color", nodeIndex,"green")
  return(g)
}

# Plots graphs as we wish
plotGraph = function (g){
  plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1)
}

graphObj = createGraph(50)
graphObj = becomeKnown(graphObj,10)
for (i in 1:length(V(graphObj))){
  graphObj = createEdges(graphObj,i,5)
}


wc <- cluster_walktrap(graphObj$known)
members <- membership(wc)
graphObj_d3 = igraph_to_networkD3(graphObj, group = members)
forceNetwork(Links = graphObj_d3$links, Nodes = graphObj_d3$nodes,
             Source = 'source', Target = 'target',
             NodeID = 'name', Group = 'group')
