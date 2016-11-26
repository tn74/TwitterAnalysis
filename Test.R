library(igraph)


# First function called, creates the nodes of the network
createGraph = function(nodes = 10){
  eg = make_empty_graph(nodes) 
  eg = set_vertex_attr(eg, "knownState",V(eg), FALSE) 
  eg = set_vertex_attr(eg, "color", V(eg), "red")
  return(eg)
}

# Create a number of edges stemming from a given node 
createEdges = function(g, nodeIndex, numConnections){
  numNodes = g.vcount()
  if (numNodes<numConnections){
    stop("More connections than nodes!")
  }
  toConnect = ceiling(runif(numConnections,0,numNodes))
  g = add_edges() 
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
  

graphObj = createGraph(100)
graphObj = becomeKnown(graphObj,10)
graphObj = createEdges(graphObj,10,5)
plotGraph(graphObj)

