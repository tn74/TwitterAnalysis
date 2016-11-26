library(igraph)


# First function called, should create the network
createGraph = function(nodes = 10){
  eg = make_empty_graph(nodes)
  set_vertex_attr(eg, "name", 1, "Trishul Nagenalli")
  plot(eg)
  return(eg)
}

# Sets a node to be "known" and changes the fill color
becomeKnown = function(g,nodeIndex){
  print("Into becomeKnown")
  set_vertex_attr(g,"known",nodeIndex,TRUE)
  set_vertex_attr(g,"color", nodeIndex,"blue")
  return(g)
}

# Plots graphs as we wish
plotGraph = function (g){
  plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1.5,
       vertex.color="pink" ) 
}
  

graphObj = createGraph(50)
plot(graphObj)