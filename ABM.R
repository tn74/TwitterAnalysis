library(igraph)
library(networkD3)
library(dplyr)
library(magrittr)
library(fGarch)
library(ggplot2)


# First function called, creates the nodes of the network
createGraph = function(nodes = 10, distribution, distPressure){
  eg = make_empty_graph(nodes) %>%
    set_vertex_attr("knownState",V(.), FALSE) 
  for (i in 1:length(V(eg))){
    print(sample(distribution$col,1))
    eg = set_vertex_attr(eg, "support", i, sample(distribution$col,1))
    eg = set_vertex_attr(eg, "pressure", i, sample(distPressure$col,1))
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
  g = set_vertex_attr(g,"knownState",nodeIndex, TRUE)
  return(g)
}


# Plots graphs as we wish
plotGraph = function (g){
  plot(g, edge.arrow.size=.5, vertex.label.color="black", vertex.label.dist=1)
}

#Parameters
people = 1000   #Number of people in the network
avgCon = 10     #Average number of followers on twitter
sdCon = 5       #Standard deviation of followers on twitter
trigger = 1     
runTime = 24

distSupport = data.frame(col = rsnorm(1000, mean = 0.1, sd = 0.15))
distPressure = data.frame(col = rsnorm(1000, mean = 0, sd = .1))
distConnection = data.frame(col = round(rsnorm(1000, mean = avgCon, sd = sdCon, xi = 20)))
ggplot(distConnection, aes(x=col)) +geom_histogram()
gr = createGraph(people,distSupport,distPressure)
for (i in 1:length(V(gr))){
  gr = createEdges(gr,i,sample(distConnection$col,1))
}
attr1 = get.vertex.attribute(gr,index = V(gr))
allData = data.frame(t = 1:runTime, numUnk= c(rep(NA,runTime)), numKnown = c(rep(NA,runTime)), avgSupport = c(rep(NA,runTime)), numSpreader=c(rep(NA,runTime)))


fireNode = ceiling(runif(1,0,people))
for (time in 1:runTime){
  for (nodeIndex in fireNode){
    #print(fireNode)
    neighborNodes = neighbors(gr,nodeIndex)
    #print(neighborNodes)
    gr = set.vertex.attribute(gr,"knownState",index=neighborNodes,TRUE)
    for (neighbor in neighborNodes){
      rand = runif(1,0,1)
      #print(sprintf('%5.3f, %5.3f',abs(get.vertex.attribute(gr, "support" ,neighbor)),rand))
      if (rand<abs(get.vertex.attribute(gr, "support" ,neighbor))){
        curNodeSupport = get.vertex.attribute(gr,"support",nodeIndex)
        curNeighborSupprot = get.vertex.attribute(gr,"support", neighbor)
        pressure = get.vertex.attribute(gr,"pressure", neighbor)
        gr = set.vertex.attribute(gr,"support",neighbor,curNeighborSupprot+curNodeSupport*pressure)
        if (sum(grepl(neighbor, fireNodeNext)==TRUE)==0){
          fireNodeNext = c(fireNodeNext, neighbor)
          print(fireNodeNext)
        }
        
      }
    }
    #print(fireNodeNext)
  }
  
  attr = get.vertex.attribute(gr,index = V(gr))
  allData$t[time] = time
  allData$numUnk[time] = sum(attr$knownState==FALSE)
  allData$numKnown [time]= people-allData$numUnk[time]
  allData$numSpreader[time] = length(fireNode)
  allData$avgSupport[time] = mean(get.vertex.attribute(gr,"support",V(gr)))
  allData$nonNeutral[time] = mean(abs(get.vertex.attribute(gr,"support",V(gr))))
  fireNode = fireNodeNext%>%unique()
  fireNodeNext={}
}


ggplot(allData,(aes(x=t))) +
geom_line(aes(y=numKnown, color = "People who Know"), size=1, alpha=0.75)










# wc <- cluster_walktrap(graphObj$known)
# members <- membership(wc)
# graphObj_d3 = igraph_to_networkD3(graphObj, group = members)
# forceNetwork(Links = graphObj_d3$links, Nodes = graphObj_d3$nodes,
#              Source = 'source', Target = 'target',
#              NodeID = 'name', Group = 'group')
