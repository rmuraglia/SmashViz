# degrees_from_kai.r

# preprocessing and sql db querying done with complementary python script
# r script used for visualization only

library(igraph)
library(RColorBrewer)

nodes <- read.table('nodes.txt', sep=',', header=FALSE, col.names=c('nodeID', 'tag', 'degree'))
edges <- read.table('edges.txt', sep=',', header=FALSE, col.names=c('ID1', 'ID2'))

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=FALSE)

plot(net, layout=layout_as_tree(net, root=1, circular=TRUE), vertex.size=5, vertex.label=NA)

png('degrees_from_kai.png', width=6, height=6, units='in', res=200)
plot(net, layout=layout_as_tree(net, root=1, circular=FALSE), vertex.size=5, vertex.label=NA)
dev.off()

trace_path<-function(name) {
    print(paste('checking path for player: ', name, sep=''))
    player_heap<-list(name)
    while(length(player_heap)>0) {
        player_pop<-player_heap[[1]]
        player_heap<-player_heap[-1]

        player_id<-nodes[which(nodes[,2]==player_pop), 1]
        defeat_ids<-edges[which(edges[,2]==player_id), 1]
        for (i in defeat_ids) {
            defeat_name<-nodes[which(nodes[,1]==i), 2]
            player_heap[[length(player_heap)+1]]<-defeat_name
            print(paste(player_pop, 'defeated: ', defeat_name))
        }
    }
}
