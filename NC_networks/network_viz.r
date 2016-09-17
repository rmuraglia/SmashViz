# network_viz.r

# based on data current imported to NC smash 4 DB (as of 09/16/2016, only complete SSF series), display network representation of W/L among notable players

library(RMySQL)
library(igraph)
library(RColorBrewer)

# connect to db
cnx <- dbConnect(MySQL(), dbname='NC_smash4')

# get all unique tournament IDs for a player based on one of their known tags
get_challonge_ids_from_maintag<-function(name) {
    p_id <- dbGetQuery(cnx, sprintf('select p_id from players where main_tag ="%s" ;', name))
    query <- sprintf('select tags.id from tags join players on tags.player_id = players.id where p_id = %s ;', p_id)
    all_ids <- dbGetQuery(cnx, query)
    return(unlist(all_ids))
}

# get win counts between players 1 and 2
p1_p2_win_counts<-function(p1, p2) {
    p1_w<-0
    p2_w<-0
    p1_ids<-get_challonge_ids_from_maintag(p1)
    p2_ids<-get_challonge_ids_from_maintag(p2)
    for (i in p1_ids) {
        for (j in p2_ids) {
            # check for p1=p1 and p2=p2
            A <- dbGetQuery(cnx, sprintf('select p1_id, p2_id, winner from sets where p1_id = %s and p2_id = %s ;', i, j))
            if (nrow(A)>0) { # a set between these two IDs actually took place
                for (k in 1:nrow(A)) { # check each set result
                    if (A[k,3]==1) { p1_w<-p1_w+1 
                    } else if (A[k,3]==2) { p2_w<-p2_w+1 }
                }
            }
            # check for p1=p2 and p2=p1 (columns flipped)
            B <- dbGetQuery(cnx, sprintf('select p1_id, p2_id, winner from sets where p1_id = %s and p2_id = %s ;', j, i))
            if (nrow(B)>0) {
                for (k in 1:nrow(B)) {
                    if (B[k,3]==1) { p2_w<-p2_w+1
                    } else if (B[k,3]==2) {p1_w<-p1_w+1 }
                }
            }
        }
    }
    return(c(p1_w, p2_w))
}

# create nodes for network : simply an ID paired to their tag
get_nodes<-function(player_names) {
    nodes<-data.frame(id = 1:length(player_names), tags = player_names, stringsAsFactors=FALSE)
    return(nodes)
}

# create edges of network for a given list of players.
# edge weights represent number of wins p1 has over p2
get_edges<-function(player_names) {
    edges<-data.frame(from = numeric(), to = numeric(), counts = numeric())
    for (i in 1:(length(player_names)-1)) {
        for (j in (i+1):length(player_names)) {
            win_counts<-p1_p2_win_counts(player_names[i], player_names[j])
            if (win_counts[1]!=0) {
                edges<-rbind(edges, data.frame(from=i, to=j, counts=win_counts[1]))
            }
            if (win_counts[2]!=0) {
                edges<-rbind(edges, data.frame(from=j, to=i, counts=win_counts[2]))
            }
        }
    }
    return(edges)
}

###########
# part 1: network of only PR players
###########

pr_names <- c('Kai', 'Dandy Penguin', 'Stingers', 'Leaf FC', 'Dragoniota', 'Kresent', 'Lazyboredom', 'Donquavious', 'ThatGuy', 'KH1COM2')
pr_colors <- brewer.pal(length(pr_names), "Set3") # for PR version, color each person differently

pr_nodes<-get_nodes(pr_names)
pr_edges<-get_edges(pr_names)

pr_net <- graph_from_data_frame(d=pr_edges, vertices=pr_nodes, directed=TRUE)
E(pr_net)$width <- E(pr_net)$counts # set default width as counts
E(pr_net)$weights <- E(pr_net)$counts # set default weight as counts

pr_layout<-layout_with_fr(pr_net, weights=E(pr_net)$weights) # use fruchterman reingold layout algorithm

# for PR version, yellow color edges are hard to see on white background, so use grey
png('pr_network_ssf.png', width=6, height=6, units='in', res=200)
par(bg='grey30')
plot(pr_net, edge.curved=0.15, layout=pr_layout, edge.width=E(pr_net)$counts/1.5, edge.arrow.size=0.75, edge.color=pr_colors[pr_edges[,1]], vertex.color=pr_colors)
legend('topleft', legend=paste(pr_names, ' (', 1:length(pr_names), ')', sep=''), pch=21, pt.bg=pr_colors, pt.cex=2, bty='n', ncol=2)
dev.off()

##############
# part 2: network of all tierlisted players
##############

tier_names <- c('Kai', 'Dandy Penguin', 'Stingers', 'Leaf FC', 'Dragoniota', 'Kresent', 'Lazyboredom', 'Donquavious', 'ThatGuy', 'KH1COM2', 'Heatstroke', 'JKid', 'Light', 'LuckyWind', 'Sigtrick', 'Mekos', 'REIGN', 'Revan', 'Cowhunter', 'Dinner', 'DEEPBLUE', 'Ferf', 'Gael', 'GQ', 'Stockfield', 'Veen', 'Hood')
tier_colors<-brewer.pal(5, "Set3") # for tier list, color by group
plot_colors<-c(rep(tier_colors[1], 10), rep(tier_colors[2], 5), rep(tier_colors[3], 3), rep(tier_colors[4], 8), rep(tier_colors[5], 1)) # set color based on group (PR, A, A-inactive, B, B-inactive)

tier_nodes<-get_nodes(tier_names)
tier_edges<-get_edges(tier_names)

tier_net<-graph_from_data_frame(d=tier_edges, vertices=tier_nodes, directed=TRUE)
E(tier_net)$width <- E(tier_net)$counts # set default width as counts
E(tier_net)$weights <- E(tier_net)$counts # set default weight as counts

tier_layout<-layout_with_fr(tier_net, weights=E(tier_net)$weights) # use fruchterman reingold layout algorithm

# for tier version, don't bother with coloring edges. white background is fine. be sure to split the legend
png('tier_network_ssf.png', width=6, height=6, units='in', res=200)
plot(tier_net, edge.curved=0.15, layout=tier_layout, edge.width=E(tier_net)$counts/1.5, edge.arrow.size=0.75, vertex.color=plot_colors)
legend('topleft', legend=paste(tier_names[1:18], ' (', 1:18, ')', sep=''), pch=21, pt.bg=plot_colors[1:18], pt.cex=1.5, bty='n', ncol=2, cex=0.5)
legend('topright', legend=paste(tier_names[19:27], ' (', 19:27, ')', sep=''), pch=21, pt.bg=plot_colors[19:27], pt.cex=1.5, bty='n', ncol=1, cex=0.5)
dev.off()

dbDisconnect(cnx)






