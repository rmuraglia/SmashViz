# pr_network.r

# based on data currently imported to nc smash 4 db (as of 09/16/2016, only SSF series), display network representation of W/L among PR players

library(RMySQL)
library(igraph)
library(RColorBrewer)

player_names <- c('Kai', 'Dandy Penguin', 'Stingers', 'Leaf FC', 'Dragoniota', 'Kresent', 'Lazyboredom', 'Donquavious', 'ThatGuy', 'KH1COM2', 'Heatstroke', 'JKid', 'Light', 'LuckyWind', 'Sigtrick', 'Mekos', 'REIGN', 'Revan', 'Cowhunter', 'Dinner', 'DEEPBLUE', 'Ferf', 'Gael', 'GQ', 'Stockfield', 'Veen', 'Hood')
# player_colors <- brewer.pal(length(player_names), "Set3") # for PR version, color each person differently
player_colors <- brewer.pal(5, "Set3") # for tier list version, color just by group

nodes<-data.frame(id = 1:length(player_names), tags = player_names, stringsAsFactors = FALSE)

cnx <- dbConnect(MySQL(), dbname='NC_smash4')

get_challonge_ids_from_maintag<-function(name) {
    p_id <- dbGetQuery(cnx, sprintf('select p_id from players where main_tag ="%s" ;', name))
    query <- sprintf('select tags.id from tags join players on tags.player_id = players.id where p_id = %s ;', p_id)
    all_ids <- dbGetQuery(cnx, query)
    return(unlist(all_ids))
}

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

# for each combination of PRs, get how many wins one has over the other
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

net <- graph_from_data_frame(d=edges, vertices=nodes, directed=T) 
E(net)$width <- E(net)$counts
E(net)$weights <- E(net)$counts

l<-layout_with_fr(net, weights=E(net)$weights)
plot_colors<-c(rep(player_colors[1], 10), rep(player_colors[2], 5), rep(player_colors[3], 3), rep(player_colors[4], 8), rep(player_colors[5], 1))

png('tierlist_network_ssf.png', width=6, height=6, units='in', res=200)
# par(bg='grey30') # if want grey background to make light color more apparent
# plot(net, edge.curved=0.15, layout=l, edge.width = E(net)$counts/1.5, edge.arrow.size = 0.75, edge.color=player_colors[edges[,1]], vertex.color=player_colors) # PR version
plot(net, edge.curved=0.15, layout=l, edge.width = E(net)$counts/1.5, edge.arrow.size = 0.75, vertex.color=plot_colors)
legend('topleft', legend=paste(player_names[1:18], '(', 1:18, ')', sep=''), pch=21, pt.bg=plot_colors[1:18], pt.cex=2, bty='n', ncol=2, cex=0.7)
legend('topright', legend=paste(player_names[19:27], '(', 19:27, ')', sep=''), pch=21, pt.bg=plot_colors[19:27], pt.cex=2, bty='n', ncol=1, cex=0.7)
dev.off()


dbDisconnect(cnx)
