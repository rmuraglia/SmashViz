# degrees_from_kai.py

# Kai is the number one player in north carolina. based on ssf series data in the databse, determine the shortest path to a victory over Kai

import mysql.connector
from pprint import pprint

# connect to db
cnx = mysql.connector.connect(database='nc_smash4', option_files='/Users/rmuraglia/.my.cnf')
cur = cnx.cursor()

def get_challonge_ids(name) :
    cur.execute('select p_id from players where main_tag = %s ;', (name, ))
    p_id = cur.fetchone()[0]
    try :
        cur.fetchall() # flush cursor in case of duplicate
    except mysql.connector.errors.InterfaceError :
        pass # if cursor was already empty just proceed
    cur.execute('select tags.id from tags join players on tags.player_id = players.id where players.p_id = %s ;', (p_id, ))
    all_ids = cur.fetchall()
    out_ids = [x[0] for x in all_ids]
    return out_ids

def get_tag_losses(tag) :
    cur.execute('select p2_id from sets where p1_id = %s and winner = 2 ;', (tag, ))
    p1_losses = [x[0] for x in cur] # strip result out of one-tuple
    cur.execute('select p1_id from sets where p2_id = %s and winner = 1 ;', (tag, ))
    p2_losses = [x[0] for x in cur]
    all_losses = [p1_losses, p2_losses]
    flattened_losses = {tag for losses in all_losses for tag in losses} # use set comprehension instead of list to remove duplicates
    return flattened_losses

def tag_to_player(tag) :
    cur.execute('select players.p_id from tags join players on tags.player_id = players.id where tags.id = %s ;', (tag, ))
    p_id = cur.fetchone()[0]
    cur.execute('select main_tag from players where p_id = %s ;', (p_id, ))
    p_name = cur.fetchall()[0][0] # only keep first result
    return(p_name)

def get_user_losses(name) :
    c_ids = get_challonge_ids(name)
    loss_tags = [get_tag_losses(x) for x in c_ids]
    loss_tags = {tag for losses in loss_tags for tag in losses}
    loss_names = {tag_to_player(x) for x in loss_tags}
    return(loss_names)

def add_new_layer(curr_map) :
    new_keys = set() # player list to expand for next layer
    for i in xrange(len(curr_map[-1])) : 
        # get unique union of previous layer additions
        new_keys = new_keys | curr_map[-1].values()[i]
    prev_keys = set() # player list from previous layers
    for i in xrange(len(curr_map)) :
        prev_keys = prev_keys | set(curr_map[i].keys())
    new_layer = {}
    for x in new_keys :
        new_layer[x] = get_user_losses(x) - prev_keys - new_keys
    curr_map.append(new_layer)
    return(curr_map)

# level 1: one degree of separation from kai. direct defeats
sep_from_kai = [{'kai' : get_user_losses('kai')}]

# level 2: who are the players that have defeated level_1?
# only include players that are not already in level 1
sep_from_kai = add_new_layer(sep_from_kai)

# level 3: you get the idea by now
sep_from_kai = add_new_layer(sep_from_kai)
sep_from_kai = add_new_layer(sep_from_kai) # original cutoff
sep_from_kai = add_new_layer(sep_from_kai)
sep_from_kai = add_new_layer(sep_from_kai)
sep_from_kai = add_new_layer(sep_from_kai)

# done creating degrees of separation list
cur.close()
cnx.close()

# write out to file for easier viz in R
written_nodes = {}
node_id = 1

with open('nodes.txt', 'w') as nodes :
    with open('edges.txt', 'w') as edges :
        for i in xrange(len(sep_from_kai)) :
            layer_keys = sep_from_kai[i].keys()
            if i==0 :
                nodes.write(str(node_id) + ',' + layer_keys[0] + ',' + str(i) + '\n')
                written_nodes[layer_keys[0]] = node_id
                node_id += 1
            for key in layer_keys :
                for val in sep_from_kai[i][key] :
                    if not val in written_nodes.keys() :
                        nodes.write(str(node_id) + ',' + val + ',' + str(i+1) + '\n')
                        written_nodes[val] = node_id
                        node_id += 1
                    edges.write(str(written_nodes[key]) + ',' + str(written_nodes[val]) + '\n')


########
# TESTING AREA FOR NETWORKX GRAPH DRAWING
########

# rearrange data into graph
# import networkx as nx
# import matplotlib.pyplot as plt

# # init graph
# G=nx.Graph()

# # add root
# G.add_node('kai')

# # add first layer
# for x in sep_from_kai[0].values()[0] :
#     G.add_node(x)
#     G.add_edge('kai', x)

# # add second layer
# for x in sep_from_kai[1] :
#     source = x.key

# nx.draw(G)
# nx.draw_circular(G)

# tim = sep_from_kai[0:2]
# H = nx.Graph()

# # write to file using this type of iteration, then read into R for use with igraph
# for i in xrange(len(tim)) :
#     layer_keys = tim[i].keys()
#     if i==0 :
#         H.add_node(layer_keys[0])
#     for key in layer_keys :
#         for val in tim[i][key] :
#             if not val in H :
#                 H.add_node(val)
#             H.add_edge(val, key)

# nx.draw_circular(H)
# plt.show()