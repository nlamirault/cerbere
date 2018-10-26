#!/usr/bin/python

import sys, os
import numpy as np
import networkx as nx

sys.path.insert(1, os.path.join(sys.path[0], '../scripts'))

import weighted_indegree_util as wiu

def test_constant_weighter():
	edges = []
	G = nx.MultiDiGraph()

	edges.append((1,2,2))
	edges.append((1,3,3))
	edges.append((1,4,4))
	edges.append((2,3,4))
	edges.append((4,2,4))
	edges.append((4,5,5))
	edges.append((5,1,5))
	edges.append((3,5,5))

	G.add_weighted_edges_from(edges)

	cons_weighter_1 = wiu.ConstantWeighter(1) # set weight=1 for all edges
	w_in_degs = wiu.weighted_in_degree(G, G.nodes(), G.number_of_nodes(), cons_weighter_1)
	assert w_in_degs[1] == 1
	assert w_in_degs[2] == 2
	assert w_in_degs[3] == 2
	assert w_in_degs[4] == 1
	assert w_in_degs[5] == 2

	cons_weighter_3 = wiu.ConstantWeighter(3) # set weight=3 for all edges
	w_in_degs = wiu.weighted_in_degree(G, G.nodes(), G.number_of_nodes(), cons_weighter_3)
	assert w_in_degs[1] == 3
	assert w_in_degs[2] == 6
	assert w_in_degs[3] == 6
	assert w_in_degs[4] == 3
	assert w_in_degs[5] == 6


def test_last_interval_weighter():
	G = nx.MultiDiGraph()
	last_weighter = wiu.LastIntervalWeighter(2) # set weight=1 for edges that last 2 interval (time is measured by node arrivals)

	G.add_weighted_edges_from([(1,2,2)]) # initially 2 nodes
	w_in_degs = wiu.weighted_in_degree(G, G.nodes(), G.number_of_nodes(), last_weighter)
	assert w_in_degs[1] == 0
	assert w_in_degs[2] == 1

	G.add_weighted_edges_from([(2,3,3), (1,3,3)]) # 1 node enters
	w_in_degs = wiu.weighted_in_degree(G, G.nodes(), G.number_of_nodes(), last_weighter)
	assert w_in_degs[1] == 0
	assert w_in_degs[2] == 1
	assert w_in_degs[3] == 2

	G.add_weighted_edges_from([(3,4,4), (1,5,5)]) # 2 node enters
	w_in_degs = wiu.weighted_in_degree(G, G.nodes(), G.number_of_nodes(), last_weighter)
	assert w_in_degs[1] == 0
	assert w_in_degs[2] == 0 # indegree decreases from 1 to 0
	assert w_in_degs[3] == 0 # indegree decreases from 2 to 0 (because 2 new nodes have entered the graph since 3 had an inedge)
	assert w_in_degs[4] == 1
	assert w_in_degs[5] == 1

	G.add_weighted_edges_from([(2,1,5), (1,6,6)]) # 1 node enters
	w_in_degs = wiu.weighted_in_degree(G, G.nodes(), G.number_of_nodes(), last_weighter)
	print w_in_degs
	assert w_in_degs[1] == 1
	assert w_in_degs[2] == 0
	assert w_in_degs[3] == 0 # indegree decreases from 2 to 0
	assert w_in_degs[4] == 0 # indegree decreases from 2 to 0 (because 2 new nodes have entered the graph since 4 had an inedge)
	assert w_in_degs[5] == 1
	assert w_in_degs[6] == 1
