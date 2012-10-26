## Ontology
## ================================================================

import pickle
f = open("lower-tri-100B.data",'r')
triangle = pickle.load(f)
f.close()

f = open("words-100B.data", 'r')
words = pickle.load(f)
f.close()
n = len(words)

# import timeit
import time
from Pycluster import *

print "Clustering data..."
t = time.time()

tree = treecluster(data=None, mask=None, weight=None, transpose=0, method='a', dist='e', distancematrix=triangle)

print "Time elapsed = ", (time.time() - t)

## generate graph

# f = open("clusters.data", 'w')
import sys
f = sys.stdout

for i, node in enumerate(tree):
	if node.left > 0:
		left = words[node.left]		## try [node.left -1]
	else:
		left = str(node.left)
	if node.right > 0:
		right = words[node.right]
	else:
		right = str(node.right)
	f.write( "%d: (%s, %s)=%1.3f\n" % (i+1, left , right , node.distance) )

# print "Saved file: ", f.name
# f.close()

## ========================== plot dendrogram ======================================

f = open("100B-clust.graphml", 'w')
f.write("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n")
f.write("<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:y=\"http://www.yworks.com/xml/graphml\" xmlns:yed=\"http://www.yworks.com/xml/yed/3\" xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd\">\n")
f.write("  <!--Created by yFiles for Java 2.9-->\n")
f.write("  <key for=\"graphml\" id=\"d0\" yfiles.type=\"resources\"/>\n")
f.write("  <key for=\"port\" id=\"d1\" yfiles.type=\"portgraphics\"/>\n")
f.write("  <key for=\"port\" id=\"d2\" yfiles.type=\"portgeometry\"/>\n")
f.write("  <key for=\"port\" id=\"d3\" yfiles.type=\"portuserdata\"/>\n")
f.write("  <key attr.name=\"url\" attr.type=\"string\" for=\"node\" id=\"d4\"/>\n")
f.write("  <key attr.name=\"description\" attr.type=\"string\" for=\"node\" id=\"d5\"/>\n")
f.write("  <key for=\"node\" id=\"d6\" yfiles.type=\"nodegraphics\"/>\n")
f.write("  <key attr.name=\"Description\" attr.type=\"string\" for=\"graph\" id=\"d7\"/>\n")
f.write("  <key attr.name=\"url\" attr.type=\"string\" for=\"edge\" id=\"d8\"/>\n")
f.write("  <key attr.name=\"description\" attr.type=\"string\" for=\"edge\" id=\"d9\"/>\n")
f.write("  <key for=\"edge\" id=\"d10\" yfiles.type=\"edgegraphics\"/>\n")
f.write("  <graph edgedefault=\"directed\" id=\"G\">\n")

## determine which node is connected to which others
## case 1:  leaf-leaf
## case 2:  leaf-node
## case 3:  node-node
## They are all nodes, so we have a graph.  Edges are labeled with distances.

## First create all nodes:  words nodes and level nodes (the latter has the same number as word nodes)
for i, x in enumerate(words):
	f.write("<node id=\"%s\">\n" % x)
	f.write("<data key=\"d6\"><y:ShapeNode>\n")
	f.write("<y:BorderStyle hasColor=\"false\" type=\"line\" width=\"1.0\"/>\n")
	f.write("<y:Fill color=\"#FFB0B0\" transparent=\"false\"/>\n")
	f.write("<y:NodeLabel fontSize=\"25\" fontStyle=\"bold\">%s</y:NodeLabel></y:ShapeNode></data></node>\n" % x)

	if (i + 1) != n:
		f.write("<node id=\"N%d\">\n" % (i+1))
		f.write("<data key=\"d6\"><y:ShapeNode>\n")
		f.write("<y:BorderStyle hasColor=\"false\" type=\"line\" width=\"1.0\"/>\n")
		f.write("<y:Fill color=\"#B0B0FF\" transparent=\"false\"/>\n")
		f.write("<y:NodeLabel fontSize=\"25\" fontStyle=\"bold\">%d</y:NodeLabel></y:ShapeNode></data></node>\n" % (i+1))

import math		# for isnan()

## Then create edges.
## For each node there are 2 links:
##		from Left and Right to Self,
##		and from Self to Parent (= Self + 1)
num_links = 0
for i, node in enumerate(tree):
	if math.isnan(node.distance):
		d = 1
		hex_d = 40
	else:
		## use this as edge thickness:
		d = (1.0 - node.distance)*5.0 + 1.0
		## use this as edge color:
		hex_d = int((1.0 - node.distance)*205.0) + 50
	num_links += 1

	if node.left >= 0:
		left = words[node.left]
	else:
		left = "N%d" % -node.left
	if node.right >= 0:
		right = words[node.right]
	else:
		right = "N%d" % -node.right

	## edge from Left leaf to Self
	f.write("<edge id=\"L%d\" source=\"%s\" target=\"N%d\"><data key=\"d10\"><y:PolyLineEdge>\n" % (num_links, left, i+1))
	f.write("<y:LineStyle color=\"#FF3333%02X\" type=\"line\" width=\"%02.3f\"/>\n" % (hex_d, d))
	f.write("<y:Arrows source=\"none\" target=\"standard\"/>\n")
	f.write("</y:PolyLineEdge></data></edge>\n")

	## edge from Right leaf to Self
	f.write("<edge id=\"R%d\" source=\"%s\" target=\"N%d\"><data key=\"d10\"><y:PolyLineEdge>\n" % (num_links, right, i+1))
	f.write("<y:LineStyle color=\"#FF3333%02X\" type=\"line\" width=\"%02.3f\"/>\n" % (hex_d, d))
	f.write("<y:Arrows source=\"none\" target=\"standard\"/>\n")
	f.write("</y:PolyLineEdge></data></edge>\n")

	## edge from Self to Self+1
	if i+2 != n:
		f.write("<edge id=\"S%d\" source=\"N%d\" target=\"N%d\"><data key=\"d10\"><y:PolyLineEdge>\n" % (num_links, i+1, i+2))
		f.write("<y:LineStyle color=\"#3333FF%02X\" type=\"line\" width=\"%02.3f\"/>\n" % (100, 4.0))	# hex_d, d
		f.write("<y:Arrows source=\"none\" target=\"standard\"/>\n")
		f.write("</y:PolyLineEdge></data></edge>\n")

f.write("</graph>\n")
f.write("</graphml>\n")

print "Written to file: ", f.name
f.close()
exit()

## =============================== old crap ======================================

f.write("digraph G {\n")

for i, n in enumerate(tree):
	f.write( "n%d" % i + " [color=red];\n" )
	f.write( "l%d" % i + " [color=yellow, label=\"%s\"];\n" % words[i] )
	if n.left < 0:
		left = "n%d" % -n.left
	else:
		left = "l%d" % n.left
	if n.right < 0:
		right = "n%d" % -n.right
	else:
		right = "l%d" % n.right
	if i != -n.left:
		f.write( "n%d" % i + " -> " + left + "[weight=\"%.3f\"]" % n.distance +";\n" )
	if i != -n.right:
		f.write( "n%d" % i + " -> " + right + "[weight=\"%.3f\"]" % n.distance +";\n")

f.write("}\n")
print "Saved file: ", f.name
f.close()
