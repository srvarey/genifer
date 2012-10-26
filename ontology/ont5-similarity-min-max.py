## Ontology
## ================================================================

## Use a smaller list of words:

words = ["again", "air", "along", "also", "always", "another", "any", "around", "away", "back", "because", "below", "between", "big", "both", "came", "children", "come", "day", "different", "end", "even", "every", "few", "find", "food", "form", "get", "give", "go", "great", "help", "here", "home", "house", "important", "keep", "large", "last", "left", "line", "look", "me", "men", "might", "much", "must", "name", "never", "new", "next", "number", "off", "old", "our", "own", "part", "place", "put", "right", "same", "set", "show", "something", "sound", "still", "such", "take", "tell", "think", "thought", "three", "through", "together", "too", "under", "until", "us", "want", "well", "went", "while", "why", "work", "world"]

# words = ["dog", "cat"]

## Use WordNet
from nltk.corpus import wordnet as wn

from sets import Set

## Generate a graph file containing these words as nodes

f = open("words.graphml", 'w')
# f.write("digraph g {\n")

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

for x in words:
	# f.write("%s [label=\"%s\", color=\"red\"]\n" % (x, x))
	f.write("<node id=\"%s\">\n" % x)
	f.write("<data key=\"d6\"><y:ShapeNode>\n")
	f.write("<y:Fill color=\"#FFB0B0\" transparent=\"false\"/>\n")
	f.write("<y:NodeLabel>%s</y:NodeLabel></y:ShapeNode></data></node>\n" % x)

print "initial words = ", len(words)

## find the syn sets of each word
syns = []
for x in words:
	syns.append(wn.synsets(x))

## declare lower triangular matrix
n = len(syns)
# triangle = [0.0] * (n*(n-1)/2)

print "number of synsets = ", n
print "size of lower triangular matrix = ", n*(n-1)/2

## Between each pair of word find similarity distance

num_links = 0

for i,x in enumerate(syns):
	for j in range(0,i):
		y = syns[j]
		k = 0
		max_sim = None
		min_sim = None
		for x1 in x:
			for y1 in y:
				sim1 = x1.path_similarity(y1)
				if sim1 is not None:
					if sim1 > max_sim: max_sim = sim1
					if min_sim is None:
						min_sim = sim1
					else:
						if sim1 < min_sim: min_sim = sim1
		if (max_sim is not None) or (min_sim is not None):
			f.write("<edge id=\"e%d\" source=\"%s\" target=\"%s\"><data key=\"d10\"><y:ArcEdge>\n" % (num_links, words[i], words[j]))
		if (max_sim is not None) and (min_sim is not None):
			f.write("<y:EdgeLabel>%.3f, %.3f</y:EdgeLabel>\n" % (max_sim, min_sim))
			num_links += 1
		elif (max_sim is not None):
			f.write("<y:EdgeLabel>%.3f</y:EdgeLabel>\n" % max_sim)
			num_links += 1
		elif (min_sim is not None):
			f.write("<y:EdgeLabel>%.3f</y:EdgeLabel>\n" % min_sim)
			num_links += 1
		if (max_sim is not None) or (min_sim is not None):
			f.write("</y:ArcEdge></data></edge>\n")
	print "\b%d" % (n-i) + '.',

print "\nreal links = ", num_links

f.write("</graph>\n")
f.write("</graphml>\n")

# f.write("}\n")

print "Written to file: ", f.name
f.close()

exit()
