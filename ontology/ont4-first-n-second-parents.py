## Ontology
## ================================================================

## Use a smaller list of words:

words = ["again", "air", "along", "also", "always", "another", "any", "around", "away", "back", "because", "below", "between", "big", "both", "came", "children", "come", "day", "different", "end", "even", "every", "few", "find", "food", "form", "get", "give", "go", "great", "help", "here", "home", "house", "important", "keep", "large", "last", "left", "line", "look", "me", "men", "might", "much", "must", "name", "never", "new", "next", "number", "off", "old", "our", "own", "part", "place", "put", "right", "same", "set", "show", "something", "sound", "still", "such", "take", "tell", "think", "thought", "three", "through", "together", "too", "under", "until", "us", "want", "well", "went", "while", "why", "work", "world"]

# words = ["dog", "cat"]

## Use WordNet
from nltk.corpus import wordnet as wn

from sets import Set

## Generate a GraphML file containing these words as nodes
## For each word find parent, repeat
## Prune parents that has only 1 child

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

nodes = Set([])
maybe_links = Set([])
new_words = []

for x in words:
	for s in wn.synsets(x):
		for h in s.hypernyms():
			name = h.name.split('.')[0]
			# name = name.replace("-", "~")
			nodes.add(h)
			new_words.append(name)
			maybe_links.add((x, name))

print "first parents = ", len(new_words)

new_words2 = []
for x in new_words:
	for s in wn.synsets(x):
		for h in s.hypernyms():
			name = h.name.split('.')[0]
			# name = name.replace("-", "~")
			if h in nodes:
				maybe_links.add((x, name))
			else:
				nodes.add(h)
				new_words2.append(name)
				modified = True
				maybe_links.add((x, name))

print "second parents = ", len(new_words2)
print "first & second parents = ", len(nodes)

for x in new_words:
	f.write("<node id=\"%s\">\n" % x)
	f.write("<data key=\"d6\"><y:ShapeNode>\n")
	if x in words:
		f.write("<y:Fill color=\"#FFB0B0\" transparent=\"false\"/>\n")
	f.write("<y:NodeLabel>%s</y:NodeLabel></y:ShapeNode></data></node>\n" % x)

for x in new_words2:
	f.write("<node id=\"%s\">\n" % x)
	f.write("<data key=\"d6\"><y:ShapeNode>\n")
	if x in words:
		f.write("<y:Fill color=\"#FFB0B0\" transparent=\"false\"/>\n")
	f.write("<y:NodeLabel>%s</y:NodeLabel></y:ShapeNode></data></node>\n" % x)

num_links = 0
for (y1, y2) in maybe_links:
	# f.write("%s -> %s\n" % (y1, y2))
	# <edge source="n0" target="n2"/>
	f.write("<edge source=\"%s\" target=\"%s\" />\n" % (y1, y2))
	num_links += 1

print "links = ", num_links

f.write("</graph>\n")
f.write("</graphml>\n")

# f.write("}\n")

print "Written to file: ", f.name
f.close()

exit()
###################################################################################

while True:
	modified = False
	words = new_words
	new_words = []
	for x in words:
		for s in wn.synsets(x):
			for h in s.hypernyms():
				name = h.name.split('.')[0]
				# name = name.replace("-", "~")
				if h in nodes:
					maybe_links.add((x, name))
				else:
					nodes.add(h)
					new_words.append(name)
					modified = True
					maybe_links.add((x, name))
	if not modified:
		break

print "total nodes = ", len(nodes)

## For any intermediate node to be meaningful, it should either:
## -- have 2 red leaves
## -- be in a /\ chain with 2 red leaves

nodes_list = []
for x in nodes:
	nodes_list.append(x.name.split('.')[0])
nodes_score = [0] * len(nodes_list)

ancestors = nodes_list

#for j in range(0, 1):
for x in ancestors:
	ancestors2 = []
	for (y1, y2) in maybe_links:
		if x == y1:
			i = nodes_list.index(y2)
			nodes_score[i] += 1
			ancestors2.append(y2)
ancestors = ancestors2

# first_parents = Set([])
# real_parents = Set([])

# for (fr, to) in maybe_links:
	# if to in first_parents:
		# real_parents.add(to)
	# else:
		# first_parents.add(to)

print "nodes scores = ", nodes_score

num_links = 0
for (y1, y2) in maybe_links:
	i = nodes_list.index(y2)
	if nodes_score[i] >= 10:
		f.write("%s -> %s\n" % (y1, y2))
		num_links += 1

# print "first parents = ", len(first_parents)
# print "real parents = ", len(real_parents)

# num_links = 0
# for (fr, to) in maybe_links:
	# if (to in real_parents) and (fr in real_parents):
		# f.write("%s -> %s\n" % (fr, to))
		# num_links += 1

print "links = ", num_links

f.write("}\n")

print "Written to file: ", f.name
f.close()

## Old crap
