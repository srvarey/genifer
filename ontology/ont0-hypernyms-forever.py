## Ontology
## ================================================================

## Use a smaller list of words:

words = ["again", "air", "along", "also", "always", "another", "any", "around", "away", "back", "because", "below", "between", "big", "both", "came", "children", "come", "day", "different", "end", "even", "every", "few", "find", "food", "form", "get", "give", "go", "great", "help", "here", "home", "house", "important", "keep", "large", "last", "left", "line", "look", "me", "men", "might", "much", "must", "name", "never", "new", "next", "number", "off", "old", "our", "own", "part", "place", "put", "right", "same", "set", "show", "something", "sound", "still", "such", "take", "tell", "think", "thought", "three", "through", "together", "too", "under", "until", "us", "want", "well", "went", "while", "why", "work", "world"]

words = ["dog"]

## Use WordNet
from nltk.corpus import wordnet as wn

from sets import Set

## Generate a Dot file containing these words as nodes
## For each word find parent, repeat
## Prune parents that has only 1 child

f = open("words.dot", 'w')
f.write("digraph g {\n")

for i,x in enumerate(words):
	f.write("%s [label=\"%s\", color=\"red\"]\n" % (words[i], words[i]))

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

print "first parents = ", len(nodes)

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

exit()

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
f.close()

print "Written to file words.dot"

## Old crap

# f.write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
# f.write("<graphml xmlns=\"http://graphml.graphdrawing.org/xmlns\"\n")
# f.write("	xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n")
# f.write("	xsi:schemaLocation=\"http://graphml.graphdrawing.org/xmlns \n")
# f.write("	http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd\">\n")
# f.write("<graph id=\"G\" edgedefault=\"undirected\">\n")
# <edge source="n0" target="n2"/>

# f.write("</graph>\n")
# f.write("</graphml>\n")
