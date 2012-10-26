## Ontology
## ================================================================

## Use a smaller list of words:

# words = ["a", "about", "all", "an", "and", "are", "as", "at", "be", "but", "by", "can", "do", "down", "first", "for", "from", "good", "have", "he", "her", "him", "his", "I", "if", "in", "into", "is", "it", "just", "like", "listen", "little", "make", "man", "many", "may", "more", "most", "my", "near", "no", "not", "now", "of", "on", "one", "only", "or", "other", "out", "over", "people", "read", "said", "say", "see", "she", "should", "slow", "small", "so", "some", "stop", "than", "that", "the", "then", "there", "they", "this", "through", "to", "true", "two", "up", "use", "very", "was", "water", "way", "we", "were", "what", "when", "where", "which", "who", "will", "with", "would", "write", "yes", "you"]

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

## The full similarity matrix is of size n*n.  So half of it is n*n/2, but we have to delete half of the diagonal, which is of size n/2.  That gives the above result.
## To map each similarity entry to the triangular matrix:  Assume coordinates are (i, j), starting with 1, and j < i, so it is the lower triangle.  From the first entry (i=1, j=1) to the last entry (i = n, j = n-1).  We should add j to the lower triangle up to i-1, which is j + (i-1)(i-2)/2.
## Lets check: If i=j=1, it is 1+0 = 1.  If (i=n, j=n-1), it is n-1 + (n-1)(n-2)/2 = (n-1)(2+n-2)/2 = (n-1)(n)/2 = exactly the size we want.
## Now adjust for the fact that arrays start with 0.  So the new formula is for (i,j) both starting at 0:
##		(i,j) -> j+1 + (i)(i-1)/2 -1   (where the last -1 is to adjust for the result array starting with 0)
## thus: (i,j) -> j + (i)(i-1)/2

## Between each pair of word find similarity distance

num_links = 0

for i,x in enumerate(syns):
	for j in range(0,i):		# range(0,i) = 0,...,i-1 excluding i
		y = syns[j]
		k = 0
		max_sim = None
		for x1 in x:
			for y1 in y:
				sim1 = x1.wup_similarity(y1)
				if sim1 > max_sim: max_sim = sim1

		if (max_sim is not None) and max_sim*5.0 > 1.5:
			f.write("<edge id=\"e%d\" source=\"%s\" target=\"%s\"><data key=\"d10\"><y:ArcEdge>\n" % (num_links, words[i], words[j]))
			hex_max_sim = int(max_sim*100)
			f.write("<y:LineStyle color=\"#0033FF%02X\" type=\"line\" width=\"%02.3f\"/>\n" % (hex_max_sim, max_sim*5.0))
			num_links += 1
			f.write("</y:ArcEdge></data></edge>\n")
	print "\b%d" % (n-i) + '.',

print "\nreal links = ", num_links

f.write("</graph>\n")
f.write("</graphml>\n")

# f.write("}\n")

print "Written to file: ", f.name
f.close()

exit()
