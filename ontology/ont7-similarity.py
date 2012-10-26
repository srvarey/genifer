## Ontology
## ================================================================

## Use a smaller list of words:

import pickle

f = open("words-100B.data",'r')
words = pickle.load(f)

# words = ["cat", "dog", "man", "worm"]

## Use WordNet
from nltk.corpus import wordnet as wn
from sets import Set

## Generate a graph file containing these words as nodes

print "initial words = ", len(words)

## find the syn sets of each word
syns = []
for x in words:
	syns.append(wn.synsets(x))

## declare lower triangular matrix
n = len(syns)
triangle = [None] * (n*(n-1)/2)

print "number of synsets = ", n
print "size of triangular matrix = ", n*(n-1)/2

## The full similarity matrix is of size n*n.  So half of it is n*n/2, but we have to delete half of the diagonal, which is of size n/2.  That gives the above result.
## To map each similarity entry to the triangular matrix:  Assume coordinates are (i, j), starting with 1, and j < i, so it is the lower triangle.  From the first entry (i=1, j=1) to the last entry (i = n, j = n-1).  We should add j to the lower triangle up to i-1, which is j + (i-1)(i-2)/2.
## Lets check: If i=j=1, it is 1+0 = 1.  If (i=n, j=n-1), it is n-1 + (n-1)(n-2)/2 = (n-1)(2+n-2)/2 = (n-1)(n)/2 = exactly the size we want.
## Now adjust for the fact that arrays start with 0.  So the new formula is for (i,j) both starting at 0:
##		(i,j) -> j+1 + (i)(i-1)/2 -1   (where the last -1 is to adjust for the result array starting with 0)
## thus: (i,j) -> j + (i)(i-1)/2

## Between each pair of word find similarity distance

num_links = 0

# for i,x in enumerate(syns):
	# for j in range(0,i):		# range(0,i) = 0,...,i-1 excluding i

for i,x in enumerate(syns):
	for j in range(0,i):		# range(0,i) = 0,...,i-1 excluding i
		y = syns[j]
		k = 0
		max_sim = None
		for x1 in x:
			for y1 in y:
				sim1 = x1.path_similarity(y1)
				if sim1 > max_sim: max_sim = sim1

		if max_sim is not None:
			num_links += 1
			## link is from words[i] to words[j]
			## The formula below is for the LOWER triangular matrix:
			triangle[j + i*(i-1)/2] = 1.0 - max_sim
			## The formula below is for the UPPER triangular matrix:
			# triangle[i + j*(j-1)/2] = 1.0 - max_sim
	print "\b%d" % (n-i) + '.',

print "\nReal similarities = ", num_links

## This outputs the triangular matrix as a flat array as .CSV file
# f = open("triangle4-flat.csv", 'w')
# for t in triangle:
	# f.write("%0.2f, " % t)
# f.write("\b\b\n")
# f.close()

f = open("lower-tri-100B.data",'w')
pickle.dump(triangle, f)

print "Written to file: ", f.name
f.close()
