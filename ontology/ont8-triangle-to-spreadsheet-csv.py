## Ontology
## ================================================================

import pickle

f = open("words100B.data", 'r')
words = pickle.load(f)
f.close()

# words = ["cat", "dog", "man", "worm"]

f = open("lower-tri-100B.data",'r')
triangle = pickle.load(f)
f.close()

f = open("dissim-100B.csv", 'w')

f.write(", ")			# blank square at upper left corner
for x in words:
	f.write("%s, " % x)
f.write("\b\b\n")		# erase last comma

n = len(words)

for i in range(0, n):			# i = row
	f.write("%s, " % words[i])
	for j in range(0, n):
		if j < i:
			sim = triangle[j + i*(i-1)/2]
		elif j > i:
			sim = triangle[i + j*(j-1)/2]
		else:
			sim = 1.0
		if sim is not None:
			sim = 1.0 - sim
			f.write( "%.3f, " % sim )
		else:
			f.write( ", " )
	f.write("\b\b\n")	# erase last comma

print "Written to file: ", f.name
f.close()
