## Ontology
## ================================================================

## Read in an external ontology and distribute the nodes in high dimensional space.

import pickle
f = open("clusters.data",'r')
tree = pickle.load(f)
f.close()

print "Generating graph..."

f = open("clusters.data",'w')
pickle.dump(triangle, f)
f.close()

print "Saved graph.data"
