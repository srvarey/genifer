##########################################################################
##																		##
##	REMEMBER TO CHECK THE LINUX VERSION IN /home/yky/ FOR LATEST MODS	##
##																		##
##########################################################################

# Omit from run_all.sh
import xmlrpclib
import random

# Create an object to represent our server.
server_url = 'http://1.36.193.52:20738/RPC2'
server = xmlrpclib.Server(server_url)
G = server.ubigraph
G.clear()

G.set_edge_style_attribute(0, "oriented", "true")
G.set_vertex_style_attribute(0, "color", "#ff00ff")
# G.set_vertex_style_attribute(0, "shape", "sphere")

# root = G.new_vertex()
# G.set_vertex_attribute(root, "label", "Doubleclick me")

highlightStyle = G.new_vertex_style(0)
G.set_vertex_style_attribute(highlightStyle, "color", "#ff80ff")
# G.set_vertex_style_attribute(highlightStyle, "size", "2.0")

myPort = random.randint(20739,20999)

# Set up a callback for left double-clicks on vertices.
G.set_vertex_style_attribute(0, "callback_left_doubleclick",
  "http://1.36.193.52:" + str(myPort) + "/vertex_callback")

# Now make an XMLRPC server to handle tha callbacks.
from SimpleXMLRPCServer import SimpleXMLRPCServer

# Create server
server2 = SimpleXMLRPCServer(("localhost", myPort))
server2.register_introspection_functions()

## YKY's stuff

words = ["again", "air", "along", "also", "always", "another", "any", "around", "away", "back", "because", "below", "between", "big", "both", "came", "children", "come", "day", "different", "end", "even", "every", "few", "find", "food", "form", "get", "give", "go", "great", "help", "here", "home", "house", "important", "keep", "large", "last", "left", "line", "look", "me", "men", "might", "much", "must", "name", "never", "new", "next", "number", "off", "old", "our", "own", "part", "place", "put", "right", "same", "set", "show", "something", "sound", "still", "such", "take", "tell", "think", "thought", "three", "through", "together", "too", "under", "until", "us", "want", "well", "went", "while", "why", "work", "world"]

## Use WordNet
from nltk.corpus import wordnet as wn

for i,x in enumerate(words):
	G.new_vertex_w_id(i)

new_words = []

G.set_vertex_attribute(30, "label", words[30])

# for i,x in enumerate(words):
	# for s in wn.synsets(x):
		# for h in s.hypernyms():
			# name = h.name.split('.')[0]
			# try:
				# j = new_words.index(name) + 90000
			# except ValueError:					# create new node
				# new_words.append(name)
				# j = len(new_words) + 89999
				# G.new_vertex_w_id(j)
				# G.set_vertex_attribute(j, "color", "#eeee77")
			# G.new_edge(i, j)

# for i,x in enumerate(new_words):
	# for s in wn.synsets(x):
		# for h in s.hypernyms():
			# name = h.name.split('.')[0]
			# try:
				# j = new_words.index(name) + 90000
			# except ValueError:					# create new node
				# new_words.append(name)
				# j = len(new_words) + 89999
				# G.new_vertex_w_id(j)
				# G.set_vertex_style_attribute(j, "color", "#eeee77")
			# G.new_edge(i, j)

def vertex_callback(v):
  G.set_vertex_attribute(30, "label", "not so great now!")
  try:
	if v < 90000:
		G.set_vertex_attribute(v, "label", "this is it")
	else:
		G.set_vertex_attribute(v, "label", new_words[v - 90000])

    # G.change_vertex_style(v, highlightStyle)
    # r = G.new_vertex()
    # s = G.new_vertex()
    # G.new_edge(v,r)
    # G.new_edge(v,s)
    # G.set_vertex_attribute(r, "color", str(random.randint(1,1024)))
    # G.set_vertex_attribute(s, "color", str(random.randint(1,1024)))

    # Make the vertex we clicked on shrink slowly
    # for i in range(1,11):
    #    time.sleep(0.1)
    #    G.set_vertex_attribute(v, "size", str(2.0 - i/10.0))
    # G.change_vertex_style(v, 0)

    # Get rid of "Doubleclick me" on root, if it's still there
    # G.change_vertex_style(root, 0)

  except:
    return -1

  return 0

server2.register_function(vertex_callback)

# Run the server's main loop
print "Listening for callbacks from ubigraph_server on port " + str(myPort)
server2.serve_forever()
