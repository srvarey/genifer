// hierarchical clustering in R

dissim <- read.table("C:\\python27\\ontology\\dissim-100B.csv", header=TRUE, sep=",")

// remove first and last columns
dissim <- subset(dissim, select = -1)
dissim <- subset(dissim, select = -86)

// convert dissimilarity matrix to (upper?) triangular
distance <- as.dist(dissim)

// call clustering, "single" seems better
hc <- fastcluster::hclust(dis, method="single")

// convert result to dendrogram data structure
den <- as.dendrogram(hc)

// plot the dendrogram, dLeaf controls the leaf labels
plot(den, ylim=c(1,86), xlim=c(0,1), horiz=TRUE, dLeaf=-0.03, cex=0.5)

// import labels from words
labels <- read.table("C:\\python27\\ontology\\labels.csv", header=FALSE, sep=",")
// try to reorder labels, but doesn't work!
reorder(labels, hc$order)  ??
// On IRC: <mrflick> how about factor(labels, levels=h$horder) ??

// this can change the labels
mtext(labels, side=2, at=1:86, las=2, col=1:86, cex=0.5, line=0)
