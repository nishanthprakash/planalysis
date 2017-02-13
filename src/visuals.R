#library(ggdendro)
setwd("~/Projects/Plan Composition/pyret-starter/planalysis/data/distances")

# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

fs1 = list.files(path = ".", pattern = "^1_*")
fs = list.files(path = ".")
dists <<- unique(lapply(fs, function (i) strsplit(i, "_")[[1]][1]))
files <<- unique(lapply(fs1, function (i) strsplit(i, "_")[[1]][2]))

cor = 
  (Reduce(function(accr, i) 
    rbind(accr, Reduce(function(accc, j) 
        cbind(accc, read.csv(paste("1_", i, "_", j, ".csv", sep=""), row.names=1, header=TRUE)), 
        c("", files))), 
    c("", files)))[-1, -1]

hc = hclust(dist(cor))

hcd = as.dendrogram(hc)

# vector of colors 
labelColors = palette(rainbow(6))

clusMember = cutree(hc, 6)

# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot
dir.create(file.path('..', 'plots'), showWarnings = FALSE)
png("../plots/1.png", width=12,height=6,units="in", res=800)
par(cex=0.4, mar=c(10,3,0.5,0.5)) 
plot(clusDendro, cex=0.5)
dev.off()
