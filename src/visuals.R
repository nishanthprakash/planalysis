#library(ggdendro)

setwd("~/Projects/Plan Composition/pyret-starter/planalysis/data/distances")




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

#ggdendrogram(hc, rotate = TRUE, size = 4, theme_dendro = FALSE, color = "tomato")
# plot(hclust(dist(cor)))
# write.csv(cor, file = "cor.csv")

hcd = as.dendrogram(hc)

# vector of colors 
labelColors = c('orange', 'darkblue', 'red', 'darkgrey', 'green', 'lightblue')

# cut dendrogram in 4 clusters
clusMember = cutree(hc, 6)
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
# using dendrapply
clusDendro = dendrapply(hcd, colLab)
# make plot
dir.create(file.path('..', 'plots'), showWarnings = FALSE)
png("../plots/1.png", width=12,height=6,units="in", res=800)
par(cex=0.3, mar=c(10,3,0.5,0.5)) 
plot(clusDendro, cex=0.5)
dev.off()
