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

fs = list.files(path = ".")
fs1 = unique(lapply(fs, function (i) strsplit(i, "f")[[1]]))
distfns <<- unique(lapply(fs1, function (i) i[1]))
fs2 <<- unique(lapply(fs1, function (i) strsplit(i[2], "x")[[1]][1]))
fs3 = unique(lapply(fs2, function (i) strsplit(i, "_")[[1]]))
subs <<- unique(lapply(fs3, function (i) i[1]))
tcs <<- unique(lapply(fs3, function (i) i[2]))

unlink(file.path('..', 'plots'), recursive = TRUE, force = FALSE)
dir.create(file.path('..', 'plots'), showWarnings = FALSE)

for (dfn in distfns){
  for (tc in tcs){
    cor = 
      (Reduce(function(accr, i) 
        rbind(accr, Reduce(function(accc, j) 
          cbind(accc, read.csv(paste(dfn, "f", i, "_", tc, "x", j, "_", tc,".csv", sep=""), row.names=1, header=TRUE)), 
          c("", subs))), 
        c("", subs)))[-1, -1]
    
    hc = hclust(dist(cor))
    
    #hcd = as.dendrogram(hc)
    #labelColors = palette(rainbow(6))
    #clusMember = cutree(hc, 6)
    #clusDendro = dendrapply(hcd, colLab)
    
    png(paste("../plots/", dfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    par(cex=0.4, mar=c(10,3,3,3)) 
    #plot(clusDendro, cex=0.5)
    plot(hc, hang = -1)
    dev.off()
  }
}
