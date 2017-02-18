setwd("~/Projects/Plan Composition/pyret-starter/planalysis/data")

dfs = list.files(path = "./distances/")
dfs1 = unique(lapply(dfs, function (i) strsplit(i, "f")[[1]]))
dfns <<- unique(lapply(dfs1, function (i) i[1]))
dfs2 <<- unique(lapply(dfs1, function (i) strsplit(i[2], "x")[[1]][1]))
dfs3 = unique(lapply(dfs2, function (i) strsplit(i, "_")[[1]]))
dsubs <<- unique(lapply(dfs3, function (i) i[1]))
dtcs <<- unique(lapply(dfs3, function (i) i[2]))

unlink(file.path('.', 'plots'), recursive = TRUE, force = FALSE)
dir.create(file.path('.', 'plots'), showWarnings = FALSE)

mfs = list.files(path = "./measures/")
mfs1 = unique(lapply(mfs, function (i) strsplit(i, "f")[[1]]))
mfns <<- unique(lapply(mfs1, function (i) i[1]))
mfs2 = unique(lapply(mfs1, function (i) strsplit(i[2], "_")[[1]]))
msubs <<- unique(lapply(mfs2, function (i) i[1]))
mtcs <<- unique(lapply(mfs2, function (i) strsplit(i[2], "[.]")[[1]][1]))

dir.create(file.path('.', 'plots/distances'), showWarnings = FALSE)
dir.create(file.path('.', 'plots/counts'), showWarnings = FALSE)
dir.create(file.path('.', 'plots/order'), showWarnings = FALSE)

for (dfn in dfns){
  for (tc in dtcs){
    cor = 
      (Reduce(function(accr, i) 
        rbind(accr, Reduce(function(accc, j) 
          cbind(accc, read.csv(paste("./distances/", dfn, "f", i, "_", tc, "x", j, "_", tc,".csv", sep=""), row.names=1, header=TRUE)), 
          c("", dsubs))), 
        c("", dsubs)))[-1, -1]
    
    hc = hclust(dist(cor))
    
    png(paste("./plots/distances/", dfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    par(cex=0.4, mar=c(10,3,3,3)) 
    plot(hc, hang = -1)
    dev.off()
  }
}

for (mfn in mfns){
  for (tc in mtcs){
    counts = Reduce(function(accc, i) 
      rbind(accc, read.csv(paste("./measures/", mfn, "f", i, "_", tc, ".csv", sep=""), header=FALSE)), msubs)
    png(paste("./plots/counts/", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    par(cex=0.4, mar=c(15,3,3,3)) 
    barplot(table(counts), las = 2)
    dev.off()
  }
}

for (mfn in mfns){
  for (tc in mtcs){
    for (sub in msubs){
      studfns = read.csv(paste("./measures/", mfn, "f", sub, "_", tc, ".csv", sep=""), header=FALSE)
      png(paste("./plots/order/", mfn, "f", sub, "_", tc,".png", sep=""), width=12,height=6,units="in", res=800)
      par(cex=0.2, mar=c(15,3,3,3))
      for (x in studfns) {}
      ord = as.numeric(interaction(x))
      names(ord) = x
      barplot(ord, las = 2)
      dev.off()
    }
  }
}