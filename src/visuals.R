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
      rbind(accc, read.csv(paste("./measures/", mfn, "f", i, "_", tc, ".csv", sep=""), header=FALSE)[3]), c("", msubs))
    png(paste("./plots/counts/", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    par(cex=0.4, mar=c(15,3,3,3)) 
    tabc = table(counts)
    cous = lapply(1:length(tabc), function(i) tabc[[i]])
    maxcou = cous[which.max(abs(as.numeric(cous)))]
    barplot(tabc, las = 2, yaxt = "n")
    ylabs = c(1:maxcou[[1]])
    axis(2, at=ylabs,labels=ylabs, pos=-1, las=2, cex=0.4)
    abline(h=ylabs, col="darkgray", lty=1, lwd=0.2)
    dev.off()
  }
}

for (mfn in mfns){
  for (tc in mtcs){
    for (sub in msubs){
      studfns = read.csv(paste("./measures/", mfn, "f", sub, "_", tc, ".csv", sep=""), header=FALSE)
      png(paste("./plots/order/", mfn, "f", sub, "_", tc,".png", sep=""), width=12,height=12,units="in", res=1000)
      par(cex=0.1, mar=c(15,3,3,8))
      c13 = studfns[, c(1, 3)]
      c23 = studfns[, c(2, 3)]
      names(c13) = names(c23)
      i = rbind(c13, c23)
      i = i[order(i[,1]), ]
      for (i1 in i[1]) {}
      for (i2 in i[2]) {}
      
      for (c1 in studfns[1]) {}
      for (c2 in studfns[2]) {}
      for (c3 in studfns[3]) {}
      j = sort(c(c1, c2))
      
      fs = unique(c3)
      fcols = rainbow(length(fs))
      
      plot.new()
      plot.window(xlim = range(j), ylim = range(j))
      axis(1, at=i1,labels=i2, pos=-5, las=2)
      axis(2, at=j,labels=j, pos=-5, las=2)
      
      par(xpd=TRUE, cex=0.5)
      legend("top",legend=lapply(fs, function(x)as.character(x)), col=fcols, pch=15, pt.cex=5,  horiz=TRUE)
      
      clip(-5, length(j), -5, length(j))
      abline(h=j, v=j, col="darkgray", lty=1, lwd=0.2)
      
      for (ind in 1:length(studfns[, 1])){
        fin = match(c(as.character(studfns[ind,3])), fs)
        x = c(studfns[ind,][1], studfns[ind,][1], studfns[ind,][2], studfns[ind,][2])
        y = c(studfns[ind,][1], studfns[ind,][2], studfns[ind,][2], studfns[ind,][1])
        polygon(x, y, col=fcols[fin])
      }
      
      dev.off()
    }
  }
}