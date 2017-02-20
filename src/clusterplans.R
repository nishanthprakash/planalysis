library(moments)

setwd("~/Projects/Plan Composition/pyret-starter/planalysis/data")

mfs = list.files(path = "./measures/")
mfs1 = unique(lapply(mfs, function (i) strsplit(i, "f")[[1]]))
mfns <<- unique(lapply(mfs1, function (i) i[1]))
mfs2 = unique(lapply(mfs1, function (i) strsplit(i[2], "_")[[1]]))
msubs <<- unique(lapply(mfs2, function (i) i[1]))
mtcs <<- unique(lapply(mfs2, function (i) strsplit(i[2], "[.]")[[1]][1]))

unlink(file.path('.', 'plots/planclusters'), recursive = TRUE, force = FALSE)
dir.create(file.path('.', 'plots/planclusters'), showWarnings = FALSE)

for (mfn in mfns){
  for (tc in mtcs){
    disj = list()
    maxints = 0
    for (sub in msubs){
      fn = list()
      sfs = read.csv(paste("./measures/", mfn, "f", sub, "_", tc, ".csv", sep=""), header=FALSE)
      for (i in 1:length(sfs[, 1])){ sfs[i,4] = TRUE}
      for(i in 1:length(sfs[, 1])){
        for(j in i:length(sfs[, 1])){
          if (j > i){
            if ((sfs[j,1] < sfs[i,2]) && (sfs[j,1] > sfs[i,1])){
              sfs[j,4] = FALSE
            }
          }
        }
      }
      
      disj[[sub]] = subset(sfs, V4)[, 1:3]
      
      max = sfs[[2]][which.max(abs(as.numeric(sfs[[2]])))]
      len = length(disj[[sub]])
    }
    m = NULL
    maxints = 0
    for(i in 1:length(disj)){ 
      maxints = if (maxints < length(disj[[i]][1:2][,1])) length(disj[[i]][1:2][,1]) else maxints
    }
    for(i in 1:length(disj)){
      disjel = as.vector(t(disj[[i]][1:2])) - 1
      disjel = disjel/tail(disjel, n=1)
      m = rbind(m, c(disjel, rep(1, 2*maxints - length(disjel))))
    }
    png(paste("./plots/planclusters/int_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    row.names(m) = names(disj)
    plot.new()
    plot(hclust(dist(m)))
    dev.off()
    
    n1 = NULL
    n2 = NULL
    n3 = NULL
    n4 = NULL
    for(i in 1:length(disj)){
      maxpos = tail(disj[[i]], n=1)[[2]] - 1
      disj[[i]] = (disj[[i]]-1)/maxpos
      
      countint = length(disj[[i]][[1]])
      startend = disj[[i]][[2]] - disj[[i]][[1]]
      
      startmean = mean(disj[[i]][[1]])
      endmean = mean(disj[[i]][[2]])
      lengthmean = mean(startend)
      
      startdev = sd(disj[[i]][[1]])
      enddev = sd(disj[[i]][[2]])
      lengthdev = sd(startend)
      
      startsk = skewness(disj[[i]][[1]])
      endsk = skewness(disj[[i]][[2]])
      lengthsk = skewness(startend)
      
      startku = kurtosis(disj[[i]][[1]])
      endku = kurtosis(disj[[i]][[2]])
      lengthku = kurtosis(startend)
      
      n1 = rbind(n1, c(startmean, endmean, lengthmean, startdev, enddev, lengthdev, startsk, endsk, lengthsk, startku, endku, lengthku))
      n2 = rbind(n2, c(startmean, endmean, lengthmean, startdev, enddev, lengthdev, startsk, endsk, lengthsk))
      n3 = rbind(n3, c(startmean, endmean, lengthmean, startdev, enddev, lengthdev))
      n4 = rbind(n4, countint)
    }
      
    png(paste("./plots/planclusters/stat1_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    row.names(n1) = names(disj)
    plot.new()
    plot(hclust(dist(n1)))
    dev.off()

    png(paste("./plots/planclusters/stat2_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    row.names(n2) = names(disj)
    plot.new()
    plot(hclust(dist(n2)))
    dev.off()
    
    png(paste("./plots/planclusters/stat3_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    row.names(n3) = names(disj)
    plot.new()
    plot(hclust(dist(n3)))
    dev.off()
    
    png(paste("./plots/planclusters/stat4_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    row.names(n4) = names(disj)
    plot.new()
    plot(hclust(dist(n4)))
    dev.off()
  }
}