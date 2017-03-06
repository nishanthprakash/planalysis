library(moments)

setwd("~/Projects/Plan Composition/planalysis/data")

dir.create(file.path('.', 'plots2'), showWarnings = FALSE)

mfs = list.files(path = "./measures/")
mfs1 = unique(lapply(mfs, function (i) strsplit(i, "f")[[1]]))
mfns <<- unique(lapply(mfs1, function (i) i[1]))
mfs2 = unique(lapply(mfs1, function (i) strsplit(i[2], "_")[[1]]))
msubs <<- unique(lapply(mfs2, function (i) i[1]))
mtcs <<- unique(lapply(mfs2, function (i) strsplit(i[2], "[.]")[[1]][1]))

unlink(file.path('.', 'plots2/planclusters'), recursive = TRUE, force = FALSE)
dir.create(file.path('.', 'plots2/planclusters'), showWarnings = FALSE)
for (mfn in mfns){
  for (tc in mtcs){
    disj = list()
    maxints = 0
    for (sub in msubs){
      studfnscsv = read.csv(paste("./measures/", mfn, "f", sub, "_", tc, ".csv", sep=""), header=FALSE)
      studfns = data.frame(lapply(studfnscsv, function(x) {gsub(":_", ":", x)}))
      c3 = as.vector(studfns[3])
      fs = unique(c3)
      fs = data.frame(fs)
      for (i in 1:nrow(fs)){ fs[i,2] = FALSE}
      
      rownames(fs) = fs[, 1]
      fs[, 1] = NULL
      
      studfns = studfns[order(as.character(studfns$V3), as.numeric(as.character(studfns$V1))), ]
      
      prevfn = ""
      prevstart = 0
      prevend = 0
      rec = FALSE
      for(i in 1:nrow(studfns)){
        if(as.character(studfns[i,3]) == prevfn && !rec){
          if (prevstart < as.numeric(as.character(studfns[i,1])) && as.numeric(as.character(studfns[i,2])) < prevend){
            rec = TRUE
          }
        } else {
          if (as.character(studfns[i, 3]) != prevfn && prevfn != ""){
            fs[as.character(prevfn), 1] = rec
            prevfn = ""
            prevstart = 0
            prevend = 0
            rec = FALSE
          }
        }
        prevfn = as.character(studfns[i, 3])
        prevstart = as.numeric(as.character(studfns[i,1]))
        prevend = as.numeric(as.character(studfns[i,2]))
      }
      
      studfns = studfns[order(as.numeric(as.character(studfns$V1))), ]
      
      c13 = cbind(studfns[, c(1, 3)], 1:nrow(studfns[, c(1, 3)]))
      c23 = cbind(studfns[, c(2, 3)], 1:nrow(studfns[, c(1, 3)]))
      names(c13) = names(c23)
      fnpos = rbind(c13, c23)
      fnpos = fnpos[order(as.numeric(as.character(fnpos[,1]))), ]
      
      for(i in 1:nrow(fnpos)){
        if (!fs[as.character(fnpos[i, 2]), 1]){
          fnpos[-(1:i), 1] = as.numeric(as.character(fnpos[-(1:i), 1])) - 1
        }
      }
      recinds = fs[as.character(fnpos[, 2]), 1]
      recfns = fnpos[recinds, ]
      
      for(i in 1:nrow(recfns)){
        recfns[i, 4] = as.numeric(as.character(recfns[recfns[, 3] == recfns[i, 3], ][2, 1]))
      }
      finalfns = (recfns[recfns[, 1] != recfns[, 4], ])[, c(1, 4, 2)]
      finalfns = finalfns[order(as.numeric(as.character(finalfns[,1]))), ]
      
      #---------------------------------------------------
      
      disj[[sub]] = data.frame(cbind(as.numeric(as.vector(finalfns[, 1])), as.vector(finalfns[, 2]), as.character(as.vector(finalfns[, 3]))))
      
      max = finalfns[[2]][which.max(abs(as.numeric(finalfns[[2]])))]
      len = length(disj[[sub]])
    }
    #break 
    m = NULL
    maxints = 0
    for(i in names(disj)){ 
      maxints = if (maxints < length(disj[[i]][1:2][,1])) length(disj[[i]][1:2][,1]) else maxints
    }
    for(i in names(disj)){
      disjel = as.numeric(as.vector(t(disj[[i]][1:2]))) - 1
      disjel = disjel/tail(disjel, n=1)
      m = rbind(m, c(disjel, rep(1, 2*maxints - length(disjel))))
    }
    png(paste("./plots2/planclusters/int_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    row.names(m) = names(disj)
    plot.new()
    plot(hclust(dist(m)))
    dev.off()
    
    n0 = NULL
    n1 = NULL
    n2 = NULL
    n3 = NULL
    n4 = NULL
    for(i in names(disj)){
      maxpos = as.numeric(as.vector(tail(disj[[i]], n=1)[[2]])) - 1
      disj[[i]] = (data.frame(lapply(lapply(disj[[i]][1:2], as.vector), as.numeric)) - 1)/maxpos
      
      countint = length(disj[[i]][[1]])
      startend = disj[[i]][[2]] - disj[[i]][[1]]
      center = (disj[[i]][[2]] + disj[[i]][[1]])/2
      
      startmean = mean(disj[[i]][[1]])
      endmean = mean(disj[[i]][[2]])
      lengthmean = mean(startend)
      centermean = mean(center)
      
      startdev = sd(disj[[i]][[1]])
      enddev = sd(disj[[i]][[2]])
      lengthdev = sd(startend)
      centerdev = sd(center)
      
      startsk = skewness(disj[[i]][[1]])
      endsk = skewness(disj[[i]][[2]])
      lengthsk = skewness(startend)
      centersk = skewness(center)
      
      startku = kurtosis(disj[[i]][[1]])
      endku = kurtosis(disj[[i]][[2]])
      lengthku = kurtosis(startend)
      centerku = kurtosis(center)
      
      n0 = rbind(n0, c(startmean, endmean, lengthmean, centermean, startdev, enddev, lengthdev, centerdev, startsk, endsk, lengthsk, centersk, startku, endku, lengthku, centerku))
      n1 = rbind(n1, c(startmean, endmean, lengthmean, startdev, enddev, lengthdev, startsk, endsk, lengthsk, startku, endku, lengthku))
      n2 = rbind(n2, c(startmean, endmean, lengthmean, startdev, enddev, lengthdev, startsk, endsk, lengthsk))
      n3 = rbind(n3, c(startmean, endmean, lengthmean, startdev, enddev, lengthdev))
      n4 = rbind(n4, countint)
    }
    
    row.names(n0) = names(disj)
    row.names(n1) = names(disj)
    row.names(n2) = names(disj)
    row.names(n3) = names(disj)
    row.names(n4) = names(disj)
    
    n0 = n0[complete.cases(n0),]
    n1 = n1[complete.cases(n1),]
    n2 = n2[complete.cases(n2),]
    n3 = n3[complete.cases(n3),]
    n4 = n4[complete.cases(n4),]
    
    png(paste("./plots2/planclusters/stat0_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    plot.new()
    par(xpd=TRUE, cex=0.8)
    plot(hclust(dist(n0)))
    dev.off()
    
    png(paste("./plots2/planclusters/stat1_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    plot.new()
    plot(hclust(dist(n1)))
    dev.off()
    
    png(paste("./plots2/planclusters/stat2_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    plot.new()
    plot(hclust(dist(n2)))
    dev.off()
    
    png(paste("./plots2/planclusters/stat3_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    plot.new()
    plot(hclust(dist(n3)))
    dev.off()
    
    png(paste("./plots2/planclusters/stat4_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    plot.new()
    plot(hclust(dist(n4)))
    dev.off()
    
    
    # The final magnifica:
    png(paste("./plots2/planclusters/ulti_", mfn, "-", tc,".png", sep=""), width=12,height=6,units="in", res=800)
    plot.new()
    plot(hclust(sqrt(dist(n4)^2 + dist(n3)^2)))
    dev.off()
    
  }
  break
}