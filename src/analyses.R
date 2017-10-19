library(jsonlite)
library(data.table)
library(moments)
library(stringdist)
library(stringr)
library(googleVis)

del_adjust <-function(studfns, fs) {
  studfns = studfns[order(as.numeric(as.character(studfns[, 1]))), ]
  
  c13 = cbind(studfns[, c(1, 3)], 1:nrow(studfns[, c(1, 3)]))
  c23 = cbind(studfns[, c(2, 3)], 1:nrow(studfns[, c(1, 3)]))
  
  c456 = cbind(studfns[, c(4, 5, 6)], 1:nrow(studfns[, c(1, 3)]))
  
  names(c13) = names(c23)
  fnpos = rbind(c13, c23)
  fnpos = fnpos[order(as.numeric(as.character(fnpos[,1]))), ]
  
  for(i in 1:nrow(fnpos)){
    if (!fs[as.character(fnpos[i, 2]), 1]){
      fnpos[-(1:i), 1] = as.numeric(as.character(fnpos[-(1:i), 1])) - 1
    }
  }
  recinds = fs[as.character(fnpos[, 2]), 1]
  recfns = fnpos[recinds, ] #deletes non recursive functions
  
  for(i in 1:nrow(recfns)){
    recfns[i, 4] = as.numeric(as.character(recfns[recfns[, 3] == recfns[i, 3], ][2, 1]))
    recfns[i, 5] = c456[recfns[i, 3] == c456[, 4], 1]
    recfns[i, 6] = c456[recfns[i, 3] == c456[, 4], 2]
    recfns[i, 7] = c456[recfns[i, 3] == c456[, 4], 3]
  }
  finalfns = (recfns[recfns[, 1] != recfns[, 4], ])[, c(1, 4, 2, 5, 6, 7)]
  finalfns = finalfns[order(as.numeric(as.character(finalfns[,1]))), ]
  
  return(finalfns)  
}

del_inner_adjust <-function(studfns) {
  studfns = studfns[order(as.numeric(as.character(studfns[, 1]))), ]
  
  c13 = cbind(studfns[, c(1, 3)], 1:nrow(studfns[, c(1, 3)]))
  c23 = cbind(studfns[, c(2, 3)], 1:nrow(studfns[, c(1, 3)]))
  
  c456 = cbind(studfns[, c(4, 5, 6)], 1:nrow(studfns[, c(1, 3)]))
  
  names(c13) = names(c23)
  fnpos = rbind(c13, c23)
  fnpos = fnpos[order(as.numeric(as.character(fnpos[,1]))), ]

  setnames(fnpos, c("X1", "X2", "row"))
  
  i = 1
  curfn = as.character(fnpos[i, 2])
  curfn_start = as.numeric(as.character(fnpos[i, 1]))
  rownum = which(fnpos$row==fnpos[i, 3])
  curfn_end = as.numeric(as.character(fnpos[rownum[2], 1]))
  inds = c()
  for(i in 1:nrow(fnpos)) {
    if(as.character(fnpos[i,2]) != curfn) {
      if(as.numeric(as.character(fnpos[i,1])) > curfn_end) {
        curfn = as.character(fnpos[i, 2])
        curfn_start = as.numeric(as.character(fnpos[i, 1]))
        rownum = which(fnpos$row==fnpos[i, 3])
        curfn_end = as.numeric(as.character(fnpos[rownum[2], 1]))
      }
      else {
        fnpos[-(1:i), 1] = as.numeric(as.character(fnpos[-(1:i), 1])) - 1
        curfn_end = curfn_end - 1
        inds = c(inds, which(fnpos$row==fnpos[i, 3]))
      }
    }
  }
  
  if(!is.null(inds)){
    recfns = fnpos[-inds, ]  
  } else {
    recfns = fnpos
  }
  
  for(i in 1:nrow(recfns)){
    recfns[i, 4] = as.numeric(as.character(recfns[recfns[, 3] == recfns[i, 3], ][2, 1]))
    recfns[i, 5] = c456[recfns[i, 3] == c456[, 4], 1]
    recfns[i, 6] = c456[recfns[i, 3] == c456[, 4], 2]
    recfns[i, 7] = c456[recfns[i, 3] == c456[, 4], 3]
  }
  finalfns = (recfns[recfns[, 1] != recfns[, 4], ])[, c(1, 4, 2, 5, 6, 7)]
  finalfns = finalfns[order(as.numeric(as.character(finalfns[,1]))), ]
  
  return(finalfns)  
}

get_recfns <- function(studfns){
  studfns = studfns[order(as.numeric(as.character(studfns[, 1]))), ]
  
  fs = data.frame(unique(as.vector(studfns[3])))
  fs[, 2] = 0
  fs[, 3] = 0
  
  rownames(fs) = fs[, 1]
  fs[, 1] = FALSE
  
  for(i in 1:nrow(studfns)){
    if(!fs[as.character(studfns[i,3]), 1]){
      if(fs[as.character(studfns[i,3]), 2]==0 && fs[as.character(studfns[i,3]), 3]==0){
        fs[as.character(studfns[i,3]), 2] = as.numeric(as.character(studfns[i,1]))
        fs[as.character(studfns[i,3]), 3] = as.numeric(as.character(studfns[i,2]))
      } else{
        if(fs[as.character(studfns[i,3]), 2] < as.numeric(as.character(studfns[i,1])) && as.numeric(as.character(studfns[i,2])) < fs[as.character(studfns[i,3]), 3]){
          fs[as.character(studfns[i,3]), 1] = TRUE
        }
      }
    }
  }
  
  return(del_adjust(studfns, fs))
}

get_toplevel_recs <- function(studfns, ss) {
  studfns = studfns[order(as.numeric(as.character(studfns[, 1]))), ]
  
  fs = data.frame(unique(as.vector(studfns[3])))
  
  rownames(fs) = fs[, 1]
  fs[, 1] = FALSE

  setnames(studfns, c("X1", "X2", "X3", "X4", "X5", "X6"))
  
  i=which(studfns$X1==1)
  while(length(i)!=0) {
    fs[as.character(studfns[i,3]), 1] = TRUE
    i = which(studfns$X1==as.numeric(as.character(studfns[i,2]))+1)
  }

  mainfn = paste("^", ss, "-daily-max-for-month$", sep="")
  if(Reduce("|", grepl(mainfn, row.names(fs)))) { 
    #if recursion starts with outermost fn., then its single traversal
    fs[, 1] = grepl(mainfn, row.names(fs))
  }
  
  tmpfns = del_adjust(studfns, fs)
  
  return(del_inner_adjust(tmpfns))
}

visualize <- function(finalfns, path){
  png(path, width=12,height=12,units="in", res=1000)

  par(cex=0.2, mar=c(12,3,3,5))
  c13 = finalfns[, c(1, 3)]
  c23 = finalfns[, c(2, 3)]
  names(c13) = names(c23)
  i = rbind(c13, c23)
  i = i[order(as.numeric(as.character(i[,1]))), ]
  i1 = as.vector(i[, 1])
  i2 = as.vector(i[, 2])
  
  c1 = as.vector(finalfns[, 1])
  c2 = as.vector(as.numeric(finalfns[, 2]))
  c3 = as.vector(finalfns[, 3])
  j = sort(c(as.numeric(c1), c2))
  
  fs = unique(c3)
  
  fcols = rainbow(length(fs))
  
  plot.new()
  plot.window(xlim = range(j), ylim = range(j))
  
  
  axis(1, at=i1,labels=i2, pos=-1, las=2,cex.axis=0.5)
  axis(2, at=j,labels=j, pos=-1, las=2, cex.axis=0.5)
  
  par(xpd=TRUE, cex=0.7)
  legend("top",legend=lapply(fs, function(x) as.character(x)), col=fcols, pch=15, pt.cex=5,  horiz=TRUE)
  
  clip(-1, length(j), -1, length(j))
  abline(h=j, v=j, col="darkgray", lty=1, lwd=0.2)
  
  for (ind in 1:nrow(finalfns)){
    fin = match(c(as.character(finalfns[ind,3])), fs)
    x = c(as.numeric(as.vector(finalfns[ind, 1])), as.numeric(as.vector(finalfns[ind, 1])), finalfns[ind, 2], finalfns[ind, 2])
    y = c(as.numeric(as.vector(finalfns[ind, 1])), finalfns[ind, 2], finalfns[ind, 2], as.numeric(as.vector(finalfns[ind, 1])))
    polygon(x, y, col=fcols[fin])
  }
  
  dev.off()
}

stats <- function(start, end){
  countint = length(start)
  startend = end - start
  center = (end + start)/2
  
  startmean = mean(start)
  endmean = mean(end)
  lengthmean = mean(startend)
  centermean = mean(center)
  
  startdev = sd(start)
  enddev = sd(end)
  lengthdev = sd(startend)
  centerdev = sd(center)
  
  startsk = skewness(start)
  endsk = skewness(end)
  lengthsk = skewness(startend)
  centersk = skewness(center)
  
  startku = kurtosis(start)
  endku = kurtosis(end)
  lengthku = kurtosis(startend)
  centerku = kurtosis(center)
  
  return(c(startmean, endmean, lengthmean, centermean, startdev, enddev, lengthdev, centerdev, 
           startsk, endsk, lengthsk, centersk, startku, endku, lengthku, centerku, countint))
}

clustering <- function(disj, path){
  
  n = vector("list", length = 5)
  for(i in names(disj)){
    st = stats(as.numeric(disj[[i]][[1]]),as.numeric(disj[[i]][[2]]))
    
    n[[1]] = rbind(n[[1]], st[c(-17)])
    n[[2]] = rbind(n[[2]], st[c(-4, -8, -12, -16:-17)])
    n[[3]] = rbind(n[[3]], st[c(-4, -8, -12:-17)])
    n[[4]] = rbind(n[[4]], st[c(-4, -8:-17)])
    n[[5]] = rbind(n[[5]], st[c(17)])
  }
  
  for(i in 1:5){
    row.names(n[[i]]) = names(disj)
    n[[i]] = (n[[i]])[complete.cases(n[i]),]
    png(paste(path, "/stat", i,".png", sep=""), width=12,height=6,units="in", res=800)
    plot.new()
    par(xpd=TRUE, cex=75/length(disj))
    plot(hclust(dist(n[[i]])))
    dev.off()
  }
  
  # The final magnifica:
  png(paste(path, "/ulti", ".png", sep=""), width=12,height=6,units="in", res=800)
  plot.new()
  plot(hclust(sqrt(dist(n[[3]])^2 + dist(n[[4]])^2)))
  dev.off()
}

dendros <- function(disj, outfolder){
  clustering(disj, file.path('.', outfolder, "FAC/")) # function agnostic plan clustering
  df = data.frame(do.call(rbind.data.frame, disj))
  allfns = split(df, f=df[3])
  clustering(allfns, file.path('.', outfolder, "AFC")) # functions clustering
  ndisj = list()
  n2disj = list()
  
  # Normalize lengths w.r.t length of the program (length in terms of fun enter and exit steps)
  
  for(i in names(disj)){
    maxpos = as.numeric(as.vector(tail(disj[[i]], n=1)[[2]])) - 1
    ndisj[[i]] = (data.frame(lapply(lapply(disj[[i]][1:2], as.vector), as.numeric)) - 1)/maxpos
  }
  for(i in names(disj)){
    n2disj[[i]] = cbind(ndisj[[i]], disj[[i]][3])
  }
  clustering(ndisj, file.path('.', outfolder, "NFAC"))
  
  dfr = data.frame(do.call(rbind.data.frame, n2disj))
  allfns = split(dfr, f=dfr[3])
  clustering(allfns, file.path('.', outfolder, "NAFC"))  
}

setup_folders <- function(outfolder) {
  unlink(file.path('.', outfolder), recursive = TRUE, force = FALSE)
  dir.create(file.path('.', outfolder), showWarnings = FALSE)
  #dir.create(file.path('.', outfolder, 'order'), showWarnings = FALSE)  
  #dir.create(file.path('.', outfolder, 'FAC'), showWarnings = FALSE)
  #dir.create(file.path('.', outfolder, 'AFC'), showWarnings = FALSE)  
  #dir.create(file.path('.', outfolder, 'NFAC'), showWarnings = FALSE)
  #dir.create(file.path('.', outfolder, 'NAFC'), showWarnings = FALSE)   
}

get_recstring <- function(studfns) {
  n = nrow(studfns)
  recstr = vector(mode="character", length=2*n)
  for(i in 1:n) {
    recstr[as.numeric(as.character(studfns[i,1]))] = '['
    recstr[as.numeric(as.character(studfns[i,2]))] = ']'
  }
  return(paste(recstr, sep="", collapse=""))
}

{
# normalize_plans <- function(studfns) {
#   maxlen = max(nchar(plan_table[,1]))
# 
#   for(i in 1:nrow(studfns)) {
#     curlen = nchar(plan_table[1,1])
#     fac = maxlen/curlen
#     tails = unlist(gregexpr("\\[\\]", plan_table[1,1]))
#     heads = unlist(gregexpr("\\]\\[", plan_table[1,1]))
#     retails = round(fac * tails)
#     reheads = round(fac * heads)
#     
#     normstr = vector(mode="character", length=maxlen)
#     opens = 0
#     opening = TRUE
#     for(i in 1:maxlen) {
#       if(opening){
#         if(is.na(match(i, tails))) {
#           
#         } else {
#           normstr[i] = "["
#           opens = opens + 1
#         }
#       } else {
#         normstr = "]"
#       }
#     }
#   }
#   
# }
}

recpar_split <- function(strn){
  if(strn == "") return(strn)
  charvec = strsplit(strn, "")[[1]]
  
  startpos = 1
  endpos = c()
  parcount = 0
  bracount = 0
  flocount = 0
  for( i in 1:length(charvec)){
    if (charvec[i] == ","){
      if(parcount == 0 && bracount == 0 && flocount == 0){
        spos = if(charvec[i+1] == " ") i+2 else 1+1
        startpos = c(startpos, spos)
        endpos = c(endpos, i-1)
      }
    } else if(charvec[i] == "("){
      parcount = parcount+1
    } else if(charvec[i] == ")"){
      parcount = parcount-1
    } else if(charvec[i] == "["){
      bracount = bracount+1
    } else if(charvec[i] == "]"){
      bracount = bracount-1
    } else if(charvec[i] == "{"){
      flocount = flocount+1
    } else if(charvec[i] == "}"){
      flocount = flocount-1
    }
  }
  endpos = c(endpos, nchar(strn))
  
  splits = substring(strn, startpos, endpos)
  splits = splits[!grepl("<function>", splits)]
  
  return(splits)
}


rem_list_pref <- function(str){
  if(substring(str, 1, 7) == "[list: "){
    str = substring(str, 8, nchar(str) - 1)
  }
  return(str)
}

get_in_out_top_strucs <- function(toprecfns){
  #first fn applications
  #firstapps = toprecfns[match(unique(toprecfns[, 3]), toprecfns[, 3]),]
  inds = c()
  i=which(toprecfns[, 1]==1)
  while(length(i)!=0) {
    inds = c(inds, i)
    i = which(toprecfns[, 1]==as.numeric(as.character(toprecfns[i, 2]))+1)
  }
  firstapps = toprecfns[inds,]
  
  fn_strucs = c()
  for(i in 1:nrow(firstapps)){
    input = as.character(firstapps[i, 4])
    output = as.character(firstapps[i, 5])
    obj = as.character(firstapps[i, 6])
    
    #input params structure
    input = rem_list_pref(input)
    arg_list = recpar_split(input) #strsplit(input, '(?:\\([^)]*,|\\[[^]]*,)(*SKIP)(*FAIL)|,\\s*', perl=TRUE)[[1]]

    nin = c()
    for(j in arg_list){
      j = rem_list_pref(j)
      nin = c(nin, length(recpar_split(j))) #strsplit(j, '(?:\\([^)]*,|\\[[^]]*,)(*SKIP)(*FAIL)|,\\s*', perl=TRUE)[[1]]))
    }
    if(length(arg_list)==0 ){
      max_nin = 0
      instr = ""
    } else {
      max_nin = max(nin)
      instr = arg_list[which.max(nin)]
    }
    nobj = length(recpar_split(rem_list_pref(obj)))
    
    if(max_nin < nobj){
      ninp = nobj 
      inpstr = obj
    } else {
      ninp = max_nin
      inpstr = instr
    }
    
    nout = length(recpar_split(rem_list_pref(output))) #strsplit(output, '(?:\\([^)]*,|\\[[^]]*,)(*SKIP)(*FAIL)|,\\s*', perl=TRUE)[[1]])
    outstr = output
    fn_strucs = rbind(fn_strucs, cbind(as.character(firstapps[i, 3]), ninp, nout, inpstr, outstr)) #paste(nin, collapse = ', ')
  }
  
  return(fn_strucs)
}

main <- function(outfolder) {
  setwd("~/Projects/Plan Composition/planalysis/data")
  setup_folders(outfolder)
  
  tsubs = list.files(path = "./json-anf/")
  subs <<- unique(lapply(tsubs, function (i) strsplit(i, "[.]")[[1]][1]))
  
  tstrucs <<- c()
  disj = list()
  compos <<- list()
  toprecstrs = list()
  cluster_num = 0
  plan_map = list()
  #subs = list("14-1_1", "14-2_1", "56-2_1", "57-1_1")
  #subs = list("12-2_1")
  for (sub in subs){
    #sub = "3-2_1"
    js = read_json(file.path('.', 'json-anf', paste(sub, '.json', sep='')))
    dt = data.table(t(sapply(js, unlist)))
    #dt[, c(3, 5:7):=NULL]
    dt = dt[, c(1, 2, 4, 5, 6, 7, 3)]
    #lapply(dt[,6], function(x) {paste(c(Filter(function (f) {as.numeric(charToRaw(f)) < 58 && as.numeric(charToRaw(f)) > 47}, unlist(strsplit(x, "")))),collapse="")})
    ss = strsplit(sub, "[_]")[[1]][1]
    studfnscsv = as.data.frame.matrix(dt)
    studfns1 = data.frame(cbind(studfnscsv[[1]], studfnscsv[[2]], paste(ss, "-", studfnscsv[[3]], sep = ""), studfnscsv[[4]], studfnscsv[[5]], studfnscsv[[6]]))
    studfns = data.frame(lapply(studfns1, function(x) {gsub("-_", "-", x)}))
    studfns = studfns[order(as.character(studfns[, 3]), as.numeric(as.character(studfns[, 1]))), ]
    
    recfns = get_recfns(studfns)
    #visualize(recfns, file.path('.', outfolder, "order", paste(sub, "l1.png", sep="")))

    toprecfns = get_toplevel_recs(recfns, ss)
    #visualize(toprecfns, file.path('.', outfolder, "order", paste(sub, "l2.png", sep="")))
    
    top_strucs = get_in_out_top_strucs(toprecfns)
    compos[[sub]] <<- top_strucs
    tstrucs <<- rbind(tstrucs, top_strucs)
    
    toprecstring = get_recstring(toprecfns) 
    if(!is.null(toprecstrs[[toprecstring]])){
      plan_map[[sub]] = c(toprecstring, toprecstrs[[toprecstring]])
    } else {
      cluster_num = cluster_num + 1
      toprecstrs[[toprecstring]] = cluster_num
      plan_map[[sub]] = c(toprecstring, cluster_num)
    }
    print(sub)
    
    disj[[sub]] = data.frame(cbind(as.numeric(as.vector(toprecfns[, 1])), as.vector(toprecfns[, 2]), as.character(as.vector(toprecfns[, 3]))))
    #break
  }
  plan_table <<- as.data.frame(t(data.frame(plan_map)))
  setnames(plan_table, c("recursion_str", "exact_recursion_structure"))
  #png(paste("./", outfolder, "/clusters1",".png", sep=""), width=12,height=6,units="in", res=800)
  #plot.new()
  #par(xpd=TRUE, cex=0.5)
  #ds <<- stringdistmatrix(unlist(plan_table[, 1]), method='lv')
  #attr(ds, "Labels") <<- unlist(subs)
  #plot(hclust(ds))
  #dev.off()
  
  png(paste("./", outfolder, "/func_clusters",".png", sep=""), width=12,height=6,units="in", res=800)
  plot.new()
  par(xpd=TRUE, cex=0.2)
  vals = cbind(as.numeric(tstrucs[, 2]), as.numeric(tstrucs[, 3]))
  row.names(vals) = tstrucs[, 1]
  fclusts <<- hclust(dist(vals))
  plot(fclusts)
  dev.off()
  
  clus_num = 0
  complans <<- list()
  plan_nums = list()
  fcs = cutree(fclusts, h=5)
  for(sub in subs){
    cmps = c()
    for(i in 1:nrow(compos[[sub]])){
      cmps = c(cmps, fcs[[compos[[sub]][i]]])
    }
    
    planstr_sorted = paste(sort(cmps, decreasing=TRUE), collapse = ",")
    planstr = paste(cmps, collapse = ",")
    if(!is.null(plan_nums[[planstr_sorted]])){
      complans[[sub]] <<- c(planstr, plan_nums[[planstr_sorted]])
    } else {
      clus_num = clus_num + 1
      plan_nums[[planstr_sorted]] = clus_num
      complans[[sub]] <<- c(planstr, clus_num)
    } 
  }

  num_recs = str_count(plan_table[,1],"\\[\\]")
  names(num_recs) = row.names(plan_table)
  plan_table2 <<- as.data.frame(cbind(plan_table, num_recs))
  plancomps = as.data.frame(t(as.data.frame(complans)))
  setnames(plancomps, c("fn_comp", "comp_cluster"))
  plan_table3 <<-merge(plan_table2, plancomps, by='row.names', all=TRUE)
  plan_table3[, 1] = substring(lapply(strsplit(plan_table3[,1], "[_]"), function(x) {x[1]}), first = 2)
  rownames(plan_table3) = plan_table3[, 1]
  plan_table3 = plan_table3[, -1]
  #fn_cls <<- merge(as.data.frame(vals), as.data.frame(fcs), by='row.names', all=TRUE)
  #write.csv(file=file.path('.', outfolder, "result1.csv"), x=plan_table2)
  #write.csv(file=file.path('.', outfolder, "strucs.csv"), x=tstrucs)
  fn_cls = as.data.frame(cbind(tstrucs, as.data.frame(fcs)))
  write.csv(file=file.path('.', outfolder, "results.csv"), x=plan_table3)
  write.csv(file=file.path('.', outfolder, "funcs.csv"), x=fn_cls)

  gold <- read.csv(file="./coding/gold.csv", header=TRUE, row.names = 3, sep=",")
  cluster_compare <<- merge(plan_table3['comp_cluster'], gold['Cluster_level1'], by='row.names')

  cluster_compare[1] = lapply(cluster_compare[1], function(x) {paste("sub-", x, sep = "")})
  cluster_compare["comp_cluster"] = lapply(cluster_compare["comp_cluster"], function(x) {paste("c-", x, sep = "")})
  cluster_compare["Cluster_level1"] = lapply(cluster_compare["Cluster_level1"], function(x) {paste("gold-", x, sep = "")})

  From = c(as.vector( unlist(cluster_compare["Cluster_level1"])), as.vector(unlist(cluster_compare[1])))
  To = c(as.vector( unlist(cluster_compare["comp_cluster"])), as.vector(unlist(cluster_compare["Cluster_level1"])))
  sank <<- data.frame(From, To, weights = array(1, c(nrow(From), 1)))
  
  ccompare <<- data.frame(From = cluster_compare["Cluster_level1"], To=cluster_compare["comp_cluster"], weights = array(1, c(nrow(cluster_compare), 1)))
  plot(gvisSankey(ccompare, from="From", to="To", weight="weights", options = list(height = 1800, width = 1200, sankey="node:{nodePadding:5}")))
  plot(gvisSankey(sank, from="From", to="To", weight="weights", options = list(height = 1800, width = 1200, sankey="node:{nodePadding:5}")))
  
  #dendros(disj, outfolder)
}

main("plots13")