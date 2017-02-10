M = csvread('../data/distances/1.csv',1,1);
squareform(M + M');
y = squareform(M + M');
z = linkage(y);
clusters = cluster(z, 'maxclust', 4);
fnames(clusters == 1)
