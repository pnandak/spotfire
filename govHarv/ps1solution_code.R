## hierarchical clustering of the Fed. Papers

setwd("c:/data/schoolwork/gov2002/scripts/kevin_clustering")


authors <- read.delim("Authors.txt")
stopfreq <- read.delim("StopAllWords.txt")
nostopfreq <- read.delim("NoStopStemAllWords.txt")
setwd("c:/data/schoolwork/gov2002/repo/problemsets/ps1")


cosfun <- function(M){
  mycos <- matrix(0, ncol(M), ncol(M))
  for (i in 1:ncol(M)){
    for (j in i:ncol(M)){
      mycos[i,j] <- mycos[j,i] <- sum(M[,i] * M[,j]) /
        (
         sqrt(sum(M[,i]^2)) * sqrt(sum(M[,j]^2))
         )
    }
  }
  diag(mycos) <- 0
  colnames(mycos) <- rownames(mycos) <- colnames(M)
  
  return(mycos)
}

# let's label the dendrogram with a combination of author name and paper number
# using regexpr here!
my_colnames = c()
for (paper in colnames(stopfreq)[-1]){
  my_colnames = c(my_colnames, strsplit(paper, "No\\.\\.")[[1]][2])
  }

authorship = as.character(authors$author)
authorship[authorship == "HAMILTON OR MADISON"] = "?"

my_colnames = paste(my_colnames, strtrim(authorship, 1))
# this gives us "1 H"  "2 J"  "3 J"  etc

# Stylistic --> stop words.
stopD <- cosfun(stopfreq[,2:ncol(stopfreq)])
rownames(stopD) <- colnames(stopD) <- my_colnames
stopD <- as.dist(1-stopD)
hclust.out <- hclust(stopD, method="ward")

pdf("stop_clustering.pdf", width=8, height=3, version="1.4", bg="white",
    pointsize=11, family="Times")
par(mar=c(.1, .1, 2, .1), yaxt = "n")
plot(hclust.out, main = "Stylistic clustering of Federalist Papers (cosine distsance)", xlab = "")
dev.off()

# Substantive
# Since a first run with just cosines did not produce substantive clustering,
# we'll try using tfidf to take emphasis off of commonly used words

# function useful for tfidf
how_many_non_zero <- function(vec){
  length(vec[vec>0])
  }
  
tfidf <- function(freqmat){
  d_vec = apply(freqmat, 1, how_many_non_zero)
  D = ncol(freqmat)
  idf_vec = log(D/d_vec)
  freqmat * idf_vec
  }
  
nostopD <- cosfun(tfidf(nostopfreq[,2:ncol(nostopfreq)]))
rownames(nostopD) <- colnames(nostopD) <- my_colnames
nostopD <- as.dist(1-nostopD)
no.hclust.out <- hclust(nostopD, method="ward")

pdf("no_stop_clustering.pdf", width=8, height=3, version="1.4", bg="white",
    pointsize=11, family="Times")
par(mar=c(.1, .1, 2, .1), yaxt = "n")
plot(no.hclust.out, main = "Substantive clustering of Federalist Papers (cosine distance w. tfidf)", xlab = "")
dev.off()

# examine the tfidf mat

tfidfmat = tfidf(nostopfreq[,2:ncol(nostopfreq)])

# let's look at tfidf scores for judiciary papers
jud_essay_num_vec = c("No..78", "No..79", "No..80", ".No..81", "No..82", "No..83")
t_mat_jud = matrix(ncol = length(jud_essay_num_vec), nrow = 10)
colnames(t_mat_jud) = jud_essay_num_vec # paste("FEDERALIST.", jud_essay_num_vec, sep = "")
for (num in jud_essay_num_vec){
  essay_name = paste("FEDERALIST.", num, sep = "")
  print(essay_name)
  top_word_row_numbers = order(tfidfmat[,essay_name], decreasing = T)[1:10]
  t_mat_jud[,num] = as.character(nostopfreq[top_word_row_numbers,1])  #  round()
  }
  
# now for executive papers
exec_essay_num_vec = paste("No..", as.character(c(67:77)), sep = "")[1:6]
t_mat_exec = matrix(ncol = length(exec_essay_num_vec), nrow = 10)
colnames(t_mat_exec) = exec_essay_num_vec # paste("FEDERALIST.", exec_essay_num_vec, sep = "")
for (num in exec_essay_num_vec){
  essay_name = paste("FEDERALIST.", num, sep = "")
  print(essay_name)
  top_word_row_numbers = order(tfidfmat[,essay_name], decreasing = T)[1:10]
  t_mat_exec[,num] = as.character(nostopfreq[top_word_row_numbers,1])  #  round()
  }

library(xtable)
xtable(t_mat_jud)
xtable(t_mat_exec)
