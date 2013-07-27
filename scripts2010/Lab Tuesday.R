#=========================================================================================================
#=== The following libraries HAVE to be installed first, before "calling" them in the following script ===
#=========================================================================================================
library(affy)
library(marray)
library(geneplotter)
library(genefilter)
library(gplots)
library(siggenes)
library(annotationTools)
?rma
#===============================
#====== Read the data ==========
#===============================
raw.data= ReadAffy()                      # You read the ".cel" files that SHOULD be ALL in the same directory

#=====================================
#====== Nomrmalize the data ==========
#=====================================
rma.data=rma(raw.data)                     # You apply the rma normalization method to the raw data files
exp.arrays=data.frame(exprs(rma.data))     # Obtain the expressions and store it in a data frame to work with.

#======================================
#====== Get the subject info ==========
#======================================
keys=read.csv(file="U133A_B_chip_Key.csv",head=TRUE,sep=",")     # Obtain the subject informartion from the key subject file 
global.keys=as.character(keys[,2])
subjectIDs=match(names(exp.arrays),global.keys)
subjectInfo=keys[subjectIDs,c(1,2)]

#===================================================================================
#==== Lymphoma subtypes ONLY for the "cel" files analized in this MINI-EXAMPLE =====
#===================================================================================
TEL.AML1=c(1,2,3,9,16,21)
BCR.ABL=c(4,6,11,12,24,28,30)
MLL=c(5,14,15,17,20,25)
E2A.PBX1=c(7,8,10,13,18,19)
T.ALL=c(22,23,26,27,29,31)

colnames(exp.arrays)[TEL.AML1]="TEL.AML"
colnames(exp.arrays)[BCR.ABL]="BCR.ABL"
colnames(exp.arrays)[MLL]="MLL"
colnames(exp.arrays)[E2A.PBX1]="E2A.PBX"
colnames(exp.arrays)[T.ALL]="T.ALL"
#========================================
#========== Box-Plots =================== 
#========================================
par(mfrow=c(1,2))
boxplot(raw.data)            # In these instructions you will obtain Boxplots of the raw expression data and the "processed" data
boxplot(exp.arrays)

#==================================================================
#========== Keeping track of the probe ID names =================== 
#==================================================================
row.names=rownames(exp.arrays)         # Assign the probeIDs to a vector called: row.names
num.expression=as.matrix(exp.arrays)   # Convert the "Normalized" expression values to a NUMERIC matrix: num.expression     
rownames(num.expression)=row.names     # Assing to num.expression the probeID names , to keep trakc of them

#=============================================
#===== Ordering the expression matrix ========
#=============================================
OrderExpression=matrix(0,nrow=nrow(num.expression),ncol=ncol(num.expression))       
rownames(OrderExpression)=row.names                          
OrderExpression[,1:6]=num.expression[,T.ALL]        # This is just to order the colums.  In this part of the code we just
OrderExpression[,7:12]=num.expression[,TEL.AML1]    # group the subjects by cancer subtypes to make the statistical analysis
OrderExpression[,13:19]=num.expression[,BCR.ABL]    # easier
OrderExpression[,20:25]=num.expression[,MLL]
OrderExpression[,26:31]=num.expression[,E2A.PBX1]
colnames(OrderExpression)=c(rep("T.ALL",6),rep("TEL.AML",6),rep("BCR.ABL",7),rep("MLL",6),rep("E2A.PBX",6))
OrderExpression[1:5,]
dim(OrderExpression)

#===========================================================
#===== PCA analisis on the normalized expression data ======
#===========================================================
pca.exp=prcomp(OrderExpression)                        # This is the instruction to obtain the principal components
summary(pca.exp)                                    # View the summary of the PCA performance
myColors=c(rep("red",6),rep("green",6),rep("blue",7),rep("magenta",6),rep("black",6))  # Defines the colors for each cancer subtype
rot=pca.exp$r                                   # Obtaines the scores of each subject from the principal components 
plot(rot[,1],rot[,2],pch=20,col=myColors)       # Scatter plot of the subjects using the principal components

library(scatterplot3d)                        # Need to load (install if not already) this library for the next function
scatterplot3d(rot[,1],rot[,2],rot[,3],pch=20,color=myColors)   # 3D scatter plot of the subjects using the first 3 components

#=====================================================================
#=====Hierarchical clustering on the normalized expression data ======
#=====================================================================
distance=dist(t(OrderExpression))    # Obtains the distance matrix between the subjects ( in this case Euclidean, but can change)
hCluster=hclust(distance)            # Performs the hierarchical clustering
plot(hCluster)                       #Visualize the hierarchical clurtering method
?dist
#============================
#============================
#=== STATISTICAL ANALYSIS ===
#============================
#============================

#===============
#=== t-test ====          # Comparison between ONLY TWO categories
#===============
# Remember how the subjects where ordered in our numeric gene expression matrix!
#.... colnames(OrderExpression)=c(rep("T.ALL",6),rep("TEL.AML",7),rep("BCR.ABL",6),rep("MLL",6),rep("E2A.PBX",6))

# An example of how to run a t-test in a FAST manner: T.ALL vs the B.ALL subtypes
# in t-test function in R, need to define a "dummy" variable that contains to the info to which cancer subtype
# the subjects belong
y =c(rep(0,6), rep(1, length(7:31)))        # Dummy variable
rtt=rowttests(OrderExpression,fac = as.factor(y))   # t-test
pvals = rtt$p.value                           # Obtains the p values of the mean comparisons made, for each gene

#===== In this part, we select the top 300 probes from the t-test based on the p-values
rank.p.vals = rank(pvals)                     # In this part the ranks of the p-values are provided
top.genes = which(rank.p.vals <=300)          # We select the smallest 300 genes based on their p-values, just as an example
OrderExpression.Top=OrderExpression[top.genes,] # Just to save the expression matrix to use it later
heatmap(OrderExpression.Top)                    # Obtain the heatmap based on the 300 top genes

#==== In this part, we select the genes based on their FOLD EXPRESSION from the 300 genes that had the smallest p-values
means.t.0=apply(OrderExpression[,c(1:6)],1,mean)
means.t.1=apply(OrderExpression[,-c(1:6)],1,mean)
diff.means.t.0.1=means.t.0-means.t.1
fold.2=which(abs(diff.means.t.0.1)>2)
same=match(fold.2,top.genes,nomatch=0)
same.no.0=which(same!=0)
same=same[same.no.0]
top.genes.fold.2=top.genes[same]
OrderExpression.Top.Fold.2=OrderExpression[top.genes.fold.2,] # Just to save the expression matrix to use it later
heatmap(OrderExpression.Top.Fold.2)                    # Obtain the heatmap based on the 300 top genes


#===============
#=== F-test ====          # Comparison between TWO categories
#===============
# An example of how to run an F-test in a FAST manner
# in F-test function in R, need to define a "dummy" variable that contains to the info to which cancer subtype
# the subjects belong

y.more=c(rep(0,6),rep(1,6),rep(2,7),rep(3,6),rep(4,6))  # Dummy variable
rft=rowFtests(OrderExpression,fac = as.factor(y.more),var.equal=FALSE)   # t-test
pvals.f = rft$p.value                           # Obtains the p values of the mean comparisons made, for each gene

#===== In this part, we select the top 300 probes from the t-test based on the p-values
rank.p.vals.f = rank(pvals.f)                     # In this part the ranks of the p-values are provided
top.genes.f = which(rank.p.vals.f <=500)          # We select the smallest 300 genes based on their p-values, just as an example
OrderExpression.Top.f=OrderExpression[top.genes.f,] # Just to save the expression matrix to use it later
heatmap(OrderExpression.Top.f)                    # Obtain the heatmap based on the 300 top genes
?heatmap
#==== In this part, we select the genes based on their FOLD EXPRESSION from the 300 genes that had the smallest p-values

means.f.0=apply(OrderExpression[,1:6],1,mean)
means.f.1=apply(OrderExpression[,7:12],1,mean)
means.f.2=apply(OrderExpression[,13:19],1,mean)
means.f.3=apply(OrderExpression[,20:25],1,mean)
means.f.4=apply(OrderExpression[,26:31],1,mean)
diff.means.f.0.1=means.f.0-means.f.1
fold.2.f.0.1=which(abs(diff.means.f.0.1)>2)
same.f=match(fold.2.f.0.1,top.genes.f,nomatch=0)
same.no.0.f=which(same.f!=0)
same=same[same.no.0.f]
top.genes.fold.2.f=top.genes.f[same.f]
OrderExpression.Top.Fold.2.f=OrderExpression[top.genes.fold.2.f,] # Just to save the expression matrix to use it later
heatmap(OrderExpression.Top.Fold.2.f)                    # Obtain the heatmap based on the 300 top genes

#======================================
#======================================
#=== SAVING THE OUT INTO A CSV FILE ===
#======================================
#======================================
output=rownames(OrderExpression.Top.Fold.2.f)

write.table(output,"Top-Expression.csv",row.names=FALSE,col.names=FALSE)























#============================================================================================================
#======= Get the gene names for the top probe IDs obtained from the T-ALL comparison against B-All ==========
#============================================================================================================
annotation_HGU133A=read.csv("HG-U133A-na30-annot.csv",colClasses="character",comment.char="#")
name.probes=rownames(OrderExpression.Top)
gene.names=getGENESYMBOL(name.probes,annotation_HGU133A,noGSsymbol = 0)
mat.nom=rep(0,nrow=dim(OrderExpression.Top)[1])
ceros=0
for(i in 1:dim(OrderExpression.Top)[1])
   {
    if(gene.names[[i]][1]!=0)
       {
       	mat.nom[i]=gene.names[[i]][1]
       }
    if(gene.names[[i]][1]==0)
      {
       ceros=cbind(ceros,i)	
      }   
   }
ceros
mat.nom=mat.nom[-c(74,152,194)]
write.table(mat.nom,"Top-Gene-Names.txt",sep=",",row.names=FALSE)
