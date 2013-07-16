###################################
# Solutions to Tutorial 1
###################################
# problem 1 					  #	
###################################
# 1A: how many nucleotides in the E.coli genome?
# set the right working directory
# take a look at the file first, is it rectangular? if no, use scan
genome <- scan("U00096.fna", what="txt") 

# we don't need the first 9 elements 
genome <- genome[-(1:9)]

# one method, stick all the pieces together, split into single units, then count 
onestring.genome <- paste(genome, collapse="", sep="")
one.genome <- strsplit(onestring.genome, "")

# one.genome is a list 
class(one.genome) 

# we want the first element of the list
one.genome <- one.genome[[1]]

# one.genome is now a vector
class(one.genome)

length(one.genome)	# answer: 4639675 bases

# 1B: how many Cs? 
C.genome <- one.genome == "C"	# T = C, F = not C
sum(C.genome)		# answer: 1179554 Cs

# 1C: write a function that calculates the number of Cs, given an input DNA string 
calcC <- function(dna.string){
		dna.list <- strsplit(dna.string, "")
		dna.vec <- dna.list[[1]]
		dna.C <- dna.vec == "C"
		numC <- sum(dna.C)
		return(numC)
	}
# does it actually work?
calcC(onestring.genome)

# 1D: what is the GC content of the E.coli genome? 
# need a function that calculates number of Gs too 
# modify calcC
calcG <- function(dna.string){
		dna.list <- strsplit(dna.string, "")
		dna.vec <- dna.list[[1]]
		dna.G <- dna.vec == "G"
		numG <- sum(dna.G)
		return(numG)
	}

numG <- calcG(onestring.genome)
numC <- calcC(onestring.genome)
(numG + numC)/length(one.genome)	# answer: 0.507897

###################################
# problem 2 					  #	
###################################
# 2A. make an environment that maps ids to their symbols
# take a look at the file geneData.txt, is it rectangular? if so, use read.table 

geneData <- read.table("geneData.txt", header=T, sep="\t")
keys <- geneData[,1] 

# keys is a factor, this needs to be a character 
keys <- as.character(keys) 
class(keys)

# ditto for symbol
symbol <- geneData[,2]
symbol <- as.character(symbol) 

# call the environment ID2SYMBOL 
ID2SYMBOL <- new.env()
for( i in 1:length(keys) ){
	assign(keys[i], symbol[i], pos=ID2SYMBOL)
}

# 2B. do the same for ids to chromosome, ids to description

ID2CHROMOSOME <- new.env()
chr <- geneData[,3]
chr <- as.character(chr)
for( i in 1:length(keys) ){
	assign(keys[i], chr[i], pos=ID2CHROMOSOME)
}

ID2DESCRIPTION <- new.env()
des <- geneData[,4]
des <- as.character(des)

for( i in 1:length(keys) ){
	assign(keys[i], des[i], pos=ID2DESCRIPTION) 
}

# 2C. 
get("210451_at", ID2SYMBOL) 
get("207858_s_at", ID2SYMBOL) 

get("210451_at", ID2CHROMOSOME) 
get("207858_s_at", ID2CHROMOSOME) 

get("210451_at", ID2DESCRIPTION) 
get("207858_s_at", ID2DESCRIPTION) 


###################################
# problem 3 					  #	
###################################
# 3A. map pathway to its genes 

# first, take a look at the file pathwayData.txt, is it rectangular? if no, use scan 
# what is the delimiter? 
pwayData <- scan("pathwayData.txt", what="txt", sep="\n")

# there is one row for each pathway 
numPathways <- length(pwayData)
# we need to split up each row separately 
# first, just focus on the first row only 
oneRow <- pwayData[1]

# split oneRow based on the "\t" delimiter 
oneRow.split <- strsplit(oneRow, "\t")

# oneRow.split is a list 
class(oneRow.split) 
oneRow.split <- oneRow.split[[1]]

# we need the key (the pathway code), this is the first element of the row
pway.one <- oneRow.split[1]
# the rest are the genes, these are the values 
pway.genes <- oneRow.split[-1]

# make environment
PWAY2GENES <- new.env()
assign(pway.one, pway.genes, pos=PWAY2GENES)

# now use a for loop to assign the rest 
for( i in 1:numPathways){
	oneRow <- pwayData[i]
	oneRow.split <- strsplit(oneRow, "\t")
	oneRow.split <- oneRow.split[[1]]
	pway.one <- oneRow.split[1]
	pway.genes <- oneRow.split[-1]
	assign(pway.one, pway.genes, pos=PWAY2GENES)
}

# 3B. how many genes in the biggest pathway? the smallest?
# convert environment to a list 

pwayList <- as.list(PWAY2GENES)
len.pwayList <- sapply(pwayList, length)	

max(len.pwayList)		# biggest pathway has 482 genes
min(len.pwayList)		# smallest pathway has 1 gene 

# which pathways are these? 
names(pwayList)[ len.pwayList == max(len.pwayList) ]	
# the biggest pathway is 04010

names(pwayList)[ len.pwayList == min(len.pwayList) ]	
# the smallest pathway is 00472

# 3C. using the file info in pwaycodes.txt
# 04010 = MAPK signaling pathway
# 00472 = D-Arginine and D-ornithine metabolism
 

