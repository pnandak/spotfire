# solutions to tutorial 1 

# problem 1a
odd.num <- seq(from=1, to=100, by=2)

# problem 1b
even.num <- seq(from=2, to=100, by=2)

# problem 1c
x <- cbind(odd.num, even.num)

# problem 1d

# The key here is to use rbind. I was hoping 
# you'd guess from cbind, that a rbind function might exist. 

y <- rbind(odd.num, even.num) 

# problem 1e 
# I didn't specify which matrix to use, so you could 
# have used either.
x.rsum <- NULL
for( i in 1:nrow(x) ){
	m <- sum(x[i,])
	x.rsum <- c(x.rsum, m) 
}

# problem 2a 
string1 <- "abcde" 
string2 <- "fghij" 
glued <- paste(string1, string2, collapse="", sep="")

# problem 2b
unglued <- strsplit(glued, "")
unglued <- unlist(unglued)

# problem 2c
alpha.db <- new.env() 

# here the letters are your keys, and their position in the alphabet are the values 
# to assign a to 1 
assign("a", 1, alpha.db)
# or equivalently: 
assign(unglued[1], 1, alpha.db)

# extending this to all letters:
for( i in 1:10 ){
	assign(unglued[i], i, alpha.db)
}

# to retrieve a key from your environment object 
get("a", alpha.db)

# to see all the keys in your environment object 
ls(alpha.db) 

# to retrieve the values for multiple keys: 
mget(ls(alpha.db), alpha.db)

# equivalently: 
mget(unglued, alpha.db)

