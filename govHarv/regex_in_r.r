# regular expressions in R 

# strsplit: split a string into two parts on a specified character/pattern
addy = "aeggers@fas.harvard.edu"
addy_parts = strsplit(addy, "@")
cat("The username is ", addy_parts[[1]][1], " and the domain is ", addy_parts[[1]][2], ".\n", sep = "")

# grep: determine whether there is a match for a specified pattern
addies = c("aeggers@fas.harvard.edu", "Cambridge, MA", "aeggers@gmail.com", "aeggers AT gmail DOT com", "meet me @ school?")
grep("@", addies)
addies[grep("@", addies)]
grep("@", addies, value = TRUE)   # equivalent to the above
# more complicated pattern to exclude the last one
grep("[a-z]+@[a-z]+", addies, value = TRUE)   # equivalent to the above

# regexpr: matching that returns the location and length of the match 
# this allows us to retrieve the whole match if we want 
addy_sentence = "My email address is aeggers@fas.harvard.edu, but please don't email me."
# let's get the email address out 
match_result = regexpr("[a-z]+@[a-z.]+", addy_sentence)    #  perl = T)
substr(addy_sentence, match_result[1], match_result[1] + attr(match_result, "match.length") -1 )

# an example of using these together 
# you get a text file with two columns: 
# a first name in the left column, and an email address (or a note indicating that you 
# don't have that email address)
# you need to prepare this list to be read into a program you wrote to send out emails
# which requires a csv with the name in the left column and a valid email address in the right column
# you grumble that this could have been input more intelligently at the start, but 
# you also know that you can handle this situation.

# here is the input file:
input = "Andy    aeggers@fas.harvard.edu\nBen Goodrich    goodrich@fas.harvard.edu\nJens     jhainmueller@fas.harvard.edu\nKevin    Not allowed to find that out\nProfessor Adam Glynn      aglynn@fas.harvard.edu\n"
cat(input)

# one solution 
my_mat = matrix(NA, nrow = 0, ncol = 2)       # set up a matrix
colnames(my_mat) = c("Name", "Email Address")
for (this_line in strsplit(input, "\n")[[1]]){  # split on the newline ("\n") character so we can deal with one line at a time.
  split_line = strsplit(this_line, "\\s{3,}")[[1]]  # now split the line into a name component and an email component
  # note that this relies on the fact that there are at least 3 spaces between the name and the email address -- otherwise would need to do something more fancy.
  this_name = split_line[1]
  addy_here = grep("[a-z]+@[a-z]+", split_line[2], value = TRUE)  # grep to determine if the second half is a valid email address -- what would you do if there was non-email text in there, or two email addresses on one line?  what non-email address strings could be matched to this?
  if (length(addy_here) == 0){ 
    this_addy = NA} else{
    this_addy = split_line[2]
    }
  my_mat = rbind(my_mat, c(this_name, this_addy))
  }
my_mat