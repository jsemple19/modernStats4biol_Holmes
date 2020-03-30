### lists

x<-list("a","b","c")
x

# lists is R can contain any type of object
x
# x is a list of characters

y<-list(1:10)
y
# y is a list of numbers

z<-list(5:1,5:7)
z
# z is alist of vectors (they can be different lengths)

q<-list(x,y,z)
#q is a list of lists

# if doing complicated lists like that it is better to name
# the items in the list:
q<-list(x=x,y=y,z=z)

# to extract items from a list, you need to use [[]]
q[[1]]
# extracts the first item (the x list)
# since this list has a name, you could also have extracted
# it by name
q[["x"]]

# to get the first item in the x list:
q[["x"]][[1]]


# lets look at the simpler z list again
z
# to apply a function to every item in the list, you can use lapply:
lapply(z,sum)
lapply(z,mean)


# to extract particular item from all sublists in a list use the '[['
# to extract 1st item from each sublist in a list:
lapply(z,'[[',1)
# or the second item
lapply(z,'[[',2)

# the output of lapply is a list, and what you often want is a vector
# to get that, use the unlist() command
unlist(lapply(z,'[[',1))

# you can also use the sapply function that guesses at what the "simplest
# output format should be (hence the s in sapply)
sapply(z,'[[',1)

# unlist is a more general function for going from list to vector. e.g.
z
unlist(z)
