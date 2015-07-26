#http://adv-r.had.co.nz/Vocabulary.html

#Basic

###operatorss and assignment
#%in%
1:10 %in% c(1,3,5,9)
c("d","e") %in% c("a","b","c","d")
which(1:36 %in% 6:10)
6:10 %in% 1:8

#assign - Assign a value to a name in an environment.
?assign ; ?get
a <- 1:4
assign("a[1]",2)
a[1] == 2
get("a[1]") == 2

###comparison
?all.equal
all.equal(pi , 355/113) #to compare R objects x and y testing 'near equality'.

?identical
identical(pi , 355/113) #test if exactly equal
identical(1, as.integer(1))
identical(.GlobalEnv , environment())

?complete.cases
complete.cases(juul) #T /F test of NAs
sum(complete.cases(caff)) #number complete wiht no NA
stopifnot(complete.cases(caff) != is.na(caff))

?is.na
is.na(caff)
sum(is.na(juul)) #no of nas
is.na(paste(c(1,NA)))

?is.finite #test if infinite or finite
is.finite(sin(pi))
is.infinite(sin(pi))
is.finite(tan(x))

#basic math operators
(sqrt(x+4)-2) / x
(cos(x)-1)/ x 
atan(x)
10%%2   ; 7%%2 #find reminder of division - modulo op
10%/%2 ; 7%/%2 #int division
          
sign(pi) ; sign(-2:3) #return sign of op

ceiling(3.75) ; floor(3.75) 
trunc(3.99)
round(3.75 , digits=2) ; signif(3.75, digits=2)

log(exp(log(exp(3))))

prod(1:5) ; 1*2*3*4*5 #factorials wiht product fun

?cummax
cumsum(1:5)
cumprod(1:5)
cummax(1:5) ; cummin(1:5)
          
?range
range(1:5)
foo <- rnorm(100)
range(foo) ; min(foo) ; max(foo)

?rle
rle(rep(6:10, 1:5))

# Functions to do with functions
function()
?missing
?on.exit
?invisible
          
# Logical & sets 
?xor
xor(1,0) 
xor(T,F)
xor(F,F)
xor(0,0)

?all
all( 2 < 0)
?any
any( 2 < 0 )

?which
which(LETTERS=="R")
which((1:12)%%2 == 0 ) #which are even
which((1:12)%%2 != 0 ) #which are odd

# Making vectors 
?rep
rep(1:4,2)
rep(1:4, each=2)
rep(1:4, each=2, len =4)
rep(list(happy= 1:10 , name = "squash"), 5)
foo = rep(factor(LETTERS[1:4]), 2)
foo
rep_len(foo,10)

?seq
seq(0,1, length.out = 11)
seq(1,9, by=2)
foo = seq(1,9, by=pi)
foo
seq_len(foo)
seq_along(foo)
rev(foo) #revesre the list 

# Lists & data.frames 
foo = list(c("Mangos","Bananas","Fish"))
foo
unlist(foo)

?split #split divides the data in the vector x into the groups defined by f
split(x=juul, f=juul$sex ) #split by m and f
split(x=juul, f=juul$tanner) #split by tanner
split(1:10 , 1:2) #split by odds and evens
split(x=caff, f=row(caff)) #split rows of matrix

?expand.grid #create df from all comboniations
expand.grid(height = seq(60,80,5), weight = seq(100,300,50),
            sex = c("Male","Female"))
expand.grid(x = seq(0,10,length.out = 100), y = seq(-1,1,length.out = 20))

# Control flow 
#&& and || go with if
#short circut so only deal with single elements
if(T || stop()) 4 else 5
if(F && stop()) 4 else 5

#https://www.udemy.com/blog/r-tutorial/
#for loop
sqr = seq(1,20, by=2)
n = length(sqr)
sqred = NULL
for (i in 1:n) {
  sqred[i] = sqr[i]^2
}
sqred

#while loop
x <- 0
while (x < 18) {
  x <- x+4
  print(x)
}

#break it when x = 3 
x <- 1 
while( x < 5) { x <- x+1 
if (x==3)
  break
print(x)
}

#skip when x = 3
x <- 1 
while( x < 5 ){
  x <- x+1 
  if (x==3)
  next
  print(x)
}

#repeat loop - the do while loop
sumin <- 1 
repeat 
{
  sumin <- sumin + 2
  print(sumin)
  if (sumin > 11)
  break
}

#this is faster than if statement
?switch
cent <- function(x , type) {
  switch(type, 
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = 0.1))
}
foo = rcauchy(10)
cent(foo, "mean")
cent(foo, "median")
cent(foo, "trimmed")

# Apply & friends
#          lapply, sapply, vapply
#         apply
#        tapply
#      replicate
