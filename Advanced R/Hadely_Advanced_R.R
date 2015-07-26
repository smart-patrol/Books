#http://adv-r.had.co.nz/Subsetting.html

dat = data.frame(x = rnorm(1000, mean=6))
dat$group1 = rbinom(n=1000, size=1, prob=0.5)
dat$y = dat$x * 5 + rnorm(1000)
dat$group2 = runif(1000) > 0.2

typeof(dat)
length(dat)
attributes(dat)
is.vector(dat) ; is.atomic(dat) ; is.list(dat)
is.data.frame(dat)

##################################
#four types of actomic vectors
#logical, int, double, and char

#create atomic vectors with c()
dbl_var <- c(1,2.5,4.5)
int_var <-c(1L, 6L, 10L)
log_var <- c(T, F, T, F)
chr_var <- c("thes are","some strings")

typeof(dbl_var); typeof(int_var)
typeof(log_var) ; typeof(chr_var)

is.integer(int_var) ; is.atomic(int_var)
is.integer(chr_var) ; is.atomic(chr_var)

is.double(dbl_var) ; is.atomic(dbl_var)
is.double(log_var) ; is.atomic(log_var)

# not to be confused with sql like isnumeric
is.numeric(int_var) ; is.numeric(dbl_var); is.numeric(chr_var)

##################################3
#lists can be constructed from any tyep
x <- list(1:3 , "a", c(TRUE, FALSE, TRUE), c(2.3,5.9 ))
str(x)

#lists are recursive because they can contain other lists
x <- list(list(list(list())))
str(x)
is.recursive(x)

#c() combines several lists into 1
x <-list(list(1,2), c(3,4)) #keeps format
y <- c(list(1,2), c(3,4)) #coerces to numeric
str(x)
str(y)

#data frames are lists!
is.list(dat)

################################
#attributes
y <- 1:10
attr(y, "my_attribute") <- "This is a vector"
attr(y, "my_attribute")
str(attributes(y))

#names
 y <- c(a=1, 2, 3) ; names(y)
 z <- c(1,2,3) ; names(z)

#remove names
unname(y)
names(y) <- NULL ; names(y)

#factors - built on class() and levels()
x <-factor(c("a","b","b","a"))
x
class(x)
levels(x)

#using factor vs char in tables
sex_char <- c("m","m","m")
sex_fac <- factor(sex_char, levels = c("m","f"))

table(sex_char)
table(sex_fac)

# matrices and arrays
# dim() will ell how an atomic vector behaves

a <- matrix(1:6, ncol=3, nrow=2)
a
b <- array(1:12, c(2,3,2))
b

#modifying object in place
#changing the object to 3x2
c <- 1:6
dim(c) <- c(3,2)
c
#chase to 2x3
dim(c) <- c(2,3)
c

# length() = nrow() and ncol() for matrixes
nrow(a) ; ncol(a)
# names() = rownames() and colnames() for matrixes
rownames(a); colnames(a)

#dim() and dimnames() does the above for arrays
dim(b); dimnames(b) 

is.array(b) ; is.matrix(b)

############################3
#dataframes

df <- data.frame(
  x = 1:3
  ,y = c("a","b","c")
  ,stringsAsFactors = F
  )
str(df)
typeof(df); class(df); is.data.frame(df)

#combining dataframes cbind() and rbind()
cbind(df, data.frame(z = 3:1))
rbind(df, data.frame(x = 10, y = "z"))




