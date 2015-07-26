# Functional Programming
# http://adv-r.had.co.nz/Functional-programming.html

# Generate a sample dataset
set.seed(1014)
df <- data.frame(replicate(6, sample(c(1:10, -99), 6, rep = TRUE)))
names(df) <- letters[1:6]
df

# rather than doing one for each column
# do this
fix_missing <- function(x){
  x[x == -99] <- NA
  x
}

?lapply
df[] <- lapply(df, fix_missing)
df

# summary function with colsure but has duplicalte
summaries <- function(x) {
  c(mean(x, na.rm =  T)
    ,median(x , na.rm = T)
    ,sd(x, na.rm = T)
    ,mad(x, na.rm = T)
    ,IQR(x , na.rm = T))
}

# summary functon without dupilcation
summaries <- function(x){
  funs <- c(mean, median, sd, mad, IQR)
  lapply(funs, function(f) f(x, na.rm = TRUE))
}

unlist(summaries(df$a))


# anyomnous functions examples
unlist(lapply(mtcars, function(x) length(unique(x))))
Filter(function(x) !is.numeric(x), mtcars)
integrate(function(x) sin(x) ^2, 0, pi)

(function(x) x + 3)(10)
f <- function(x) x + 3
f(10)


#2 
unlist(lapply(mtcars, function(x) sd(x, na.rm =T)/ mean(x, na.rm = T)))

# Functionals
#--------------------------------------------------------------------
#http://adv-r.had.co.nz/Functionals.html

mylist <- replicate(20, runif(sample(1:10, 1)), simplify = F)

# with a loop
out <- vector("list", length(mylist))
for(i in seq_along(mylist)){
  out[[i]] <- length(mylist[[i]])
}

unlist(out)

# using lapply
unlist(lapply(mylist, length))

data(mtcars)

unlist(lapply(mtcars, class))
# divide each column by mean
lapply(mtcars[1], function(x) x / mean(x))

# excercises
# 1
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)

lapply(trims, function(trim) mean(x , trim = trim))
lapply(trims, mean, x = x)
# being passed as expresion object to vector x 

#2 
scale01 <- function(x) {
  rng <- range(x, na.rm = T)
  (x - rng[1]) / (rng[2] - rng[1])
}

lapply(mtcars[1], scale01)

#3

formulas <- list(
   mpg ~ disp
  ,mpg ~ I(1 / disp)
  ,mpg ~ disp + wt
  ,mpg ~ I(1 / disp) + wt
)

mods <- lapply(formulas, function(x) lm(x, data = mtcars))


mods <- NULL
for(i in 1:length(formulas))
{
mods[[i]] <- lm(formulas[[i]], data = mtcars) 
}
mods

#4










