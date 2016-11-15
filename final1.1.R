#process for generating best decision tree on whether an employee will pay off loan

# function for information gained
#calculates entropy for binary target
entropy <- function(T){
  if(!is.vector(T)){
    return(-1)
  }
  size <- length(T)
  values <- unique(T)
  
  e <- 0.0
  for(v in values){
    rept <- length(which(T==v))
    probability <- rept/size
    e <- e + (probability * log2(probability))
  }
  
  return(-1.0*e)
}

#calculates entropy for binary target based on x column
conditional_entropy <- function(T, X){
  if(!is.vector(T) || !is.vector(X)){
    return(-1)
  }
  values <- unique(X)
  total_size <- length(X)
  
  e <- 0.0
  for(v in values){
    indexes <- which(X==v)
    size <- length(indexes)
    in_target <- T[indexes]
    in_conditional <- X[indexes]
    prob <- size/total_size
    
    targets <- unique(in_target)
    ce <- 0.0
    for(t in targets){
      rept <- length(which(in_target==t))
      probability <- rept/size
      ce <- ce + (probability * log2(probability))
    }
    e <- e + prob * ce
  }
  
  return(-1.0*e)
}

information_gained <- function(T, X){
  e <- entropy(T)
  ce <- conditional_entropy(T, X)
  return(e - ce)
}

# generate data frame from table given
t <- c(0,1,0,0,1,0,0,1,0,0)
s <- c(0,0,1,1,0,1,0,0,0,0)
h <- c(1,1,1,3,2,1,2,2,1,3)
d <- c(1,0,0,0,0,0,1,1,1,1)
df <- data.frame(t,s,h,d)

# now we generate the first partition
ig_t <- information_gained(df$d, df$t)
ig_s <- information_gained(df$d, df$s)
ig_h <- information_gained(df$d, df$h)

#ig_t gives us 0.03485155
#ig_s gives us 0.3958156
#ig_h gives us 0.03903595
# it's pretty clear that s has superior information gained
# now we spli based on the s column
split1 <- which(df$s==1)
split0 <- which(df$s==0)

df_s1 <- df[split1,]
df_s0 <- df[split0,]
# we can see that df_s1 is 100% accurate, if an employee asks
#  for more than $10,000 he/she won't pay

# now we split sf_s0 to search for the greatest information_gained
# we won't need column s since it is already all 0's
ig_s0_t <- information_gained(df_s0$d, df_s0$t)
ig_s0_h <- information_gained(df_s0$d, df_s0$h)

#ig_s0_t gives us 0.4695652
#ig_s0_h gives us 0.07600985
# now its clear the next way of splitting is based on t
split1 <- which(df_s0$t==1)
split0 <- which(df_s0$t==0)

df_s0_t1 <- df_s0[split1,]
df_s0_t0 <- df_s0[split0,]

# current certainty on categorizng for decision tree is:
# splitDF(s) + (1-splitDF(s) * splitDF_S0(t))
# .3958156 + ((1-.3958156) * .4695652) > .5
# so minimum certainty is reached with these splits

# with this we get the resulting tree of
#           s 
#        p ~ .61
#
#   n = 3             t
#   y=(1,0)         p ~ .1
#
#               n = 4        n = 3
#               y=(1,0)      y=(0.33,0.66)

# with the previous tree, the case where a new loaner comes with
# t = 0, s = 0, h = 1
# we can predict that the loaner will pay with an accuracy of 
# .3958156 + ((1-.3958156) * .4695652) > .5 = .68
# so we would classify him/her as someone that pays

# to further increase accuracy we could also split based on h (house ownership)
# however the required threshold has been reached
