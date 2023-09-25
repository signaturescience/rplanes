# ecp:::e.divisive

set.seed(100)
 X = matrix(c(rnorm(100),rnorm(100,3),rnorm(100,0,2)))
#X = matrix(c(rnorm(10),rnorm(10,3),rnorm(10,0,2)))

sig.lvl=0.05
R=199
k=NULL
min.size=30
alpha=1

function (X, sig.lvl = 0.05, R = 199, k = NULL, min.size = 30,
          alpha = 1)
{
  if (R < 0 && is.null(k))
    stop("R must be a nonnegative integer.")

  if ((sig.lvl <= 0 || sig.lvl >= 1) && is.null(k))
    stop("sig.lvl must be a positive real number between 0 and 1.")

  if (min.size < 2)
    stop("min.size must be an integer greater than 1.")

  if (alpha > 2 || alpha <= 0)
    stop("alpha must be in (0,2].")

  n = nrow(X)

  energy = new.env(parent = emptyenv())

  if (is.null(k)) {
    k = n
  }
  else R = 0

  ret = pvals = permutations = NULL

  changes = c(1, n + 1)

  ret$k.hat = 1

  setDim <- function (r, c, env)
  {
    env$done_ = matrix(0, nrow = r, ncol = c)
  }

  setDim(n, 2, energy)

  D = as.matrix(dist(X))^alpha

  con = NULL


 splitPointC <- function (s_, e_, D_, min_size_)
    .Call("splitPointC", s_, e_, D_, min_size_, PACKAGE = "ecp")


  splitPoint <- function (start, end, D, min.size)
  {
    if (end - start + 1 < 2 * min.size)
      return(c(-1, -Inf))
    D = D[start:end, start:end]
    return(splitPointC(start, end, D, min.size))
  }



 setVal <- function (i, j, val, env)
  {
    old = env$done_[i, j]
    env$done_[i, j] = val
    invisible(old)
  }


  e.split <- function (changes, D, min.size, for.sim = FALSE, env = emptyenv())
  {
    splits = sort(changes)
    best = c(-1, -Inf)
    ii = jj = -1
    if (for.sim) {
      for (i in 2:length(splits)) {
        tmp = splitPoint(splits[i - 1], splits[i] - 1, D,
                         min.size)
        if (tmp[2] > best[2]) {
          ii = splits[i - 1]
          jj = splits[i] - 1
          best = tmp
        }
      }
      changes = c(changes, best[1])
      return(list(first = ii, second = jj, third = changes,
                  fourth = best[2]))
    }
    else {
      for (i in 2:length(splits)) {
        if (env$done_[splits[i - 1], 1])
          tmp = env$done_[splits[i - 1], ]
        else {
          tmp = splitPoint(splits[i - 1], splits[i] - 1,
                           D, min.size)
          setVal(splits[i - 1], 1, tmp[1], env)
          setVal(splits[i - 1], 2, tmp[2], env)
        }
        if (tmp[2] > best[2]) {
          ii = splits[i - 1]
          jj = splits[i] - 1
          best = tmp
        }
      }
      changes = c(changes, best[1])
      setVal(ii, 1, 0, env)
      setVal(ii, 2, 0, env)
      return(list(first = ii, second = jj, third = changes,
                  fourth = best[2]))
    }
  }


  perm.cluster<- function (D, points)
  {
    points = sort(points)
    K = length(points) - 1
    for (i in 1:K) {
      u = sample(points[i]:(points[i + 1] - 1))
      D[points[i]:(points[i + 1] - 1), points[i]:(points[i +
                                                           1] - 1)] = D[u, u]
    }
    return(D)
  }


  sig.test <- function (D, R, changes, min.size, obs, env = emptyenv())
  {
    if (R == 0)
      return(0)
    over = 0
    for (f in 1:R) {
      D1 = perm.cluster(D, changes)
      tmp = e.split(changes, D1, min.size, TRUE)
      if (tmp[[4]] >= obs)
        over = over + 1
    }
    p.val = (1 + over)/(f + 1)
    return(c(p.val, f))
  }


  while (k > 0) {
    tmp = e.split(changes, D, min.size, FALSE, energy)
    i = tmp[[1]]
    j = tmp[[2]]
    Estat = tmp[[4]]
    tmp = tmp[[3]]
    con = tail(tmp, 1)
    if (con == -1)
      break
    result = sig.test(D, R, changes, min.size, Estat, env = energy)
    pval = result[1]
    permutations = c(permutations, result[2])
    pvals = c(pvals, pval)
    if (pval > sig.lvl)
      break
    changes = tmp
    ret$k.hat = ret$k.hat + 1
    k = k - 1
  }


  tmp = sort(changes)
  ret$order.found = changes
  ret$estimates = tmp
  ret$considered.last = con
  ret$p.values = pvals
  ret$permutations = permutations
  ret$cluster = rep(1:length(diff(tmp)), diff(tmp))
  return(ret)
}
