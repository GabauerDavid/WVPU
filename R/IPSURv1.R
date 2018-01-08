`Prob` <- function (x, ...)
  UseMethod("Prob")

`prob` <- function (x, ...){
  message("'prob' is deprecated; use 'Prob' instead.")
  Prob(x, ...)
}

`Prob.default` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs column")
    stop("see ?probspace")
  }
  if (missing(event)) {
    r <- TRUE
  }
  else {
    e <- substitute(event)
    r <- eval(e, x, parent.frame())
    if (!is.logical(r))
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1)))
      warning("'space' does not have probability 1.")
  }
  A <- x[r, ]
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- eval(f, x, enclos = parent.frame())
    if (!is.logical(g)) {
      if (!is.data.frame(given))
        stop("'given' must be data.frame or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event))
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- x[g, ]
    }
    if (sum(B$probs <= 0))
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}


`Prob.ps` <- function (x, event = NULL, given = NULL, ...){
  if (is.null(x$probs)) {
    message("'space' is missing a probs component")
    stop("see ?probspace")
  }
  if (missing(event)) {
    A <- x
  }
  else {
    e <- substitute(event)
    r <- sapply(x$outcomes, function(t) {
      eval(e, t, enclos=parent.frame())
    })
    if (!is.logical(r))
      stop("'event' must evaluate to logical")
    r <- r & !is.na(r)
    if (!isTRUE(all.equal(sum(x$probs), 1)))
      warning("'space' does not have probability 1.")
    A <- list(outcomes = x$outcomes[r], probs = x$probs[r])
  }
  if (missing(given)) {
    p <- sum(A$probs)
  }
  else {
    f <- substitute(given)
    g <- sapply(x$outcomes, function(t) {
      eval(f, t, enclos=parent.frame())
    })
    if (!is.logical(g)) {
      if (!is.probspace(given))
        stop("'given' must be a probspace or evaluate to logical")
      B <- given
    }
    else {
      if (missing(event))
        stop("'event' must be specified when 'given' is an expression")
      g <- g & !is.na(g)
      B <- list(outcomes = x$outcomes[g], probs = x$probs[g])
    }
    if (sum(B$probs <= 0))
      stop("prob(given) must be positive")
    p <- sum(intersect(A, B)$probs)/sum(B$probs)
  }
  return(p)
}



`nsamp` <- function (n, k, replace = FALSE, ordered = FALSE){
  m <- length(n)
  if (length(k) != m)
    stop("number of urns doesn't equal number of sample sizes")
  if (length(replace) != m) {
    replace <- rep(replace, length.out = m)
  }
  if (length(ordered) != m) {
    ordered <- rep(ordered, length.out = m)
  }
  res <- c()
  for (i in 1:m) if (isTRUE(replace[i])) {
    if (isTRUE(ordered[i])) {
      res[i] <- n[i]^k[i]
    }
    else {
      res[i] <- choose(n[i] - 1 + k[i], k[i])
    }
  }
  else {
    if (isTRUE(ordered[i])) {
      res[i] <- factorial(n[i])/factorial(n[i] - k[i])
    }
    else {
      res[i] <- choose(n[i], k[i])
    }
  }
  return(res)
}



`permsn` <- function (x, m)
{

  # require(combinat)
  if (is.numeric(x) && length(x) == 1 && x > 0 && trunc(x) == x)

    x <- seq(x)
  temp <- combn(x, m)
  if ( isTRUE(all.equal(m,1)) ) {

    P <- temp
  } else if (isTRUE(all.equal(m, length(x)))) {

    temp <- matrix(x, ncol = 1)
    P <- array(unlist(permn(temp[, 1])), dim = c(m, factorial(m)))
  } else {
    k <- dim(temp)[1]
    n <- dim(temp)[2]
    P <- array(unlist(permn(temp[, 1])), dim = c(k, factorial(k)))
    for (i in 2:n) {
      a <- temp[, i]
      perms <- array(unlist(permn(a)), dim = c(k, factorial(k)))
      P <- cbind(P, perms)
    }


  }
  return(P)
}




`cards` <- function (jokers = FALSE, makespace = FALSE){
  x <- c(2:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(rank = x, suit = y)
  if (jokers) {
    levels(res$rank) <- c(levels(res$rank), "Joker")
    res <- rbind(res, data.frame(rank = c("Joker", "Joker"),
                                 suit = c(NA, NA)))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}


`euchredeck` <- function (benny = FALSE, makespace = FALSE){
  x <- c(9:10, "J", "Q", "K", "A")
  y <- c("Club", "Diamond", "Heart", "Spade")
  res <- expand.grid(value = x, suit = y)
  if (benny) {
    levels(res$value) <- c(levels(res$value), "Joker")
    res <- rbind(res, data.frame(value = c("Joker"), suit = NA))
  }
  if (makespace) {
    res$probs <- rep(1, dim(res)[1])/dim(res)[1]
  }
  return(res)
}



`rolldie` <- function (times, nsides = 6, makespace = FALSE){
  temp = list()
  for (i in 1:times) {
    temp[[i]] <- 1:nsides
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("X", times), 1:times, sep = ""))
  if (makespace)
    res$probs <- rep(1, nsides^times)/nsides^times
  return(res)
}



`roulette` <- function (european = FALSE, makespace = FALSE){
  if (european) {
    num = c("0", "26", "3", "35", "12", "28", "7", "29",
            "18", "22", "9", "31", "14", "20", "1", "33", "16",
            "24", "5", "10", "23", "8", "30", "11", "36", "13",
            "27", "6", "34", "17", "25", "2", "21", "4", "19",
            "15", "32")
    color <- c("Green", rep(c("Black", "Red"), 18))
  }
  else {
    num = c("27", "10", "25", "29", "12", "8", "19", "31",
            "18", "6", "21", "33", "16", "4", "23", "35", "14",
            "2", "0", "28", "9", "26", "30", "11", "7", "20",
            "32", "17", "5", "22", "34", "15", "3", "24", "36",
            "13", "1", "00")
    color <- c(rep(c("Red", "Black"), 9), "Green", rep(c("Black",
                                                         "Red"), 9), "Green")
  }
  res <- data.frame(num = num, color = color)
  if (makespace) {
    res$probs <- rep(1, length(num))/length(num)
  }
  return(res)
}



`tosscoin` <- function (times, makespace = FALSE){
  temp <- list()
  for (i in 1:times) {
    temp[[i]] <- c("H", "T")
  }
  res <- expand.grid(temp, KEEP.OUT.ATTRS = FALSE)
  names(res) <- c(paste(rep("toss", times), 1:times, sep = ""))
  if (makespace)
    res$probs <- rep(1, 2^times)/2^times
  return(res)
}



`urnsamples` <- function (x, ...)
  UseMethod("urnsamples")


`urnsamples.data.frame` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
  nurn <- dim(x)[1]
  if (isTRUE(replace)) {
    if (isTRUE(ordered)) {
      temp <- list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
    }
    else {
      temp <- list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
      ind <- t(unique(t(apply(res, 1, sort))))
    }
  }
  else {
    if (size > nurn)
      stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
    if (isTRUE(ordered)) {
      ind <- permsn(1:nurn, size)
    }
    else {
      ind <- combn(1:nurn, size)
    }
  }
  if (!is.null(x$probs))
    x$probs <- NULL
  nss <- dim(ind)[2]
  out <- list()
  for (i in 1:nss) {
    out[[i]] <- x[ind[, i], ]
  }
  return(out)
}



`urnsamples.default` <- function (x, size, replace = FALSE, ordered = FALSE, ...){
  nurn <- length(x)
  if (isTRUE(replace)) {
    if (isTRUE(ordered)) {
      temp = list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      ind <- t(as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE)))
    }
    else {
      temp = list()
      for (i in 1:size) {
        temp[[i]] <- 1:nurn
      }
      res <- as.matrix(expand.grid(temp, KEEP.OUT.ATTRS = FALSE))
      ind <- t(unique(t(apply(res, 1, sort))))
    }
  }
  else {
    if (size > nurn)
      stop("cannot take a sample larger than the urn size when 'replace = FALSE'")
    if (isTRUE(ordered)) {
      ind <- permsn(1:nurn, size)
    }
    else {
      ind <- combn(1:nurn, size)
    }
  }
  nss <- dim(ind)[2]
  out <- matrix(nrow = nss, ncol = size)
  for (i in 1:nss) {
    out[i, ] <- x[ind[, i]]
  }
  return(data.frame(out))
}



`countrep` <- function (x, ...)
  UseMethod("countrep")

`countrep.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = countrep, ...)
}


`countrep.default` <- function (x, vals = unique(x), nrep = 2, ...){
  res <- 0
  if (length(x) >= nrep) {
    for (i in 1:length(vals)) {
      if (sum(mapply(all.equal, x, vals[i]) == TRUE) ==
          nrep) {
        res <- res + 1
      }
    }
  }
  return(res)
}


`isin` <- function (x, ...)
  UseMethod("isin")



`isin.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = isin, ...)
}


`isin.default` <- function (x, y, ordered = FALSE, ...){
  res <- (length(y) <= length(x))
  if (res) {
    temp <- x
    for (i in 1:length(y)) {
      if (is.element(y[i], temp)) {
        if (!ordered) {
          temp <- temp[-which(temp %in% y[i])[1]]
        }
        else {
          temp <- temp[-(1:which(temp %in% y[i])[1])]
        }
      }
      else {
        res <- FALSE
        i <- length(y)
      }
    }
  }
  return(res)
}



`isrep` <- function (x, ...)
  UseMethod("isrep")


`isrep.data.frame` <- function (x, ...){
  apply(x, MARGIN = 1, FUN = isrep, ...)
}



`isrep.default` <- function (x, vals = unique(x), nrep = 2, ...){
  res <- FALSE
  if (length(x) >= nrep) {
    for (i in 1:length(vals)) {
      if (sum(mapply(all.equal, x, vals[i]) == TRUE) ==
          nrep) {
        res <- TRUE
        i <- length(vals)
      }
    }
  }
  return(res)
}



`addrv` <- function (space, FUN = NULL, invars = NULL, name = NULL, ...){
  if (any(class(space) == "ps"))
    stop("not implemented for class 'ps'")
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  bnames <- names(space)[which(names(space) != "probs")]
  out <- subset(space, select = bnames)
  probs <- subset(space, select = probs)
  if (is.null(invars))
    invars <- bnames
  if (!is.character(invars))
    stop("vars should be a character vector")
  if (!is.null(FUN)) {
    if (is.null(name))
      name <- "X"
    temp <- apply(subset(space, select = invars), 1, FUN)
    val <- cbind(out, temp, probs)
    names(val) <- c(bnames, name, "probs")
  }
  else {
    val <- transform(out, ...)
    val$probs <- probs
  }
  return(val)
}



`marginal` <- function (space, vars = NULL){
  if (!is.data.frame(space) | is.null(space$probs)) {
    message("'space' is not a proper probability space")
    stop("see ?probspace")
  }
  if (is.null(vars))
    vars <- names(space)[names(space) != "probs"]
  if (!is.character(vars)) {
    stop("'vars' must be a character vector")
  }
  if (length(vars) > 1) {
    res <- aggregate(space$probs, by = as.list(space[, vars]),
                     FUN = sum)
  }
  else {
    res <- aggregate(space$probs, by = list(space[, vars]),
                     FUN = sum)
  }
  names(res) <- c(vars, "probs")
  return(res)
}


`noorder` <- function (space){
  if (!is.data.frame(space)) {
    message("'space' is missing a probs column")
    stop("see ?probspace")
  }
  if (is.null(space$probs)) {
    if (dim(space)[2] < 2)
      stop("'space' has only one column of outcomes; already unordered")
    n <- names(space)
    res <- unique(data.frame(t(apply(space, 1, sort))))
    names(res) <- n
  }
  else {
    if (dim(space)[2] < 3)
      stop("'space' has only one column of outcomes; already unordered")
    A <- subset(space, select = -probs)
    probs <- subset(space, select = probs)
    n <- names(A)
    sA <- data.frame(t(apply(A, 1, sort)))
    res <- cbind(sA, probs)
    res <- aggregate(res$probs, by = as.list(sA), sum)
    names(res) <- c(n, "probs")
  }
  return(res)
}




`intersect` <- function (x, ...)
  UseMethod("intersect")


`intersect.data.frame` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  x[match(intersect(a, b), a), ]
}



`intersect.default` <- function (x, y, ...){
  y <- as.vector(y)
  unique(y[match(as.vector(x), y, 0)])
}



`intersect.ps` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  e <- match(intersect(a, b), a)
  res <- list(outcomes = x$outcomes[e], probs = x$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}



`setdiff` <- function (x, ...)
  UseMethod("setdiff")


`setdiff.data.frame` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  x[match(setdiff(a, b), a), ]
}


`setdiff.default` <- function (x, y, ...){
  x <- as.vector(x)
  y <- as.vector(y)
  unique(if (length(x) || length(y))
    x[match(x, y, 0) == 0]
    else x)
}


`setdiff.ps` <- function (x, y, ...){
  a <- do.call("paste", c(x, sep = "\r"))
  b <- do.call("paste", c(y, sep = "\r"))
  e <- match(setdiff(a, b), a)
  res <- list(outcomes = x$outcomes[e], probs = x$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}


`subset.ps` <- function (x, subset, ...){
  e <- substitute(subset)
  r <- sapply(x$outcomes, function(t) {
    eval(e, t)
  })
  if (!is.logical(r))
    stop("'subset' must be logical")
  res <- list(outcomes = x$outcomes[r & !is.na(r)], probs = x$probs[r &
                                                                      !is.na(r)])
  class(res) <- c("ps", "list")
  return(res)
}


`union` <- function (x, ...)
  UseMethod("union")


`union.data.frame` <- function (x, y, ...){
  res <- unique(rbind(as.data.frame(y), x))
  res[order(as.numeric(rownames(res))), ]
}



`union.default` <- function (x, y, ...)
  unique(c(as.vector(x), as.vector(y)))


`union.ps` <- function (x, y, ...){
  na <- length(x$outcomes)
  nb <- length(y$outcomes)
  temp <- x
  for (i in 1:nb) {
    temp$outcomes[[na + i]] <- y$outcomes[[i]]
    temp$probs[[na + i]] <- y$probs[[i]]
  }
  a <- do.call("paste", c(temp, sep = "\r"))
  e <- !duplicated(a)
  res <- list(outcomes = temp$outcomes[e], probs = temp$probs[e])
  class(res) <- c("ps", "list")
  return(res)
}
