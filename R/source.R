rmrlm <- function(x, ...) UseMethod("rmrlm")

rmrlm.default <- function(inputfile, input.format = "native", formula, xlev = NULL, combine = F, method = "qr") {
  
  cl <- match.call()
  hdfs.init()
  
  if(method == "qr") {
    # step 1: map
    map.fun4_1 <- function(k, v){
      
      dat <- if(!is.null(xlev)) model.frame(formula, v, xlev = xlev) else model.frame(formula, v)
      Xi <- model.matrix(attr(dat, "terms"), dat) # n/b x m
      yi <- model.response(dat) # n/b x 1
      
      yty <- crossprod(yi)
      qrx <- qr(Xi) # n/b x m
      Ri <- qr.R(qrx)
      Q1iyi <- crossprod(qr.Q(qrx), yi) # m x 1
      
      keyval(1, list(Q1iyi, Ri, yty))
    }
    
    combine.fun4_1 <- function(k,v) {
      Q1iyi <- Reduce(rbind, v[seq_along(v) %% 3 == 1]) # bm x 1
      Ri <- Reduce(rbind, v[seq_along(v) %% 3 == 2]) # bm x m
      yty <- Reduce("+", v[seq_slong(v) %% 3 == 0])
      keyval(1, list(Q1iyi, Ri, yty))
    }
    
    reduce.fun4_1 <- function(k, v) {
      Q1y <- Reduce(rbind, v[seq_along(v) %% 3 == 1]) # bm x 1
      Rtemp <- Reduce(rbind, v[seq_along(v) %% 3 == 2]) # bm x m
      yty <- Reduce("+", v[seq_along(v) %% 3 == 0])
      
      qrRtemp <- qr(Rtemp)
      Rfinal <- qr.R(qrRtemp)
      Qty <- crossprod(qr.Q(qrRtemp), Q1y)
      
      # betahat <- solve(Rfinal, Qty)
      
      
      keyval(1, list(Rfinal, Qty, yty))
    }
    
    step1_res <- mapreduce( input = inputfile, 
                            input.format = input.format,
                            map = map.fun4_1,
                            reduce = reduce.fun4_1,
                            combine = if(combine) combine.fun4_1 else FALSE )
    # browser()
    res <- values(from.dfs(step1_res))
    z <- list(Rfinal = res[[1]], Qty = res[[2]], yty = res[[3]], cl = cl)
    coef <- solve(z$Rfinal, z$Qty)
    
  }else if(method == "ne") {
    map.fun1 <- function(k, v){
      
      dat <- if(!is.null(xlev)) model.frame(formula, v, xlev = xlev) else model.frame(formula, v)
      Xi <- model.matrix(attr(dat, "terms"), dat) # n/b x m
      yi <- model.response(dat) # n/b x 1
      
      XtXi <- crossprod(Xi, Xi)
      Xtyi <- crossprod(Xi,yi)
      ytyi <- crossprod(yi,yi)
      keyval(1, list(XtXi, Xtyi, ytyi))
    }
    
    # combine.fun1 <- function(k, v) {
    #   XtX <- Reduce("+",v[seq_along(v) %% 3 == 1])
    #   Xty <- Reduce("+",v[seq_along(v) %% 3 == 2])
    #   yty <- Reduce("+",v[seq_along(v) %% 3 == 0])
    #   keyval(1, list(XtXk, Xtyk, ytyk))
    # }
    
    reduce.fun1 <- function(k, v){
      XtX <- Reduce("+",v[seq_along(v) %% 3 == 1])
      Xty <- Reduce("+",v[seq_along(v) %% 3 == 2])
      yty <- Reduce("+",v[seq_along(v) %% 3 == 0])
      # beta.hat <- solve(XtX,Xty) # estimate for beta
      keyval(1,list(XtX, Xty, yty))
    }
    
    mr <- mapreduce( input = inputfile, 
                     input.format = input.format, 
                     map = map.fun1, 
                     reduce = reduce.fun1, 
                     combine = combine )
    res <- values(from.dfs(mr))
    z <- list(XtX = res[[1]], Xty = res[[2]], yty = res[[3]], cl = cl)
    coef <- solve(z$XtX, z$Xty)
    
  } else cat("method argument should be qr or ne!")
  
  print(coef)
  res <- structure(z, class = "rmrlm")
  invisible(res)
}


summary.rmrlm <- function(x, digits = max(3L, getOption("digits") - 3L)) {
  
  if(names(x)[1] == "Rfinal") {
    Rfinal <- x[[1]]
    XtX <- crossprod(Rfinal)
    Xty <- crossprod(x[[1]], x[[2]])
    yty <- x[[3]]
    cl <- x[[4]]
  }else {
    XtX <- x[[1]]
    Xty <- x[[2]]
    yty <- x[[3]]
    cl <- x[[4]]
  }
  betahat <- solve(x[[1]], x[[2]])
  nn <- XtX[1,1]
  ysum <- Xty[1]
  ybar <- ysum/nn
  
  SSE <- yty - crossprod(betahat, Xty)
  SST <- yty - ysum^2/nn
  SSR <- SST - SSE
  
  df.reg <- dim(XtX)[1L] - 1
  df.tot <- nn - 1
  df.res <- df.tot - df.reg
  
  MSR <- SSR / df.reg
  MST <- SST / df.tot
  MSE <- SSE / df.res
  
  r.squared <- SSR/SST
  adj.r.squared <- 1 - MSE/MST
  f <- MSR / MSE
  f.pval <- pf(f,df.reg, df.res, lower.tail = FALSE)
  
  se.betahat <- if(names(x)[1] == "Rfinal"){
    sqrt(diag(solve(Rfinal, t(solve(Rfinal)))*as.numeric(MSE)))
  }else {
    sqrt(diag(solve(XtX)*as.numeric(MSE)))
  }
  
  cmat <- cbind(betahat, se.betahat, betahat/se.betahat)
  cmat <- cbind(cmat, ifelse(cmat[,3] > 0, 2*pt(-cmat[, 3], df = nn-length(betahat)),
                             2*pt(cmat[, 3], df = nn-length(betahat))))
  row.names(cmat) <- dimnames(betahat)[[1]]
  colnames(cmat) <- c("Estimate", "Std.Err", "t value", "Pr(>|t|)")
  
  cat("\nCall:\n", paste( deparse(cl), sep = "\n", 
                         collapse = "\n"), "\n\n", sep = "")
  
  printCoefmat(cmat, digits = digits)
  
  cat("Multiple R-squared: ", formatC(r.squared, digits = digits))
  cat(",\tAdjusted R-squared: ", formatC(adj.r.squared,digits = digits), 
      "\nF-statistic:", formatC(f,digits = digits), "on", df.reg, "and", 
      df.res, "DF,  p-value:", format.pval(f.pval,digits = digits))
  cat("\n")
  z <- list(formula = deparse(formula), coef = cmat, 
            r.squared = r.squared, adj.r.squared = adj.r.squared, 
            fstatistic = list(f = f, df.reg = df.reg, df.res = df.res, pval = f.pval))
  invisible(z)
}
