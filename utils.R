camel.case<-function(x=as.character()) {
  gsub('(\\w)(\\w*)', '\\U\\1\\L\\2', as.character(x), perl=TRUE)
}

enum <- function(x=c()) {
  c<-match.call()
  tryCatch({
    e <- ordered(1:length(as.vector(x)), labels=as.vector(x))
    class(e)<-c(class(e),'enum')
    return(e)
  },
  error = function(e) {
    e$call<-c
    e$message<-sub('labels','x',e$message)
    stop(e)
  })  
}

as.enum <- function(x) {
  enum(x)
}

is.enum <- function(x) {
  'enum' %in% class(x)
}

match.enum <- function(x , enum) {
  c<-match.call()
  tryCatch({
    if(!is.null(x)) x<- as.character(x)
    match.arg(x,enum)
  },
  error = function(e) {
    e$call<-c
    e$message<-sub('arg','x',e$message)
    stop(e)
  })
}


tree.new <- function(depth, children) {
  calling. <- match.call()
  
  # Create empty tree
  size <- tree.size(depth=depth, children=children)
  tree <- ifelse(depth < 1 || children < 1, list(), list(rep(NA, size)))
  class(tree) <- append(class(tree),'tree')
  attr(tree,'depth') <- depth
  attr(tree,'children') <- children
  
  return(tree)
}

tree.size  <- function(tree=NULL, depth=NULL, children=NULL) {
  calling. <- match.call()
  
  # Validate parameters
  if(is.null(tree)) {
    
    if(is.null(depth)) 
      stop(paste('undefined',sQuote('depth')))
    
    if(is.null(children))
      stop(paste('undefined',sQuote('children')))
    
  } else {
    
    tryCatch(
      stopifnot('tree' %in% class(tree)),
      error = function(e) {
        e$call <- calling.
        e$message <- paste('invalid type', sQuote('tree'),':',sQuote(class(tree)))
        stop(e)
    })
    
    if(is.null(depth)) 
      depth <- attr(tree,"depth")
    
    if(!is.null(children))
      warning(paste('parameter',sQuote('tree'),'defined, ignoring', sQuote('children')))
    
    children <- attr(tree,"children")
  }
  
  return(ifelse(children < 1 || depth < 1, 0,
                sum(children ^ (0:(depth - 1)))))
}

tree.node.children <- function(tree, node, index.=F) {
  .R.OFFSET <- 1
  calling.  <- match.call()
  children  <- attr(tree,'children')
  math.expr <- (((node - .R.OFFSET) * children) + 1:children) + .R.OFFSET
  
  if(index.) return(math.expr)
  return(tree.get.nodes(tree, node, math.expr))
}

`tree.node.children<-` <- function(tree, node, value) {
  
  .R.OFFSET <- 1
  calling.  <- match.call()
  children  <- attr(tree,'children')
  math.expr <- (((node - .R.OFFSET) * children) + 1:children) + .R.OFFSET
  
  tree.set.nodes(tree, node, math.expr) <- value
  return(tree)
}

tree.node.parent <- function(tree, node, index.=F) {
  
  .R.OFFSET <- 1
  calling.  <- match.call()
  children  <- attr(tree,'children')
  math.expr <- ((node - 1) %/% children) + .R.OFFSET
  
  if(index.) return(math.expr)
  return(tree.get.nodes(tree, node, math.expr))
}

`tree.node.parent<-` <- function(tree, node, value) {
  
  .R.OFFSET <- 1
  calling.  <- match.call()
  children  <- attr(tree,'children')
  math.expr <- ((node - 1) %/% children) + .R.OFFSET
  
  tree.set.nodes(tree, node, math.expr) <- value
  return(tree)
}

tree.leafs <- function(tree, index.=F) {
  depth  <- attr(tree,'depth')
  return(tree.level(tree, depth, index.))
}

tree.level <- function(tree, level, index.=F) {
  prev      <- tree.size(tree, depth=level-1)
  this      <- tree.size(tree, depth=level)
  math.expr <- ((prev + 1) : this)

  if(index.) return(math.expr)
  return(tree.get.nodes(tree, 1, math.expr))
}

`tree.level<-` <- function(tree, level, value) {
  prev      <- tree.size(tree, depth=level-1)
  this      <- tree.size(tree, depth=level)
  math.expr <- ((prev + 1) : this)

  tree.set.nodes(tree, 1, math.expr) <- value
  return(tree)
}

tree.path <- function(tree, from=1, to=length(tree), index.=F) {
  path <- c()
  for(level in attr(tree,'depth'): 1) {
    path <- ifelse(index., tree[[1]][to], c(to,path))
    to <- tree.node.parent(tree,to,index.)
  }
  return(path)
}

tree.get.nodes <- function(tree, node, expr) {
  nodes <- tree[[1]][expr]
  names(nodes)<-as.character(expr)
  return(nodes)
}

`tree.set.nodes<-` <- function(tree, node, expr, value) {
  tree[[1]][expr] <- value
  
  children <- tree(tree,'children')
  attr(tree,'depth') <- trunc(log(length(tree[[1]]), children)) + 1

  return(tree)
}

length.tree <- function(x) {
  if(inherits(x,'tree')) {
    length(x[[1]])
  }
  else UseMethod('length',x)
}

plot.tree <- function(x, ...) {
  require(igraph, quietly = T)
  
  if(inherits(x,'tree')) {
    a <- list(...)
    g <- graph.tree(length(x[[1]]), attr(x,'children'))
    
    if(exists('a$layout')) l = a$layout
    else l = layout.reingold.tilford(g, root=1)
    
    plot(g, layout = l, vertex.label=x[[1]], ...)
  }
  else UseMethod('plot',x)
}

print.tree <- function(x, index.=F, ...) {
  if(inherits(x,'tree')) {
    cat(paste('depth:', attr(x,'depth'),'\t'))
    cat(paste('children:', attr(x,'children'),'\n'))
    if(index.) {
      names(x[[1]])<-1:length(t)
      print(x[[1]])
    }
    else cat(x[[1]],'\n')
  }
  else UseMethod('print',x)
}
