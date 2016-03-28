.R.OFFSET <- 1

tree.new <- function(depth, children, value=NULL) {
  
  size <- tree.size(depth=depth, children=children)
  if(size > .Machine$integer.max)
    stop('depth x children exceeds integer maximum')
  
  if(depth < 1 || children < 1)
    tree <- list()
  else if(is.null(value))
    tree <- rep(list(NA), size)
  else {
    value <- unlist(value)
    if(size < length(value))
      warning('number of items exceeds tree size')
    tree <- list(value[1:size])
  }
  class(tree) <- append(class(tree),'tree')
  attr(tree,'depth') <- depth
  attr(tree,'children') <- children
  names(tree) <- 1:length(tree)
  
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
                1 + (children*((children^(depth-1))-1))/(children-1)))
}

tree.depth <- function(tree,node=length(tree)) {
  calling. <- match.call()
  
  tryCatch(
    stopifnot('tree' %in% class(tree)),
    error = function(e) {
      e$call <- calling.
      e$message <- paste('invalid type', sQuote('tree'),':',sQuote(class(tree)))
      stop(e)
  })
  
  .R.OFFSET<-1
  if(node < .R.OFFSET || node > length(tree)) {
    warning(paste('node', sQuote(node), 'does not exist in tree'))
    return(0)    
  }
  
  children <- attr(tree,'children')
  return(ceiling(log(node*((children-1)/children),children))+.R.OFFSET)  
}

tree.node.children <- function(tree, node, index.=F) {
  
  
  children  <- attr(tree,'children')
  math.expr <- (((node - .R.OFFSET) * children) + 1:children) + .R.OFFSET
  
  if(index.) return(math.expr)
  return(.tree.get.nodes(tree, node, math.expr))
}

`tree.node.children<-` <- function(tree, node, value) {

  children  <- attr(tree,'children')
  math.expr <- (((node - .R.OFFSET) * children) + 1:children) + .R.OFFSET
  
  .tree.set.nodes(tree, node, math.expr) <- value
  return(tree)
}

tree.node.parent <- function(tree, node, index.=F) {

  children  <- attr(tree,'children')
  nod_fset  <- node - .R.OFFSET
  math.expr <- .R.OFFSET + nod_fset%/%children - (!(nod_fset%%children))
  
  if(index.) return(math.expr)
  return(.tree.get.nodes(tree, node, math.expr))
}

`tree.node.parent<-` <- function(tree, node, value) {

  children  <- attr(tree,'children')
  nod_fset  <- node - .R.OFFSET
  math.expr <- .R.OFFSET + nod_fset%/%children - (!(nod_fset%%children))
  
  .tree.set.nodes(tree, node, math.expr) <- value
  return(tree)
}

tree.leaves <- function(tree, index.=F) {
  depth  <- attr(tree,'depth')
  return(tree.level(tree, depth, index.))
}

tree.level <- function(tree, level, index.=F) {
  prev      <- tree.size(tree, depth=level-1)
  this      <- tree.size(tree, depth=level)
  math.expr <- ((prev + 1) : this)
  
  if(index.) return(math.expr)
  return(.tree.get.nodes(tree, 1, math.expr))
}

`tree.level<-` <- function(tree, level, value) {
  prev      <- tree.size(tree, depth=level-1)
  this      <- tree.size(tree, depth=level)
  math.expr <- ((prev + 1) : this)
  
  .tree.set.nodes(tree, 1, math.expr) <- value
  return(tree)
}

tree.path <- function(tree, from=1, to=length(tree), index.=F) {
  path <- c()
  goal <- to
  
  # impose limit of iterations == tree depth
  # use backwards search for simplicity
  for(level in attr(tree,'depth'):1) {
    step <- to
    path <- c(path,step)
    if(from == to) {
      if(index.) return(path)
      else return(tree[path])
    }
    to <- tree.node.parent(tree, to, index.=T)
  }
  
  warning(paste('No path found from',from, 'to', goal))
  return(NULL)
}

.tree.get.nodes <- function(tree, node, expr) {
  
  
  if(node < .R.OFFSET || node > length(tree)) {
    warning(paste('node', sQuote(node), 'does not exist in tree'))
    return(NULL)    
  }
  
  if(any(expr < .R.OFFSET) || any(expr > length(tree))) {
    warning(paste('requested nodes (', paste(expr,collapse=','),
                  ') do not exist in tree', sep=""))
    return(NULL)    
  }
  
  if(length(expr) == 1) return(tree[[expr]])
  else return(tree[expr])
}

`.tree.set.nodes<-` <- function(tree, node, expr, value) {
  
  if(node < .R.OFFSET) {
    warning(paste('node', sQuote(node), 'does not exist in tree'))
    return(tree)    
  }
  if(any(expr < .R.OFFSET)) {
    warning(paste('requested nodes (',paste(expr,collapse=','),
                  ') do not exist in tree', sep=""))
    return(tree)    
  }
  
  if(length(expr) == 1) tree[[expr]] <- value
  else tree[expr] <- value

  olddepth <- attr(tree,'depth')
  newdepth <- tree.depth(tree)
  attr(tree,'depth') <- newdepth
  
  if(newdepth - olddepth > 2) {
    warning('unexpected leap in tree depth:', newdepth - olddepth)
  }
  
  return(tree)
}

plot.tree <- function(x, ...) {
  require(igraph, quietly = T)
  
  if(inherits(x,'tree')) {
    a <- list(...)
    g <- graph.tree(length(x), attr(x,'children'))
    
    if(exists('a$layout')) l = a$layout
    else l = layout.reingold.tilford(g, root=1)
    
    plot(g, layout = l, edge.arrow.size = 0.2,
         vertex.label=names(x), vertex.label.cex=0.8, vertex.size=20, ...)
  }
  else UseMethod('plot',x)
}

print.tree <- function(x, index.=F, pretty.=F, ...) {
  if(inherits(x,'tree')) {
    cat(paste('depth:', attr(x,'depth'),'\t'))
    cat(paste('children:', attr(x,'children'),'\n'))
    if(pretty. && length(t)>1) {
      
      children  <- attr(x,'children')
      buffer <- c()
      for(node in 2:length(t)) {
        nod_fset <- node - .R.OFFSET
        str <- paste(.R.OFFSET + nod_fset%/%children - (!(nod_fset%%children)),'->',node)
        buffer <- c(buffer, str)
      }
      cat(paste(buffer,collapse=', ',sep='\n'))
    }
    else for(k in seq_along(x)) {
      cat(paste('$`',k,'`\n',sep=""))
      print(x[[k]])
    }
  }
  else UseMethod('print',x)
}
