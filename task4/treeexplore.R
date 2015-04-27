# explores a binary tree data structure


maketree<- function(elements) {
  if(length(elements)==1) return(list(node=elements))
  split <- floor((length(elements)+1)/2)
  return(list(left=maketree(elements[1:split]),
              right=maketree(elements[(split+1) : length(elements)])))
} 

maketree(c("a"))
maketree(c("a","b"))
maketree(c("a","b","c"))
maketree(c("a","b","c","d"))
maketree(c("a","b","c","d","e"))
maketree(c("a","b","c","d","e","f"))
maketree(letters)

treeMapReduce <- function(tree,fMap,fReduce) {
  if("node" %in% names(tree)) return(fMap(tree$node))
  return(fReduce(treeMapReduce(tree$left,fMap,fReduce), treeMapReduce(tree$right,fMap,fReduce)))
}

tree <- maketree(c("a"))
treeMapReduce(tree,function(x) x, function(x,y) paste(x,y))

tree <- maketree(c("a","b"))
treeMapReduce(tree,function(x) x, function(x,y) paste(x,y))

tree <- maketree(c("a","b","c"))
treeMapReduce(tree,function(x) x, function(x,y) paste(x,y))

tree <- maketree(c("a","b","c","d"))
treeMapReduce(tree,function(x) x, function(x,y) paste(x,y))

tree <- maketree(c("a","b","c","d","e"))
treeMapReduce(tree,function(x) x, function(x,y) paste(x,y))

tree <- maketree(c("a","b","c","d","e","f"))
treeMapReduce(tree,function(x) x, function(x,y) paste(x,y))


