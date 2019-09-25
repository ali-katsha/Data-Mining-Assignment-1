#bestSplitNode FUNCTION 
#INPUT predictor values, class Values, minleaf 
#FUNCTION calculate the gini-index for each predicor based on the class values 
#OUTPUT bestNodeToSplit as a list() has NodeImpurity and NodeCondition

bestSplitNode <- function(predictorValue, classValues, minleaf) {
  
  sortedPredictorValue <- sort(unique(predictorValue))
  NodeImpurity <- NULL
  NodeCondition <- NULL
  for (index in 1:(length(sortedPredictorValue))) {
    
    conditionOfNode     <- (sortedPredictorValue[index] + sortedPredictorValue[index + 1]) / 2
    subGroupZeros       <- classValues[which(predictorValue <= conditionOfNode)]
    subGroupOnes        <- classValues[which(predictorValue > conditionOfNode)]
    LengthSubGroupZeros <- length(subGroupZeros)
    LengthSubGroupOnes  <- length(subGroupOnes)
    
    if ((LengthSubGroupZeros < minleaf) || (LengthSubGroupOnes < minleaf)) {
      next
    }
    
    # gini index for each child node(impurity) 
    subGroupNode1ZerosProbability <- length(which(subGroupZeros == 0))/LengthSubGroupZeros
    subGroupNode1OnesProbability  <- length(which(subGroupZeros == 1))/LengthSubGroupZeros
    subGroupNode2ZerosProbability <- length(which(subGroupOnes == 0))/LengthSubGroupOnes
    subGroupNode2OnesProbability  <- length(which(subGroupOnes == 1))/LengthSubGroupOnes
    
    giniSubNode1 <-  1-(subGroupNode1ZerosProbability)^2-(subGroupNode1OnesProbability)^2
    giniSubNode2 <-  1-(subGroupNode2ZerosProbability)^2-(subGroupNode2OnesProbability)^2
    WeightedGiniNode <- (LengthSubGroupZeros * giniSubNode1 + LengthSubGroupOnes * giniSubNode2)/(LengthSubGroupZeros+LengthSubGroupOnes)
    
    if (is.null(NodeImpurity) || (WeightedGiniNode < NodeImpurity)) {
      NodeImpurity <- WeightedGiniNode
      NodeCondition <- conditionOfNode
    }
    
  }
  
  if (is.null(NodeImpurity)) {
    return(NULL)
  }
  
  bestNodeToSplit <- list()
  bestNodeToSplit[["NodeImpurity"]] <- NodeImpurity
  bestNodeToSplit[["NodeCondition"]] <- NodeCondition
  return(bestNodeToSplit)
}

#splitNode FUNCTION
#INPUT interimDataMatrix, interimClassColumnNumber, nmin,and minleaf
#FUNCTION finding the best node by finding the impurity of each nodes that have been used by calculate gini index of each of them in function bestSplitNode 
#OUTPUT dataTree as a list() which keeps predictorLabel and its condition.

splitNode <- function(interimDataMatrix, interimClassColumnNumber, nmin, minleaf) {
  
  calssValues      <- interimDataMatrix[, interimClassColumnNumber] 
  lengthGroup      <- length(calssValues)
  lengthGroupZeros <- length(which(calssValues == 0))
  p                <- lengthGroupZeros / lengthGroup
  giniOfClass      <- 1-(p)^2-(1-p)^2
  
  if ((nrow(interimDataMatrix) < nmin) || (giniOfClass == 0))
  {
    return(NULL)
  }
  
  predictorsNames    <- names(interimDataMatrix[,-interimClassColumnNumber])
  classColumnValue   <- interimDataMatrix[, interimClassColumnNumber]
  nodeToSplit        <- NULL
  predictorLabel     <- NULL
  predictorCondition <- NULL
  
  for (index in 1:length(predictorsNames)) {
    
    predictorName        <- predictorsNames[index]
    predictorValues      <- interimDataMatrix[, predictorName]
    interimBestSplit     <- bestSplitNode(predictorValues, classColumnValue, minleaf)
    
    if (is.null(interimBestSplit)) {
      next
    }
    
    interimPurityOfNode  <- interimBestSplit[["NodeImpurity"]]
    
    if (is.null(nodeToSplit) || (interimPurityOfNode < nodeToSplit)) {
      nodeToSplit        <- interimPurityOfNode 
      predictorLabel     <- predictorName
      predictorCondition <- interimBestSplit[["NodeCondition"]]
      
    }  
  }
  
  if (is.null(predictorLabel)) {
    return(NULL)
  }
  
  dataTree                         <- list()
  dataTree[["predictorLabel"]]     <- predictorLabel
  dataTree[["predictorCondition"]] <- predictorCondition
  return(dataTree)
  
}

#treeGrowBuild FUNCTION
#INPUT dataMatrix, classColumnNumber, nmin, minleaf,and nfeat
#FUNCTION after selecting the best predictor in function splitNode we splitting the data matrix based on two classes (left and right) of that predictor and we apply the same procedures recursively to the right subtree and left subtree untill the nodes becomes pure (leaves)
#OUTPUT   tree as list() which stores predictor labelRoot left subTree and right subTree

treeGrowBuild <- function(dataMatrix, classColumnNumber, nmin, minleaf, nfeat) {
  
  numberOfColumns <- ncol(dataMatrix)
  
  if (numberOfColumns <= nfeat + 1) {
    
    temproryDataMatrix        <- dataMatrix
    temproryClassColumnNumber <- classColumnNumber
    
  } else {
    
    rnd_columns                <- sample(c(1:numberOfColumns)[-classColumnNumber], size = nfeat, replace = FALSE)
    temproryDataMatrix         <- dataMatrix[, c(rnd_columns, classColumnNumber), drop = FALSE] 
    temproryClassColumnNumber  <- nfeat + 1
    
  }
  
  bestSplit <- splitNode(temproryDataMatrix, temproryClassColumnNumber, nmin, minleaf)
  
  if (is.null(bestSplit)) {
    n_0 <- length(which(dataMatrix[, classColumnNumber] == 0))
    n_1 <- length(which(dataMatrix[, classColumnNumber] == 1))
    if (n_0 >= n_1) {
      return(0)
    }
    return(1)
  }
  
  bestNodeSelected          <- bestSplit[["predictorLabel"]]
  bestNodeSelectedCondition <- bestSplit[["predictorCondition"]]
  rowsFilter                <- which(dataMatrix[, bestNodeSelected] <= bestNodeSelectedCondition)
  
  leftDataMatrix            <- dataMatrix[rowsFilter, , drop = FALSE]
  leftTree                  <- treeGrowBuild(leftDataMatrix, classColumnNumber, nmin, minleaf, nfeat)
  rightDataMatrix           <- dataMatrix[-rowsFilter, , drop = FALSE]
  rightTree                 <- treeGrowBuild(rightDataMatrix, classColumnNumber, nmin, minleaf, nfeat)
  
  tree                      <- list()
  tree[["labelRoot"]]       <- bestSplit
  tree[["leftSubTree"]]     <- leftTree
  tree[["rightSubTree"]]    <- rightTree
  
  return(tree)
}

#tree.grow FUNCTION
#INPUT dataPredictors, y, nmin, minleaf,and nfeat
#FUNCTION bind the values of predictors its class value and calling treeGrowBuild to build the  classification tree
#OUTPUT   tree 

tree.grow <- function (dataPredictors,y, nmin, minleaf, nfeat){
  dataMatrix <- cbind(dataPredictors, y)
  return(treeGrowBuild(dataMatrix, ncol(dataMatrix), nmin, minleaf, nfeat))
}

#treeClassifyBuild FUNCTION
#INPUT sample, treeStructure
#FUNCTION finding the calss value of each predictor based on nodes and it conditions by tracking each nodes based on the predictors value until its reach a node which is pure (node leaf) which has numeric value 
#OUTPUT   predictors call values  

treeClassifyBuild <- function(sample, treeStructure) {
  
  if (class(treeStructure) == "numeric") {
    return(treeStructure)
  }
  
  branchValues       <- treeStructure[["labelRoot"]]
  predictorLabel     <- branchValues[["predictorLabel"]]
  predictorCondition <- branchValues[["predictorCondition"]]
  predictorValue     <- sample[ 1, predictorLabel]
  
  
  
  if (predictorValue <= predictorCondition) {
    tree <- treeStructure[["leftSubTree"]]
  } else {
    tree <- treeStructure[["rightSubTree"]]
  }
  return(treeClassifyBuild(sample, tree))
}

#tree.grow FUNCTION
#INPUT testingExamples, treeStructure
#FUNCTION taking testing examples and calling treeClassifyBuild which returns the predicted class values for each example 
#OUTPUT   predictionValues 

tree.classify <- function(testingExamples, treeStructure) {
  
  predictionValues <- c()
  
  for (example in 1:nrow(testingExamples)) {
    p                <- treeClassifyBuild(testingExamples[example, , drop = FALSE], treeStructure)
    predictionValues <- c(predictionValues, p)
  }
  
  return(predictionValues)
}

#tree.grow.bag FUNCTION
#INPUT x, y, nmin, minleaf, nfeat, m
#FUNCTION taking training examples and extract some samples randomly, then calling tree.grow function to build a tree from each sample
#OUTPUT   trees as a list() which saves m trees

tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m, ...) {
  
  trees <- list()
  
  for (i in 1:m) {
    randomData            <- sample(nrow(x), nrow(x), replace = TRUE)
    interimRandomExamples <- x[randomData, , drop = FALSE]
    interimClasses        <- y[randomData]
    trees[[i]]            <- tree.grow(interimRandomExamples, interimClasses, nmin, minleaf, nfeat)
  }
  
  return(trees)
}

#tree.classify.bag FUNCTION
#INPUT trees, x
#FUNCTION applying each row in the training example to each calssification tree in the trees list 
#OUTPUT   predictions 

tree.classify.bag <- function(trees, x) {
  
  predictions   <- c()
  numberOftrees <- length(trees)
  
  for (r in 1:nrow(x)) {
    interimExamples    <- x[r, , drop = FALSE]
    interimPredictions <- c()
    for (t in 1:numberOftrees) {
      interimTree        <- trees[[t]]
      n                  <- tree.classify(interimExamples, interimTree)
      interimPredictions <- c(interimPredictions, n)
    }
    
    n <- length(which(interimPredictions == 0))
    if (n >= (numberOftrees / 2)) {
      predictions <- c(predictions, 0)
    } else {
      predictions <- c(predictions, 1)
    }
  }
  
  return(predictions)
}



#part1 
datam2 <- read.csv("credit data.csv", header = TRUE, sep = ";")
n<- datam2[,-6]
m<- datam2[,6]
treeclass<- tree.grow(n,m,2,1,5)
tree.classify(n, treeclass)



#part2
