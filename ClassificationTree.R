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
  predictions <- c()
  num_trees <- length(trees)
  
  for (r in 1:nrow(x)) {
    row <- x[r, , drop = FALSE]
    tmp_predictions <- c()
    for (t in 1:num_trees) {
      current_tree <- trees[[t]]
      p <- tree.classify(row, current_tree)
      tmp_predictions <- c(tmp_predictions, p)
    }
    
    n <- length(which(tmp_predictions == 0))
    if (n >= (num_trees / 2)) {
      predictions <- c(predictions, 0)
    } else {
      predictions <- c(predictions, 1)
    }
  }
  return(predictions)
}


test_tree <- function(tree){
  
  
  eclipse_testing_data <- read.csv("eclipse-metrics-packages-3.0.csv", header = TRUE, sep = ";")
  
  eclipse_testing_data[,4] <- as.numeric(eclipse_testing_data[,4] > 0)
  eclipse_testing_data[,4] 
  
  eeclipse_testingN <- eclipse_testing_data[, c(3, 5:44)]
  eclipse_testingM <- eclipse_testing_data[, 4]
  
  classification_result <-tree.classify(eeclipse_testingN, tree)
  
  
  
  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  u_p =classification_result
  eclipse_testingM
  
  
  for ( i in 1:length(classification_result) ) {
    if ( (classification_result[i] == 1) && (eclipse_testingM[i] == 1) )
      TP <- TP + 1
      
    if ( (classification_result[i] == 0) && (eclipse_testingM[i] == 0) ) 
        TN <- TN + 1
    
    if ( (classification_result[i] == 0) && (eclipse_testingM[i] == 1) ) 
      FN <- FN + 1
    
    if ( (classification_result[i] == 1) && (eclipse_testingM[i] == 0) ) 
      FP <- FP + 1
  }
  
  
  
  precision <-(TP/(TP + FP))
    
  accuracy<- (TP + TN)/(TP + TN + FP + FN)
  
  recall <- (TP/(TP + FN))
  
  print("Precision : ")  
print( precision)
  print("Accuracy : ")
  print(  accuracy)
  print("Recall : ")
  print(recall)
  return(c(TP+TN, FP+FN))  
}


test_forest <- function(forest){
  
  
  eclipse_testing_data <- read.csv("eclipse-metrics-packages-3.0.csv", header = TRUE, sep = ";")
  
  eclipse_testing_data[,4] <- as.numeric(eclipse_testing_data[,4] > 0)
  eclipse_testing_data[,4] 
  eclipse_testingN <- eclipse_testing_data[, c(3, 5:44)]
  eclipse_testingM <- eclipse_testing_data[, 4]
  
 # print(tree)
  
  classification_result <-  tree.classify.bag( forest,eclipse_testingN)
  
 # print(eclipse_testingM)

  TP <- 0
  TN <- 0
  FP <- 0
  FN <- 0
  #u_p =classification_result
  #eclipse_testingM
  
  
  for ( i in 1:length(classification_result) ) {
    if ( (classification_result[i] == 1) && (eclipse_testingM[i] == 1) )
      TP <- TP + 1
    
    if ( (classification_result[i] == 0) && (eclipse_testingM[i] == 0) ) 
      TN <- TN + 1
    
    if ( (classification_result[i] == 0) && (eclipse_testingM[i] == 1) ) 
      FN <- FN + 1
    
    if ( (classification_result[i] == 1) && (eclipse_testingM[i] == 0) ) 
      FP <- FP + 1
  }
  
  
  
  precision <-(TP/(TP + FP))
  
  accuracy<- (TP + TN)/(TP + TN + FP + FN)
  
  recall <- (TP/(TP + FN))
  
  print("Precision : ")  
  print( precision)
  print("Accuracy : ")
  print(  accuracy)
  print("Recall : ")
  print(recall)
  return(c(TP+TN, FP+FN))  
}


mcnemar <- function(classifier1,classifier2 ){
  
  classifier1_correct = classifier1[1]
  classifier1_wrong = classifier1[2]
  
  classifier2_correct = classifier2[1]
  classifier2_wrong = classifier2[2]
    
  a= classifier1_correct +classifier2_correct
  b= classifier1_correct +classifier2_wrong
  
  c= classifier1_wrong +classifier2_correct
  d= classifier1_wrong +classifier2_wrong
  
  score=((b -c )* (b -c )) / (b+c)
  print (score)
}


#part1 
datam2 <- read.csv("credit data.csv", header = TRUE, sep = ";")
n<- datam2[,-6]
m<- datam2[,6]
treeclass<- tree.grow(n,m,2,1,5)
tree.classify(n, treeclass)




#part2

eclipse_training_data <- read.csv("eclipse-metrics-packages-2.0.csv", header = TRUE, sep = ";")
eclipse_training_data[,4] <- as.numeric(eclipse_training_data[,4] > 0)
eclipse_training_data[,4] 
eclipse_trainingN <- eclipse_training_data[, c(3, 5:44)]
eclipse_trainingM <- eclipse_training_data[, 4]




#single_Tree
print("Single tree")
single_tree_eclipse <- tree.grow(eclipse_trainingN, eclipse_trainingM,15,5,41)
classification_numbers_tree = test_tree(single_tree_eclipse)

#bagging
print("Bagging")
bagging_tree_eclipse <- tree.grow.bag(eclipse_trainingN, eclipse_trainingM,15,5,41,100)
classification_numbers_bagging = test_forest(bagging_tree_eclipse)


#Random Forest
print("Random Forest")
forest_eclipse <- tree.grow.bag(eclipse_trainingN, eclipse_trainingM,15,5,6,100)
classification_numbers_forest = test_forest(forest_eclipse)


mcnemar(classification_numbers_tree,classification_numbers_bagging)



mcnemar(classification_numbers_bagging,classification_numbers_forest)


mcnemar(classification_numbers_forest,classification_numbers_tree)






