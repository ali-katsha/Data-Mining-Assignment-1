#Utrecht University, Data Mining. 
#Assignment 1.
#Implemented by:
#Ahmad Alassaf (6557864)
#Ali Katsheh (6661548)
#Mahmud Jabri (6356672)


#Function 1: tree.grow()
#Input :     (x::Data matrix of attribute values, y::class values vector, nmin::Int, minleaf::Int,nfeat::Int)  
#Output:     tree 
#This function binds the values of predictors with its class value and calling treeGrowBuild() to build the classification tree
tree.grow <- function (x,y, nmin, minleaf, nfeat){
  dataMatrix <- cbind(x, y)
  return(treeGrowBuild(dataMatrix, ncol(dataMatrix), nmin, minleaf, nfeat))
}#End tree.grow()


#Function 2: tree.classify()
#Input :  (x::atrributes matrix, tr::tree object)
#Output:  predictionValues 
#This function takes testing examples and calling treeClassifyBuild which returns the predicted class values for each example 
tree.classify <- function(x, tr) {
  
  predictionValues <- c()
  
  for (example in 1:nrow(x)) {
    ##call treeClassifyBuild on a (row with predictorLabel,and tree)
    p                <- treeClassifyBuild(x[example, , drop = FALSE], tr)
    print("P=")
    print(p)
    predictionValues <- c(predictionValues, p)
    
  }
  
  return(predictionValues)
}#End tree.classify()


#Function 3: tree.grow.bag()
#Input  :    (x::Data matrix of attribute values, y::class value vector, nmin::Int, minleaf::Int, nfeat::Int, m)
#Output :    returns a list of trees
#FUNCTION taking training examples and extract some samples randomly, then calling tree.grow function to build a tree from each sample
tree.grow.bag <- function(x, y, nmin, minleaf, nfeat, m, ...) {
  
  trees <- list()
  
  for (i in 1:m) {
    randomData            <- sample(nrow(x), nrow(x), replace = TRUE)
    interimRandomExamples <- x[randomData, , drop = FALSE]
    interimClasses        <- y[randomData]
    trees[[i]]            <- tree.grow(interimRandomExamples, interimClasses, nmin, minleaf, nfeat)
  }
  
  return(trees)
}#End tree.grow.bag()



#Function 4: tree.classify.bag()
#Input  :    (trees::List of trees, x::Data matrix of attributes values)
#Output :    predicted class values 
#Function applying each row in the training example to each calssification tree in the trees list 
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
}#End tree.classify.bag()




######################################################################################  Auxiliary Functions  ##############################################

#Function :treeGrowBuild() 
#Input (dataMatrix::Data matrix, classColumnNumber::Int, nmin::Int, minleaf::Int,nfeat::Int)
#FUNCTION after selecting the best predictor in function splitNode we splitting the data matrix based on two classes (left and right) of that predictor and we apply the same procedures recursively to the right subtree and left subtree untill the nodes becomes pure (leaves)
#Output   tree as list() which stores predictor labelRoot left subTree and right subTree

treeGrowBuild <- function(dataMatrix, classColumnNumber, nmin, minleaf, nfeat) 
{
  
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
  
  #?
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
} # End function treeGrowBuild()





#Function : splitNode()
#Input: (interimDataMatrix::data Matrix,interimClassColumnNumber:Int,nmin::Int,minleaf::Int)
#Output: dataTree as a list() which keeps predictorLabel and its condition.
#By calling bestSplitNode() for every predictor vector and keeping the lawest impurity returned. This function will return the best node (represented as list of label name and condition value) to split the training data at.
splitNode <- function(interimDataMatrix, interimClassColumnNumber, nmin, minleaf) {
  # contains the class values
  calssValues      <- interimDataMatrix[, interimClassColumnNumber] 
  lengthGroup      <- length(calssValues)
  lengthGroupZeros <- length(which(calssValues == 0))
  p                <- lengthGroupZeros / lengthGroup
  giniOfClass      <- 1-(p)^2-(1-p)^2
  
  if ((nrow(interimDataMatrix) < nmin) || (giniOfClass == 0))
  {
    return(NULL)
  }
  # get the name of the predictors and put it in a victor
  predictorsNames    <- names(interimDataMatrix[,-interimClassColumnNumber])
  classColumnValue   <- interimDataMatrix[, interimClassColumnNumber]
  nodeToSplit        <- NULL # to be used later
  predictorLabel     <- NULL # to be used later
  predictorCondition <- NULL# to be used later
  
  for (index in 1:length(predictorsNames)) {
    
    predictorName        <- predictorsNames[index]
    predictorValues      <- interimDataMatrix[, predictorName]
    interimBestSplit     <- bestSplitNode(predictorValues, classColumnValue, minleaf) # we are passing P vector, class label, and minleaf
    
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
  
}#End function splitNode()




#Function :bestSplitNode() 
#Input (predictorvalues::Vector, classValues:Vector, minleaf::Int) 
#Output bestNodeToSplit as a list() has NodeImpurity and NodeCondition
#This calculates the gini-index for each predicor based on the class values 
#this function will be called for each pair of predictor-vector & class vector 
bestSplitNode <- function(predictorValue, classValues, minleaf) 
{
  
  sortedPredictorValue <- sort(unique(predictorValue))
  NodeImpurity <- NULL
  NodeCondition <- NULL
  
  for (index in 1:(length(sortedPredictorValue))) {
    
    conditionOfNode     <- (sortedPredictorValue[index] + sortedPredictorValue[index + 1]) / 2
    leftNodeItems       <- classValues[which(predictorValue <= conditionOfNode)]
    rightNodeItems      <- classValues[which(predictorValue > conditionOfNode)]
    leftNodeItemsNum    <- length(leftNodeItems)
    rightNodeItemsNum   <- length(rightNodeItems)
    
    if ((leftNodeItemsNum < minleaf) || (rightNodeItemsNum < minleaf)) {
      next
    }
    
    # gini index for each child node(impurity) 
    leftNodeClassOneProbability  <- length(which(leftNodeItems == 0))/leftNodeItemsNum
    leftNodeClassTwoProbability  <- length(which(leftNodeItems == 1))/leftNodeItemsNum
    rightNodeClassOneProbability <- length(which(rightNodeItems == 0))/rightNodeItemsNum
    rightNodeClassTwoProbability <- length(which(rightNodeItems == 1))/rightNodeItemsNum
    
    giniLeftNode <-  1-(leftNodeClassOneProbability)^2-(leftNodeClassTwoProbability)^2
    giniRightNode <-  1-(rightNodeClassOneProbability)^2-(rightNodeClassTwoProbability)^2
    WeightedGiniNode <- (leftNodeItemsNum * giniLeftNode + rightNodeItemsNum * giniRightNode)/(leftNodeItemsNum+rightNodeItemsNum)
    
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
}#End function bestSplitNode()





#Function : treeClassifyBuild()
#Input:  (sample::row of training data, tr::tree object)
#Output   predictors call values  
#This function will assign a class to a data row, by evaluating the values of the data row, with the values of nodes conditions starting from the root node, then repeating this process recursively until it reaches a nummeric node, which means it reached the class.  
treeClassifyBuild <- function(sample, tr) {
  
  if (class(tr) == "numeric") {
    
    return(tr)
  }
  
  branchValues       <- tr[["labelRoot"]]
  predictorLabel     <- branchValues[["predictorLabel"]]
  predictorCondition <- branchValues[["predictorCondition"]]
  predictorValue     <- sample[ 1, predictorLabel]

  
  if (predictorValue <= predictorCondition) {
    tree <- tr[["leftSubTree"]]
  } else {
    tree <- tr[["rightSubTree"]]
  }
  return(treeClassifyBuild(sample, tree))
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
  
  
  print("True Positive")
  print(TP)
  print("False Positive")
  print(FP)
  print("True Negative")
  print(TN)
  print("False Negative")
  print(FN)
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
  
  print("True Positive")
  print(TP)
  print("False Positive")
  print(FP)
  print("True Negative")
  print(TN)
  print("False Negative")
  print(FN)
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
##################################Right side is bigger >, left smaller  
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






