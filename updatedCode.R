getAlgoWeights <- function(sliderWeight){
  algoWeightMap <- data.frame(sliderWeight=-5:5,algoWeight=c(-10^6,-100,-10,-1,-0.1,0,0.1,1,10,100,1000))
  tmp <- data.frame(sliderWeight=sliderWeight) %>% inner_join(algoWeightMap,by="sliderWeight")
  weight <- tmp$algoWeight
  names(weight) <- names(sliderWeight)
  return(weight)
}