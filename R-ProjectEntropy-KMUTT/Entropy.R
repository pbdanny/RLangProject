#dat <- read.table("agaricus-lepiota.data.txt", sep = ",", stringsAsFactors = FALSE, na.strings = "?")

#names(dat) <- c("classes","cap-shape","cap-surface","cap-color","bruises?",
#                "odor","gill-attachment","gill-spacing","gill-size","gill-color",
#                "stalk-shape","stalk-root","stalk-surface-above-ring",
#                "stalk-surface-below-ring","stalk-color-above-ring",
#                "stalk-color-below-ring","veil-type","veil-color","ring-number",
#                "ring-type","spore-print-color","population","habitat")

weightedEntro <- function(li = data.frame(), target = character(), class, par.n){
  # prop = no. of observation with desire target attribute / no. of all observation
  # find no. of observation with desired target by create logical vector 
  # and test if each element == desire target attribute

  # prop = no. of desired value (class) in target column / all observation
  prop <- (sum(li[[target]] == class) / length(li[[target]]))
  # Entropy = ( prop * log2 of prob ) + (1-prop) * log 2 of (1-prop)
  entropy <- (-(prop*log2(prop)+(1-prop)*log2(1-prop)))
  # wighted = entropy * lenght of child / lenght of parent
  
  weighted.entropy <- ((length(li[[target]])/par.n)*entropy)
  return(weighted.entropy)
}


# Main function
# induction = data frame with target attribute in one colunm
# target = name of column target attribute 
# class = desired value in the target attribute
# return dataframe of attributes and IG

RankEntro <- function(induction = data.frame(), target = character(), class){
  
  # define output data structure
  df.out <- data.frame(Attrib = character(), Entropy = numeric(), stringsAsFactors = FALSE)
  
  # Store nrow of induction in par.n for weighted calculation
  par.n <- nrow(induction)
  
  # Calculate parent entropy
  parent.entropy <- weightedEntro(induction, target, class, par.n)
  
  # The most outer for-loop traverse all attribute except the first on which fixed as target attribute
  for (i in seq_along(induction)) {
    
    # Skip if names of column = name of target index
    if(names(induction[i]) == target) next 
    else {
      
      # subset only target column and column i
      # Split data by column i value
      induction.subset <- induction[,c(target, names(induction[i]))]
      # Save colnames for data.split
      Attrib <- names(induction[i])
      data.split <- split(induction.subset, induction.subset[[Attrib]])
      
      # Calculate entropy of data.split
      childEntropy <- sapply(data.split, weightedEntro , target=target, class=class, par.n = par.n )
      
      # IG = parent entropy - sum of each child Entroyp
      IG <- parent.entropy - sum(childEntropy, na.rm = TRUE)
    } # End of 1 child attribute entropy calculation
    
    # append attritbuted names and IG to df.out also Nulified row.names
    append.row <- data.frame(Attrib, IG, stringsAsFactors = FALSE)
    df.out <- rbind(df.out, append.row)
  } # Next child attribute entropy calcualtion
  
  # Order output with descendgin entropy 
  df.out <- df.out[order(df.out$IG, decreasing = TRUE),]
  # Reset rownames
  row.names(df.out) <- NULL
  return(df.out)
}