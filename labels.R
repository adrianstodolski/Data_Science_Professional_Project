#----------------------------- LABELS -------------------#

# 1. Adding labels to factor.
MyLabels <- function(text){
  
  return(factor(text, levels = c(101, 
                                 102, 
                                 103, 
                                 104, 
                                 105, 
                                 106, 
                                 107, 
                                 108,
                                 109,
                                 110
  ),
  labels = c("Arnika",
             "Babka",
             "Chaber",
             "Dzwonek",
             "Fiołek",
             "Goździk",
             "Jaskier",
             "Łubin",
             "Mięta",
             "Narcyz"
  )
  ))
}