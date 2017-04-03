#########################################################
#YASIF2MATRIX
#Yet Another SIF-to-weightmatrix script
#takes a SIF file with a weight column
#returns a weight matrix
#optimized for large, non-sparse graphs
#########################################################

#########################################################
#libraries
#########################################################
library(data.table)
library(parallel)

#########################################################
#Main function
#########################################################

sif_to_matrix <-function(sif, type = 1, out, cores = 2){
  dt = fread(sif)
  dt = ifelse(type == 2, dt[,c(1,3,2)], dt) #force middle column to contain weights
  
  #helper function
  node_weight_row<- function(node, dt, targets){
    zeros = rep(x = 0, length(targets))
    zeros[match(dt[V1 %in% node]$V3, targets)] <- dt[V1 %in% node]$V2
    zeros[match(dt[V3 %in% node]$V1, targets)] <- dt[V3 %in% node]$V2 #forces simmetry
    zeros = data.table(t(zeros))
    return(zeros)
  }
  
  genes = unique(c(unique(dt$V1), unique(dt$V3)))

  lista = mclapply(X = genes, FUN = node_weight_row, mc.cores = cores, dt = dt, targets = genes)
  names(lista) <- genes
  matriz = rbindlist(lista, idcol = TRUE)
  colnames(matriz)<-c(" ", genes)
  fwrite(x = matriz, file = out, quote = FALSE, sep = "\t", col.names = TRUE)  
}

