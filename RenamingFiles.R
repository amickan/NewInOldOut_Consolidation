### Renaming pretest files

A = c(766:767)

### for each participant get subset of pretest trials that need RT coding
for (i in 1:length(A)){
  pNumber <- A[i]
  new.parent <- paste("//cnas.ru.nl/wrkgrp/STD-EXP_5_Katya/BACK-UP/", pNumber,"/",pNumber,"_Pre-test/", sep="")
  
  setwd(new.parent)
  ppfiles <- list.files(new.parent, pattern = ".wav")
  
  for (k in 1:length(list.files(new.parent, pattern = ".wav"))){
        trial <- gsub("PP\\d+.?_Pretest_Trial(\\d+)-001.wav","\\1", ppfiles[k])
        newname <- paste("PP",pNumber,"_Pretest_Trial", trial, "-001.wav", sep = "")
        file.rename(ppfiles[k], newname)
  }

  print(pNumber)
}

