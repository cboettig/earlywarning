
rm(list=ls())
load("glaciation.rda")
rawdata <- data.frame(time=as.numeric(time(X)), state=X@.Data)
write.csv(glaciation, file="Figure4_PanelA.csv")
write.csv(rawdata, file="Figure4_PanelB.csv")
write.csv(lr, file="Figure4_PanelC.csv")
write.csv(roc, file="Figure5_glaciation.csv")


rm(list=ls())
load("simulation.rda")
rawdata <- data.frame(time=as.numeric(time(X)), state=X@.Data)
write.csv(ibm_critical, file="Figure2_PanelA.csv")
write.csv(rawdata, file="Figure2_PanelB.csv")
write.csv(lr, file="Figure2_PanelC.csv")
write.csv(roc, file="Figure5_simulation.csv")


rm(list=ls())
load("chemostat.rda")
write.csv(chemostat, file="Figure3_PanelA.csv")
write.csv(rawdata, file="Figure3_PanelB.csv")
write.csv(lr, file="Figure3_PanelC.csv")
write.csv(roc, file="Figure5_chemostat.csv")

