dbrecap <- as.data.frame(read.csv("C://Data/PhD/Rawdata/Dungbeetles/dungbeetlerecaptures.csv"))
names(dbrecap) <- c("Distance","NumberRecaptures","Proportion")
head(dbrecap)
head(dbrecap)
png(file="C://Data/PhD/Outputs/Dungbeetles/dbrecaptures.png",width=8,height=7,units="in",res=200,pointsize=18)
barplot(dbrecap$NumberRecaptures, xlab=("Release distance (m)"), names.arg=c("0", "12.5", "25","50","75"),
        cex.names=1.2,cex.axis=1.2,cex.lab=1.5,ylab=("Number of beetles recaptured"),ylim=c(0,25),col="royalblue")
dev.off()


