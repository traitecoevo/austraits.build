data1 <- read.csv("~/austraits/data/Catford_2014/data.csv")
data2 <- read.csv("~/austraits/data/Catford_2014/data_leaves_to_merge.csv")
data2a <- subset(data2,data2$Leaf.part!="sheath") %>%
    select(c("Taxa","Sample","Mass.per.leaf..g.")) %>%
    group_by(Taxa) %>%
    summarise(Mass.per.leaf..g.=mean(Mass.per.leaf..g.))
data_x <- merge(data1,data2a,by="Taxa",all.x=TRUE)

data2b <- subset(data2,data2$Leaf.part!="sheath") %>%
  select(c("Taxa","Sample","Area.per.leaf..cm.2.","SLA..mm2.mg.","Mass.per.leaf..g.")) %>%
  group_by(Taxa) %>%
  summarise_at(c("Area.per.leaf..cm.2.","SLA..mm2.mg.","Mass.per.leaf..g."),.funs=mean)
names(data2b) <- c("Taxa","Leaf.size..cm.2.","SLA..mm2.mg.","Mass.per.leaf..g.")

data1b <- select(data_x,c("Taxa","Leaf.size..cm.2.","SLA..mm2.mg.","Mass.per.leaf..g."))

data2c <- merge(data2b,data1b,by="Taxa",all.y=FALSE, all.x=TRUE)
data2d <- subset(data2c,is.na(data2c[,5]))
data2d <- select(data2d,c("Taxa","Leaf.size..cm.2..x","SLA..mm2.mg..x","Mass.per.leaf..g..x"))
names(data2d) <- c("Taxa","Leaf.size..cm.2.","SLA..mm2.mg.","Mass.per.leaf..g.")

data_x<-bind_rows(data_x,data2d)
data_x<-subset(data_x,data_x$Taxa!="")

write.csv(data,"~/austraits/data/Catford_2014_2/data.csv",row.names=FALSE)

