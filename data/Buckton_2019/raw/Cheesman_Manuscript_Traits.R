require(plyr)
require(ggplot2)
require(cowplot)
require(readr)

#pull in data
Licor_ForR<-readr::read_csv("C:/Users/jc268753/OneDrive - James Cook University/Plant_Ecophys_Lab/Buckton/Managed Data/Cheesman_reanalysis/Licor_ForR.csv")
LeafData<-readr::read_csv("C:/Users/jc268753/OneDrive - James Cook University/Plant_Ecophys_Lab/Buckton/Managed Data/Cheesman_reanalysis/LeafData.csv")
StemData<-readr::read_csv("C:/Users/jc268753/OneDrive - James Cook University/Plant_Ecophys_Lab/Buckton/Managed Data/Cheesman_reanalysis/StemData.csv")
PhotoComb<-readr::read_csv("C:/Users/jc268753/OneDrive - James Cook University/Plant_Ecophys_Lab/Buckton/Managed Data/Cheesman_reanalysis/Photo_Combined.csv")


#G data
#Stem_Licor_ForR_Names<-readr::read_csv("C:/Users/chees/OneDrive - James Cook University/Plant_Ecophys_Lab/Buckton/Managed Data/Cheesman_reanalysis/Stem_Licor_ForR_Names.csv")

#summarize Licor Data
colnames(Licor_ForR)

#CLEAN UP LICOR DATA TO REMOVE EXISTING EXTRANEOUS DATA
Licor<-Licor_ForR[!(Licor_ForR$Tree.code %in% c("NEO_4","ROC_1","MUC_1","MYR_3","UNK K_1","UNK L_1","AGY_3","AGY_5")), ]

#CLEAN UP LICOR DATA TO REMOVE IMPOSSIBLE CONDUCTANCE: ISSUE WITH NOT HAVING ALLOWED FOR 
#STABLE CONDUCTANCE TO BE ACHIEVED, RATHER CONCENTRATING ON PHOTOSYNTHESIS
Licor<-subset(Licor,Ci_Ca >=0.4 & Ci_Ca<=0.95 )
Licor$WUEi<-Licor$Photo/Licor$Cond
  
  
head(Licor)

summary.Licor<- ddply(Licor, .(Species,Tree), summarise, 
                Growth.Form=unique(Growth.Form),
                number=length(Leaf),
                Aa=mean(Photo,na.rm=TRUE),
                gs=mean(Cond,na.rm=TRUE),
                WUEi=mean(WUEi,na.rm=TRUE),
                CiCa=mean(Ci_Ca,na.rm=TRUE))

View(summary.Licor)
#use summary.Licor or combined to calcuylate Amass


summary.Licor.2<- ddply(PhotoComb, .(Species), summarise, 
                Growth.Form=unique(Growth.Form),
                Trees=length(Tree),
                Aa=mean(Aa,na.rm=TRUE),
                gs=mean(gs,na.rm=TRUE),
                WUEi=mean(WUEi,na.rm=TRUE),
                CiCa=mean(CiCa,na.rm=TRUE),
                LMA=mean(LMA,na.rm=TRUE),
                LDMC=mean(LDMC,na.rm=TRUE),
                LS=mean(LS,na.rm=TRUE),
                Lamina=mean(Lamina,na.rm=TRUE),
                Amass=mean(Amass,na.rm=TRUE))

####################
#summarize stem data

Stem<-StemData[!(StemData$Species %in% c("CAY","MUC","UNK D","UNK E","UNK K","UNK L","PAR")), ]

summary.Stem<- ddply(Stem, .(Species), summarise, 
                      Growth.Form=unique(Growth.Form),
                      #number=length(Density,na.rm=TRUE),
                      Density=mean(Density,na.rm=TRUE),
                      S.13C =mean(S_true13C,na.rm=TRUE),
                      S.per.C = mean(S_per.C,na.rm=TRUE),
                      S.15N =mean(S_true15N,na.rm=TRUE),
                      S.per.N = mean(S_per.N,na.rm=TRUE ),
                      L.13C =mean(L_true13C,na.rm=TRUE),
                      L.per.C = mean(L_per.C,na.rm=TRUE),
                      L.15N =mean(L_true15N,na.rm=TRUE),
                      L.per.N = mean(L_per.N,na.rm=TRUE ),
                      L_C.N = mean(L_C.N,na.rm=TRUE),
                      X_Ratio =mean( X_Tot_Ratio,na.rm=TRUE) )

###############################
#summarize leaf data
Leaf<-LeafData[!(LeafData$Species %in% c("PAR","MUC","UNK K", "UNK L","CAY")),]
         

summary.Leaf<- ddply(Leaf, .(Species,Tree), summarise, 
                     Growth.Form=unique(Growth.Form),
                     number=length(LMA),
                     LMA=mean(LMA,na.rm=TRUE),
                     LDMC =mean(LDMC,na.rm=TRUE),
                     LS = mean(AreaTotal,na.rm=TRUE),
                     Lamina=mean(Lamina,na.rm=TRUE))


summary.Leaf.2<- ddply(summary.Leaf, .(Species), summarise, 
                     Growth.Form=unique(Growth.Form),
                     number=length(LMA),
                     LMA=mean(LMA,na.rm=TRUE),
                     LDMC =mean(LDMC,na.rm=TRUE),
                     LS = mean(LS,na.rm=TRUE),
                     Lamina=mean(Lamina,na.rm=TRUE))

#################################################################################
#Analysis of variance using species as a random factor nested within growth form#
#################################################################################

#species mean averaged
#require(nlme)
#LMA_Model<-aov(LMA~Growth.Form,data=wrk)
#summary(LMA_Model)

#lme(data=wrk.2, LMA~Growth.Form,random=~1|Species,method="REML")


#LMA nested design species within Growth form
LMA_Model.2<-aov(LMA~Growth.Form+Growth.Form:Species,data=summary.Leaf)
summary(LMA_Model.2)
LMA.F.value=36270/2643
LMA.F.value
LMA.pvalue=1-pf(LMA.F.value,1,16)
LMA.pvalue

#LDMC nested design species within Growth form
LDMC_Model.2<-aov(LDMC~Growth.Form+Growth.Form:Species,data=summary.Leaf)
summary(LDMC_Model.2)
LDMC.F.value=0.19060/0.02114
LDMC.F.value
LDMC.pvalue=1-pf(LDMC.F.value,1,16)
LDMC.pvalue

#LS nested design species within Growth form
LS_Model.2<-aov(LS~Growth.Form+Growth.Form:Species,data=summary.Leaf)
summary(LS_Model.2)
LS.F.value=83394/50955
LS.F.value
LS.pvalue=1-pf(LS.F.value,1,16)
LS.pvalue


#LS nested design species within Growth form
Lamina_Model.2<-aov(Lamina~Growth.Form+Growth.Form:Species,data=summary.Leaf)
summary(Lamina_Model.2)
Lamina.F.value=0.00539/0.01208
Lamina.F.value
Lamina.pvalue=1-pf(Lamina.F.value,1,16)
Lamina.pvalue

#STEM characteristics

#WD nested design species within Growth form

head(Stem)
WD_Model.2<-aov(Density~Growth.Form+Growth.Form:Species,data=subset(Stem,Species!="CAL"& Species!="NOR"))

summary(WD_Model.2)
WD.F.value=0.13464/0.02147
WD.F.value
WD.pvalue=1-pf(WD.F.value,1,14)
WD.pvalue

#Xylem ratio nested design species within Growth form
XRatio_Model.2<-aov(X_Tot_Ratio~Growth.Form+Growth.Form:Species,data=Stem)

summary(XRatio_Model.2)
XRatio.F.value=0.01595/0.07837
XRatio.F.value
XRatio.pvalue=1-pf(XRatio.F.value,1,16)
XRatio.pvalue

########################################
#Plotting for Figure 4W#################
########################################

#wood density 4a
Fig4a<-ggplot(data=subset(summary.Stem,Species!="CAL"& Species!="NOR"))+
  geom_boxplot(aes(x=Growth.Form,y= Density,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("Stem density (g ",cm^-3,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#LMA 4b
Fig4b<-ggplot(data = summary.Leaf.2)+
  geom_boxplot(aes(x=Growth.Form,y= LMA,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("LMA (g ",m^-2,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#LDMC 4c
Fig4c<-ggplot(data = summary.Leaf.2)+
  geom_boxplot(aes(x=Growth.Form,y= LDMC,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("LDMC (g ",g^-1,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#LS 4d
Fig4d<-ggplot(data = summary.Leaf.2)+
  geom_boxplot(aes(x=Growth.Form,y= LS,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("Leaf size (",cm^2,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#Lamina 4e
Fig4e<-ggplot(data = summary.Leaf.2)+
  geom_boxplot(aes(x=Growth.Form,y= Lamina,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("Lamina thickness (mm)",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    #axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#Lamina 4f


Fig4f<-ggplot(data = summary.Stem)+
  geom_boxplot(aes(x=Growth.Form,y= X_Ratio,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("Xylem: Total stem area",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    #axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#multi plot using cowplot


plot_grid(Fig4a, Fig4b, Fig4c, Fig4d,Fig4e,Fig4f, 
          align='vh', 
          ncol=2)
#export as 750 height 550 width         ,

####################################
####################################
#Figure 5
##
Manus<-readr::read_csv("C:/Users/jc268753/OneDrive - James Cook University/Plant_Ecophys_Lab/Buckton/Managed Data/Cheesman_reanalysis/Manuscript_Table.csv")


Licor<-summary.Licor.2[complete.cases(summary.Licor.2), ]

#Photosynthesis by area
Fig5a<-ggplot(data =Manus)+
  geom_boxplot(aes(x=Growth.Form,y= Aa,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("A"[a], " (",mu,"mol ",m^-2," ", s^-1,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#Photosynthesis by mass


Fig5b<-ggplot(data = Manus)+
  geom_boxplot(aes(x=Growth.Form,y= Amass,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("A"[mass], " (nmol ",g^-1," ", s^-1,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

Fig5c<-ggplot(data = Manus)+
  geom_boxplot(aes(x=Growth.Form,y= WUEi,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("WUE"[i],"(",mu,"mol CO"[2],mol^-1," H"[2],"O)",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    #axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

Fig5d<-ggplot(data = Manus)+
  geom_boxplot(aes(x=Growth.Form,y= PNUE,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("PNUE"," (",mu,"mol CO"[2]," ",mol^-1,"N ",s^-1,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    #axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )


#multi plot using cowplot


plot_grid(Fig5a, Fig5b, Fig5c, Fig5d,
          align='vh', 
          ncol=2)
#export as 550 height 550 width    

#########################################
#######Figure 6##########################
#########################################
summary.Stem
#leaf delta 13C
Fig6a<-ggplot(data =summary.Stem)+
  geom_boxplot(aes(x=Growth.Form,y= L.13C,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  scale_y_continuous(limits=c(-32,-27))+
  theme_bw()+
  ylab(expression(paste(delta^13,"C "[Leaf], " (???)",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#stem delta 13C

Fig6b<-ggplot(data =summary.Stem)+
  geom_boxplot(aes(x=Growth.Form,y= S.13C,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste(delta^13,"C "[Stem], " (???)",sep="")))+
  scale_y_continuous(limits=c(-32,-27))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#Leaf C:N ratio
Fig6c<-ggplot(data =summary.Stem)+
  geom_boxplot(aes(x=Growth.Form,y= L_C.N,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("C:N ratio",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    #axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#Leaf N mass
Fig6d<-ggplot(data =Manus)+
  geom_boxplot(aes(x=Growth.Form,y= Nmass,fill=Growth.Form))+
  scale_fill_manual(values=c("grey55","lightgrey"))+
  theme_bw()+
  ylab(expression(paste("N"[mass]," (mg ",g^-1,")",sep="")))+
  theme(
    aspect.ratio=2,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title.x=element_blank(),
    axis.title=element_text(size=16),
    axis.ticks=element_line(size=2),
    #axis.text.x=element_blank(),
    panel.grid=element_blank(),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

#multi plot using cowplot


plot_grid(Fig6a, Fig6b, Fig6c, Fig6d,
          align='vh', 
          ncol=2)
#export as 550 height 550 width    


######################################
######################################
#Photosynthesis nested design 
Photo.model<-aov(Aa~Growth.Form+Growth.Form:Species,data=summary.Licor)
summary(Photo.model)
#check Photo.model<-aov(Aa~Growth.Form+Growth.Form:Species,data=PhotoComb)
Photo.F.value=15.405/17.199
Photo.F.value
Photo.pvalue=1-pf(Photo.F.value,1,16)
Photo.pvalue


#Photosynthesis Amass nested
Photo.mass.model<-aov(Amass~Growth.Form+Growth.Form:Species,data=PhotoComb)
summary(Photo.mass.model)
Photo.mass.F.value=87567/8801
Photo.mass.F.value
Photo.mass.pvalue=1-pf(Photo.mass.F.value,1,16)
Photo.mass.pvalue

#gs nested
#gs.model<-aov(gs~Growth.Form+Growth.Form:Species,data=PhotoComb)
#summary(gs.model)
#gs.F.value=0.000809/0.013578
#gs.F.value
#gs.pvalue=1-pf(gs.F.value,1,16)
#gs.pvalue

#gs nested altenrate table
gs.model<-aov(gs~Growth.Form+Growth.Form:Species,data=summary.Licor)
summary(gs.model)
gs.F.value=0.000809/0.013578
gs.F.value
gs.pvalue=1-pf(gs.F.value,1,16)
gs.pvalue

#WUEi nested altenrate table
WUEi.model<-aov(WUEi~Growth.Form+Growth.Form:Species,data=summary.Licor)
summary(WUEi.model)
WUEi.F.value=987.9/769.9
WUEi.F.value
WUEi.pvalue=1-pf(WUEi.F.value,1,16)
WUEi.pvalue

#PNUE
PNUE.model<-aov(PNUE~Growth.Form+Growth.Form:Species,data=PhotoComb)
summary(PNUE.model)
PNUE.F.value=716.7/2386.9
PNUE.F.value
PNUE.pvalue=1-pf(PNUE.F.value,1,16)
PNUE.pvalue

#######################################
head(StemData)
#Ld13C
Ld13C.model<-aov(L_true13C~Growth.Form+Growth.Form:Species,data=StemData)
summary(Ld13C.model)
Ld13C.F.value=4.255/6.857
Ld13C.F.value
Ld13C.pvalue=1-pf(Ld13C.F.value,1,16)
Ld13C.pvalue

#Sd13C
Sd13C.model<-aov(S_true13C~Growth.Form+Growth.Form:Species,data=StemData)
summary(Sd13C.model)
Sd13C.F.value=0.259/5.356
Sd13C.F.value
Sd13C.pvalue=1-pf(Sd13C.F.value,1,16)
Sd13C.pvalue

head(StemData)
#Ld15N
Ld15N.model<-aov(L_true15N~Growth.Form+Growth.Form:Species,data=StemData)
summary(Ld15N.model)
Ld15N.F.value=15.225/9.831
Ld15N.F.value
Ld15N.pvalue=1-pf(Ld15N.F.value,1,16)
Ld15N.pvalue

#Sd15N
Sd15N.model<-aov(S_true15N~Growth.Form+Growth.Form:Species,data=StemData)
summary(Sd15N.model)
Sd15N.F.value=29.106/8.920
Sd15N.F.value
Sd15N.pvalue=1-pf(Sd15N.F.value,1,16)
Sd15N.pvalue

#LNmass

L_per.N.model<-aov(L_per.N~Growth.Form+Growth.Form:Species,data=StemData)
summary(L_per.N.model)
L_per.N.F.value=4.674/0.896
L_per.N.F.value
L_per.N.pvalue=1-pf(L_per.N.F.value,1,16)
L_per.N.pvalue
head(StemData)

# L_C.N Nmass

L_C.N.model<-aov( L_C.N~Growth.Form+Growth.Form:Species,data=StemData)
summary( L_C.N.model)
L_C.N.F.value=968.2/155.8
L_C.N.F.value
L_C.N.pvalue=1-pf( L_C.N.F.value,1,16)
L_C.N.pvalue
head(StemData)

###################################
#PCA data and analysis
###################################
PCAdata<-readr::read_csv("C:/Users/chees/OneDrive - James Cook University/Plant_Ecophys_Lab/Buckton/Managed Data/Cheesman_reanalysis/PCA_data.csv")

require(FactoMineR)
require(factoextra)

PCAdata$LS<-log10(PCAdata$LS)

#Get Species to be row name
#result <- as.data.frame(PCAdata[-1])
result<-as.data.frame(PCAdata)
row.names(result) <- PCAdata$Species


pca1<-prcomp(result[,3:12], scale.=T,center=T)
head(pca1$rotation)
head(pca1$x)

summary(pca1)

pca1$sdev^2

scores=as.data.frame(pca1$x)

head(scores)
scores$Growth.Form<-PCAdata$Growth.Form
scores$Species<-PCAdata$Species

#plot Species against princial dimensions
PCA1<-ggplot(data = scores, aes(x = PC1, y = PC2,fill=Growth.Form))+
   geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_point(size=5,shape=21)+
  #geom_text(label = rownames(scores)) + 
  #stat_ellipse(data = scores,aes(x = PC1, y = PC2,fill=factor(Growth.Form)),
  #             geom="polygon", level=0.90, alpha=0.5)+
  scale_fill_manual(values=c("black","white"))+
  theme_bw()+
  theme(
    aspect.ratio=1,
    axis.text=element_text(size=20,colour="black"),
    line=element_line(colour="black"),
    axis.title=element_blank(),
    #axis.title=element_text(size=18),
    panel.grid=element_blank(),
    axis.ticks=element_line(size=2),
    panel.border=element_rect(size=2),
    legend.position='none'
)
 
#circle of correlations   
# function to create a circle
circle <- function(center = c(0, 0), npoints = 100) {
  r = 1
  tt = seq(0, 2 * pi, length = npoints)
  xx = center[1] + r * cos(tt)
  yy = center[1] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
corcir = circle(c(0, 0), npoints = 100)

# create data frame with correlations between variables and PCs

correlations = as.data.frame(cor(result[,3:12], pca1$x))
correlations$Growth.Form<-PCAdata$Growth.Form

# data frame with arrows coordinates
arrows = data.frame(x1 = c(0,0,0,0,0,0,0,0,0,0), y1 = c(0,0,0,0,0,0,0,0,0,0), x2 = correlations$PC1, 
                    y2 = correlations$PC2)

# geom_path will do open circles
PCA2<-ggplot() + 
  geom_path(data = corcir, aes(x = x, y = y), size=1,colour = "darkgrey") + 
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), size=1,colour = "black") + 
  #geom_text(data = correlations, aes(x = PC1, y = PC2, label = rownames(correlations))) + 
  geom_hline(yintercept = 0, colour = "gray65") + 
  geom_vline(xintercept = 0, colour = "gray65") + 
  xlim(-1.1, 1.1) + 
  ylim(-1.1, 1.1) + 
   theme_bw()+
    theme(
    aspect.ratio=1,
    axis.text=element_text(size=20,colour="black"),
    axis.title=element_blank(),
    line=element_line(colour="black"),
    #axis.title=element_text(size=18),
    panel.grid=element_blank(),
    axis.ticks=element_line(size=2),
    panel.border=element_rect(size=2),
    legend.position='none'
  )
  
plot_grid(PCA1,PCA2,
          align='vh', 
          ncol=1)

###################################################################


#G statistics
#wrk<-Stem_Licor_ForR_Names
#anova11<-aov(data=wrk,AvLMA~Growth.Form+Growth.Form:Species)
#summary(anova11)
#LMA.F.value=36922/2559
#LMA.F.value
#LMA.pvalue=1-pf(LMA.F.value,1,16)
#LMA.pvalue

summary(LMA_Model.2)

anova(LMA_Model)
summary(LMA_Anova)
summary(LMA_Anova.2)



###########################
#plot Leaf characters
###
ggplot(data=summary.Leaf.2)+
  geom_boxplot(aes(x=Growth.Form,y=LMA,fill=Growth.Form))+
  #scale_fill_grey()+
  scale_fill_manual(values=c(L="darkgrey", T="lightgrey"))+
  theme_bw()+
  xlab(expression(paste("Growth Form",sep="")))+
  ylab(expression(paste("LMA g ",cm^-1,,sep="")))+
  #ylab(expression(paste("Flow Rate (l",s^-1,")",sep="")))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18,colour="black"),
    line=element_line(colour="black"),
    axis.title=element_text(size=18),
    axis.ticks=element_line(size=2),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

###########################
#plot Leaf characters
###
ggplot(data=summary.Stem)+
  geom_boxplot(aes(x=Growth.Form,y=Density,fill=Growth.Form))+
  scale_fill_grey()+
  #scale_fill_manual(values=c(L="darkgrey", T="lightgrey"))+
  theme_bw()+
  xlab(expression(paste("Growth Form",sep="")))+
  ylab(expression(paste("LMA g ",cm^-1,,sep="")))+
  #ylab(expression(paste("Flow Rate (l",s^-1,")",sep="")))+
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text=element_text(size=18,colour="black"),
    line=element_line(colour="black"),
    axis.title=element_text(size=18),
    axis.ticks=element_line(size=2),
    panel.border=element_rect(size=2),
    legend.position='none'
  )

############################
###G Code###################
############################

detach(Stem_Licor_ForR_Names)
attach(Stem_Licor_ForR_Names)

s<-Species
g<-Growth.Form
sc<-Species.Code
ld13c<-L_true13C
sd13c<-S_true13C
ld15n<-L_true15N
sd15n<-S_true15N
cn<-L_C.N
Aa<-Aarea
Am<-Amass
gs<-AvCond
wue<-AvWUE_g
cica<-AvCiCa
d<-Density
rat<-Xy_Tot_Ratio

#boxplots for carbon and nitrogen analysis
par(mfrow = c(2,3),oma=c(1,1,0,0),mar=c(1.5,3.5,1.5,3.5),
    mgp=c(1,0.5,0),bty="o", las=1,cex.axis=0.55,cex.main=0.5)
boxplot(ld13c~g, main="Leaf d13C",col=c("grey37","seashell3"))
boxplot(sd13c~g, main="Stem d13C",col=c("grey37","seashell3"))
boxplot(ld15n~g, main="Leaf d15N",col=c("grey37","seashell3"))
boxplot(sd15n~g, main="Stem d15N",col=c("grey37","seashell3"))
boxplot(cn~g,    main="C:N Ratio",col=c("grey37","seashell3"))
boxplot(Nmass~g,    main="Nitrogen Percent",col=c("grey37","seashell3"))
#boxplots for photosynthetic traits
par(mfrow = c(2,3),oma=c(1,1,0,0),mar=c(1.5,3.5,1.5,3.5),
    mgp=c(1,0.5,0),bty="o", las=1,cex.main=1.2,cex.axis=0.55)
boxplot(Aa~g, main="Aarea",
        col=c("grey37","seashell3"))
boxplot(Am~g, main="Amass",
        col=c("grey37","seashell3"))
boxplot(gs~g, main="Stomatal Conductance",
        col=c("grey37","seashell3"))
boxplot(wue~g, main="Intrinsic WUE",
        col=c("grey37","seashell3"))
boxplot(cica~g, main="Ci/Ca",
        col=c("grey37","seashell3"))
boxplot(PNUE~g, main="PNUE",
        col=c("grey37","seashell3"))
#boxplots for morphological traits 
par(mfrow = c(2,3),oma=c(1,1,0,0),mar=c(1.5,3.5,1.5,3.5),
    mgp=c(1,0.5,0),bty="o", las=1,cex.main=0.8,cex.axis=0.55)
boxplot(AvArea~g, main="Leaf Size",
        col=c("grey37","seashell3"))
boxplot(AvLMA~g, main="LMA",
        col=c("grey37","seashell3"))
boxplot(LDMC~g, main="LDMC",
        col=c("grey37","seashell3"))
boxplot(d~g, main="Wood Density",
        col=c("grey37","seashell3"))
boxplot(AvLam~g, main="Laminar Thickness",
        col=c("grey37","seashell3"))
boxplot(rat~g, main="Xylem:Total Stem Area",
        col=c("grey37","seashell3"))

#anovas for carbon nitrogen analysis
wrk<-Stem_Licor_ForR_Names
colnames(wrk)
anova1<-aov(wrk$L_true13C~wrk$Growth.Form + wrk$Growth.Form:wrk$Species)
ld13c.F.value=5.94/6.013
ld13c.F.value
ld13c.pvalue=1-pf(ld13c.F.value,1,16)
ld13c.pvalue


summary(anova1)
anova1<- aov(ld13c~g + g:s)
summary(anova1)
ld13c.F.value=5.930/6.509
ld13c.F.value
ld13c.pvalue=1-pf(ld13c.F.value,1,16)
ld13c.pvalue

anova2<- aov(sd13c~g+ g:s)
summary(anova2)
sd13c.F.value=0.129/5.315
sd13c.F.value
sd13c.pvalue=1-pf(sd13c.F.value,1,16)
sd13c.pvalue

anova3<- aov(ld15n~g+ g:s)
summary(anova3)
ld15n.F.value=14.028/9.785
ld15n.F.value
ld15n.pvalue=1-pf(ld15n.F.value,1,16)
ld15n.pvalue

anova4<- aov(sd15n~g+ g:s)
summary(anova4)
sd15n.F.value=32.04/9.31
sd15n.F.value
sd15n.pvalue=1-pf(sd15n.F.value,1,16)
sd15n.pvalue

anova15<-aov(cn~g+g:s)
summary(anova15)
cn.F.value=1018.0/153.0
cn.F.value
cn.pvalue=1-pf(cn.F.value,1,16)
cn.pvalue

anova15<-aov(L_.N~g+g:s)
summary(anova15)
.n.F.value=5.029/0.872
.n.F.value
.n.pvalue=1-pf(.n.F.value,1,16)
.n.pvalue

#Anovas for Photosynthetic traits
anova5<-aov(Aa~g+g:s)
summary(anova5)
Aa.F.value=18.26/28.83
Aa.F.value
Aa.pvalue=1-pf(Aa.F.value,1,16)
Aa.pvalue

anova6<-aov(Am~g+g:s)
summary(anova6)
Am.F.value=0.15115/0.01281
Am.F.value
Am.pvalue=1-pf(Am.F.value,1,16)
Am.pvalue

anova7<-aov(gs~g+g:s)
summary(anova7)
gs.F.value=0.000978/0.010245
gs.F.value
gs.pvalue=1-pf(gs.F.value,1,16)
gs.pvalue
anova7a<-aov(gs~g)
summary(anova7a)

anova8<-aov(wue~g+g:s)
summary(anova8)
anova8a<-aov(wue~g)
summary(anova8a)

anova9<-aov(cica~g+g:s)
summary(anova9)
anova9a<-aov(cica~g)
summary(anova9a)

anovaa<-aov(PNUE~g+g:s)
summary(anovaa)
PNUE.F.value=34811/5568
PNUE.F.value
PNUE.pvalue=1-pf(PNUE.F.value,1,16)
PNUE.pvalue

#Anovas for morphological traits
anova10<-aov(AvArea~g+g:s)
summary(anova10)
LS.F.value=89283/50569
LS.F.value
LS.pvalue=1-pf(LS.F.value,1,16)
LS.pvalue

anova11<-aov(AvLMA~g+g:s)
summary(anova11)
LMA.F.value=36922/2559
LMA.F.value
LMA.pvalue=1-pf(LMA.F.value,1,16)
LMA.pvalue

anova12<-aov(LDMC~g+g:s)
summary(anova12)
LDMC.F.value=0.18044/0.02095
LDMC.F.value
LDMC.pvalue=1-pf(LDMC.F.value,1,16)
LDMC.pvalue

test<-aov(LDMC~Growth.Form+Growth.Form:Species,data=Stem_Licor_ForR_Names)
summary( test)

anova13<-aov(Density~g+g:s)
summary(anova13)
WD.F.value=0.10059/0.02097
WD.F.value
WD.pvalue=1-pf(WD.F.value,1,16)
WD.pvalue

anova14<-aov(AvLam~g+g:s)
summary(anova14)
LAM.F.value=0.002476/0.013600
LAM.F.value
LAM.pvalue=1-pf(LAM.F.value,1,16)
LAM.pvalue

anova15<-aov(rat~g+g:s)
summary(anova15)
rat.F.value=0.00713/0.07531
rat.F.value
rat.pvalue=1-pf(rat.F.value,1,16)
rat.pvalue

## non-linear mixed effects model with species as a random factor
model1<-lme(Aarea ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model1)
model2<-lme(Amass ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model2)
model3<-lme(gs ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model3)
model4<-lme(wue ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model4)
modela<-lme(PNUE ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(modela)

model5<-lme(AvLMA ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model5)
model6<-lme(d ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model6)
model7<-lme(AvArea ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
summary(model7)
anova(model7)
model8<-lme(AvLam ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model8)
model9<-lme(LDMC ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model9)
model10<-lme(rat ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model10)

model11<-lme(ld13c ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model11)
model12<-lme(sd13c ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model12)
model13<-lme(ld15n ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model13)
model14<-lme(sd15n ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model14)
model15<-lme(cn ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model15)
model16<-lme(L_.N ~ Growth.Form,random= ~1|Species,method='ML',data=wrk,na.action=na.omit)
anova(model16)



summary(manova(cbind(Aarea,Amass,gs,wue) ~ g), 
        test = "Hotelling-Lawley")

tapply(Amass, g, mean)

require(gglot2)

ggplot(data=Stem_Licor_ForR_Names,aes(x=AvCond,y=Aarea),)+
  geom_point(aes(col=factor(g)))+
  geom_smooth(method="lm")+
  theme_bw()+
  xlab("")+
  ylab("")

names(Stem_Licor_ForR_Names)

ggplot(data=Stem_Licor_ForR_Names,aes(x=AvTrans,y=Aarea),)+
  geom_point(aes(col=factor(g)))+
  ylim(0,25)+
  xlab("")+
  theme
  opts(panel.background=theme(fill='white'))
  geom_smooth(method="lm")

par(mfrow = c(2,2),oma=c(1,1,0,0),mar=c(2,2,2,1),mgp=c(1,0.3,0))
boxplot(Aarea~g, ylab="A area (umol/m2s)", cex.lab=0.8, cex.axis=0.5, col=colours)
boxplot(Amass~g, ylab="A mass (umol/gs)", cex.lab=0.8, cex.axis=0.5, col=colours)
boxplot(AvTrans~g, xlab="Growth Form", ylab="Transpiration",cex.lab=0.8, cex.axis=0.5, col=colours)
boxplot(AvWUE_g~g, xlab="Growth Form", ylab="Instantaneous WUE (umol/mol)", cex.lab=0.6, cex.axis=0.5, col=colours)

par(mfrow = c(2,2),oma=c(1,1,0,0),mar=c(2,2,2,1),mgp=c(1,0.3,0))
boxplot(Density~g, ylab="Wood Density (g/cm3)", cex.lab=0.8, cex.axis=0.75, col=colours)
boxplot(AvLMA~g, ylab="Leaf mass per area (g/cm3)", cex.lab=0.8, cex.axis=0.75, col=colours)
boxplot(LDMC~g, xlab="Growth Form", ylab="Leaf Dry Matter Content",cex.lab=0.8, cex.axis=0.75, col=colours)
boxplot(AvArea~g, xlab="Growth Form", ylab="Leaf Size (cm2)", cex.lab=0.8, cex.axis=0.75, col=colours)
colour="red")

library(lattice)
dotplot(d18O.av ~ Week | Source, data=Water_ForR)


plot(AvLMA~ld13c)
abline(lm(AvLMA~ld13c))

dotchart(ld13c~g)
