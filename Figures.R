#Figure 1: Boxplot
##Boxplot analysis
jpeg("figure1.jpeg", quality = 100, units = "in", width =6, height = 6, res = 300)

par(mfrow = c(1,2))
boxplot(log_pm_mean ~ Month,data = d_pm, range = 0, xlab = "Month", ylab = TeX("log(PM$_{2.5}$)"), col = "blue", border = "navy")
title("(q)", cex.main = 1, font.main = 2, cex.sub = 0.75, adj = 0)

boxplot(log_oz_max ~ Month,data = d_oz, range = 0, xlab = "Month", ylab = TeX("log(O$_{3}$)"), col = "violet", border = "navy")
title("(b)", cex.main = 1, font.main = 2, cex.sub = 0.75, adj = 0)

dev.off()


#Figure3 generation
d_pm <- subset(d_pm,d_pm$month == 12) 
d_oz <- subset(d_oz,d_oz$month == 8)

##Create categorical air quality data according to the air quality criteria of the U.S. EPA
###For pm
d_pm$Air <- d_pm$pm_mean
d_pm$Airtype <- NA
d_pm$Airtype[d_pm$Air>0 & d_pm$Air<=15.4] <- "Good"
d_pm$Airtype[d_pm$Air>15.4 & d_pm$Air<=40.4] <- "Moderate"
d_pm$Airtype[d_pm$Air>40.4 & d_pm$Air<=65.4] <- "USG"
d_pm$Airtype[d_pm$Air>65.4] <- "Unhealthy"

d_pm$Airtype <- as.factor(d_pm$Airtype)

###For ozone
d_oz$Air <- d_oz$o_max
d_oz$Airtype <- NA
d_oz$Airtype[d_oz$Air>0 & d_oz$Air<=54] <- "Good"
d_oz$Airtype[d_oz$Air>54 & d_oz$Air<=70] <- "Moderate"
d_oz$Airtype[d_oz$Air>70 & d_oz$Air<=85] <- "USG"
d_oz$Airtype[d_oz$Air>85 & d_oz$Air<=105] <- "Unhealthy"
d_oz$Airtype[d_oz$Air>105 & d_oz$Air<=200] <- "Very Unhealthy"
d_oz$Airtype <- as.factor(d_oz$Airtype)


###Pre data analysis
d_sf_pm <- st_as_sf(d_pm, coords = c("long", "lat"),crs = 4326)
d_sf_oz <- st_as_sf(d_oz, coords = c("long", "lat"),crs = 4326)

head(d_pm)

jpeg("figure2.jpeg", quality = 100, units = "in", width = 8, height = 8, res = 300)

d_sf_pm <- d_sf_pm[order(d_sf_pm$Airtype),]

###Plot
oj <- ggplot(CA_sf)+ geom_sf(data = CA_sf, fill = "transparent") + 
  ggtitle("(b)")+
  geom_sf(data = d_sf_oz, aes(color = Airtype), size = 1) + 
  labs(x = "", y = "", colour = "Air Quality Category") +
  facet_wrap(~Year,nrow = 1) +scale_color_manual(name="Ozone",values = c("#66CC00","orange","red","purple","brown"),
                                                 limits = c("Good", "Moderate", "USG","Unhealthy","Very Unhealthy"))+
  theme_bw()+
  theme(legend.justification = c("right", "bottom"),
        plot.title = element_text(size = 20))

pj <- ggplot(CA_sf)+ geom_sf(data = CA_sf, fill = "transparent") + 
  ggtitle("(a)")+
  geom_sf(data = d_sf_pm, aes(color = Airtype), size = 1) + 
  labs(x = "", y = "", colour = "Air Quality Category") +
  facet_wrap(~Year,nrow = 1) +scale_color_manual(name = TeX("PM$_{2.5}$"),values = c("#66CC00","orange","red","purple"),
                                                 limits = c("Good", "Moderate", "USG","Unhealthy"))+
  theme_bw()+
  theme(legend.justification = c("right", "bottom"),
        plot.title = element_text(size = 20))

ggarrange(pj, oj, ncol = 1, nrow = 2)

dev.off()




#Figure 4 generation
##Extract the model result.
index.pm.val <- inla.stack.index(stack, "pm_mean.val")$data
index.oz.val <- inla.stack.index(stack, "Ozone.val")$data

pred.pm.val <- model3$summary.fitted.values$mean[index.mean.val]
pred.oz.val <- model3$summary.fitted.values$mean[index.max.val]

fitted.pm.val <- data.frame(obser = d$log_pm_mean[index_val_pm], pred = pred.pm.val)
fitted.oz.val <- data.frame(obser = d$log_oz_max[index_val_oz], pred = pred.oz.val)

index.pm.train <- inla.stack.index(stack, "pm_mean.train")$data
index.oz.train <- inla.stack.index(stack, "Ozone.train")$data

pred.pm.train <- model3$summary.fitted.values$mean[index.pm.train]
pred.oz.train <- model3$summary.fitted.values$mean[index.oz.train]

fitted.pm.train <- data.frame(obser = d$log_pm_mean[index_pm], pred = pred.pm.train , type = pred.pm1)
fitted.oz.train <- data.frame(obser = d$log_oz_max[index_oz], pred = pred.oz.train , type = pred.oz1)

#Fixed effects' plot
fitted.pm.val <- data.frame(obser = d$log_pm_mean[index_val_pm], pred = pred.pm.val)
fitted.oz.val <- data.frame(obser = d$log_oz_max[index_val_oz], pred = pred.oz.val)
fitted.pm.train <- data.frame(obser = d$log_pm_mean[index_pm], pred = pred.pm.train)
fitted.oz.train <- data.frame(obser = d$log_oz_max[index_oz], pred = pred.oz.train)

pc <- ggplot(data = fitted.pm.val , aes(x = obser, y = pred)) +
  ggtitle("(c)")+
  geom_point(size = 1,color = "blue") + geom_abline(intercept = 0, slope = 1) + xlim(0,5) + 
  ylim(0,5)+
  theme_bw()+
  theme(axis.title = element_text(size = 17),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))

pa <- ggplot(data = fitted.pm.train , aes(x = obser, y = pred)) +
  ggtitle("(a)")+
  geom_point(size = 1,color = "blue") + geom_abline(intercept = 0, slope = 1) + xlim(0,5) + 
  ylim(0,5) + 
  theme_bw() +
  theme(axis.title = element_text(size = 17),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))

pd <- ggplot(data = fitted.oz.val , aes(x = obser, y = pred)) +
  ggtitle("(d)")+
  geom_point(size = 1,color = 6) + geom_abline(intercept = 0, slope = 1) + xlim(2.5,5) + 
  ylim(2.5,5) + 
  theme_bw() +
  theme(axis.title = element_text(size = 17),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))

pb <- ggplot(data = fitted.oz.train, aes(x = obser, y = pred)) +
  ggtitle("(b)")+
  geom_point(size = 1,color = 6) + geom_abline(intercept = 0, slope = 1) + xlim(2.5,5) + 
  ylim(2.5,5) + 
  theme_bw() +
  theme(axis.title = element_text(size = 17),
        plot.title = element_text(size = 20),
        axis.text = element_text(size = 15))

jpeg("figure4.jpeg", quality = 100, units = "in", width =14, height = 14, res = 500)

ggarrange(pa,pb,pc,pd,ncol = 2, nrow = 2)

dev.off()

#Figure5: Plot for Fixed effects
coeff1$Variable <- factor(coeff1$Variable,levels = c("Population density","GDP","Extreme drought","Fire burnt area","Wind speed","Surface pressure","Precipitation","Temperature"))
coeff1$Type <- factor(coeff1$Type,levels = c("PM","OZ"))
jpeg("figure5.jpeg", quality = 100, units = "in", width = 10, height = 4, res = 500)

ggplot(coeff1, aes(mean, Variable, color=Type)) + geom_point(size=1) +
  geom_errorbarh(aes(xmax =upper, xmin = lower), height = 0.2,linewidth = 1,alpha = 0.8,linetype = 1) +
  scale_x_continuous(limits= c(-0.3,0.7)) + 
  scale_color_manual(values=c("blue",6),label = (c(TeX('PM$_{2.5}$'),TeX('O$_{3}$')))) +
  geom_vline(aes(xintercept = 0)) +
  xlab('') + ylab(' ')+
  theme_bw()+
  theme(legend.text = element_text(hjust = 0))

dev.off()

#Figure 6: Full prediction
##Create New type for PM
d_sf2$pred.meantype <- NA
d_sf2$pred.meantype[d_sf2$pred.mean>0 & d_sf2$pred.mean<=log(15.4)] <- "Good"
d_sf2$pred.meantype[d_sf2$pred.mean>log(15.4) & d_sf2$pred.mean<=log(40.4)] <- "Moderate"
d_sf2$pred.meantype[d_sf2$pred.mean>log(40.4) & d_sf2$pred.mean<=log(65.4)] <- "USG"
d_sf2$pred.meantype[d_sf2$pred.mean>log(65.4) & d_sf2$pred.mean<=log(150.4)] <- "Unhealthy"
d_sf2$pred.meantype <- as.factor(d_sf2$pred.meantype)

##Create New type for OZ
d_sf2$pred.maxtype <- NA
d_sf2$pred.maxtype[d_sf2$pred.max>0 & d_sf2$pred.max<=log(54)] <- "Good"
d_sf2$pred.maxtype[d_sf2$pred.max>log(54) & d_sf2$pred.max<=log(70)] <- "Moderate"
d_sf2$pred.maxtype[d_sf2$pred.max>log(70) & d_sf2$pred.max<=log(85)] <- "USG"
d_sf2$pred.maxtype[d_sf2$pred.max>log(85) & d_sf2$pred.max<=log(105)] <- "Unhealthy"
d_sf2$pred.maxtype[d_sf2$pred.max>log(105) & d_sf2$pred.max<=log(200)] <- "Very Unhealthy"
d_sf2$pred.maxtype[d_sf2$pred.max>log(200)] <- "Hazardous"
d_sf2$pred.maxtype <- as.factor(d_sf2$pred.meantype)

pf <- ggplot(CA_sf)+ geom_sf(data = d_sf2, aes(color = pred.meantype), size = 1) + 
  ggtitle("(a)")+
  scale_x_continuous(breaks = seq(-124, -112, 3)) + 
  labs(x = "", y = "", colour = "Air Quality Category") +
  facet_wrap(~Mtypr,nrow = 2) +scale_color_manual(name = TeX("PM$_{2.5}$"), values = c("#66CC00","orange","red","purple"),
                                                  limits = c("Good", "Moderate", "USG","Unhealthy"))+
  theme_bw()+
  theme(legend.justification = c("right", "bottom"),
        plot.title = element_text(size = 20))+
  geom_sf(data = CA_sf, fill = "transparent")

of <- ggplot(CA_sf)+
  ggtitle("(b)")+
  scale_x_continuous(breaks = seq(-124, -112, 3)) + 
  geom_sf(data = d_sf2, aes(color = pred.maxtype), size = 1) + 
  labs(x = "", y = "", colour = "Air Quality Category") +
  facet_wrap(~Mtypr,nrow=2) +scale_color_manual(name=TeX("O$_3$"),values = c("#66CC00","orange","red","purple","blue","brown"),
                                                limits = c("Good", "Moderate", "USG","Unhealthy","Very Unhealthy","Hazardous"))+
  theme_bw()+
  theme(legend.justification = c("right", "bottom"),
        plot.title = element_text(size = 20))+
  geom_sf(data = CA_sf, fill = "transparent") 

jpeg("figure6.jpeg", quality = 100, units = "in", width = 14, height = 14, res = 500)
ggarrange(pf,of,ncol = 1, nrow = 2)
dev.off()

##Figure7: Excursion.
excursion_71_pos <- excursions.inla(model3,stack = stack,tag="Ozone.pred",alpha = 0.05,u=log(71),type = ">",method = "QC",F.limit = 1)

d2021$excursions1 <- excursion_71_pos$F

excursion_54_neg <- excursions.inla(model3,stack = stack,tag="Ozone.pred",alpha = 0.05,u=log(54),type = "<",method = "QC",F.limit = 1)

d2021$excursions2 <- excursion_71_neg$F

d_sf3$Mtypr <- d_sf2$Mtypr
d_sf3 <- st_as_sf(d2021, coords = c("long", "lat"),crs = 4326)

ex1 <- ggplot(CA_sf) +
  ggtitle("(a)")+
  geom_sf(data = d_sf3, aes(color = excursions1), size = 1) + 
  labs(x = "", y = "",colour = "Excursion (+)") +
  coord_sf(crs = '+proj=utm +zone=12') + #
  scale_x_continuous(breaks = seq(-124, -112, 3)) + 
  facet_wrap(~Mtypr,nrow = 2) +
  theme_bw(base_family = "serif") + 
  scale_colour_gradientn(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1), colors=c("grey","yellow","orange","red"))+
  #scale_color_viridis_c(breaks = seq(0, 7, 1)) + 
  theme(axis.title = element_blank(),
        legend.key.height = unit(0.5, "inch"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 16),
        axis.text = element_text(colour = "black"),
        legend.justification = c("right", "bottom"),
        plot.title = element_text(size = 20))+
  geom_sf(data = CA_sf, fill = "transparent")

ex2 <- ggplot(CA_sf) + 
  ggtitle("(b)")+
  geom_sf(data = CA_sf, fill = "transparent") + 
  geom_sf(data = d_sf3, aes(color = excursions2), size = 1) + 
  labs(x = "", y = "",colour = "Excursion (-)") +
  coord_sf(crs = '+proj=utm +zone=12') + #
  scale_x_continuous(breaks = seq(-124, -112, 3)) + 
  facet_wrap(~Mtypr,nrow = 2) +
  theme_bw(base_family = "serif") + 
  scale_colour_gradientn(limits=c(0,1),breaks=c(0,0.2,0.4,0.6,0.8,1), colors=c("grey","green","cyan"))+
  #scale_color_viridis_c(breaks = seq(0, 7, 1)) + 
  theme(axis.title = element_blank(),
        legend.key.height = unit(0.5, "inch"),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 10),
        strip.text = element_text(size = 16),
        axis.text = element_text(colour = "black"),
        legend.justification = c("right", "bottom"),
        plot.title = element_text(size = 20))+
  geom_sf(data = CA_sf, fill = "transparent")

jpeg("figure7.jpeg", quality = 100, units = "in", width = 14, height = 14, res = 500)
ggarrange(ex1, ex2, ncol = 1, nrow = 2)
dev.off()

