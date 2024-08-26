
# results are read from saved results RData files, here, to plot our figure 3.

library(ggplot2)
Outlier_percentage=as.character(rep(c(0,0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2),each=6))
Method=factor(rep(c("lasso","enet","mcp","pawph","sis","RSF"),9),levels=c("lasso","enet","mcp","pawph","sis","RSF"))
value=c(6,6,5,5,7,7,6,6,5,5,7,7,6,6,5,5,7,7,6,6,5,5,7,7,6,6,4,5,7,7,6,6,3,5,7,7,5,5,3,5,7,7,5,5,3,5,7,7,5,5,3,4,6,7)
highdim_param1_tp_all_long=cbind.data.frame(Outlier_percentage,Method,value)

tpgg=ggplot(data = highdim_param1_tp_all_long,
            aes(x = Outlier_percentage,y = value,fill=Method)) +geom_bar(stat= "identity",position="dodge")+theme_bw()+
  ggtitle("True positive")+geom_hline(yintercept=7,color="black")
name="scenario1_revision"
#ggsave(tpgg,file=paste(name,"tp_plot.pdf",sep = "_"),height=5,width=10)



Outlier_percentage=as.character(rep(c(0,0.001,0.002,0.005,0.01,0.02,0.05,0.1,0.2),each=6))
Method=factor(rep(c("lasso","enet","mcp","pawph","sis","RSF"),9),levels=c("lasso","enet","mcp","pawph","sis","RSF"))
value=c(5,5,5,5,5,3,5,5,5,5,5,3,5,5,5,5,5,4,5,5,5,5,5,4,5,5,3,5,5,4,5,5,3,5,5,4,5,5,2,5,5,4,5,5,3,5,5,4,5,5,3,5,5,4)
highdim_param1_tp_all_long=cbind.data.frame(Outlier_percentage,Method,value)

tpgg=ggplot(data = highdim_param1_tp_all_long,
            aes(x = Outlier_percentage,y = value,fill=Method)) +geom_bar(stat= "identity",position="dodge")+theme_bw()+
  ggtitle("True positive")+geom_hline(yintercept=5,color="black")
name="scenario2_revision"
#ggsave(tpgg,file=paste(name,"tp_plot.pdf",sep = "_"),height=5,width=10)


