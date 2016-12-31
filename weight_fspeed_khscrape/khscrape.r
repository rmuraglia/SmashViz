# khscrape.r

library(jsonlite)
library(ggplot2)
library(png)

# check attribute ID mapping with
# http://api.kuroganehammer.com/api/smashattributetypes

# fetch weights
url_weight<-'http://api.kuroganehammer.com/api/smashattributetypes/25/characterattributes'
dat_weight<-fromJSON(url_weight)

# parse weights to dataframe
getWeight<-function(x) { x[8][[1]][4] }
weights<-unlist(apply(dat_weight, 1, getWeight))
w_order<-dat_weight[,5]
w_df<-cbind(w_order, weights)
w_df<-w_df[order(w_df[,1]),]

# fetch fall speeds
url_fspeed<-'http://api.kuroganehammer.com/api/smashattributetypes/8/characterattributes'
dat_fspeed<-fromJSON(url_fspeed)

# parse fall speed (if want fast fall speed, use [2,4]) to dataframe
getFSpeed<-function(x) { x[8][[1]][1,4] }
fspeeds<-unlist(apply(dat_fspeed, 1, getFSpeed))
fs_order<-dat_fspeed[,5]
fs_df<-cbind(fs_order, fspeeds)
fs_df<-fs_df[order(fs_df[,1]),]

# make sure ordering is correct
if (!all(fs_df[,1]==w_df[,1])) { print('Character ordering does not seem consistent. Double check the w_df and fs_df dataframes') }

# make plotting DF
plot_df<-cbind(fs_df, w_df[,2])
colnames(plot_df)<-c('name', 'FallSpeed', 'Weight')
plot_df<-data.frame(plot_df)
plot_df[,2]<-as.numeric(as.character(plot_df[,2]))
plot_df[,3]<-as.numeric(as.character(plot_df[,3]))

