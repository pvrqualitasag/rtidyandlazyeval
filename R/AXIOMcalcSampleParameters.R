#https://www.r-bloggers.com/using-r-quickly-calculating-summary-statistics-from-a-data-frame/
inst_pack <- installed.packages()
pkgs <- c("plyr","dplyr","reshape2","data.table")
for (p in pkgs){
  if(!p %in% inst_pack)
    install.packages(p, repos = "https://cran.rstudio.com")
}

library(plyr)
library(dplyr)
library(reshape2)
library(data.table)
args <- commandArgs(TRUE)
#########################################################################################
#Script berechnet Callrate und Heterozygotie Rate pro Tier
#fsf 12.09.2019
#########################################################################################
#https://csgillespie.github.io/efficientR/data-carpentry.html
#define function to calc frequency table
freq_table <- function(x, group_var, prop_var) {
  group_var <- enquo(group_var)
  prop_var  <- enquo(prop_var)
  x %>% 
    group_by(!!group_var, !!prop_var) %>% 
    summarise(n = n()) %>% 
    mutate(freq = n /sum(n))
}
#########################################################################################
# create a small data set
#+
# the complete data set
# s_data_path <- "/qualstore03/data_tmp/zws/snp/150Kimputing/test.gt"
s_data_path <- "inst/extdata/head1000_test.gt"
if (!file.exists(s_data_path)){
  system(paste0("head -1000 /Volumes/data_tmp/zws/snp/150Kimputing/test.gt > ", s_data_path))
}

#read data
#https://www.r-project.org/nosvn/pandoc/dplyr.html
print("Started reading input file")
system.time(gt.df<-data.frame(scan(s_data_path, what=list(animal="",snp=0,gt=""),sep=" ",na.strings = "--")))
#calculate summary table manuall since it does not work using the function above
freq_table(gt.df,animal,gt)
print("Summary Table calculation started")
system.time(sumtab<-data.frame((gt.df %>%
                                group_by(animal, gt) %>%
                                summarise(n = n()) %>%
                                mutate(freq = n / sum(n)))))

head(sumtab)
system.time(csum<-filter(sumtab,is.na(sumtab$gt)))
system.time(csum$clrt<-round((1-csum$freq),digits=3))
system.time(hsum<-filter(sumtab,gt == "AB"))
system.time(csum$heteroprop<-round((hsum$freq),digits=3))
csum


#alternatively do manually:
#system.time(NAS<-filter(gt,is.na(gt$gt)))
#system.time(ca<-data.frame(count(gt,c('animal'))))
#system.time(cna<-data.frame(count(NAS,c('animal'))))
#system.time(csum<-data.frame(merge(ca,cna,by.x="animal",by.y="animal")))
#system.time(csum$clrt<-round(1-((csum$freq.y) / (csum$freq.x)),digits=3)
#csum
