#This is the code used for the didactic example, sequence and cluster analysis applied 
#to transition to retirement trajectories in the Health and Retirement Study.
#Contact: lucia.pacca@ucsf.edu

#install.packages("cluster")
library(cluster) 

#install.packages("WeightedCluster")
library(WeightedCluster)

#install.packages("devtools")
library(devtools)

#install.packages("TraMineR")
library(TraMineR)

#install.packages("TraMineRextras")
library(TraMineRextras)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("NbClust")
library(NbClust)

install.packages("ggseqplot")
library(ggseqplot)

install.packages("gridExtra")
library(gridExtra)

install.packages("Tmisc")
library(Tmisc)

# Define the states
state_alphabet <- c("1", "2", "3", 
                       "4", "5", "6", "7")   

# Create a vector that allows for more helpful labels if applicable 
state_labels <- c("Employed Full Time", "Employed Part Time", "Retired", 
                  "Disabled", "Out of Work", "Unreported", "Not Yet Observed")

# Choose a color for each state (for graphics)
EmployedFullTime <- "#0033FF"      
EmployedPartTime <- "#99FFFF"  
Retired <- "#00BA38"       
Disabled <- "#CC00FF"          
OutOfWork <-  "#FF3300"       
Unreported <- "#666666"     
NotYetObserved <- "#E5E5E5"  

##############Import data (data available upon request)
library(haven)
States_Wide <- read_dta("/Users/lpacca/Downloads/08 - Archived - SA Didactic Paper/final_sequences.dta")
# rename states
colnames(States_Wide) <- stringr::str_extract(colnames(States_Wide), "[0-9]+")
View(States_Wide)

#Set sequences 
States_Wide.seq <- seqdef(
                   States_Wide, # Select data   
                   var = 2:26, # Columns containing the sequences
                   alphabet = state_alphabet,
                   labels = state_labels,
                   start=51, left="7", right="6", gaps="6", #this refers to missing gaps. We are labeling left gaps as "left missing", and right and middle gaps as "unreported"
                   xtstep = 4,  cpal=c(EmployedFullTime, EmployedPartTime, Retired, 
                                       Disabled, OutOfWork, Unreported, NotYetObserved))

View(States_Wide.seq)


#Create Index Plot: this graph will represent the individual sequences for ALL the participants
seqIplot(States_Wide.seq, sortv ="from.start",               
         with.legend = TRUE,
         cex.legend=1, #size of legend (can make smaller, e.g. 0.5, or bigger, e.g. 1.5)
         main = "Sequence Index Plot", # Plot title
         xlab = "Age (years)") # x-axis label

seqlegend(States_Wide.seq)

#define cost matrix based on transition rates
costmatrix <- seqsubm(States_Wide.seq,             # Sequence object
                      method = "TRATE",  # Method to determine costs: TRATE=transition rates
                      time.varying = FALSE) # Does not allow the cost to vary over time)
costmatrix #display cost matrix

# Conduct sequence analysis 
#HAMMING DISSIMILARITY MEASURE: prioritizes timing of events
dist_ham <- seqdist(States_Wide.seq,
                   method = "HAM",
                   sm= costmatrix)

# Examine the top left corner of the dissimilarity matrix
corner(dist_ham, n=15)

#Conduct Cluster Analysis
#We will be comparing two clustering agorithms: hierarchical clustering and partitioning around medoids.

#Using HIERARCHICAL, AGGLOMERATIVE CLUSTERING (fastest) 
om_agnes <- hclust(as.dist(dist_ham), method = "ward.D2")
# Plot dendrogram
plot(as.dendrogram(om_agnes), main = "OM Dendrogram - Ward")

#Assess cluster quality
om_agnes_c <- as.clustrange(om_agnes, diss=as.dist(dist_ham))
summary(om_agnes_c, max.rank=10)
# Plot cluster quality measures
plot(om_agnes_c, stat = c("ASW", "HC", "CH"), main = "OM Cluster Quality")

##Using PARTITION AROUND MEDOIDS
##Look at cluster quality for a variety of cluster solutions
pamRange <- wcKMedRange(dist_ham, kvals=2:20)
summary(pamRange, max.rank=10)
#We choose PAM, as the cluster quality measures were overall better 

#We look at ASW, HC and CH, three of the most commonly used cluster quality indicators
plot(pamRange, stat = c("ASW","HC", "CH"), norm="zscore", lwd = 2, cex=2, col = c('#6666ff', '#cc0000', '#008000'), legendpos = "topright")
abline(v=7, col="#666666", lty="longdash", lwd = 2) #7 clusters is our preferred solution

#7 cluster solution based on PAM algorithm
clust7 <- wcKMedoids(dist_ham, k=7, cluster.only=TRUE)
seqIplot(States_Wide.seq, group=clust7, border=NA) #index plots by cluster

###############################################################################$
######REORDER AND RENAME THE CLUSTERS

clusters7 <- ggseqiplot(States_Wide.seq,  #index plots by cluster with ggplot
                       group=clust7) + 
  theme(legend.text = element_text(size=10),
        legend.key.size = unit(0.50, 'cm'),
        legend.key = element_rect(colour="black"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

###
clusters7_modal <- ggseqmsplot(States_Wide.seq,  #index plots by cluster with ggplot
                        group=clust7) + 
  theme(legend.text = element_text(size=10),
        legend.key.size = unit(0.50, 'cm'),
        legend.key = element_rect(colour="black"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

###
clusters7_chronograms <- clusters7_chronograms +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.35, "cm")
  ) +
  guides(
    fill = guide_legend(nrow = 2, byrow = TRUE)
  )

# export 7 clusters solution
View(clust7)
df_cl7 <-clust7
write.csv(df_cl7, "/Users/lpacca/Downloads/clusters_pam_7.csv")


# Reorder 7 cluster solution ----------------------------------------------

one <- States_Wide.seq[which(clust7 == 9171),]
clus1 <-  
  ggseqiplot(one) +
  labs(title = "TYPICAL RETIREMENT (N=3,316)",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


two <- States_Wide.seq[which(clust7 == 9184),]
clus2 <-  
  ggseqiplot(two) +
  labs(title = "EARLY RETIREMENT (N=1,360)",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


three <- States_Wide.seq[which(clust7 == 9099),]
clus3 <-  
  ggseqiplot(three) +
  labs(title = "FULL-TIME EMPLOYMENT TO LATE RETIREMENT (N=1,701)",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


four <- States_Wide.seq[which(clust7 == 8970),]
clus4 <-  
  ggseqiplot(four) +
  labs(title = "PART-TIME EMPLOYMENT TO LATE RETIREMENT (N=905)",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


five <- States_Wide.seq[which(clust7 == 9156),]
clus5 <-  
  ggseqiplot(five) +
  labs(title = "DISABILITY (N=648)",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

six <- States_Wide.seq[which(clust7 == 1190),]
clus6 <-  
  ggseqiplot(six) +
  labs(title = "TYPICAL RETIREMENT, INITIAL MISSINGNESS (N=790)",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

seven <- States_Wide.seq[which(clust7 == 9040),]
clus7 <-  
  ggseqiplot(seven) +
  labs(title = "OUT OF WORK GAPS (N=469)",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

mylegend7 <- ggpubr::get_legend(clusters7)
# Combine plots
clusters_pam_7 <- gridExtra::grid.arrange(arrangeGrob(clus1, clus2, clus3,
                                    clus4, clus5, clus6, clus7,
                                    ncol = 2),
                        mylegend7, nrow=2, heights=c(15, 1))

ggsave("/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7.png", plot = clusters_pam_7) 

# Reorder 7 cluster solution - MODAL PLOTS ------------------------------------------

one <- States_Wide.seq[which(clust7 == 9171),]
clus1 <-  
  ggseqmsplot(one) +
  labs(title = "TYPICAL RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


two <- States_Wide.seq[which(clust7 == 9184),]
clus2 <-  
  ggseqmsplot(two) +
  labs(title = "EARLY RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


three <- States_Wide.seq[which(clust7 == 9099),]
clus3 <-  
  ggseqmsplot(three) +
  labs(title = "FULL-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


four <- States_Wide.seq[which(clust7 == 8970),]
clus4 <-  
  ggseqmsplot(four) +
  labs(title = "PART-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


five <- States_Wide.seq[which(clust7 == 9156),]
clus5 <-  
  ggseqmsplot(five) +
  labs(title = "DISABILITY",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

six <- States_Wide.seq[which(clust7 == 1190),]
clus6 <-  
  ggseqmsplot(six) +
  labs(title = "TYPICAL RETIREMENT, INITIAL MISSINGNESS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

seven <- States_Wide.seq[which(clust7 == 9040),]
clus7 <-  
  ggseqmsplot(seven) +
  labs(title = "OUT OF WORK GAPS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

mylegend7 <- ggpubr::get_legend(clusters7_modal)
# Combine plots
clusters_pam_7_modal <- gridExtra::grid.arrange(arrangeGrob(clus1, clus2, clus3,
                                                      clus4, clus5, clus6, clus7,
                                                      ncol = 2),
                                          mylegend7, nrow=2, heights=c(15, 1))

ggsave("/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7_modal.png", plot = clusters_pam_7_modal) 

# Reorder 7 cluster solution - CHRONOGRAMS ------------------------------------------

one <- States_Wide.seq[which(clust7 == 9171),]
clus1 <-  
  ggseqdplot(one) +
  labs(title = "TYPICAL RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


two <- States_Wide.seq[which(clust7 == 9184),]
clus2 <-  
  ggseqdplot(two) +
  labs(title = "EARLY RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


three <- States_Wide.seq[which(clust7 == 9099),]
clus3 <-  
  ggseqdplot(three) +
  labs(title = "FULL-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


four <- States_Wide.seq[which(clust7 == 8970),]
clus4 <-  
  ggseqdplot(four) +
  labs(title = "PART-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


five <- States_Wide.seq[which(clust7 == 9156),]
clus5 <-  
  ggseqdplot(five) +
  labs(title = "DISABILITY",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

six <- States_Wide.seq[which(clust7 == 1190),]
clus6 <-  
  ggseqdplot(six) +
  labs(title = "TYPICAL RETIREMENT, INITIAL MISSINGNESS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

seven <- States_Wide.seq[which(clust7 == 9040),]
clus7 <-  
  ggseqdplot(seven) +
  labs(title = "OUT OF WORK GAPS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

mylegend7 <- ggpubr::get_legend(clusters7_chronograms)


# Combine plots
clusters_pam_7_chrono <- gridExtra::grid.arrange(
  arrangeGrob(
    clus1, clus2, clus3,
    clus4, clus5, clus6, clus7,
    ncol = 3
  ),
  mylegend7,
  nrow = 2,
  heights = c(14, 3)
)

ggsave("/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7_chrono.png", plot = clusters_pam_7_chrono) 

######SENSITIVITY: OPTIMAL MATCHING
dist_om <- seqdist(States_Wide.seq,
                   method = "OM",
                   indel= 1.0,
                   sm= costmatrix)

##Using PARTITION AROUND MEDOIDS FOR OPTIMAL MATCHING
##Look at cluster quality for a variety of cluster solutions
pamRange <- wcKMedRange(dist_om, kvals=2:20)
summary(pamRange, max.rank=10)
plot(pamRange, stat = c("ASW","HC"), norm="zscore", lwd = 2, cex=2, col = c('#6666ff', '#cc0000'), legendpos = "topright")
abline(v=7, col="#666666", lty="longdash", lwd = 2)

#7 cluster solution based on PAM algorithm
clust7_om <- wcKMedoids(dist_om, k=7, cluster.only=TRUE)
seqIplot(States_Wide.seq, group=clust7_om, border=NA) #index plots by cluster

# Reorder 7 cluster solution ----------------------------------------------

clusters7_om <- ggseqiplot(States_Wide.seq,  #index plots by cluster with ggplot
                        group=clust7_om) + 
  theme(legend.text = element_text(size=5),
        legend.key.size = unit(0.25, 'cm'),
        legend.key = element_rect(colour="black"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

one <- States_Wide.seq[which(clust7_om == 9114),]
clus1 <-  
  ggseqiplot(one) +
  labs(title = "TYPICAL RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


two <- States_Wide.seq[which(clust7_om == 177),]
clus2 <-  
  ggseqiplot(two) +
  labs(title = "EARLY RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


three <- States_Wide.seq[which(clust7_om == 237),]
clus3 <-  
  ggseqiplot(three) +
  labs(title = "FULL-TIME EMPLOYMENT TO LATE RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


four <- States_Wide.seq[which(clust7_om == 1995),]
clus4 <-  
  ggseqiplot(four) +
  labs(title = "PART-TIME EMPLOYMENT TO RETIREMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 


five <- States_Wide.seq[which(clust7_om == 1275),]
clus5 <-  
  ggseqiplot(five) +
  labs(title = "FULL_TIME TO PART-TIME EMPLOYMENT",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

six <- States_Wide.seq[which(clust7_om == 8093),]
clus6 <-  
  ggseqiplot(six) +
  labs(title = "DISABILITY",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

seven <- States_Wide.seq[which(clust7_om == 6495),]
clus7 <-  
  ggseqiplot(seven) +
  labs(title = "OUT OF WORK GAPS, INITIAL MISSINGNESS",
       x = "Age (years)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
        legend.position = "none") 

mylegend7 <- ggpubr::get_legend(clusters7_om)
# Combine plots
clusters_pam_7_om <- gridExtra::grid.arrange(arrangeGrob(clus1, clus2, clus3,
                                                      clus4, clus5, clus6, clus7,
                                                      ncol = 2),
                                          mylegend7, nrow=2, heights=c(15, 1))

ggsave("/Users/lpacca/Library/CloudStorage/Box-Box/08 - SA Didactic Paper/clusters_pam_7_om.png", plot = clusters_pam_7_om) 

