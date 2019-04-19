##Technician Monthly Performance report

#import libraries
library(dplyr)  ##for manipulating the data
library(xlsx) ##for importing and reading excel file into R
library(ggplot2) ##plotting the graphs
library(reshape2) ##for reshaping the datasets
library(gridExtra)
library(ggpubr) ##for axis label adjustments

##read the report
techperf_data <- read.xlsx("Technician Monthly Performance March.xlsx", sheetIndex = 1)


##Manipulate the data
##data for individual techinicians
####indtech_dt: data for individual techs and activities column
indtech_dt <- techperf_data %>% select(O...M.activities, names(techperf_data)[7:19])

##melt the dataset
indtech_mdt <- melt(indtech_dt, id = c("O...M.activities"))
names(indtech_mdt) <- c("Activity.Total", "Technician", "Performance.Score")
##select only totals
techrank <- indtech_mdt %>% filter(Activity.Total == "TOTAL (%)") 


##plot the bar graph
#png(filename = "TechPerformance.png")
ggplot(data = techrank, aes(Technician, Performance.Score)) + 
        geom_col(aes(fill = Performance.Score)) + 
        coord_flip() + 
        labs(title = "Technician Monthly Performance for March") + 
        scale_fill_gradient(low="red", high="green")+
        geom_text(aes(y=Performance.Score, label = Performance.Score), hjust = 1.6)
#dev.off()
ggsave("TechPerformance.png", width = 10, height = 5)

##sumtech_dt to carry summary of the data
sumtech_dt <- techperf_data %>% select(c(1,2,3,6)) %>% 
        filter(!is.na(Scoring)) %>% 
        mutate(Expected.Score = Scoring*100)

##plot the graph
ggplot(data = sumtech_dt, aes(Final.Scoring, Expected.Score)) + geom_count()

##Test dataframe d
d <- data.frame(l = c("a", "b", "c", "d"),
                nm = c(1, 2, 4, 5), 
                tk = c(4, 3, 1, 6), 
                ref = c(2, 4, 6, 7))
##bar graph making comparisons
####first melt the data
d_melt <- melt(d, id.vars = "l")
####plot
ggplot(data = d_melt, aes(x = l, y = value, fill = variable)) + 
        geom_bar(stat = "identity", position = "dodge")

##summary performance data
sumperf_dt <- sumtech_dt %>% select(O...M.activities, Expected.Score, Final.Scoring)
names(sumperf_dt) <- c("Activity", "Goal", "Scored")
sumperf_dt$Scored <- round(sumperf_dt$Scored, 1)
sumperf_dt_melt <- melt(sumperf_dt, id.vars = "Activity")
###plot the bar graph
#png(filename = "ActivityScore.png")
ggplot(data = sumperf_dt_melt, aes(x = Activity, y = value, fill = variable)) +
        geom_bar(stat = "identity", position = "dodge") + 
        rotate_x_text() +
        labs(title = "Overall Score Per O&M Activity") +
        xlab("O&M Activity") +
        ylab("Percentage (%)") +
        geom_text(aes(y=value, label = value),  
                  color="black", size=3.5)
#dev.off()
ggsave("ActivityScore.png", width = 10, height = 5)

##Work on plot to summary individual variable contributors
###Activity score of each technician
actscore_dt <- techperf_data %>% select(c(1, 7:length(names(techperf_data)))) %>% 
        filter(O...M.activities != "TOTAL (%)") 

###melt the data
actscore_mdt <- melt(actscore_dt, id.vars = "O...M.activities")
###Change names of actscore_mdt
names(actscore_mdt) <- c("Activity", "Technician", "Achievement")

###plot the graph
#png(filename = "AchievementStatus.png")
ggplot(data = actscore_mdt, aes(x = Technician, y = Achievement, fill = Activity)) +
        geom_bar(stat = "identity") +
        rotate_x_text() +
        labs(title = "Achievement Status Per O&M Activity") 
#dev.off()
ggsave("AchievementStatus.png", width = 10, height = 5)




#####################################################################
#Analysing footprint data/report
#####################################################################
ftdata <- read.csv("ftreportmarch.csv")
##select data for only "Allan Kavuma"
allan_ftdt <- ftdata %>% filter(ATC.Regional.Supervisor.Name == "Allan Kavuma")
##Power tickets
power_tts <- allan_ftdt[allan_ftdt$Root.Cause == "Power",]
##graph of all power tts
ggplot(data = power_tts, aes(ATC.Site.Name)) + 
        geom_bar(fill = "salmon1") +
        rotate_x_text()+
        labs(title = "Total Power Tickets")+
        ggsave("TotalPowerTickets.png", width = 10, height = 5)
##Select onlysites with power tickets
site_ptt <- power_tts %>% select(ATC.Site.Name)
##sites that got at least a callout
site1_ptt <- unique(site_ptt$ATC.Site.Name)
##group power tts by site
gpower_tss <- power_tts %>% group_by(ATC.Site.Name) %>% 
        summarise(Total.TTs = n()) %>% 
        arrange(desc(Total.TTs))
##power tts more than 1
rpt_ptts <- gpower_tss %>% filter(Total.TTs > 1)
##graph of all power tts less than 1
##graph of all power tts
ggplot(data = rpt_ptts, aes(ATC.Site.Name, Total.TTs)) + 
        geom_col(aes(fill = Total.TTs)) +
        rotate_x_text() +
        labs(title = "Repeat Power Tickets")
ggsave("RepeatPowerTickets.png", width = 10, height = 5)


##DC Faults
dc_tts <- allan_ftdt[allan_ftdt$Root.Cause == "DC Power Faults",]
##Graph of DC tickets
ggplot(data = dc_tts, aes(ATC.Site.Name)) + 
        geom_bar(fill = "steelblue") +
        rotate_x_text()+
        labs(title = "DC Faults per site")
ggsave("DcFaults.png")


##Pending faults
Pending_tts <- allan_ftdt[allan_ftdt$Root.Cause == "Pending Faults",]
ggplot(data = Pending_tts, aes(ATC.Site.Name)) + 
        geom_bar(fill = "firebrick2") +
        rotate_x_text()+
        labs(title = "Pending Faults per site")
ggsave("PendingFaults.png", width = 10, height = 5)



######################################################################

#####################################################################
#Functions to call in code
#####################################################################
#Build Function to Return Element Text Object(for rotated axes)
rotatedAxisElementText = function(angle,position='x'){
        angle     = angle[1]; 
        position  = position[1]
        positions = list(x=0,y=90,top=180,right=270)
        if(!position %in% names(positions))
                stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
        if(!is.numeric(angle))
                stop("'angle' must be numeric",call.=FALSE)
        rads  = (angle - positions[[ position ]])*pi/180
        hjust = 0.5*(1 - sin(rads))
        vjust = 0.5*(1 + cos(rads))
        element_text(angle=angle,vjust=vjust,hjust=hjust)
}