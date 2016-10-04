######### Importing modules
library(zoo)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)

######## Functions

# Fix date
fixdate <- function(x) {
        date_split <- str_split(x, '[/]')
        if (nchar(date_split[[1]][3]) == 2) {
                century <- ifelse(as.numeric(date_split[[1]][3]) <= as.numeric(format(Sys.Date(), '%y')), '20', '19')
                date_split_year <- paste(century, date_split[[1]][3], sep='')
                date_split_month <- str_pad(date_split[[1]][1], 2, side='left', pad='0')
                date_split_day <- str_pad(date_split[[1]][2], 2, side='left', pad='0')
        } else {
                date_split_year <- date_split[[1]][3]
                date_split_month <- date_split[[1]][2]
                date_split_day <- date_split[[1]][1]
        }
        date_fix <- paste(date_split_year, date_split_month, date_split_day, sep='-')
        return(as.Date(date_fix))
}

# Fix salary
fixsalary <- function(x) {
        x <- str_replace_all(x, '[R$ .]', '')
        x <- as.numeric(str_replace(x, '[,]', '.'))
        return(x)
}

# Roundup function: returns integer for scale limit
roundup <- function(x, power=1) {
        
        while (x > 10) {
                x <- x/10
                power <- power*10
        }
        
        return (ceiling(x)*power) 
}

# Multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

######### Loading and prepping data
data <- read.csv('questao1.csv', fileEncoding='ISO-8859-1', sep=';', dec=',', stringsAsFactors=F, strip.white=TRUE)
# data_raw <- data

# Fixing date vectors
data$Data.de.admissão <- as.Date(sapply(data$Data.de.admissão, fixdate))
data$Data.de.nascimento <- as.Date(sapply(data$Data.de.nascimento, fixdate))
data$ano_admissao <- as.numeric(format(data$Data.de.admissão, "%Y"))

# Fixing salary vector
data$Salário.mensal.médio <- sapply(data$Salário.mensal.médio, fixsalary)

# Setting factors
data$Sexo <- as.factor(data$Sexo)
data$Cargo <- as.factor(data$Cargo)
data$Área <- as.factor(data$Área)
data$Avaliação.de.desempenho <- ordered(data$Avaliação.de.desempenho, 
                                        levels=c('INSATISFATORIO', 'BOM', 'OTIMO', 'EXCELENTE'))
#data$Turnover.mercado.factor <- as.factor(data$Turnover.mercado)

######## Analyses

# Turnover geral
plotTurnover <- ggplot(data) +
        geom_histogram(aes(x=Turnover.mercado, y=..density.., fill=TRUE), binwidth=0.01, colour="black") +
        geom_area(aes(x=Turnover.mercado, y=..density..), stat ='density', fill='yellow', colour='black', alpha=0.2) +
        stat_bin(binwidth=0.01, geom="text", aes(x=Turnover.mercado, y=..density.., label=..count..), vjust=-0.3, size=4) +
        geom_vline(aes(xintercept=0.19), colour='#990000', linetype = "longdash", size=1.0) +
        scale_x_continuous(limits=c(min(data$Turnover.mercado), 
                                    max(data$Turnover.mercado)), 
                           breaks=seq(min(data$Turnover.mercado), max(data$Turnover.mercado), 0.02)) +
                           #expand=c(0,0)) +
        scale_y_continuous(limits=c(0, 8), 
                           breaks=seq(0, 8, 2), 
                           expand=c(0,0)) +
        theme(legend.position="none") +
        xlab("Turnover de mercado") +
        ylab("Densidade")
plotTurnover

# 
