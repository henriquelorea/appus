######### Importing modules
library(zoo)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyr)

######### Loading and prepping data
setwd('/home/pensa/Documents/appus')
data <- read.csv('questao1.csv', fileEncoding='ISO-8859-1', sep=';', dec=',', stringsAsFactors=F, strip.white=TRUE)
data_raw <- data

# Fixing date vectors
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

data$Data.de.admissão <- as.Date(sapply(data$Data.de.admissão, fixdate))
data$Data.de.nascimento <- as.Date(sapply(data$Data.de.nascimento, fixdate))
data$ano_admissao <- as.numeric(format(data$Data.de.admissão, "%Y"))

# Fixing salary vector
fixsalary <- function(x) {
  x <- str_replace_all(x, '[R$ .]', '')
  x <- as.numeric(str_replace(x, '[,]', '.'))
  return(x)
}

data$Salário.mensal.médio <- sapply(data$Salário.mensal.médio, fixsalary)

# Setting factors
data$Sexo <- as.factor(data$Sexo)
data$Cargo <- as.factor(data$Cargo)

data$Área <- as.factor(data$Área)
data$Avaliação.de.desempenho <- ordered(data$Avaliação.de.desempenho, 
                                        levels=c('INSATISFATORIO', 'BOM', 'OTIMO', 'EXCELENTE'))

# Roundup function returns integer for scale limit
roundup <- function(x, power=1) {
        
        while (x > 10) {
                x <- x/10
                power <- power*10
        }
        
        return (ceiling(x)*power) 
}

# Multiple plot function
# Source: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)

# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

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

######### Subsets

# Admissões e desligamentos por ano - soma acumulada
dfAdmissoes <- data.frame()
for (area in levels(data$Área)) {
        
        dfAno <-        data.frame(ano_admissao = rep(min(data$ano_admissao):max(data$ano_admissao)),
                                   area = area, 
                                   zero = 0)

        dfSummary <-    data[data$Área == area, ] %>%
                        mutate(admissao = 1) %>%
                        group_by(ano_admissao, Área) %>%
                        summarize(admitido = sum(admissao), desligado = sum(Desligamento)) %>%
                        mutate(area = Área) %>%
                        select(-Área)
        
        dfValor <-      dfAno %>%
                        left_join(dfSummary) %>%
                        mutate(admitido = ifelse(is.na(admitido), 0, admitido),
                               desligado = ifelse(is.na(desligado), 0, desligado)) %>% 
                        mutate(admitido = zero + admitido,
                               ligado = admitido - desligado) %>%
                        select(-zero, -admitido)

        dfValGather <-  dfValor %>%
                        gather(key = desligamento, 
                               value = valor, 
                               ligado, 
                               desligado)

        dfCumsum <-     dfValor %>%
                        mutate(ligado = cumsum(ligado),
                               desligado = cumsum(desligado))

        dfCumGather <-  dfCumsum %>%
                        gather(key = desligamento, 
                               value = cumsum, 
                               ligado, 
                               desligado)
        
        dfValCumsum <-  dfValGather %>%
                        left_join(dfCumGather) %>%
                        mutate(desligamento = ifelse(desligamento == 'ligado', 1, 0)) %>%
                        mutate(desligamento = as.factor(desligamento))

        dfAdmissoes <- rbind(dfAdmissoes, dfValCumsum)
        rm(area, dfAno, dfSummary, dfValor, dfValGather, dfCumsum, dfCumGather, dfValCumsum)
}

dfAdmissoes <- dfAdmissoes %>% arrange(area, desligamento, ano_admissao)
dfAdmissoes$area <- as.factor(dfAdmissoes$area)
dfAdmissoes$ano_admissao <- as.numeric(dfAdmissoes$ano_admissao)

######### Plots

# Histograma Área
ggplot(data[data$Desligamento==0,], aes(x=Área)) +
        stat_bin(aes(fill=Área)) + 
        stat_bin(geom='text', aes(label=..count..), vjust=-1.5) +
        stat_bin(geom='text', aes(label=paste0('(',round(..count../sum(..count..),3)*100, '%)'),vjust=-0.5, size=0.8)) +
        scale_y_continuous(limits=c(0, roundup(nrow(data[data$Desligamento==0 & data$Área=='Operacional',])))) +
        theme(legend.position="none")

# Area Plot
ggplot(dfAdmissoes) +
        geom_line(aes(x=ano_admissao, y=cumsum, colour=area), alpha=1)
#        geom_area(aes(x=ano_admissao, y=valor, fill=desligamento), position='stack', alpha=0.5)
        scale_y_continuous(limits=c(0, roundup(nrow(data[data$Desligamento==0 & data$Área=='Operacional',])))) +
#       theme(legend.position="none")

# Histograma data de nascimento
ggplot(data) +
geom_bar(aes(x=as.integer(format(Data.de.nascimento, '%Y'))), binwidth=2)

# Histograma data de admissão
ggplot(data) +
geom_bar(aes(x=as.integer(format(Data.de.admissão, '%Y'))), binwidth=2)

# Boxplot salários
ggplot(data) +
geom_boxplot(aes(x=Área, y=Salário.mensal.médio))

# Scatter data nasc x data adm ~ desligamento
ggplot(data) +
        geom_point(aes(x=Salário.mensal.médio, y=Turnover.mercado))

# Multiplot boxplots distancia e tempo por área
boxT <- ggplot(data) +
        geom_boxplot(aes(x=Área, y=Tempo.deslocamento..min., fill=Área)) +
        theme(legend.position="none")

boxD <- ggplot(data) +
        geom_boxplot(aes(x=Área, y=Distância.residência.trabalho..Km., fill=Área)) +
        theme(legend.position="none")

multiplot(boxD, boxT, cols=2)

# Scatter salário
ggplot(data) +
        geom_point(aes(y=Salário.mensal.médio, x=Distância.residência.trabalho..Km., colour=as.factor(Desligamento))) +
        theme(legend.position="none")



# Densidade de desligamento por ano de admissão
ggplot(data) +
  geom_density(aes(x=as.integer(format(Data.de.admissão, '%Y')), fill=Desligamento), binwidth=2, alpha=0.5, position = "stack")

# Densidade de desligamento por ano de nascimento
ggplot(data[data$Desligamento == 1, ]) +
  geom_bar(aes(x=as.integer(format(Data.de.nascimento, '%Y')), fill=Sexo), alpha=0.5)

# Scatter data nasc x data nasc ~ turnover
ggplot(data) +
  geom_point(aes(x=Data.de.admissão, y=Turnover.mercado, colour=Desligamento), binwidth=2)

# Histograma turnover
ggplot(data) +
  geom_point(aes(y=Turnover.mercado, x=Área, colour=))

##### Graph




dataAno <- data %>%
        group_by(Cargo) %>%
        mutate(a2015 = if(ano_admissao==2015) 1 else 0,
               aRest = if(ano_admissao<2015) 1 else 0) %>%
        summarize(total = n(),
                  t2015 = sum(a2015),
                  tAntes = sum(aRest),
                  total_desl = sum(Desligamento),
                  turnover = mean(Turnover.mercado))

                
                 total_desl = sum(Desligamento),
                 t2014 = count()[ano_admissao<2015]),
                 t2015 = n()[ano_admissao==2015])

c11 <- data[data$Cargo == 11, ] %>%
        arrange(Data.de.admissão) %>%
        mutate(adm = 1) %>%
        mutate(adm_cumsum = cumsum(adm),
               desl_cumsum = cumsum(Desligamento))
        
ggplot(c11) +
        geom_line(aes(x=Data.de.admissão, y=adm_cumsum)) +
        geom_point(aes(x=Data.de.admissão, y=adm_cumsum, colour = factor(Desligamento)))
        # geom_point(aes(x=Data.de.admissão, y=desl_cumsum)) +
        # geom_line(aes(x=Data.de.admissão, y=desl_cumsum))

c123 <- data[data$Cargo == 123, ] %>%
        arrange(Data.de.admissão) %>%
        mutate(adm = 1) %>%
        mutate(adm_cumsum = cumsum(adm),
               desl_cumsum = cumsum(Desligamento))

ggplot(c123) +
        geom_line(aes(x=Data.de.admissão, y=adm_cumsum)) +
        geom_point(aes(x=Data.de.admissão, y=adm_cumsum, colour = factor(Desligamento)))
# geom_point(aes(x=Data.de.admissão, y=desl_cumsum)) +
# geom_line(aes(x=Data.de.admissão, y=desl_cumsum))

cAll <- data %>%
        arrange(Data.de.admissão) %>%
        mutate(adm = 1) %>%
        mutate(adm_cumsum = cumsum(adm),
               desl_cumsum = cumsum(Desligamento))

ggplot(cAll) +
        geom_line(aes(x=Data.de.admissão, y=adm_cumsum)) +
        geom_point(data=cAll[cAll$Desligamento==1, ], aes(x=Data.de.admissão, y=adm_cumsum, colour = factor(Posição.crítica)))
# geom_point(aes(x=Data.de.admissão, y=desl_cumsum)) +
# geom_line(aes(x=Data.de.admissão, y=desl_cumsum))

