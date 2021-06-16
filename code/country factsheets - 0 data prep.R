library(gtalibrary)
library(ggplot2)
library(data.table)
library(splitstackshape)
library(tidyverse)
library(openxlsx)
gta_colour_palette()

gta_setwd()

project.path="0 projects/57 SECO C4TP/"


## setup
seco.country="Serbia"
result.path=paste0(project.path,"results/",seco.country,"/")
xlsx.path=paste0(result.path, "GTA data for ",seco.country, ".xlsx")
data.path=paste0(project.path, "data/")

inst.traditional=c(int.mast.types$intervention.type[int.mast.types$is.murky==F & int.mast.types$mast.chapter.id!="D"], "Internal taxation of imports", "Import ban", "Import licensing requirement")
inst.traditional=inst.traditional[! inst.traditional %in% c("Instrument unclear")]
inst.trade.defense=int.mast.types$intervention.type[int.mast.types$mast.chapter.id=="D"]
inst.subsidies=int.mast.types$intervention.type[int.mast.types$mast.chapter.id=="L"]
inst.procurement=int.mast.types$intervention.type[int.mast.types$mast.chapter.id=="M"]
inst.export.barrier=int.mast.types$intervention.type[int.mast.types$mast.chapter.id=="P" & int.mast.types$is.export.promotion ==F]
inst.export.promotion=int.mast.types$intervention.type[int.mast.types$is.export.promotion ==T]
inst.localisation=c("Labour market access", "Localisation incentive", "Local operations", "Local sourcing", "Local labour")
inst.other=c("Import-related non-tariff measure, nes", "Instrument unclear", "Trade balancing measure", "Trade payment measure")
inst.all=unique(c(inst.export.barrier, inst.export.promotion, inst.localisation, inst.procurement, inst.subsidies, inst.trade.defense, inst.traditional, inst.other))
int.mast.types$intervention.type[!int.mast.types$intervention.type %in% inst.all]



exports.base = data.frame(i.un = numeric(), a.un = numeric(), hs6 = numeric(), trade.value = numeric(), Year = numeric())

## loading country trade data
for (i in (2016:2020)){
  gta_trade_value_bilateral(exporting.country = seco.country,
                            keep.exporter=T,
                            trade.data=i)
  trade.base.bilateral = trade.base.bilateral%>% mutate(Year = i)
  exports.base = rbind(exports.base, trade.base.bilateral)
}


gta_trade_value_bilateral(importing.country = seco.country,
                          keep.importer = T,
                          trade.data=2020)
imports=trade.base.bilateral


# Top ten exporting destinations (for 2020 values)
exports = exports.base%>% group_by(i.un,Year)%>% summarize(`Total exports` = sum(trade.value))
countries = head(unique(exports$i.un[order(exports$`Total exports`, exports$Year, decreasing = TRUE)]), 10)
exports = exports[exports$i.un %in% countries, ]
exports.2 = spread(exports, key = Year, value = `Total exports`)
exports.2$i.un = gtalibrary::country.names$name[gtalibrary::country.names$un_code %in% exports.2$i.un]
exports.2 = exports.2[order(-exports.2$`2020`), ]
exports.2 = data.frame(exports.2)
names(exports.2) = c("Export destination", "2016", "2017", "2018", "2019", "2020")
xlsx::write.xlsx(exports.2, file=xlsx.path, sheetName = "Top 10 export destinations", row.names = F)



## top sectors

## top origins



## (1) GTA intervention counts
gta_data_slicer(affected.country = seco.country,
                keep.affected = T,
                keep.others = F)

master.sliced$currently.in.force="No"
master.sliced$currently.in.force[master.sliced$date.implemented<=Sys.Date() & (is.na(master.sliced$date.removed) | master.sliced$date.removed>=Sys.Date())]="Yes"

master.sliced$instrument.group="Other"
master.sliced$instrument.group[master.sliced$intervention.type %in% inst.export.barrier]="Export barrier"
master.sliced$instrument.group[master.sliced$intervention.type %in% inst.traditional]="Traditional insrument"
master.sliced$instrument.group[master.sliced$intervention.type %in% inst.trade.defense]="Trade defense"
master.sliced$instrument.group[master.sliced$intervention.type %in% inst.export.promotion]="Export incentive"
master.sliced$instrument.group[master.sliced$intervention.type %in% inst.procurement]="Public procurement"
master.sliced$instrument.group[master.sliced$intervention.type %in% inst.subsidies]="Subsidies"
master.sliced$instrument.group[master.sliced$intervention.type %in% inst.localisation]="Localisation requirements"

gta.data=unique(master.sliced[,c("state.act.id","intervention.id", "implementing.jurisdiction","gta.evaluation",
                                 "title","mast.chapter","instrument.group","intervention.type", "implementation.level",
                                 "currently.in.force", "date.announced","date.implemented","date.removed",
                                 "affected.sector","affected.product")])


## Total interventions per year
int.per.year = expand.grid(Year = c(2009:year(Sys.Date())))
 
for (date in unique(int.per.year$Year)){

  int.per.year$`Total number of interventions`[int.per.year$Year==date] = length(unique(subset(gta.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date))$intervention.id))
  int.per.year$`of which liberalising`[int.per.year$Year==date] = length(unique(subset(gta.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date) & gta.evaluation=="Green")$intervention.id))
  int.per.year$`of which harmful`[int.per.year$Year==date] = length(unique(subset(gta.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date) & gta.evaluation!="Green")$intervention.id))
}

xlsx::write.xlsx(int.per.year, file=xlsx.path, sheetName = "Total interventions, annual", row.names = F, append = T)


# Total interventions per year for main markets (US, Eu-27 and China) and top 5 importing jurisdictions

countries.5 = countries[1:5]
top.five.countries = countries.5[!(countries.5 %in% c(840, 156))]
top.five.countries = gtalibrary::country.names$name[gtalibrary::country.names$un_code %in% top.five.countries]

int.per.year.country = expand.grid(Year = c(2009:year(Sys.Date())), `Main markets` = c("United States of America", "China", top.five.countries))
#re add EU afterwards on its own

for (date in unique(int.per.year.country$Year)){
  for (country in unique(int.per.year.country$`Main markets`)){
    
    int.per.year.country$`Total number of interventions`[int.per.year.country$Year==date & int.per.year.country$`Main markets`==country] = length(unique(subset(gta.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date) & implementing.jurisdiction==country)$intervention.id))
    int.per.year.country$`of which liberalising`[int.per.year.country$Year==date & int.per.year.country$`Main markets`==country] = length(unique(subset(gta.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date) & gta.evaluation=="Green" & implementing.jurisdiction==country)$intervention.id))
    int.per.year.country$`of which harmful`[int.per.year.country$Year==date & int.per.year.country$`Main markets`==country] = length(unique(subset(gta.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date) & gta.evaluation!="Green" & implementing.jurisdiction==country)$intervention.id))
  }
}

#EU-27 chart
eu.data = gta.data[gta.data$implementing.jurisdiction %in% gtalibrary::country.names$name[country.names$is.eu==T],]
eu.data = eu.data[eu.data$implementing.jurisdiction!="United Kingdom", ]
int.per.year.eu = expand.grid(Year = c(2009:year(Sys.Date())), `Main markets` = "EU-27")

for (date in unique(int.per.year.eu$Year)){
    
  int.per.year.eu$`Total number of interventions`[int.per.year.eu$Year==date] = length(unique(subset(eu.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date))$intervention.id))
  int.per.year.eu$`of which liberalising`[int.per.year.eu$Year==date] = length(unique(subset(eu.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date) & gta.evaluation=="Green")$intervention.id))
  int.per.year.eu$`of which harmful`[int.per.year.eu$Year==date] = length(unique(subset(eu.data, year(date.implemented)<=date & (is.na(year(date.removed)) | year(date.removed)>date) & gta.evaluation!="Green")$intervention.id))
}

int.per.year.country = rbind(int.per.year.country, int.per.year.eu)

xlsx::write.xlsx(int.per.year.country, file=xlsx.path, sheetName = "Total interventions, annual", row.names = F, append = T)


# Interventions per year

new.int.per.year=merge(data.frame(year=c(2009:year(Sys.Date()))),
                   aggregate(intervention.id  ~ year(date.implemented), gta.data, function(x) length(unique(x))),
                   by.x="year", by.y="year(date.implemented)", all.x=T)

new.int.per.year=merge(new.int.per.year,
                   aggregate(intervention.id  ~ year(date.implemented), subset(gta.data, gta.evaluation=="Green"), function(x) length(unique(x))),
                   by.x="year", by.y="year(date.implemented)", all.x=T)

new.int.per.year=merge(new.int.per.year,
                   aggregate(intervention.id  ~ year(date.implemented), subset(gta.data, gta.evaluation!="Green"), function(x) length(unique(x))),
                   by.x="year", by.y="year(date.implemented)", all.x=T)

new.int.per.year[is.na(new.int.per.year)]=0
names(new.int.per.year)=c("Year", "Total number of newly implemented interventions", "of which liberalising","of which harmful")


xlsx::write.xlsx(new.int.per.year, file=xlsx.path, sheetName = "New interventions, annual", row.names = F, append = T)


# interventions per instrument group

int.per.group=merge(aggregate(intervention.id  ~ instrument.group, gta.data, function(x) length(unique(x))),
                    merge(aggregate(intervention.id  ~ instrument.group, subset(gta.data, gta.evaluation=="Green"), function(x) length(unique(x))), 
                          aggregate(intervention.id  ~ instrument.group, subset(gta.data, gta.evaluation!="Green"), function(x) length(unique(x))),
                          by="instrument.group", all.x=T),
                    by="instrument.group", all.x=T)

int.per.group=merge(int.per.group, 
                    merge(aggregate(intervention.id  ~ instrument.group, subset(gta.data, currently.in.force=="Yes"), function(x) length(unique(x))),
                          merge(aggregate(intervention.id  ~ instrument.group, subset(gta.data, gta.evaluation=="Green" & currently.in.force=="Yes"), function(x) length(unique(x))), 
                                aggregate(intervention.id  ~ instrument.group, subset(gta.data, gta.evaluation!="Green" & currently.in.force=="Yes"), function(x) length(unique(x))),
                                by="instrument.group", all.x=T),
                          by="instrument.group", all.x=T),
                    by="instrument.group", all.x=T)  
int.per.group[is.na(int.per.group)]=0
names(int.per.group)=c("Instrument group","Total implemented since 2009","of which liberalising","of which harmful",
                       "Total in force today","of which liberalising","of which harmful")

xlsx::write.xlsx(int.per.group, file=xlsx.path, sheetName = "Interventions per group", row.names = F, append=T)


## interventions per implementer
int.per.ij=merge(aggregate(intervention.id  ~ implementing.jurisdiction, gta.data, function(x) length(unique(x))),
                  merge(aggregate(intervention.id  ~ implementing.jurisdiction, subset(gta.data, gta.evaluation=="Green"), function(x) length(unique(x))), 
                        aggregate(intervention.id  ~ implementing.jurisdiction, subset(gta.data, gta.evaluation!="Green"), function(x) length(unique(x))),
                        by="implementing.jurisdiction", all.x=T),
                  by="implementing.jurisdiction", all.x=T)

int.per.ij=merge(int.per.ij, 
                  merge(aggregate(intervention.id  ~ implementing.jurisdiction, subset(gta.data, currently.in.force=="Yes"), function(x) length(unique(x))),
                        merge(aggregate(intervention.id  ~ implementing.jurisdiction, subset(gta.data, gta.evaluation=="Green" & currently.in.force=="Yes"), function(x) length(unique(x))), 
                              aggregate(intervention.id  ~ implementing.jurisdiction, subset(gta.data, gta.evaluation!="Green" & currently.in.force=="Yes"), function(x) length(unique(x))),
                              by="implementing.jurisdiction", all.x=T),
                        by="implementing.jurisdiction", all.x=T),
                  by="implementing.jurisdiction", all.x=T)  
int.per.ij[is.na(int.per.ij)]=0
names(int.per.ij)=c("Implementing jurisdiction","Total implemented since 2009","of which liberalising","of which harmful",
                     "Total in force today","of which liberalising","of which harmful")

xlsx::write.xlsx(int.per.ij, file=xlsx.path, sheetName = "Interventions per implementer", row.names = F, append=T)


## interventions per sector
gta.data.sec=unique(cSplit(gta.data, which(names(gta.data)=="affected.sector"), direction = "long", sep=","))
int.sec=subset(cpc.names, cpc.digit.level==3)[,c("cpc","cpc.name")]

int.sec=merge(int.sec, 
              merge(aggregate(intervention.id  ~ affected.sector, subset(gta.data.sec), function(x) length(unique(x))),
                    merge(aggregate(intervention.id  ~ affected.sector, subset(gta.data.sec, gta.evaluation=="Green" ), function(x) length(unique(x))), 
                          aggregate(intervention.id  ~ affected.sector, subset(gta.data.sec, gta.evaluation!="Green" ), function(x) length(unique(x))),
                          by="affected.sector", all.x=T),
                    by="affected.sector", all.x=T),
              by.x="cpc", by.y="affected.sector")


int.sec=merge(int.sec, 
              merge(aggregate(intervention.id  ~ affected.sector, subset(gta.data.sec, currently.in.force=="Yes"), function(x) length(unique(x))),
                    merge(aggregate(intervention.id  ~ affected.sector, subset(gta.data.sec, gta.evaluation=="Green" & currently.in.force=="Yes"), function(x) length(unique(x))), 
                          aggregate(intervention.id  ~ affected.sector, subset(gta.data.sec, gta.evaluation!="Green" & currently.in.force=="Yes"), function(x) length(unique(x))),
                          by="affected.sector", all.x=T),
                    by="affected.sector", all.x=T),
              by.x="cpc", by.y="affected.sector", all.x=T)


int.sec[is.na(int.sec)]=0
int.sec$cpc[nchar(int.sec$cpc)==2]=paste0("0", int.sec$cpc[nchar(int.sec$cpc)==2])

names(int.sec)=c("Sector code (CPC 2.1)", "Sector name","Total implemented since 2009","of which liberalising","of which harmful",
                     "Total in force today","of which liberalising","of which harmful")

xlsx::write.xlsx(int.sec, file=xlsx.path, sheetName = "Interventions per sector", row.names = F, append=T)



## interventions per product
gta.data.hs=unique(cSplit(gta.data, which(names(gta.data)=="affected.product"), direction = "long", sep=","))
int.hs=hs.names
int.hs$HS12code=as.numeric(int.hs$HS12code)

int.hs=merge(int.hs, 
             merge(aggregate(intervention.id  ~ affected.product, subset(gta.data.hs), function(x) length(unique(x))),
                   merge(aggregate(intervention.id  ~ affected.product, subset(gta.data.hs, gta.evaluation=="Green" ), function(x) length(unique(x))), 
                         aggregate(intervention.id  ~ affected.product, subset(gta.data.hs, gta.evaluation!="Green" ), function(x) length(unique(x))),
                         by="affected.product", all.x=T),
                   by="affected.product", all.x=T),
             by.x="HS12code", by.y="affected.product")


int.hs=merge(int.hs, 
             merge(aggregate(intervention.id  ~ affected.product, subset(gta.data.hs, currently.in.force=="Yes"), function(x) length(unique(x))),
                   merge(aggregate(intervention.id  ~ affected.product, subset(gta.data.hs, gta.evaluation=="Green" & currently.in.force=="Yes"), function(x) length(unique(x))), 
                         aggregate(intervention.id  ~ affected.product, subset(gta.data.hs, gta.evaluation!="Green" & currently.in.force=="Yes"), function(x) length(unique(x))),
                         by="affected.product", all.x=T),
                   by="affected.product", all.x=T),
             by.x="HS12code", by.y="affected.product", all.x=T)


int.hs[is.na(int.hs)]=0
int.hs$HS12code[nchar(int.hs$HS12code)==5]=paste0("0", int.hs$HS12code[nchar(int.hs$HS12code)==5])

names(int.hs)=c("Product code (HS 2012)", "Product name","Total implemented since 2009","of which liberalising","of which harmful",
                "Total in force today","of which liberalising","of which harmful")

xlsx::write.xlsx(int.hs, file=xlsx.path, sheetName = "Interventions per product", row.names = F, append=T)




## (2a) export coverage statistics - harmful

## all instruments
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.all, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","all.instruments")
trade.coverage=trade.coverage.estimates
trade.coverage$exporter=NULL
trade.coverage$nr.interventions=NULL

## all instruments excl. export incentives
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.all[! inst.all %in% inst.export.promotion], 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","all.instruments.noes")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","all.instruments.noes")],
                     by="importer", all=T)


## traditional
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.traditional, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","traditional.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","traditional.instruments")],
                     by="importer", all=T)


## trade defense
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.trade.defense, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","defense.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","defense.instruments")],
                     by="importer", all=T)


## subsidies
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.subsidies, 
                   keep.type = T,
                   affected.flows = "inward")
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","subsidy.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","subsidy.instruments")],
                     by="importer", all=T)


## procurement
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.procurement, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","procurement.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","procurement.instruments")],
                     by="importer", all=T)


## localisation
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.localisation, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","localisation")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","localisation")],
                     by="importer", all=T)


## export subsidy
gta_trade_coverage(gta.evaluation=c("red","amber"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.export.promotion, 
                   keep.type = T,
                   affected.flows = "outward subsidy")
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","export.subsidy")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","export.subsidy")],
                     by="importer", all=T)

trade.coverage[is.na(trade.coverage)]=0

trade.coverage.xlsx=trade.coverage
names(trade.coverage.xlsx)=c("Importer","All instruments", "All instruments excl. export incentives","Traditional instruments","Trade defense","Domestic subsidies",
                             "Procurement measures", "Localisation measures","Export incentives")

xlsx::write.xlsx(trade.coverage.xlsx, file=xlsx.path, sheetName = "Export coverage (harmful)", append=T, row.names = F)
rm(trade.coverage.xlsx, trade.coverage)


## (2b) export coverage statistics - liberalising

## all instruments
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.all, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","all.instruments")
trade.coverage=trade.coverage.estimates
trade.coverage$exporter=NULL
trade.coverage$nr.interventions=NULL


## all instruments excl. export incentives
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.all[! inst.all %in% inst.export.promotion], 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","all.instruments.noes")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","all.instruments.noes")],
                     by="importer", all=T)

## traditional
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.traditional, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","traditional.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","traditional.instruments")],
                     by="importer", all=T)


## trade defense
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.trade.defense, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","defense.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","defense.instruments")],
                     by="importer", all=T)


## subsidies
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.subsidies, 
                   keep.type = T,
                   affected.flows = "inward")
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","subsidy.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","subsidy.instruments")],
                     by="importer", all=T)


## procurement
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.procurement, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","procurement.instruments")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","procurement.instruments")],
                     by="importer", all=T)


## localisation
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.localisation, 
                   keep.type = T)
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","localisation")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","localisation")],
                     by="importer", all=T)


## export subsidy
gta_trade_coverage(gta.evaluation=c("green"),
                   exporter=seco.country,
                   keep.exporters = T,
                   group.importers = F,
                   coverage.period = c(2021,2021),
                   intervention.types = inst.export.promotion, 
                   keep.type = T,
                   affected.flows = "outward subsidy")
names(trade.coverage.estimates)=c("importer", "exporter","nr.interventions","export.subsidy")
trade.coverage=merge(trade.coverage,
                     trade.coverage.estimates[,c("importer","export.subsidy")],
                     by="importer", all=T)

trade.coverage[is.na(trade.coverage)]=0

trade.coverage.xlsx=trade.coverage
names(trade.coverage.xlsx)=c("Importer","All instruments", "All instruments excl. export incentives","Traditional instruments","Trade defense","Domestic subsidies",
                             "Procurement measures", "Localisation measures","Export incentives")

xlsx::write.xlsx(trade.coverage.xlsx, file=xlsx.path, sheetName = "Export coverage (liberalising)", append=T, row.names = F)


# Export gap analysis #
## top 10 destinations
countries
## top 10 products
exports.products = exports.base%>% group_by(hs6,Year)%>% summarize(`Total exports per product` = sum(trade.value))
products = head(unique(exports.products$hs6[order(exports.products$`Total exports per product`, exports.products$Year, decreasing = TRUE)]), 10)
exports.products = exports.products[exports.products$hs6 %in% products, ]
exports.products = exports.products[exports.products$Year==2020, ]
export.products.other = exports.base%>% group_by(hs6,Year,i.un)%>% summarize(`Total exports per product` = sum(trade.value))
exports.base.2020 = exports.base[exports.base$Year==2020,]
#imports of top 10 products for the top 10 destinations
gta_trade_value_bilateral(importing.country = countries,
                          keep.importer = T,
                          hs.codes = products,
                          keep.hs = T,
                          trade.data = 2020, 
                          df.name = "source.2020")
#find out which tuples are missing
`%notin%` = Negate(`%in%`)
export.gap = data.frame(`export destination` = character(),`export product name` = character(),	
                        `global product export value` = numeric(),	`exports 2018` = numeric(),	`exports 2019` = numeric(),	
                        `exports 2020` = numeric(), `source in 2020` = character())
source.country.2020 = c()

for (country in countries){
  for(product in products[products %notin% exports.base.2020$hs6[exports.base.2020$i.un==country & exports.base.2020$Year==2020]]){
    new = data.frame(`export destination` = gtalibrary::country.names$name[country.names$un_code==country],
                    #`export destination` = country,
                     `export product name` = gtalibrary::hs.codes$hs.description[hs.codes$hs.code==product],
                     `global product export value` = exports.products$`Total exports per product`[exports.products$hs6 == product],
                     `exports 2018` = if(country %in% export.products.other$i.un[export.products.other$Year==2018 & export.products.other$hs6==product]){
                       export.products.other$`Total exports per product`[export.products.other$Year==2018 & export.products.other$i.un==country & export.products.other$hs6==product]
                     }else {0},
                     `exports 2019` = if(country %in% export.products.other$i.un[export.products.other$Year==2019 & export.products.other$hs6==product]){
                       export.products.other$`Total exports per product`[export.products.other$Year==2019 & export.products.other$i.un==country & export.products.other$hs6==product]
                     }else {0},
                     `exports 2020` = 0,
                     `source in 2020` = NA)
    #adding the sourcing of this product in 2020 from other countries 
    if(country %in% source.2020$i.un[source.2020$hs6==product]){
      source.country.2020 = source.2020$a.un[source.2020$i.un==country & source.2020$hs6==product]
      source.country.2020 = gtalibrary::country.names$name[country.names$un_code %in% source.country.2020]
      new$source.in.2020 = as.character(paste(source.country.2020, collapse = ", "))
    }else {new$source.in.2020 = "No imports for this product in 2020"}
    export.gap = rbind(export.gap, new)
  }
}

xlsx::write.xlsx(export.gap, file=xlsx.path, sheetName = "Export gap analysis", append=T, row.names = F)


## (3) GTA interventions

gta.data$gta.url=paste0("http://www.globaltradealert.org/state-act/", gta.data$state.act.id)
gta.data.xlsx=gta.data
names(gta.data.xlsx)=c("State Act ID","Intervention ID", "Implementing jurisdiction","GTA evaluation",
                       "Title","MAST chapter","Instrument group","Instrument type", "Implementation level",
                       "Currently in force", "Date announced","Date implemented","Date removed",
                       "Affected sector","Affected product", "GTA entry URL")

xlsx::write.xlsx(gta.data.xlsx, file=xlsx.path, sheetName = "GTA entries", append=T, row.names = F, showNA = F)


file.copy("0 projects/57 SECO C4TP/0 country assessment note/GTA country assessment support - accompanying note.pdf",
          paste0(result.path,"GTA country assessment support - accompanying note.pdf"), overwrite = T)



#######
save(gta.data, path = data.path, file = paste0(data.path, seco.country, ".Rdata"))

