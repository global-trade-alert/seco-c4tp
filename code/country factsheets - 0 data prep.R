library(gtalibrary)
library(ggplot2)
library(data.table)
library(splitstackshape)
gta_colour_palette()

gta_setwd()

project.path="0 projects/57 SECO C4TP/"


## setup
seco.country="Peru"
result.path=paste0(project.path,"results/",seco.country,"/")
xlsx.path=paste0(result.path, "GTA data for ",seco.country, ".xlsx")

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



## loading country trade data
gta_trade_value_bilateral(exporting.country = seco.country,
                          keep.exporter=T,
                          trade.data=2020)
exports=trade.base.bilateral

gta_trade_value_bilateral(importing.country = seco.country,
                          keep.importer = T,
                          trade.data=2020)
imports=trade.base.bilateral


## top products



## top sectors

## top origins

## top destinations


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

## interventions per year
int.per.year=merge(data.frame(year=c(2009:year(Sys.Date()))),
                   aggregate(intervention.id  ~ year(date.implemented), gta.data, function(x) length(unique(x))),
                   by.x="year", by.y="year(date.implemented)", all.x=T)

int.per.year=merge(int.per.year,
                   aggregate(intervention.id  ~ year(date.implemented), subset(gta.data, gta.evaluation=="Green"), function(x) length(unique(x))),
                   by.x="year", by.y="year(date.implemented)", all.x=T)

int.per.year=merge(int.per.year,
                   aggregate(intervention.id  ~ year(date.implemented), subset(gta.data, gta.evaluation!="Green"), function(x) length(unique(x))),
                   by.x="year", by.y="year(date.implemented)", all.x=T)

int.per.year[is.na(int.per.year)]=0
names(int.per.year)=c("Year", "Total number of newly implemented interventions", "of which liberalising","of which harmful")

xlsx::write.xlsx(int.per.year, file=xlsx.path, sheetName = "New interventions, annual", row.names = F)


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
                     trade.coverage.estimates[,c("importer","traditional.instruments")],
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
                     trade.coverage.estimates[,c("importer","traditional.instruments")],
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


