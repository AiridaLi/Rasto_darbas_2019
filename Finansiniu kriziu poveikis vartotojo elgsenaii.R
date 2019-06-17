#################################################
#patikriname ar turime reikiamus paketus, jeigu ne - instaliuojame juos
#################################################
if(!require(eurostat))install.packages("eurostat")
require(eurostat)
if(!require(tidyverse))install.packages("tidyverse")
require(tidyverse)
#####################################################
#importuojame: Labour cost, wages and salaries, direct remuneration (excluding apprentices) by NACE Rev. 2 activity ) - LCS surveys 2008, 2012 and 2016
#################################################
lc_ncost_r2<-get_eurostat("lc_ncost_r2", stringsAsFactors = FALSE)
#################################################
##išfiltruojame atsisiųstus duomenis taikydami šiuos kriterijus:
#currency naudoju: Euro
#geo naudoju: European Union - 28 countries
#lcstruct naudoju: Wages and salaries (excluding apprentices)
#nace_r2 naudoju: Industry, construction and services (except public administration, defense, compulsory social security)
#sizeclas naudoju: total (neišskirstant pagal tam tikrus kriterijus)
#time naudoju: Visus pateiktus laikotarpius, todėl pagal juos nefiltruoju
#unit naudoju: Per employee in full-time equivalents, per year
#####################################################
df3<- lc_ncost_r2 %>% filter (
        currency=="EUR",
        geo=="EU28",
        lcstruct=="D111",
        nace_r2=="B-S_X_O",
        sizeclas=="TOTAL",
        unit=="P_SAL_Y"
)
#####################################################
#Kadangi išfiltravau duomenis taip kaip norėjau, tai dabar patikrinu kaip atrodo mano df3
#####################################################
df3
#####################################################
#time and values stulpelius nukopijuoju į https://www.tablesgenerator.com/ 
#ir sugeneruoju norimą lentelę, kurią naudosiu overleaf'e kuriant pdf dokumentą
#####################################################
#Kadangi lentelę pavyko sugeneruoti, tai dabar metas atlikti elementarius skaičiavimus, kurie padės aprašant lentelę:
#Būtų patogu tarkime 2012m. ir 2016m. values prisilyginti 100% ir tada paskaičiuoti keliais % buvo sumažėjas darbo užmokestis 2008m. 
#####################################################
D8<-100-(df3[3,8]*100/df3[1,8])
#Viršuje apskaičiavau keliais procentais sumažėjo 2008 values reikšmės palyginus su 2016 (15.61447)
D88<-100-(df3[3,8]*100/df3[2,8])
#Viršuje apskaičiavau keliais procentais sumažėjo 2008 values reikšmės palyginus su 2012 (7.434369)
#####################################################
#####################################################
#importuojame: HICP - inflation rate
#####################################################
tec00118<-get_eurostat("tec00118", strinAsFactors = FALSE)
#####################################################
##išfiltruojame atsisiųstus duomenis taikydami šiuos kriterijus:
#geo naudoju: European Union - 28 countries
#pagal likusius rodiklius nieko nefiltruoju, nes visa kita bus tinakam ggplot'o braižymui
#####################################################
df<- tec00118%>% filter (
        geo=="EU28")
#####################################################
#toliau braižome HICP - inflation rate grafiką
####################################################
ggplot(df, aes(x=time, y=values))+
        geom_line()+
        scale_x_date(breaks=seq(min(df$time), max(df$time),
                                by="12 months"), date_labels = "%Y")+
        theme(axis.text.x=element_text(angle=45, hjust=1))+
        labs(title="HICP - inflation rate",
             subtitle="Source: Eurostat (tec00118)",
             x="Time",
             y="inflation rate(EU - 28 countries)")
####################################################
#išsaugome guatą grafika .png formatu
####################################################
ggsave("HICP.png", width=100, height=80, units="mm")
####################################################
#Kadangi grafiką pavyko išsaugoti, tai dabar metas atlikti elementarius skaičiavimus, kurie padės trumpai komentuojant grafiką:
#Būtų patogu apskaičiuoti kaip nuo 2007m. iki 2008m. išaugo infliacija, nes šią info naudosiu pdf dokumente 
#####################################################
Ir<-df[2,5]-df[1,5]
#####################################################
#apskaičiavus gaunasi, kad infliacijos skirtumas tarp 2007m.ir 2008m.lygus 1.3
#####################################################
#####################################################
#importuojame domenis: Final consumption expenditure of households by consumption purpose (COICOP 3 digit)
#################################################
nama_10_co3_p3<-get_eurostat("nama_10_co3_p3", stringsAsFactors = FALSE)
#################################################
##išfiltruojame atsisiųstus duomenis taikydami šiuos kriterijus:
#coicop naudoju: Food and non-alcoholic beverages, Clothing, Transport, Housing, water, electricity, gas and other fuels
#geo naudoju: European Union - 28 countries
#time naudoju: 2006-2008
#unit naudoju: Percentage of total
#####################################################
df2008<- nama_10_co3_p3 %>% filter (
        coicop %in% c("CP01","CP031","CP07", "CP04"),
        geo=="EU28",
        time=="2008-01-01",
        unit=="PC_TOT"
)
#####################################################
#nusprendžiau, kad patogiau bus turėti 3 atskirus data frame'us su skirtingu laikotarpiu
#todėl naudodamasi prieš tai užrašytu filtravimu ir pakeisdama laikotarpį susigeneruoju likusius data frame
#####################################################
df2007<- nama_10_co3_p3 %>% filter (
        coicop %in% c("CP01","CP031","CP07", "CP04"),
        geo=="EU28",
        time=="2007-01-01",
        unit=="PC_TOT"
)
#####################################################
df2006 <- nama_10_co3_p3 %>% filter (
        coicop %in% c("CP01","CP031","CP07", "CP04"),
        geo=="EU28",
        time=="2006-01-01",
        unit=="PC_TOT"
)
#####################################################
######################################################
#Kadangi išfiltravau duomenis taip kaip norėjau, tai dabar patikrinu kaip atrodo mano data frame'ai
#####################################################
df2008
df2007
df2006
#####################################################
#time and values stulpelius nukopijuoju į https://www.tablesgenerator.com/ 
#ir sugeneruoju norimą lentelę, kurią naudosiu overleaf'e kuriant pdf dokumentą