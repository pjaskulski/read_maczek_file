# dane z serwisu dane.imgw.pl, średnia teperatura maksymalna i minimalna w zadanym miesiącu i roku

# lista stacji pogodowych:
#stacje <- read.table(file = "https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/wykaz_stacji.csv",
#                     header = FALSE,
#                     sep = ",",
#                     fileEncoding = 'WINDOWS-1250')

# pliki zip z dobowymi pomiarami z 2019 pobierane z:
# https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/2019/
#

# Pola: Kod stacji, Nazwa stacji, Rok, Miesiąc, Dzień, Maksymalna temperatura dobowa, 
# Status pomiaru TMAX, Minimalna temperatura dobowa, Status pomiaru TMIN, 
# średnia temperatura dobowa, Status pomiaru STD,
# Temperatura minimalna przy gruncie, Status pomiaru TMNG, Suma dobowa opadów, Status pomiaru SMDB, 
# rodzaj opadu [S/W/ ], Wysokość pokrywy śnieżnej, status pomiaru PKSW
#
# Status 8 - brak pomiaru, status 9 - brak zjawiska

calc_mean_month <- function(year, month) {
    
    kolumny <- c('KOD','NAZWA','ROK','MIESIAC','DZIEN', 'TMAX', 'TMAX_st','TMIN', 
                 'TMIN_st', 'TSTD', 'TSTD_st', 'TMNG', 'TMNG_st', 'SMDB', 'SMDB_st', 
                 'RODZ_OP', 'PKSW', 'PKSW_st')
    
    temp <- paste(year,"_",month, "_k.zip", sep="")
    if (!file.exists(temp)) {
        adres <- paste("https://dane.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/klimat/",year,"/",year,"_",month, "_k.zip", sep="")
        download.file(adres, temp)
    }
    month01 <- read.table(unz(temp, paste("k_d_", month,"_", year, ".csv", sep = "")), header = FALSE, sep = ",", fileEncoding = "WINDOWS-1250")
    
    colnames(month01) <- kolumny
    
    month01 <- within(month01, TMAX[TMAX_st == '8'] <- NA)
    month01 <- within(month01, TMIN[TMIN_st == '8'] <- NA)
    
    tmax <- mean(month01$TMAX, na.rm = TRUE)
    tmin <- mean(month01$TMIN, na.rm = TRUE)
    
    mean_values <- c(tmax, tmin)  
    return(mean_values)
}

january <- calc_mean_month("2019", "01")
sprintf("Styczeń 2019 - średnia temperatura maksymalna: %.2f °C, średnia temperatura minimalna: %.2f °C", january[1], january[2])
