#Pakiety które wykorzystalem
install.packages("ggplot2")
install.packages("sjmisc")
library("ggplot2")
library("dplyr")
library("tidyr")
library(tibble)
library(sjmisc)
#wczytuje i konwertuje pliki csv

r2006 <-  read.csv2("2006.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2007 <-  read.csv2("2007.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2008 <-  read.csv2("2008.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2009 <-  read.csv2("2009.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2010 <-  read.csv2("2010.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2011 <-  read.csv2("2011.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2012 <-  read.csv2("2012.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2013 <-  read.csv2("2013.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2014 <-  read.csv2("2014.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2015 <-  read.csv2("2015.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2016 <-  read.csv2("2016.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2017 <-  read.csv2("2017.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2018 <-  read.csv2("2018.csv",encoding = iconv('UTF-8', 'UTF-8'))
r2019 <-  read.csv2("2019.csv",encoding = iconv('UTF-8', 'UTF-8'))

#łącze dataframe

a <- bind_cols(r2006,r2007)
b <- bind_cols(a,r2008)
c <- bind_cols(b,r2009)
d <- bind_cols(c,r2010)
e <- bind_cols(d,r2011)
f <- bind_cols(e,r2012)
g <- bind_cols(f,r2013)
h <- bind_cols(g,r2014)
i <- bind_cols(h,r2015)
j <- bind_cols(i,r2016)
k <- bind_cols(j,r2017)
l <- bind_cols(k,r2018)
m <- bind_cols(l,r2019)

#Tworze zmienną zawierającą date/co miesiąc

date <- seq(as.Date("2006-01-01"), as.Date("2019-12-01"), by = "month")
date_rajstopy <- seq(as.Date("2012-01-01"),as.Date("2019-12-01"),by  = "month")
date_jaja <- seq(as.Date("2019-01-01"),as.Date("2019-12-01"),by  = "month")
date_karp <- seq(as.Date("2006-01-01"), as.Date("2016-12-01"), by = "month")
date_karp2 <- seq(as.Date("2018-01-01"), as.Date("2019-12-01"), by = "month")
date_karp3 <- c(date_karp,date_karp2)
#Tworze zmienne zawierające średnią cene poszczegulnych produktów

bulka <- (m %>% select(contains("bułka"))  %>%  colMeans())
karp <- (m %>% select(contains("karp"))  %>% colMeans()) 
jaja <- (m %>% select(contains("jaja.kurze.świeże..chów.wolnowybiegowy"))  %>% colMeans())
maslo <- (m %>% select(contains("masło"))  %>% colMeans()) 
pasta <- (m %>% select(contains("pasta")) %>% colMeans())
margaryna <- (m %>% select(contains("margaryna"))  %>% colMeans()) 
olej <- (m %>% select(contains("olej"))  %>% colMeans()) 
rajstopy <- (m %>% select(contains("rajstopy.damskie.gładkie"))  %>% colMeans()) 
garnitur <- (m %>% select(contains("garnituru"))  %>% colMeans()) 
woda <- (m %>% select(contains("woda"))  %>% colMeans() )

#Tworze data.frame z datą/ produktami zawierającymi pełne dane
data_rajstopy = data.frame(rajstopy, date_rajstopy)
data_jaja = data.frame(jaja, date_jaja)
data_karp = data.frame(karp, date_karp3)
data_zywnosc = data.frame(bulka, maslo, pasta, margaryna, olej, garnitur, woda, date,row.names = NULL)

data_zywnosc = data.frame(bulka, maslo, pasta, margaryna, olej, garnitur, woda, date)

data_zywnosc_long <- data_zywnosc %>% gather(c("bulka","maslo","pasta","margaryna","olej","garnitur","woda"),key="Produkty",value="Value")

# Wykres dla wiekszosci towarów
data_zywnosc_long %>% ggplot(aes(x= date,y=Value,col=Produkty,group=Produkty))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena[zl]",
       x = "czas",
       title = "Ceny produktów w czasie w Polsce")
# Wykresy dla poszczegulnych towarów
ggplot(data_zywnosc, aes(x = date, y = bulka))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena bułki pszennej",
       x = "czas",
       title = "Cena bułki pszennej w czasie")

ggplot(data_karp, aes(x = date_karp3, y = karp))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena karpia",
       x = "czas",
       title = "Cena karpia  w czasie")

ggplot(data_jaja, aes(x = date_jaja, y = jaja))+
  theme_bw() +
  geom_line()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena Jaja kurze świerze (chów wolnowybiegowy) za 1 szt",
       x = "czas",
       title = "Cena Jaja kurze świerze (chów wolnowybiegowy) za 1 szt w czasie")

ggplot(data_zywnosc, aes(x = date, y = maslo))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena masła świeżego  w czasie",
       x = "czas",
       title = "Cena masła świeżego o zawartości tłuszczu ok. 82,5% za 200 g")

ggplot(data_zywnosc, aes(x = date, y = pasta))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena Pasty do zębów",
       x = "czas",
       title = "Cena Pasta do zębów w czasie")

ggplot(data_zywnosc, aes(x = date, y = margaryna))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena Margaryny za 400 g",
       x = "czas",
       title = "Cena Margaryny za 400 g w czasie")

ggplot(data_zywnosc, aes(x = date, y = olej))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena Oleju rzepakowego produkcji krajowej",
       x = "czas",
       title = "Cena Oleju rzepakowego produkcji krajowej w czasie")

ggplot(data_rajstopy,aes(x = date_rajstopy, y = rajstopy))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena Rajstopy damskie gładkie, 15 den",
       x = "czas",
       title = "Cena Rajstopy damskie gładkie, 15 den w czasie")

ggplot(data_zywnosc, aes(x = date, y = garnitur))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena Oczyszczanie chemiczne garnituru męskiego 2-częściowego - za 1kpl",
       x = "czas",
       title = "Cena Oczyszczanie chemiczne garnituru męskiego 2-częściowego - za 1kpl w czasie")

ggplot(data_zywnosc, aes(x = date, y = woda))+
  theme_bw() +
  geom_point()+
  geom_vline(xintercept = as.Date(date[124]),colour = "red")+
  labs(y = "Cena Ciepła woda",
       x = "czas",
       title = "Cena Ciepłej wody w czasie")

#tworze ramke lata/srednie wartosci towarów

r2007cut <- r2007 %>%  select((contains('a')))
r2007cut <- rotate_df(r2007cut, cn = TRUE)
r2007cut <- r2007cut %>% colMeans()

r2008cut <- r2008 %>%  select((contains('a')))
r2008cut <- rotate_df(r2008cut, cn = TRUE)
r2008cut <- r2008cut %>% colMeans()

r2006cut <- r2006 %>%  select((contains('a')))
r2006cut <- rotate_df(r2006cut, cn = TRUE)
r2006cut <- r2006cut %>% colMeans()

r2009cut <- r2009 %>%  select((contains('a')))
r2009cut <- rotate_df(r2009cut, cn = TRUE)
r2009cut <- r2009cut %>% colMeans()

r2010cut <- r2010 %>%  select((contains('a')))
r2010cut <- rotate_df(r2010cut, cn = TRUE)
r2010cut <- r2010cut %>% colMeans()

r2011cut <- r2011 %>%  select((contains('a')))
r2011cut <- rotate_df(r2011cut, cn = TRUE)
r2011cut <- r2011cut %>% colMeans()

r2012cut <- r2012 %>%  select((contains('a')))
r2012cut <- rotate_df(r2012cut, cn = TRUE)
r2012cut <- r2012cut %>% colMeans()

r2013cut <- r2013 %>%  select((contains('a')))
r2013cut <- rotate_df(r2013cut, cn = TRUE)
r2013cut <- r2013cut %>% colMeans()

r2014cut <- r2014 %>%  select((contains('a')))
r2014cut <- rotate_df(r2014cut, cn = TRUE)
r2014cut <- r2014cut %>% colMeans()

r2015cut <- r2015 %>%  select((contains('a')))
r2015cut <- rotate_df(r2015cut, cn = TRUE)
r2015cut <- r2015cut %>% colMeans()

r2016cut <- r2016 %>%  select((contains('a')))
r2016cut <- rotate_df(r2016cut, cn = TRUE)
r2016cut <- r2016cut %>% colMeans()

r2017cut <- r2017 %>%  select((contains('a')))
r2017cut <- rotate_df(r2017cut, cn = TRUE)
r2017cut <- r2017cut %>% colMeans()

r2018cut <- r2018 %>%  select((contains('a')))
r2018cut <- rotate_df(r2018cut, cn = TRUE)
r2018cut <- r2018cut %>% colMeans()

r2019cut <- r2019 %>%  select((contains('a')))
r2019cut <- rotate_df(r2019cut, cn = TRUE)
r2019cut <- r2019cut %>% colMeans()

Rok2018 <- r2018cut

wojewodztwa <- c("Polska","Dolnoslaskie","Kujawsko-pomorskie","Lubelskie","Lubuskie","Lodzkie","Malopolskie","Mazowieckie","Opolskie","Podkarpackie","Podlaskie","Pomorskie","Slaskie","Swietokrzyskie","Warminsko-mazruskie","Wielkopolskie","Zachodniopomorskie")

lata_srednia <- data.frame(r2006cut,r2007cut,r2008cut,r2009cut,r2010cut,r2011cut,r2012cut,r2013cut,r2014cut,r2015cut,r2016cut,r2017cut,Rok2018,r2019cut,wojewodztwa)

max(lata_srednia)

min(lata_srednia)
roznicaProcentowa<-min(lata_srednia)/max(lata_srednia)*100

#Tworze wykres dla dwoch najwazniejszych lat

lata_srednia %>% ggplot(aes(x= wojewodztwa,y= r2006cut,fill=Rok2018))+
  theme_bw() +
  geom_col()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  labs(y = "Średnia cena produktów[zł]",
       x = "",
       title = "Ceny produktów w czasie - lata 2006 i 2018")

