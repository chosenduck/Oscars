#Pacotes =======================================================================
if(!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)
if(!require(janitor)){install.packages("janitor")};library(janitor)
if(!require(skimr)){install.packages("skimr")};library(skimr)
if(!require(rio)){install.packages("rio")};library(rio)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(writexl)){install.packages("writexl")};library(writexl)

#Limpando todos os objetos atualmente ativos no Ambiente (Environment)
rm(list=ls(all=T))

#Configurando diretório ========================================================
getwd()
setwd()

##Abrindo base =================================================================
base <- read.csv("Oscars-demographics-DFE.csv")
names(base)
glimpse(base)

#Selecionando variáveis de interesse ===========================================
base_mod <- base %>% 
  select(id = X_unit_id, nacionalidade = birthplace, data_nasc = date_of_birth,
         raca_etnia = race_ethnicity, religiao = religion, orientacao_sex = sexual_orientation,
         ano_premiacao = year_of_award, premio = award, url_bio = biourl, 
         filme = movie, nome = person)

skim(base_mod)

#Transformando variáveis =======================================================
##Religiao =====================================================================
unique(base_mod$religiao)

base_mod <- base_mod %>% 
  mutate(religiao_mod = case_when(
    religiao %in% c('Roman Catholic', 'Presbyterian', 'Baptist', 'Anglican/Episcopalian', 
                    'Protestant', 'Lutheran', 'Quaker', 'Born-Again Christian', 'Christian Science', 
                    'Disciples of Christ', 'Christian', 'Methodist', 'Congregationalist') ~ 'Cristianismo',
    religiao %in% c('Deist', 'Atheist', 'Agnostic') ~ 'Sem Religião',
    religiao == 'Jewish' ~ 'Judaísmo',
    religiao == 'Hindu' ~ 'Hinduísmo',
    religiao == 'Sufism' ~ 'Islamismo',
  ))

tabyl(base_mod$religiao_mod)

##Sexualidade ==================================================================
unique(base_mod$orientacao_sex)

base_mod <- base_mod %>% 
  mutate(orientacao_sex_mod = case_when(
    orientacao_sex %in% c('Gay', 'Lesbian') ~ 'Homossexual',
    orientacao_sex == 'Straight' ~ 'Héterossexual',
    orientacao_sex == 'Bisexual' ~ 'Bissexual',
    orientacao_sex == 'Matter of Dispute' ~ 'NS/NR',
  ))

tabyl(base_mod$orientacao_sex_mod)

##Cor ou Raça ==================================================================
unique(base_mod$raca_etnia)

base_mod <- base_mod %>% 
  mutate(raca_etnia_mod = case_when(
    raca_etnia %in% c('Hispanic', 'Multiracial', 'Middle Eastern') ~ 'Outros',
    raca_etnia == 'White' ~ 'Branca',
    raca_etnia == 'Black' ~ 'Preta',
    raca_etnia == 'Asian' ~ 'Amarela',
  ))

tabyl(base_mod$raca_etnia_mod)

##Premio =======================================================================
unique(base_mod$premio)

base_mod <- base_mod %>% 
  mutate(premio_mod = case_when(
    premio == 'Best Actress' ~ 'Melhor Atriz',
    premio == 'Best Actor' ~ 'Melhor Ator',
    premio == 'Best Supporting Actor' ~ 'Melhor Ator Coadjuvante',
    premio == 'Best Director' ~ 'Melhor Diretor',
    premio == 'Best Supporting Actress' ~ 'Melhor Atriz Coadjuvante',
  ))

tabyl(base_mod$premio_mod)

##Nacionalidade ================================================================
unique(base_mod$nacionalidade)

base_mod <- base_mod %>% 
  mutate(nacionalidade_mod = case_when(
    nacionalidade %in% c('Chicago, Il','Salt Lake City, Ut','Cape Elizabeth, Me',
                         'Los Angeles, Ca','Pasadena, Ca','New York City',
                         'Nevada, Mo','Wilkes-Barre, Pa','Oakland, Ca',
                         'Lawrence, Ks','Winchester, In','St. Louis Park, Mn',
                         'Minneapolis, Mn','Detroit, Mi','Oak Park, Il',
                         'Brooklyn, Ny','Waxahachie, Tx','Santa Monica, Ca',
                         'Richmond, Va','North Bergen, Nj','Lafayette, In',
                         'Baltimore, Md','Lynwood, Ca','Baldwin, Ny',
                         'San Francisco, Ca','Cincinnati, Oh','Peekskill, Ny',
                         'Atlanta, Ga','Duncan, Ok','Flushing, Ny','Columbus, Oh',
                         'Philadelphia, Pa','Kansas City, Mo','Cadiz, Oh',
                         'Milwaukee, Wi','Indiana, Pa','Helena, Mt','Manhattan, Ny',
                         'Tacoma, Wa','Racine, Wi','Omaha, Ne',"O'fallon, Il",
                         'Hamden, Ct','Evanston, Il','La Jolla, Ca','Miami, Fl',
                         'Westhampton, Ny','Winterset, Ia','Wise, Va',
                         'San Bernardino, Ca','Newton, Ma','Mount Vernon, Ny',
                         'Yonkers, Ny','Grand Island, Ne','San Diego, Ca',
                         'Pittsburgh, Pa','Washington, Dc','Shaker Heights, Oh',
                         'New Brunswick, Nj','East Harlem, New York City',
                         'Concord, Ca','Long Beach, Ca','South Orange, Nj',
                         'Burbank, Ca','Terrell, Tx','Uvalde, Tx','Swampscott, Ma',
                         'Elizabeth, Nj','Walters, Ok','Savannah, Ga','Lima, Oh',
                         'Hoboken, Nj','Hunt City, Il','Norwood, Oh','Hartford, Ct',
                         'Macon, Ga','Bronx, Ny','Malden, Ma','St. Cloud, Mn',
                         'Shidler, Ok','Cleveland, Oh','Astoria, Ny','Malibu, Ca',
                         'Kenosha, Wi','St. Louis, Mo','Newark, Nj','Lattimer Mines, Pa',
                         'San Saba, Tx','Laurel, Ne','West Covina, Ca','Memphis, Tn',
                         'Lexington, Ky','Bossier City, La','Lowell, Ma','Independence, Mo',
                         'Tulsa, Ok','San Antonio, Tx','St. Joseph, Mo','Thomasville, Ga',
                         'Packard, Ky','Birmingham, Al','Bascom, Fl','Quitman, Tx',
                         'Summit, Nj','Kirksville, Mo','Morton Grove, Il','El Centro, Ca',
                         'Conyers, Ga','Cloquet, Mn','Queens, Ny','Culver City, Ca',
                         'Lincoln, Ne','Smyrna, Ga','Honolulu, Hi','New Orleans, La',
                         'Arlington, Va','Louisville, Ky','Litchfield, Mn','Wichita, Ks',
                         'Palmyra, Mo','Quincy, Il','Michigan City, In','Joliet, Il',
                         'Newtonville, Ma','Denison, Ia','Charleroi, Pa','Elmhurst, Ny',
                         'Hastings, Ne','Marblehead, Ma','Quincy, Ma','Silver Spring, Md',
                         'Des Moines, Ia','Old Westbury, Ny','Newport, Ar','Troy, Ny',
                         'Morristown, Nj','Wareham, Ma','Tenafly, Nj','Athens, Ga',
                         'Catskill Mountains, Ny','Katy, Tx','Woodlawn, Md',
                         'Montgomery, Al') ~ 'Estados Unidos',
    nacionalidade == 'Chisinau, Moldova' ~ 'Moldávia',
    nacionalidade %in% c('Glasgow, Scotland','Edinburgh, Scotland') ~ 'Escócia',
    nacionalidade %in% c('Bisacquino, Sicily, Italy','Parma, Emilia-Romagna, Italy',
                         'Misericordia, Arezzo, Tuscany, Italy','Rome, Italy',
                         'Pozzuoli, Italy') ~ 'Itália',
    nacionalidade %in% c('Mulhouse, Haut-Rhin, Alsace, France','Paris, France',
                         'Rueil-Malmaison, Hauts-De-Seine, France') ~ 'França',
    nacionalidade == 'Budapest, Hungary' ~ 'Hungria',
    nacionalidade %in% c('Sucha, Galicia, Austria', 'Vienna, Austria') ~ 'Áustria',
    nacionalidade == 'Istanbul, Turkey' ~ 'Turquia',
    nacionalidade %in% c('Croydon, Surrey, England', 'Shipley, Yorkshire, England',
                         'London, England','Cambridge, England','Ryde, Isle of Wight, England',
                         'Reading, Berkshire, England','Manchester, England','San Carlos, Ca',
                         'Victoria Hotel, Scarborough, Yorkshire, England','Tunbridge Wells, Kent, England',
                         'Withington, Manchester, England','Richmond, Surrey, England',
                         'Dorking, Surrey, England','Huyton, Lancashire, England',
                         'Hurstpierpoint, West Sussex, England','South Kensington, London, England',
                         'Scarborough, Yorkshire, England','Cowes, Isle of Wight, England',
                         'Grayshott, Hampshire, England','Felixstowe, Suffolk, England',
                         'Bermondsey, England','Lincoln, Lincolnshire, England','Walton-on-Thames, Surrey, England',
                         'Ilford, Essex, England','Birkenhead, Cheshire, England','Chiswick, London, England',
                         'Reading, England','Bramhall, Cheshire, England','York, North Yorkshire, England') ~ 'Inglaterra',
    nacionalidade %in% c('Berlin, Germany','Dusseldorf, Germany','Wiesbaden, Germany') ~ 'Alemanha',
    nacionalidade %in% c('Tokyo, Japan','Otaro, Hokkaido, Japan') ~ 'Japão',
    nacionalidade == 'C\xcc\xc1slav, Czechoslovakia' ~ 'Tchecoslováquia',
    nacionalidade %in% c('Kapuskasing, Ontario, Canada','North Sydney, Nova Scotia, Canada',
                         'Toronto, Ontario, Canada','Montreal, Quebec, Canada','Cobourg, Ontario, Canada',
                         'Winnipeg, Manitoba, Canada') ~ 'Canadá',
    nacionalidade == 'Wellington, New Zealand' ~ 'Nova Zelândia',
    nacionalidade == 'Pingtung, Taiwan' ~ 'Taiwan',
    nacionalidade == 'Rorschach, Switzerland' ~ 'Suíça',
    nacionalidade == 'Lviv, Ukraine' ~ 'Ucrânia',
    nacionalidade %in% c('Santurce, Puerto Rico','Humacao, Puerto Rico') ~ 'Porto Rico',
    nacionalidade %in% c('Neath, Wales','Port Talbot, Wales','Vale of Glamorgan, Wales',
                         'Marian Glas, Anglesey, Wales','Haverfordwest, Pembrokeshire, Wales',
                         'Mumbles, Wales') ~ 'País de Gales',
    nacionalidade %in% c('Vladivostok, Russia','St. Petersburg, Russia') ~ 'Rússia',
    nacionalidade %in% c('Toowoomba, Queensland, Australia','Perth, Australia',
                         'Melbourne, Victoria, Australia') ~ 'Austrália',
    nacionalidade == 'Dublin, Ireland' ~ 'Irlanda',
    nacionalidade %in% c('Chihuahua, Mexico','Mexico City, Mexico') ~ 'México',
    nacionalidade == 'Bucharest, Romania' ~ 'Romênia',
    nacionalidade == 'Samrong Young, Cambodia' ~ 'Camboja',
    nacionalidade %in% c('Las Palmas De Gran Canaria, Canary Islands',
                         'Madrid, Spain') ~ 'Espanha',
    nacionalidade %in% c('Darjeeling, India','Chabua, Assam, India') ~ 'Índia',
    nacionalidade == 'Stockholm, Sweden' ~ 'Suécia',
    nacionalidade == 'Brussels, Belgium' ~ 'Bélgica',
    nacionalidade == 'Benoni, South Africa' ~ 'África do Sul',
    nacionalidade == 'Jerusalem, Israel' ~ 'Israel',
    nacionalidade == 'Athens, Greece' ~ 'Grécia',
  ))

tabyl(base_mod$nacionalidade_mod)

##Data =========================================================================
unique(base_mod$data_nasc)

#Salvando ======================================================================
names(base_mod)
base_final <- base_mod %>% 
  select(id,ano_premiacao,premio_mod,filme,nome,data_nasc,url_bio,nacionalidade_mod,
         raca_etnia_mod,religiao_mod,orientacao_sex_mod)

write_xlsx(base_final, "oscars_tratado.xlsx")

# Outra base ===================================================================
oscars <- read.csv("oscars.csv", header = F, sep = '')
															
