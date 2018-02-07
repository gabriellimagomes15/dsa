#path <- 'E:/Curso Cientista de Dados/Projeto Final/analysisFacebook/jobsAnalysis'
#path <- 'E:/jobsAnalysis'

# https://www.portalsql.com.br/vagas.php
# 

i <- 2
path <- c('C:/Users/gabri/Desktop/jobsAnalysis','~/Documents/jobsAnalysis/','E:/Curso Cientista de Dados/Projeto Final/analysisFacebook/jobsAnalysis')
setwd(path[i])
source(paste(path[i],'/ETL/utils.R', sep = '') )
source(paste(path[i],'/ETL/dataMining.R', sep = '') )
#source(paste(path,'/ETL/dataMining.R', sep = '') )




#### **** JOBS VACANCIES **** ####
collect <- function(url = ''){
  if(url == ''){
    stop('Informe uma url para fazer scrapping')
  }else{
    dados <- scrapGlassDoor(urls = url)
    fwrite(dados,paste('data/jobsGlassDoor',Sys.Date(),'.csv', sep = '') ) 
  }
}

jobsGlassDoor <- function(){
  urlRoot <- 'https://www.glassdoor.com'
  urls    <- c('https://www.glassdoor.com/Job/canada-data-scientist-jobs-SRCH_IL.0,6_IN3_KO7,21_IP','https://www.glassdoor.com/Job/us-data-scientist-jobs-SRCH_IL.0,2_IN1_KO3,17_IP')
  
  ## Loop para identificar aumtomaticamente o pa?s 
  country <- sapply(urls, function(x){
    final <- gregexpr('-',x)[[1]][1] - 1
    x <- substr(x,31,final)
    x
  })
  
  urlDF   <- data.frame(urls, country, stringsAsFactors = F)
  dataDF  <- data.frame()
  i <- 1
  for(i in 1:dim(urlDF)[1]){
    urlPage <- urlDF$urls[i]
    
    ## LENDO P?GINA E IDENTIFICANDO QUANTOS REGISTROS EXISTEM PARA IDENTIFICAR AUTOMATICAMENTE A QUANTIDADE DE PAGINAS QUE SER?O LIDAS
    
    urlPage2 <- paste(urlPage,'1','.htm', sep = '')
    page     <- read_html(urlPage2)
    totalReg <- as.integer(page %>% html_nodes('.jobsCount') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .)) 
    
    if(!identical(totalReg,integer(0) ) & !is.na(totalReg) ){
      totalPages <- (round(totalReg/30))
    }else{
      totalPages <- 10
    }
    cat('Total Pages = ', totalPages,  ' - ', urlPage, '\n\n')
    
    tryCatch({
      ## FOR PARA LER AS P?GINAS COM TODOS AS VAGAS...TO READ THE PAGES WITH ALL JOB POST. TotalPages+1 is because the first page doesn't work with this url
      for( pag in 1:totalPages){
        cat('\n == > Page = ', pag)
        urlPage2 <- paste(urlPage,pag,'.htm', sep = '')
        cat(' ',urlPage2)
        page     <- read_html(urlPage2)
        links    <- page %>% html_nodes('.jlGrid.hover') %>% html_nodes('.jl .flexbox a') %>% html_attr('href')
        
        tryCatch({    
          ## FOR PARA LER A P?GINA COM TODOS OS DETALHES DA VAGA
          for(href in links){
            url <- paste(urlRoot, href, sep = '')
            cat('\n \t --', url, '\t')
            
            pageJob <- read_html(url)
            
            id <- pageJob %>% html_nodes('.jobViewHeader') %>% html_attr('id')
            id <- ifelse(identical(id,character(0)), '--', id)
            
            data <- scrapJobsVac(pageJob = page, id = id,tagPosit = '.empInfo.tbl h2',tagCompany = '.empInfo .ib',
                                 tagCityState = '.empInfo .subtle.ib',tagDate = '.empLinks .minor',
                                 tagDescrip = '.jobDescriptionContent', country = urlDF$country[i], url = url)
            
            dataDF <- rbind.fill(dataDF,data)
            
            print('Saving data set')
            #fwrite(dataDF, paste('data/Glassdoor',Sys.Date(),'.csv', sep = '') )
            fwrite(dataDF, paste('data/glassDoor',urlDF$country[i],Sys.Date(),'.csv',sep = '') )
            
            Sys.sleep(4) # DELAY
          }## END FOR LINKS
        }, error = function(e){
          print(paste('ERROR READ LINKS: ', e , sep = ' ') )
        })
      }## END FOR TOTALPAGE
    }, error = function(e){
      print(paste('ERROR: ', e , sep = ' ') )
    })
  }## END FOR URLPAGE
  return(dataDF)
}

jobsLoveM <- function(){
  urlRoot <- 'https://www.lovemondays.com.br'
  urls    <- c('https://www.lovemondays.com.br/pesquisa/vaga/pagina/1?external_job_city_id=&external_job_city_name=&q=Data+Analyst')
  country <- c('Brazil')
  urlDF   <- data.frame(urls, country, stringsAsFactors = F)
  dataDF  <- data.frame()
  
  for(i in 1:dim(urlDF)[1]){
    urlPage <- urlDF$urls[i]
    
    ## LENDO P?GINA E IDENTIFICANDO QUANTOS REGISTROS EXISTEM PARA IDENTIFICAR AUTOMATICAMENTE A QUANTIDADE DE PAGINAS QUE SER?O LIDAS
     
    totalPages <- page %>% html_nodes('.lm-Pagination-list-item.is-last a') %>% html_attr('href') %>% 
                            gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) %>% as.numeric(.)
    
    if(!identical(totalPages,integer(0) ) & !is.na(totalReg) ){
      totalPages <- 5
    }
    
    cat('Total Pages = ', totalPages,  ' - ', urlPage, '\n\n')
    
    pag <- as.character(2)
    
    
    tryCatch({
      ## FOR PARA LER AS P?GINAS COM TODOS AS VAGAS...TO READ THE PAGES WITH ALL JOB POST. TotalPages+1 is because the first page doesn't work with this url
      for( pag in 1:totalPages){
        cat('\n == > Page = ', pag)
        urlPage2 <- str_replace(url, '[[:digit:]]',as.character(pag) ) #paste(urlPage,pag,'.htm', sep = '')
        cat(' ',urlPage2)
        page     <- read_html(urlPage2)
        links    <- page %>% html_nodes('.lm-List-default-row.lm-List-jobs-row a') %>% html_attr('href')
        
        tryCatch({    
          
          href <- links[1]
          ## FOR PARA LER A P?GINA COM TODOS OS DETALHES DA VAGA
          for(href in links){
            url <- paste(urlRoot, href, sep = '')
            cat('\n \t --', url, '\t')
            
            pageJob <- read_html(url)
            
            ## Bloco para recuperar o ID da vaga
            textId <- pageJob %>% html_nodes('title') %>% html_text() %>% trimws(.)
            iniId <- regexpr("#",textId)[[1]] + 1
            fimId <- regexpr("[[:digit:]]\\)",textId)
            
            stringr::str_extract(string = textId, pattern = "#[[:digit:]]+")
            
            
            
            id <- substr(textId,iniId,fimId)
            id <- ifelse(identical(id,character(0)), '--', id)
            
            ' html_innerHTML <- function(x, css, xpath) {
            file <- tempfile()
              html_node(x,css) %>% write_xml(file)
              txt <- readLines(file, warn=FALSE)
              unlink(file)
              txt
            }
            rm(html_innerHTML)
            html_innerHTML(pageJob, "header section h1") %>% 
            gsub("<br>"," \n ", .) %>% .[2]
            read_html %>%
            html_text
            xml_
            #posit, company
            pageJob %>% html_nodes'
            #('.lm-Typography-titleFour') %>% gsub("<br>"," \n ", .) %>% html_text() %>% xml_find_all(., "//text()")
            
            
            t <- pageJob %>% html_nodes("[type='application/ld+json']") %>% html_text()
            d <- t[which(grepl("datePosted",t))]
            stringr::str_extract(string = d,pattern = "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}")
            
            ## Comando para recuperar o cargo (position), nome da empresa e localização (location).
             # Foi necessário adotar essa estratégia por causa da estrutura da página.
            infos <- pageJob %>% html_nodes('header section h1') %>% gsub("<br>"," \n", .) %>% str_split(.,'\n|em')
            
            data <- scrapJobsVac(pageJob = pageJob, id = id,tagPosit = '.empInfo.tbl h2',tagCompany = '.empInfo .ib',
                                 tagCityState = '.empInfo .subtle.ib',tagDate = '.empLinks .minor',
                                 tagDescrip = '.jobDescriptionContent', country = urlDF$country[i], url = url)
            
            
            dataDF <- rbind.fill(dataDF,data)
            
            print('Saving data set')
            #fwrite(dataDF, paste('data/Glassdoor',Sys.Date(),'.csv', sep = '') )
            fwrite(dataDF, paste('data/glassDoor',urlDF$country[i],Sys.Date(),'.csv',sep = '') )
            
            Sys.sleep(4) # DELAY
          }## END FOR LINKS
        }, error = function(e){
          print(paste('ERROR READ LINKS: ', e , sep = ' ') )
        })
      }## END FOR TOTALPAGE
    }, error = function(e){
      print(paste('ERROR: ', e , sep = ' ') )
    })
  }## END FOR URLPAGE
  return(dataDF)
}

#### **** PRE-PROCESS DATA **** ####
preProcess <- function(dados){
  
  dados$descrip <- cleanText(data = dados,column = 'descrip', stopWords = stopwords('en'))
  dadosClean    <- dados
  
  ## Extraindo do campo 'descrip' da vaga dados sobre skill, education e language, requeridos para a vaga.
  dadosClean$skills     <- getSkills(dadosClean,column = 'descrip')
  dadosClean$education  <- getEducation(dadosClean,'descrip')
  dadosClean$language   <- getLanguage(dadosClean,'descrip')
  
  ## Limpando o campo 'positJob' 
  dadosClean$JobClear   <- clearPostJob(dadosClean,'positJob')
  
  ## Limpando o campo 'company' 
  dadosClean[is.na(dadosClean$company),]$company <- 'NI'
  dadosClean$companyClear <- clearCompany(dadosClean,'company')
  
  ## Separando 'city' e 'state' do campo 'city_state'
  dadosClean$cityClear    <- clearCity(dadosClean$city_state)
  dadosClean$cityClear    <- clearCity(dadosClean$cityClear)
  
  dadosClean$stateClear   <- clearState(dadosClean$city_state)

  dadosClean$dateClean    <- ""
  dadosClean              <- clearDate(data = dadosClean)
  
  ## GET THE PROVINCE OF THE CANADA
  ## CAPTURANDO A PROVINCIAS DAS CIDADES DO CANADA. ? NECESS?RIO PORQUE NO ANUNCIO DA VAGA S? VEM SOMENTE A CIDADE
  canadaCity           <- data.frame(jsonlite::fromJSON('data/cities/canadaCities.json'), stringsAsFactors = F)
  colnames(canadaCity) <- c('city','prov')
  
  dadosClean[dadosClean$country == 'canada',]$stateClear <- unlist( sapply(dadosClean[dadosClean$country == 'canada',]$cityClear, function(x){
    r <- canadaCity[tolower(canadaCity$city) == tolower(x),'prov']
    if( length(r) > 0){
      r[1] 
    }else{
      x
    }
  }) )
  
  brazilCity <- fread('data/cities/brasilCities.csv',encoding = 'UTF-8') 
  dadosClean[dadosClean$country == 'brazil',]$stateClear <- unlist( sapply(dadosClean[dadosClean$country == 'brazil',]$cityClear, function(x){
    r <- brazilCity[tolower( iconv( enc2native(brazilCity$city), to = "ASCII//TRANSLIT")  ) == tolower(x),'state']
    if( length(r) > 0){
      r[1] 
    }else{
      x
    }
  }) )
  dadosClean[dadosClean$stateClear == 'Fort Saint John',]$stateClear <- 'BC'
  dadosClean[dadosClean$stateClear == 'RiviA?re-du-Loup',]$stateClear <- 'QC'
  dadosClean[dadosClean$stateClear == 'ByWard Market',]$stateClear <- 'ON'
  dadosClean[dadosClean$stateClear == 'New Brunswick',]$stateClear <- 'NB'
  dadosClean[dadosClean$stateClear == 'Saguenay',]$stateClear <- 'QC'
  dadosClean[dadosClean$stateClear == 'Ontario',]$stateClear <- 'ON'
  
  dadosClean <- dadosClean[,c('JobClear','companyClear','dateClean', 'skills','education','language','dateColect','urlDF.country.i.','cityClear','stateClear','url')]
  #data.table::fwrite(dadosClean, 'data/dadosFinal.csv')
  return(dadosClean)
}

#### **** COMPANIES REVIEWS *****####
collectReviews <- function(){
  urls <- c('https://www.glassdoor.com/Reviews/Microsoft-Reviews-E1651_P','https://www.glassdoor.com/Reviews/IBM-Reviews-E354_P',
            'https://www.glassdoor.com/Reviews/Amazon-Reviews-E6036_P') #2.htm'
  reviewFinal <- data.frame()
  for(url in urls){
    reviews  <- scrapReviewGlass(urlPage = url)
    reviewDF <- rbind(reviewFinal, reviews)
    save(reviews, paste('data/reviews-',Sys.Date(),'.csv',sep = '') )  
  }
}

reviewsLoveMondays <- function(){
  print('reviewsLoveMondays')
  urls     <- c('https://www.lovemondays.com.br/trabalhar-na-ifood/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-nubank/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-ibm/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-microsoft/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-google/avaliacoes/pagina/',
                'https://www.lovemondays.com.br/trabalhar-na-oracle/avaliacoes/pagina/') #1
  
  reviewDF <- data.frame()
  
  for(urlPage in urls){
    tryCatch({
      ## READ THE FIRST PAGE TO GET THE COMPANY'S NAME AND TOTAL OF PAGES
      webPage  <- read_html(paste(urlPage,'1',sep = '') )
      company  <- webPage %>% html_nodes('.lm-CompanyHeader-companyName') %>% html_text()
      
      ## RECUPERANDO O TOTAL DE AVALIA??ES
      totalRew   <- as.numeric(webPage %>% html_nodes('.lm-Tabs-default--companyHeader .lm-Tabs-default-item.is-active') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .))
      if(!identical(totalRew,numeric(0)) & !is.na(totalRew) ){
        totalPages <- round(totalRew/10)  
      }else{
        totalPages <- 1
      }
      for( page in 1:totalPages){
        cat('==> Page: ',page)
        urlPage2 <- paste(urlPage,page, sep = '')
        cat('\t',urlPage2)
        
        webPage <- read_html(urlPage2)
        nodes   <- webPage %>% html_nodes('section .lm-List-default .lm-List-default-row')
        
        ids     <- nodes %>% html_nodes('.lm-List-item-header-title a') %>% html_attr('href') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .)
        ratings <- c()
        for(review in nodes){
          rating <- 5 * (review %>% html_nodes('.lm-RatingStar-starContainer div:nth-child(2).lm-RatingStar-starContainer-starsActive') %>% 
                           html_attrs() %>% .[[1]] %>% unlist(.) %>% paste(.,collapse = ' ') %>% 
                           gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) %>% as.numeric(.) / 100)
          ratings <- c(ratings,rating)
        }## END review in nodes
        
        reviewLoveMondays <- scrapReviews(nodes = nodes,tagDate = '.lm-Company-dateTime-label', tagTitle = '.lm-List-item-header-title',
                                          tagStatus = '.reviewer ',tagLocation = '.lm-List-item-contributions',
                                          tagRecommend = '.lm-Review-contribution p:nth-child(3)',tagPros = '.lm-Review-contribution p:nth-child(1)',
                                          tagCons = '.lm-Review-contribution p:nth-child(2)',tagAdvice = 'ni' ,
                                          company = company,ids = ids, ratings = ratings)
        
        reviewDF <- rbind(reviewDF,reviewLoveMondays)
        fwrite(reviewDF, paste('data/reviewLoveM-',Sys.Date(),'.csv',sep = ''))
      }## END FOR totalPages
    }, error = function(e){
      print(paste('ERROR IN FOR URLS: ', e , sep = ' ') )
    })
  }#END FOR urls
}

reviewsGlassDoor <- function(){
  print('reviewsGlassDoor')
  urls  <- c('https://www.glassdoor.com/Reviews/Dell-Reviews-E1327_P',  
             'https://www.glassdoor.com/Reviews/IBM-Reviews-E354_P',
             'https://www.glassdoor.com/Reviews/Microsoft-Reviews-E1651_P',
             'https://www.glassdoor.com/Reviews/Google-Reviews-E9079_P',
             'https://www.glassdoor.com/Reviews/Oracle-Reviews-E1737_P',
             'https://www.glassdoor.com/Reviews/Tesla-Reviews-E43129_P')
  reviewDF <- data.frame()
  
  
    for(urlPage in urls){
      tryCatch({
        webPage  <- read_html(paste(urlPage,'1','.htm',sep = ''))
        company  <- webPage %>% html_nodes('.module.filterableContents .h2') %>% html_text()
        
        totalRew   <- as.numeric(webPage %>% html_nodes('.padTopSm.margRtSm.margBot.minor') %>% html_text() %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .))
        
        if(!identical(totalRew,numeric(0)) & !is.na(totalRew) ){
          totalPages <- round(totalRew/10)  
        }else{
          totalPages <- 1
        }
        ## LOOP TO READ EACH PAGE
        for( page in 1:totalPages){
          cat('==> Page: ',page)
          urlPage2 <- paste(urlPage,page,'.htm', sep = '')
          cat('\t',urlPage2)
          
          webPage <- read_html(urlPage2) ## SCRAPPING THE PAGE
          nodes   <- webPage %>% html_nodes('.empReview') ## GET NODES THAT CONTAIN THE REVIEWS
          
          ## RECUPERANDO OS IDS DAS AVALIA??ES
          ids <- nodes %>% html_attr('id') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) #html_attr('id') 
          
          ## RECUPERANDO RATING DAS AVALIA??ES
          ratings <- nodes %>% html_nodes('.rating span') %>% html_attr('title') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) %>% as.numeric(.) / 10
          
          reviewGlassDoor <- scrapReviews(nodes = nodes,tagDate = '.date', tagTitle = '.tbl .h2.summary',
                                            tagStatus = '.tbl.reviewMeta .authorJobTitle.middle',tagLocation = '.tbl.reviewMeta .authorLocation.middle',
                                            tagRecommend = '.recommends .tightLt:nth-child(1)',tagOutlook = '.recommends .tightLt:nth-child(2)',
                                            tagCeo = '.recommends .tightLt:nth-child(3) .showDesk',tagPros = '.pros.mainText',
                                            tagCons = '.cons.mainText',tagAdvice = '.adviceMgmt' ,
                                            company = company,ids = ids, ratings = ratings)
          
          reviewDF <- rbind(reviewDF,reviewGlassDoor)
          fwrite(reviewDF, paste('data/reviewGlassD-',Sys.Date(),'.csv',sep = ''))
        }## END FOT totalPages
    }, error = function(e){
      print(paste('ERROR IN FOR URLS: ', e , sep = ' ') )
    })
  }## END FOR urls
}

## FUN??O PARA FAZER PRE-PROCESSAMENTO DA AVALIA??ES DO SITE LOVE MONDAYS
transLoveMond <- function(){
  dados <- data.table::fread('data/reviewLoveM-2018-01-28.csv',encoding = 'UTF-8')
  dadosClean <- convertDate(dados)
  dadosClean$city  <- clearCity(dadosClean$location)
  dadosClean$state <- clearState(dadosClean$location)
  dadosClean$adviceManag <- sapply(dadosClean$recommend, function(x){
    if(grepl('conselhos',x, ignore.case = T)){
      x
    }else{
      x <- 'NI'
    }
  })
  dadosClean$recommend <- sapply(dadosClean$recommend, function(x){
    if(grepl('conselhos',x, ignore.case = T)){
      x <- 'NI'
    }else{
      x <- gsub('Recomenda a empresa:','',x)
      x <- gsub('\n','',x)
      x
    }
  })
  dadosClean$pros <- gsub('Pr?s:','',dadosClean$pros)#sapply(dadosClean$pros, function)
  dadosClean$cons <- gsub('Contras:','',dadosClean$cons)#sapply(dadosClean$pros, function)
  dadosClean$adviceManag <- gsub('Conselhos para presid?ncia:','',dadosClean$cons)
  
  dadosClean$position <- sapply(dadosClean$status,function(x){
    fim <- gregexpr('Ex-funcion?rio',x)[[1]]-1
    if(fim > 0){
      x <- substr(x,0,fim)
      x <- ifelse(x == '','NI',x)
    }
    x
  })
  
  dadosClean$status <- sapply(dadosClean$status,function(x){
    if(grepl('Ex-funcion?rio|saiu',x)){
      x <- 'Ex-funcion?rio'
    }else{
      x <- 'Funcion?rio'
    }
    x
  })
  
  cols <-c("id","company","dateClear","rating","city","state","title","status","recommend","pros","cons","adviceManag","outlook","ceo")
  dadosClean <- dadosClean[,cols]
  fwrite(dadosClean,'data/reviewLoveM.csv')
}

## FUN??O PARA FAZER PRE-PROCESSAMENTO DA AVALIA??ES DO SITE GLASSDOOR
transGlassDoor <- function(){
  dados <- fread('data/reviewGlassD-2018-01-29.csv',encoding = 'UTF-8')
  dados$date <- sapply(dados$date, function(x){
    if(grepl('seconds',x)){
      x <- 'Jan 29, 2018'
    }
    x
  })
  dadosClean <- dados
  dadosClean$dateClear <- lubridate::mdy(dadosClean$date)
  dadosClean$title <- gsub('\"','', dadosClean$title)
  dadosClean$city  <- clearCity(dadosClean$location)
  dadosClean$state <- clearState(dados$location)
  dadosClean$recommend <- sapply(dadosClean$recommend, function(x){
    if(x == "Doesn't Recommend"){
      x <- 'No'
    }else if( x == "Recommends"){
      x <- 'Yes'
    }else{
      x <- 'NI'
    }
    x
  })
  dadosClean$outlook <- sapply(dados$outlook, function(x){
    x <- gsub('Outlook','',x)
    if(grepl('of',x)){
      x <- 'NI'
    }
    x
  })
  dadosClean$ceo <- sapply(dadosClean$ceo, function(x){
    x <- gsub('of','',x)
    x
  }) 
  dadosClean$position <- sapply(dadosClean$status, function(x){
    ini <- regexpr('-',x)[[1]] + 1
    if(ini > 0){
      x <- trimws(substr(x,ini,str_length(x)))  
    }
    x  
  })
  dadosClean$status <-sapply(dadosClean$status, function(x){
    fim <- regexpr('-',x)[[1]] - 1
    if(fim > 0){
      x <- trimws(substr(x,0,fim))  
    }
    x
  })
  
  cols <-c("id","company","dateClear","rating","city","state","title","status","position","recommend","pros","cons","adviceManag","outlook","ceo")
  
  dadosClean <- dadosClean %>% select_(.dots = cols)
  fwrite(dadosClean,'data/reviewGlassD.csv')
}



#### **** MAIN **** ####

#### >>>> JOBS VACANCIES ####

#print(getwd())
#url <- c('https://www.glassdoor.com/Job/canada-data-scientist-jobs-SRCH_IL.0,6_IN3_KO7,21_IP','https://www.glassdoor.com/Job/us-data-scientist-jobs-SRCH_IL.0,2_IN1_KO3,17_IP')
#url <- 'https://www.glassdoor.com/Job/brazil-data-scientist-jobs-SRCH_IL.0,6_IN36_KO7,21_IP'

#collect(url)

#dados <- data.table::fread('data/jobsGlassDoor.csv', stringsAsFactors = F, encoding = 'UTF-8')
#load('data/jobsGlassDoor.RData')
#dadosClean <- preProcess(dados)
#fwrite(dadosClean,'data/dadosFinal2.csv')
#clusterDM()

#### >>>> COMPANIES' REVIEWS  ####
reviewsLoveMondays() ## SCRAPPING AVALIA??ES NO SITE LOVE MONDAYS
reviewsGlassDoor() ## SCRAPPING AVALIA??ES NO GLASSDOOR
transGlassDoor() ## TRANSFORMA??ES AVALIA??ES DO SITE GLASSDOOR
transLoveMond() ## TRANSFORMA??ES AVALIA??ES DO SITE LOVE MONDAYS

