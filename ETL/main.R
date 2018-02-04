#path <- 'E:/Curso Cientista de Dados/Projeto Final/analysisFacebook/jobsAnalysis'
#path <- 'E:/jobsAnalysis'
path <- 'C:/Users/gabri/Desktop/jobsAnalysis'
setwd(path)
source(paste(path,'/ETL/utils.R', sep = '') )
source(paste(path,'/ETL/dataMining.R', sep = '') )
#source(paste(path,'/ETL/dataMining.R', sep = '') )




#### **** COLECT DATA **** ####
collect <- function(url = ''){
  if(url == ''){
    stop('Informe uma url para fazer scrapping')
  }else{
    dados <- scrapGlassDoor(urls = url)
    fwrite(dados,paste('data/jobsGlassDoor',Sys.Date(),'.csv', sep = '') ) 
  }
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
  ## CAPTURANDO A PROVINCIAS DAS CIDADES DO CANADA. É NECESSÁRIO PORQUE NO ANUNCIO DA VAGA SÓ VEM SOMENTE A CIDADE
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
      
      ## RECUPERANDO O TOTAL DE AVALIAÇÕES
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
          
          ## RECUPERANDO OS IDS DAS AVALIAÇÕES
          ids <- nodes %>% html_attr('id') %>% gsub('[[:alpha:]]|\\s|[[:punct:]]','', .) #html_attr('id') 
          
          ## RECUPERANDO RATING DAS AVALIAÇÕES
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

## FUNÇÃO PARA FAZER PRE-PROCESSAMENTO DA AVALIAÇÕES DO SITE LOVE MONDAYS
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
  dadosClean$pros <- gsub('Prós:','',dadosClean$pros)#sapply(dadosClean$pros, function)
  dadosClean$cons <- gsub('Contras:','',dadosClean$cons)#sapply(dadosClean$pros, function)
  dadosClean$adviceManag <- gsub('Conselhos para presidência:','',dadosClean$cons)
  
  dadosClean$position <- sapply(dadosClean$status,function(x){
    fim <- gregexpr('Ex-funcionário',x)[[1]]-1
    if(fim > 0){
      x <- substr(x,0,fim)
      x <- ifelse(x == '','NI',x)
    }
    x
  })
  
  dadosClean$status <- sapply(dadosClean$status,function(x){
    if(grepl('Ex-funcionário|saiu',x)){
      x <- 'Ex-funcionário'
    }else{
      x <- 'Funcionário'
    }
    x
  })
  
  cols <-c("id","company","dateClear","rating","city","state","title","status","recommend","pros","cons","adviceManag","outlook","ceo")
  dadosClean <- dadosClean[,cols]
  fwrite(dadosClean,'data/reviewLoveM.csv')
}

## FUNÇÃO PARA FAZER PRE-PROCESSAMENTO DA AVALIAÇÕES DO SITE GLASSDOOR
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
reviewsLoveMondays() ## SCRAPPING AVALIAÇÕES NO SITE LOVE MONDAYS
reviewsGlassDoor() ## SCRAPPING AVALIAÇÕES NO GLASSDOOR
transGlassDoor() ## TRANSFORMAÇÕES AVALIAÇÕES DO SITE GLASSDOOR
transLoveMond() ## TRANSFORMAÇÕES AVALIAÇÕES DO SITE LOVE MONDAYS

