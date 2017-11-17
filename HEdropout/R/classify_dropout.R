classify_dropout <-
  function(database,cv_folds=4,pct_training=0.75,classifier="CART",num_cores=2,balance="down",preprocess="yes"){
    library(dplyr)
    library(caret)
    
    if(preprocess=="yes"){
    
      # indicar quais atributos nao sao factors
      attr_nao_factor = c("vagas","candidatos","num_ies","idade","ano_CES","ano_ENEM","ano_ing","nu_integralizacao_integral","nu_integralizacao_matutino","nu_integralizacao_vespertino","nu_integralizacao_noturno","ano_concluiu","doc_exercicio","doc_qualifcacao","doc_exer_outro_org","doc_afastado_outro","doc_afas_saude","doc_sem_grad","doc_graduacao","doc_especializacao","doc_mestrado","doc_doutorado","doc_integ_de","doc_integ_sem_de","doc_temp_parcial","doc_horista","doc_brasileiro","doc_brasileiro_nat","doc_estrangeiro","nota_cn","nota_ch","nota_lc","nota_mt","nu_nota_comp1","nu_nota_comp2","nu_nota_comp3","nu_nota_comp4","nu_nota_comp5","nu_nota_redacao","tmp_perman")

      nao_factor_pos = sapply(attr_nao_factor,function(x)grep(paste0("^",x,"$"),names(database)))
      
      nao_factor_pos = unlist(nao_factor_pos)
      
      var_factor = (1:ncol(database))[-nao_factor_pos]
      
      # funcao para remover espacos antes e depois de respostas
      tira_espaco = function (x) gsub("^\\s+|\\s+$", "", x)
      
      # funcao para trocar valores vazios ("") por "branco"
      val_branco = function (x) sub("^$","branco",x)
      
      # funcao para trocar o valor NA por "branco"
      val_NA = function (x) ifelse(is.na(x),"branco",x) 
      
      # aplicando funcao trim nos atributos que sao "fatores"
      database[,var_factor] = lapply(database[,var_factor],tira_espaco)
      
      # aplicando funcao val_branco nos atributos que sao "fatores"
      database[,var_factor] = lapply(database[,var_factor],val_branco)
      
      # aplicando funcao val_branco nos atributos que sao "fatores"
      database[,var_factor] = lapply(database[,var_factor],val_NA)
      
      
      if(any(grepl("cod_municipio_residencia",names(database))) & any(grepl("cod_municipio_esc",names(database))) & any(grepl("cod_municipio_nascimento",names(database))) & any(grepl("cod_municipio_prova",names(database)))){
        # removendo municipios de nascimento e de escola - criando atributo indicativo de municipios diferentes
        database = database %>% mutate(mun_res_dif_nasc = cod_municipio_residencia!=cod_municipio_nascimento) 
        database = database %>% mutate(mun_res_dif_esc = cod_municipio_residencia!=cod_municipio_esc) 
        database = database %>% mutate(mun_res_dif_prova = cod_municipio_residencia!=cod_municipio_prova) 
  
        database = database %>% mutate(mun_nasc_dif_esc = cod_municipio_nascimento!=cod_municipio_esc) 
        database = database %>% mutate(mun_nasc_dif_prova = cod_municipio_nascimento!=cod_municipio_prova) 
        
        database = database %>% mutate(mun_esc_dif_prova = cod_municipio_esc!=cod_municipio_prova) 
        
        database = database %>% mutate(uf_res = substr(cod_municipio_residencia,1,2))
        database = database %>% mutate(uf_nasc = substr(cod_municipio_nascimento,1,2))
        database = database %>% mutate(uf_esc = substr(cod_municipio_esc,1,2))
        database = database %>% mutate(uf_prova = substr(cod_municipio_prova,1,2))
        
        database = dplyr::select(database,-c(cod_municipio_residencia,cod_municipio_esc,cod_municipio_prova,cod_municipio_nascimento))
      }    
    
      # adicionando atributo dummy para indicar se aluno concluiu ensino medio e adicionando zero ao ano de conclusao caso nao tenha concluido  
      if(any(grepl("ano_concluiu",names(database)))){
        database$concluiu_ens_med = ifelse(is.na(database$ano_concluiu),FALSE,TRUE)
        database$ano_concluiu[is.na(database$ano_concluiu)] = 0
      }
      
      # substituindo quatro indicadores de turno por apenas um (alunos possuem apenas um turno)
      if(any(grepl("turno",names(database)))){
        database$turno = ifelse(database$in_matutino_curso==1,1,
                                ifelse(database$in_vespertino_curso==1,2,
                                       ifelse(database$in_noturno_curso==1,3,
                                              ifelse(database$in_integral_curso==1,4,NA))))
        database = dplyr::select(database,-c(in_matutino_curso,in_vespertino_curso,in_noturno_curso,in_integral_curso))
        
        database$nu_integralizacao_matutino[is.na(database$nu_integralizacao_matutino)] = 0
        database$nu_integralizacao_vespertino[is.na(database$nu_integralizacao_vespertino)] = 0
        database$nu_integralizacao_noturno[is.na(database$nu_integralizacao_noturno)] = 0
        database$nu_integralizacao_integral[is.na(database$nu_integralizacao_integral)] = 0
      }
      
      # adicionando diferenca entre ano de ingresso e ultimo ano do aluno na base
      if(any(grepl("ano_max",names(database)) & grepl("ano_ing",names(database)))){
        database$tmp_perman = database$ano_max - database$ano_ing 
        database = dplyr::select(database,-c(ano_max,ano_ing))
      }
      
      # atualizando a posicao dos atributos fatores
      nao_factor_pos = sapply(attr_nao_factor,function(x)grep(paste0("^",x,"$"),names(database)))
      nao_factor_pos = unlist(nao_factor_pos)
      var_factor = (1:ncol(database))[-nao_factor_pos]
      
      # transformando os atributos em factors
      database[,var_factor] = lapply(database[,var_factor],factor,exclude=NULL)
      
      # modificando os levels do atributo de evasao para nomes 
      levels(database$evasao) = c("N","S")
      
      # Removendo missing
      database = na.omit(database)
      
      # Removendo as colunas que nao possuem variancia - as colunas a serem retiradas pode variar de acordo com os dados (nao pode ser retirada a evasao)
      nzv = nearZeroVar(database)
      if(any(names(database)[nzv]=="evasao")){
        nzv = nzv[-which(names(database)[nzv]=="evasao")]  
      }
      database = database[,-nzv]
      
    }

    summary_accuracy = function (data, lev = NULL, model = NULL){
      lvls <- levels(data$obs)
      if (length(lvls) > 2) 
        stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
      requireNamespace("ModelMetrics",quietly=T)
      if (!all(levels(data[, "pred"]) == lvls)) 
        stop("levels of observed and predicted data do not match")
      data$y = as.numeric(data$obs == lvls[2])
      rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 1), data[, lvls[1]])
      acc <- sum(ifelse(data$obs == data$pred,1,0))/nrow(data)
      out <- c(acc,rocAUC, sensitivity(data[, "pred"], data[, "obs"],lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
      names(out) <- c("Accuracy","ROC", "Sens", "Spec")
      out
    }

  
    # Determinando o tipo de controle a ser feito sobre os modelos
    fitcontrol = trainControl(method = "cv",number = cv_folds,summaryFunction = summary_accuracy, classProbs = T)
    
    # Numero de nucleos para processamento paralelo (LINUX) -- Para windows usar Microsoft R OPEN
    if(Sys.info()[1]=="Linux"){library(doParallel);registerDoParallel(cores = num_cores)}
    
    # Separando base de treinamento da base de teste
    intraining = createDataPartition(database$evasao, p = pct_training, list = FALSE)
    
    base_treina = database[intraining,]
    
    base_teste = database[-intraining,]
    
    rm(database);gc()
    
    # Fazendo as classificacoes com upsampling na base de treinamento
    if(balance=="up"){
      up_data = upSample(x = dplyr::select(base_treina,-evasao), y = base_treina$evasao,yname="evasao")
      rm(base_treina);gc()
    }
    
    # Downsampling
    if(balance=="down"){
      dwn_data = downSample(x = dplyr::select(base_treina,-evasao), y = base_treina$evasao,yname="evasao")
      rm(base_treina);gc()
    }
    
    
    # Classificando com Naive Bayes
    if(classifier=="all" | classifier=="NB"){
      if(balance=="up"){
        naive_fit_up = train(x = dplyr::select(up_data,-evasao),y=up_data$evasao,method="nb",trControl=fitcontrol,metric = 'Spec')
        naive_pred_up = predict(naive_fit_up,base_teste)
        mc_naive_up = confusionMatrix(naive_pred_up,base_teste$evasao)
        return(list(naive_fit_up,naive_pred_up,mc_naive_up))
      }else if(balance=="down"){
        naive_fit_dwn = train(x = dplyr::select(dwn_data,-evasao),y=dwn_data$evasao,method="nb",trControl=fitcontrol,metric = 'Spec')
        naive_pred_dwn = predict(naive_fit_dwn,base_teste)
        mc_naive_dwn = confusionMatrix(naive_pred_dwn,base_teste$evasao)
        return(list(naive_fit_dwn,naive_pred_dwn,mc_naive_dwn))
      }else{
        naive_fit = train(x = dplyr::select(base_treina,-evasao),y=base_treina$evasao,method="nb",trControl=fitcontrol,metric = 'Spec')
        naive_pred = predict(naive_fit,base_teste)
        mc_naive = confusionMatrix(naive_pred,base_teste$evasao)
        return(list(naive_fit,naive_pred,mc_naive))
      }
    }
    
    
    # Classificando com CART
    if(classifier=="all" | classifier=="CART"){
      if(balance=="up"){
        cart_fit_up = train(x = dplyr::select(up_data,-evasao),y=up_data$evasao,method="rpart",trControl=fitcontrol,tuneLength = 10,metric = 'Spec')
        cart_pred_up = predict(cart_fit_up,base_teste)
        mc_cart_up = confusionMatrix(cart_pred_up,base_teste$evasao)
        return(list(cart_fit_up,cart_pred_up,mc_cart_up))
      }else if(balance=="down"){
        cart_fit_dwn <<- train(x = dplyr::select(dwn_data,-evasao),y=dwn_data$evasao,method="rpart",trControl=fitcontrol,tuneLength = 10,metric = 'Spec')
        cart_pred_dwn <<- predict(cart_fit_dwn,base_teste)
        mc_cart_dwn <<- confusionMatrix(cart_pred_dwn,base_teste$evasao)
        return(list(cart_fit_dwn,cart_pred_dwn,mc_cart_dwn))
      }else{
        cart_fit = train(x = dplyr::select(base_treina,-evasao),y=base_treina$evasao,method="rpart",trControl=fitcontrol,tuneLength = 10,metric = 'Spec')
        cart_pred = predict(cart_fit,base_teste)
        mc_cart = confusionMatrix(cart_pred,base_teste$evasao)
        return(list(cart_fit,cart_pred,mc_cart))
      }
    }
    
    
    # Classificando com C5.0
    if(classifier=="all" | classifier=="C50"){
      grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,10,20,30,40), .model="tree" )
      if(balance=="up"){
        c50_fit_up = train(x=dplyr::select(up_data,-evasao),y=up_data$evasao,method="C5.0",tuneGrid = grid,trControl=fitcontrol,metric = 'Spec') 
        c50_pred_up = predict(c50_fit_up,base_teste)
        mc_c50_up = confusionMatrix(c50_pred_up,base_teste$evasao)
        return(list(c50_fit_up,c50_pred_up,mc_c50_up))
      }else if(balance=="down"){
        c50_fit_dwn = train(x=dplyr::select(dwn_data,-evasao),y=dwn_data$evasao,method="C5.0",tuneGrid = grid,trControl=fitcontrol,metric = 'Spec') 
        c50_pred_dwn = predict(c50_fit_dwn,base_teste)
        mc_c50_dwn = confusionMatrix(c50_pred_dwn,base_teste$evasao)
       	return(list(c50_fit_dwn,c50_pred_dwn,mc_c50_dwn))
      }else{
        c50_fit = train(x=dplyr::select(base_treina,-evasao),y=base_treina$evasao,method="C5.0",tuneGrid = grid,trControl=fitcontrol,metric = 'Spec') 
        c50_pred = predict(c50_fit,base_teste)
        mc_c50 = confusionMatrix(c50_pred,base_teste$evasao)
        return(list(c50_fit,c50_pred,mc_c50))
      }
    }
    
    
    # Classificando com regressao logistica
    if(classifier=="all" | classifier=="reglog"){
      if(balance=="up"){
        reglog_fit_up = train(evasao ~ .,data=up_data ,method="glm",family=binomial(link="logit"),trControl=fitcontrol,metric = 'Spec',maxit=100) 
        reg_pred_up = predict(reglog_fit_up,base_teste)
        mc_reg_up = confusionMatrix(reg_pred_up,base_teste$evasao)
        return(list(reglog_fit_up,reg_pred_up,mc_reg_up))
      }else if(balance=="down"){
        reglog_fit_dwn = train(evasao ~ .,data=dwn_data ,method="glm",family=binomial(link="logit"),trControl=fitcontrol,metric = 'Spec',maxit=100) 
        reg_pred_dwn = predict(reglog_fit_dwn,base_teste)
        mc_reg_dwn = confusionMatrix(reg_pred_dwn,base_teste$evasao)
        return(list(reglog_fit_dwn,reg_pred_dwn,mc_reg_dwn))
      }else{
        reglog_fit = train(evasao ~ .,data=base_treina ,method="glm",family=binomial(link="logit"),trControl=fitcontrol,metric = 'Spec',maxit=100) 
        reg_pred = predict(reglog_fit,base_teste)
        mc_reg = confusionMatrix(reg_pred,base_teste$evasao)
        return(list(reglog_fit,reg_pred,mc_reg))
      }
    }
    
    # Classificando com redes neurais
    if(classifier=="all" | classifier=="Nnet"){
      if(balance=="up"){
        nnet_fit_up = train(evasao ~ .,data=up_data,method="nnet",trControl=fitcontrol,metric = 'Spec', maxit=1000,MaxNWts=3000)
        nnet_pred_up = predict(nnet_fit_up,base_teste)
        mc_nnet_up = confusionMatrix(nnet_pred_up,base_teste$evasao)
        return(list(nnet_fit_up,nnet_pred_up,mc_nnet_up))
      }else if(balance=="down"){
        nnet_fit_dwn = train(evasao ~ .,data=dwn_data,method="nnet",trControl=fitcontrol,metric = 'Spec', maxit=1000,MaxNWts=3000) 
        nnet_pred_dwn = predict(nnet_fit_dwn,base_teste)
        mc_nnet_dwn = confusionMatrix(nnet_pred_dwn,base_teste$evasao)
        return(list(nnet_fit_dwn,nnet_pred_dwn,mc_nnet_dwn))
      }else{
        nnet_fit = train(evasao ~ ., data=base_treina,method="nnet",trControl=fitcontrol,metric = 'Spec', maxit=1000,MaxNWts=3000) 
        nnet_pred = predict(nnet_fit,base_teste)
        mc_nnet = confusionMatrix(nnet_pred,base_teste$evasao)
        return(list(nnet_fit,nnet_pred,mc_nnet))
      }
    }
  }
