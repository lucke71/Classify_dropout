preprocessamento <- function(database){
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
  
  # Removendo as colunas que nao possuem variancia - as colunas a ser retiradas pode variar de acordo com os dados (nao pode ser retirada a evasao)
  nzv = nearZeroVar(database)
  if(any(names(database)[nzv]=="evasao")){
    nzv = nzv[-which(names(database)[nzv]=="evasao")]  
  }
  database = database[,-nzv]
  database
}