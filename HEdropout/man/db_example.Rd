\name{db_example}
\alias{db_example}
\docType{data}
\title{Data frame containing random raw data on 1000 students of higher education in Brazil.
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
A data frame with 1000 students. The primary attribute is "evasao" which indicates whether or not a given student has dropped out of his/her major.
It should be used as a test for the main function in this package, "classify_dropout".
}
\usage{data("db_example")}
\format{
  A data frame with 1000 observations on the following 159 variables.
  \describe{
    \item{\code{semestre_ingresso}}{a numeric vector}
    \item{\code{vagas}}{a numeric vector}
    \item{\code{candidatos}}{a numeric vector}
    \item{\code{co_grau_academico}}{a numeric vector}
    \item{\code{co_nivel_academico}}{a numeric vector}
    \item{\code{co_ocde_area_geral}}{a numeric vector}
    \item{\code{areas_3}}{a numeric vector}
    \item{\code{abi}}{a numeric vector}
    \item{\code{in_integral_curso}}{a numeric vector}
    \item{\code{in_matutino_curso}}{a numeric vector}
    \item{\code{in_vespertino_curso}}{a numeric vector}
    \item{\code{in_noturno_curso}}{a numeric vector}
    \item{\code{nu_integralizacao_integral}}{a numeric vector}
    \item{\code{nu_integralizacao_matutino}}{a numeric vector}
    \item{\code{nu_integralizacao_vespertino}}{a numeric vector}
    \item{\code{nu_integralizacao_noturno}}{a numeric vector}
    \item{\code{co_nacionalidade_aluno}}{a numeric vector}
    \item{\code{in_reserva_vagas}}{a numeric vector}
    \item{\code{in_bolsa_estagio}}{a numeric vector}
    \item{\code{in_bolsa_monitoria}}{a numeric vector}
    \item{\code{in_bolsa_pesquisa}}{a numeric vector}
    \item{\code{in_bolsa_extensao}}{a numeric vector}
    \item{\code{num_ies}}{a numeric vector}
    \item{\code{evasao}}{a numeric vector}
    \item{\code{doc_exercicio}}{a numeric vector}
    \item{\code{doc_qualifcacao}}{a numeric vector}
    \item{\code{doc_exer_outro_org}}{a numeric vector}
    \item{\code{doc_afastado_outro}}{a numeric vector}
    \item{\code{doc_falecido}}{a numeric vector}
    \item{\code{doc_sem_grad}}{a numeric vector}
    \item{\code{doc_graduacao}}{a numeric vector}
    \item{\code{doc_especializacao}}{a numeric vector}
    \item{\code{doc_mestrado}}{a numeric vector}
    \item{\code{doc_doutorado}}{a numeric vector}
    \item{\code{doc_integ_de}}{a numeric vector}
    \item{\code{doc_integ_sem_de}}{a numeric vector}
    \item{\code{doc_temp_parcial}}{a numeric vector}
    \item{\code{doc_horista}}{a numeric vector}
    \item{\code{doc_brasileiro}}{a numeric vector}
    \item{\code{doc_brasileiro_nat}}{a numeric vector}
    \item{\code{doc_estrangeiro}}{a numeric vector}
    \item{\code{in_estuda_classe_hospitalar}}{a numeric vector}
    \item{\code{id_dependencia_adm_esc}}{a numeric vector}
    \item{\code{id_localizacao_esc}}{a numeric vector}
    \item{\code{sit_func_esc}}{a numeric vector}
    \item{\code{st_conclusao}}{a numeric vector}
    \item{\code{ano_concluiu}}{a numeric vector}
    \item{\code{tp_escola}}{a numeric vector}
    \item{\code{in_tp_ensino}}{a numeric vector}
    \item{\code{tp_estado_civil}}{a numeric vector}
    \item{\code{tp_cor_raca}}{a numeric vector}
    \item{\code{in_baixa_visao}}{a numeric vector}
    \item{\code{in_cegueira}}{a numeric vector}
    \item{\code{in_surdez}}{a numeric vector}
    \item{\code{in_deficiencia_auditiva}}{a numeric vector}
    \item{\code{in_surdo_cegueira}}{a numeric vector}
    \item{\code{in_deficiencia_fisica}}{a numeric vector}
    \item{\code{in_deficiencia_mental}}{a numeric vector}
    \item{\code{in_deficit_atencao}}{a numeric vector}
    \item{\code{in_dislexia}}{a numeric vector}
    \item{\code{in_gestante}}{a numeric vector}
    \item{\code{in_lactante}}{a numeric vector}
    \item{\code{in_idoso}}{a numeric vector}
    \item{\code{in_autismo}}{a numeric vector}
    \item{\code{in_sabatista}}{a numeric vector}
    \item{\code{in_braille}}{a numeric vector}
    \item{\code{in_ampliada_24}}{a numeric vector}
    \item{\code{in_ampliada_18}}{a numeric vector}
    \item{\code{in_ledor}}{a numeric vector}
    \item{\code{in_acesso}}{a numeric vector}
    \item{\code{in_transcricao}}{a numeric vector}
    \item{\code{in_libras}}{a numeric vector}
    \item{\code{in_leitura_labial}}{a numeric vector}
    \item{\code{in_mesa_cadeira_rodas}}{a numeric vector}
    \item{\code{in_mesa_cadeira_separada}}{a numeric vector}
    \item{\code{in_guia_interprete}}{a numeric vector}
    \item{\code{in_certificado}}{a numeric vector}
    \item{\code{nu_nota_comp1}}{a numeric vector}
    \item{\code{nu_nota_comp2}}{a numeric vector}
    \item{\code{nu_nota_comp3}}{a numeric vector}
    \item{\code{nu_nota_comp4}}{a numeric vector}
    \item{\code{nu_nota_comp5}}{a numeric vector}
    \item{\code{nu_nota_redacao}}{a numeric vector}
    \item{\code{q001}}{a character vector}
    \item{\code{q002}}{a character vector}
    \item{\code{q003}}{a character vector}
    \item{\code{q004}}{a numeric vector}
    \item{\code{q005}}{a character vector}
    \item{\code{q006}}{a character vector}
    \item{\code{q007}}{a character vector}
    \item{\code{q008}}{a character vector}
    \item{\code{q009}}{a character vector}
    \item{\code{q010}}{a character vector}
    \item{\code{q011}}{a character vector}
    \item{\code{q012}}{a character vector}
    \item{\code{q013}}{a character vector}
    \item{\code{q014}}{a character vector}
    \item{\code{q015}}{a character vector}
    \item{\code{q016}}{a character vector}
    \item{\code{q017}}{a character vector}
    \item{\code{q018}}{a character vector}
    \item{\code{q019}}{a character vector}
    \item{\code{q020}}{a character vector}
    \item{\code{q021}}{a character vector}
    \item{\code{q022}}{a character vector}
    \item{\code{q023}}{a numeric vector}
    \item{\code{q024}}{a numeric vector}
    \item{\code{q025}}{a numeric vector}
    \item{\code{q026}}{a numeric vector}
    \item{\code{q027}}{a numeric vector}
    \item{\code{q028}}{a numeric vector}
    \item{\code{q029}}{a numeric vector}
    \item{\code{q030}}{a character vector}
    \item{\code{q031}}{a character vector}
    \item{\code{q032}}{a character vector}
    \item{\code{q033}}{a character vector}
    \item{\code{q034}}{a character vector}
    \item{\code{q035}}{a character vector}
    \item{\code{q036}}{a character vector}
    \item{\code{q037}}{a character vector}
    \item{\code{q038}}{a character vector}
    \item{\code{q039}}{a character vector}
    \item{\code{q040}}{a numeric vector}
    \item{\code{q041}}{a character vector}
    \item{\code{q042}}{a numeric vector}
    \item{\code{q043}}{a numeric vector}
    \item{\code{q044}}{a numeric vector}
    \item{\code{q045}}{a numeric vector}
    \item{\code{q046}}{a numeric vector}
    \item{\code{q047}}{a character vector}
    \item{\code{q048}}{a character vector}
    \item{\code{q049}}{a character vector}
    \item{\code{q050}}{a character vector}
    \item{\code{q051}}{a character vector}
    \item{\code{q052}}{a character vector}
    \item{\code{q053}}{a character vector}
    \item{\code{q054}}{a character vector}
    \item{\code{q055}}{a character vector}
    \item{\code{q056}}{a character vector}
    \item{\code{q057}}{a character vector}
    \item{\code{q058}}{a character vector}
    \item{\code{q059}}{a character vector}
    \item{\code{q060}}{a character vector}
    \item{\code{q061}}{a character vector}
    \item{\code{q062}}{a character vector}
    \item{\code{q063}}{a character vector}
    \item{\code{q064}}{a character vector}
    \item{\code{q065}}{a character vector}
    \item{\code{q066}}{a character vector}
    \item{\code{q067}}{a character vector}
    \item{\code{q068}}{a character vector}
    \item{\code{q069}}{a character vector}
    \item{\code{q070}}{a character vector}
    \item{\code{q071}}{a character vector}
    \item{\code{q072}}{a character vector}
    \item{\code{q073}}{a character vector}
    \item{\code{q074}}{a character vector}
    \item{\code{q075}}{a character vector}
    \item{\code{q076}}{a character vector}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(db_example)
}
\keyword{datasets}
