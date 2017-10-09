# Classify_dropout

Gerador de classificadores para alunos do ensino superior brasileiro com dados do Inep.

Este projeto possui um framework para treinar classificadores de evasão, que podem ser utilizados para determinar
em um grupo específico quais alunos possuem maior tendência de evadir.

O projeto contém a preparação dos dados disponibilizados pelo Instituto Nacional de Estudos e Pesquisas Educacionais
Anísio Teixeira (Inep) em SQL e o pacote em R para treinar os classificadores. Os dados utilizados são os microdados
do Censo da Educação Superior (CES) e do Exame Nacional do Ensino Médio (ENEM). Apesar deles serem disponibilizados
pelo INEP em "http://portal.inep.gov.br/web/guest/microdados", para utilizar este framework é necessário 
estar fisicamente dentro do instituto, pois para juntar as bases é preciso utilizar a identificação do CPF dos 
estudantes. Para ter acesso é necessário solicitar ao INEP em 
"http://portal.inep.gov.br/web/guest/servico-de-informacao-ao-cidadao-sic".

