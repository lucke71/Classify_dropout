# Classify_dropout

Gerador de classificadores para alunos do ensino superior brasileiro com dados do Inep.

Este projeto possui um pacote feito em R para treinar classificadores de evasão, que podem ser utilizados para determinar
em um grupo específico quais alunos possuem maior tendência de evadir.

O pacote contém funções para preparar os dados disponibilizados pelo Instituto Nacional de Estudos e Pesquisas Educacionais
Anísio Teixeira (Inep) em SQL e para treinar classificadores de evasão. Os dados utilizados são os microdados
do Censo da Educação Superior (CES) e do Exame Nacional do Ensino Médio (ENEM). Apesar deles serem disponibilizados
pelo INEP em http://portal.inep.gov.br/microdados, para utilizar esse pacote é necessário 
estar fisicamente dentro do instituto, pois para juntar as bases é preciso utilizar a identificação do CPF dos 
estudantes. Para ter acesso é necessário solicitar ao INEP em 
http://portal.inep.gov.br/web/guest/dados/sedap/solicitacao-de-acesso.

O Minimal Working Example (MWE) encontra-se na documentação das funções do pacote. Foi disponibilizada, com o pacote, uma base de dados com algumas observações para mostrar o funcionamento da função de treinar classificadores. 

Além disso, o framework do projeto foi escrito em formato de dissertação de mestrado. Em breve, o link para acessá-la estará disponível nesta página. Sugere-se a leitura do framework para entender as motivações das escolhas realizadas na construção do pacote.

