# Comment mettre à jour l'application

## Intro 
Si pour une raison ou une autre une modification à l'application est nécessaire, voilà la marche à suivre. À la fin, il est toujours nécessaire de reconstruire l'image et la push, ce qui prend autour d'une heure.

## Mettre à jour les packages
Dans le cas où les packages sont trop anciens, que pour une raison ou une autre il est souhaitable de faire une mise à jour de ceux-ci, il suffit de démarrer R dans la racine du projet (pas dans ./R/), exécuter ```renv::activate()``` si ça n'a pas été fait de manière automatique, puis ```renv::update(except = c("ggplot2"))```. Normalement tous les packages sont mis à jour, il faut alors ```renv::snapshot()``` pour enregistrer les changements, tester, puis finalement reconstruire l'image de Radish (voir plus loin). Ces changements doivent être commités (seul le renv.lock change) pour pour être certain que tous soient enregistrés. 

Attention, il y avait un bug avec ggplot2 3.4.2 et pour l'instant seule la version 3.4.1 est validée pour Radish.

## Introduire un changement
Pour introduire un changement dans un des fichiers .R comme module\_input.R, module\_download.R ou module\_geneselect.R, il faut faire attention à bien observer les dépendances décrites dans le fichier ./docs/files_organisation.png. Les autres fichiers sont plus isolés, en particulier ceux définissant des fonctions (utils.R, my\_volcano\_plot.R...). Il faut également toujours tenir compte des attentes en terme d'input de l'application elle-même, décrites dans le fichier ./docs/expected\_data.md.

## Tester 
Pour chaque changement introduit dans Radish, il vaut mieux lancer les quelques tests pour vérifier si rien n'a changé. Pour ce faire il suffit d'exécuter ```devtools::test()```. Les fichiers tests situés en ./inst/app/tests/testthat/ sont alors exécutés un à un. Si les calculs ou interactions (effectués sur les données de démo, à ./inst/extdata) donnent des résultats différents, les tests échouent et indique l'erreur. Les erreurs peuvent être revues avec ```testthat::snapshot_review(path = "./inst/app/tests/testthat")``` puis éventuellement acceptées en utilisant ```testthat::snapshot_accept(path = "./inst/app/tests/testthat")```.


## Reconstruire l'image
Pour reconstruire l'image de Radish, et la pousser ensuite sur le Docker hub, il faut être loggé comme bsgenomique.
Il faut commencer par tagger l'image précédente d'un autre tag et le push s'il on souhaite conserver les deux versions en lignes (```docker tag docker.io/bsgenomique/radish:latest bsgenomique/radish:v1.0.0``` puis ```docker push -a bsgenomique/radish:v1.0.0``` (-a c'est pour all tags)). Ensuite il faut construire l'image : ```docker build -t bsgenomique/radish .``` qui sera automatiquement taggué latest. Puis enfin il faut pusher ```docker push bsgenomique/radish```.
