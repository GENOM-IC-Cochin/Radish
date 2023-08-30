# Comment Radish démarre

## Comment Radish démarre au boot du serveur
Pour démarrer Radish à chaque boot du serveur, j'ai décidé d'utiliser systemd, qui m'a semblé être la manière recommandée (de ce que j'ai lu) pour démarrer une application à chaque démarrage du serveur.

Son activation est gérée par `systemd`. Il y a donc un fichier service `/etc/systemd/system/container-radish.service` qui définit quand et comment démarrer Radish. Celui-ci indique que la commande doit être exécutée après `network-online.target`, quel service en a besoin, et comment l'éteindre si on veut le faire avec la commande `systemctl stop`.

Ce fichier a été auto généré par Podman via la commande `podman generate systemd`

- Pour avoir avoir le statut du service démarrant Radish :
`sudo systemctl status container-radish.service`
- Pour arrêter Radish : 
`sudo systemctl stop container-radish.service`
- Pour démarrer Radish :
`sudo systemctl start container-radish.service`
- Pour empêcher Radish de démarrer au démarrage du serveur :
`sudo systemctl disable container-radish.service`
- Pour établir le démarrage de Radish au démarrage du serveur :
`sudo systemctl enable container-radish.service`

Je me suis servi de ceci : https://www.redhat.com/sysadmin/container-systemd-persist-reboot sans toute la partie qui crée un utilisateur spécifique parce que je veux que Radish soit exécuté par root.


## Comment relancer Radish

Le fichier situé à `/home/etheimer/update_radish.sh` permet de redémarrer le service qui contrôle Radish.
Mais dans tous les cas il ne consiste qu'en 3 lignes dont je ne suis pas certain que la deuxième soit toujours nécessaire (il devrait faire le pull tout seul).
Il doit toujours être exécuté avec `sudo`.
```sh
systemctl stop container-radish.service
podman pull docker.io/bsgenomique/radish:latest
systemctl start container-radish.service
```
