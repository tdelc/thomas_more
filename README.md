# thomas_more
Code source de la réplication de la méthode de classification politique de Thomas More

Toutes les infos sur l'usage sont présentées dans cet article : https://blogs.mediapart.fr/thomas-delclite/blog/070426/pourquoi-lia-classe-la-meteo-gauche-et-le-negationnisme-droite

Le dashboard produit par ce code est disponible ici : https://tdelc-thomas-more.share.connect.posit.cloud/

## Corpus vidéo
Les données proviennent de cinq chaînes YouTube : franceinfo (@franceinfo), BFMTV (@BFMTV), LCI (@LCI), CNEWS (@CNEWSofficiel) et Europe 1 (@Europe1). La chaîne YouTube de CNEWS ne proposant que très peu de vidéos, et quasi exclusivement des capsules de moins de deux minutes, les vidéos d'Europe 1 ont été intégrées au corpus CNEWS. Ce choix se justifie par le partenariat éditorial désormais établi entre les deux chaînes, Europe 1 rediffusant une large part du contenu de CNEWS (émissions de Pascal Praud, Laurence Ferrari, etc.).

Seules les vidéos d'une durée supérieure à deux minutes ont été conservées, afin d'exclure les capsules destinées aux réseaux sociaux (et pour suivre la méthodologie de Thomas More). Le corpus est limité aux vidéos diffusées en 2025.

Les transcriptions ont été obtenues via les sous-titres automatiques générés par YouTube.

## Analyse IA
Les transcriptions complètes ont été soumises au modèle Gemini 2.5 Flash (Google) via l'API Gemini, en utilisant les prompts publiés par l'Institut Thomas More dans le cadre de son Rapport 35 (février 2026). La méthodologie est donc identique à celle de Thomas More, à deux différences près : le modèle utilisé (Gemini 2.5 Flash au lieu de Gemini 1.5 Pro, ce dernier ayant été retiré par Google) et le périmètre (quatre chaînes d'information en continu au lieu du seul audiovisuel public).

## Données produites
Trois jeux de données sont issus du pipeline :

- L'analyse par vidéo attribue à chaque vidéo une note de biais (0–100) et une orientation (gauche, droite ou neutre), reflétant l'orientation idéologique globale du contenu ;

- L'analyse par personnalité identifie, pour chaque vidéo, les personnalités et formations politiques mentionnées, leur courant politique et l'attitude du journaliste à leur égard (échelle de -10 à +10) ;

- L'analyse par thème classe chaque vidéo dans une à trois thématiques parmi une liste fermée de 20 catégories, avec pour chacune une note de biais et une orientation.

## Retraitements manuels
Les courants politiques attribués par l'IA ont été partiellement recodés manuellement afin de correspondre aux catégories utilisées par Thomas More. Les thèmes ont fait l'objet du même retraitement. Un nettoyage manuel complémentaire a été effectué pour retirer les scories (personnalités mal identifiées, thèmes incohérents, doublons).

## Avertissement
Ce dashboard ne prétend pas valider la méthodologie de l'Institut Thomas More ni affirmer que l'analyse par IA générative constitue une mesure fiable de l'orientation idéologique des médias. L'objectif est triple : répliquer la démarche sur France Info afin de comparer les résultats avec ceux publiés par Thomas More ; étendre l'analyse aux autres chaînes d'information en continu, ce que Thomas More n'a pas fait ; et documenter les biais méthodologiques détectés au fil de l'exercice (gestion de l'ironie, confusion entre fait et opinion, sensibilité au prompt, axe unidimensionnel gauche–droite, biais propres au modèle).

Le code source du pipeline et du dashboard est disponible librement.