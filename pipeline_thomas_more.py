#!/usr/bin/env python3
"""
Pipeline de réplication de la méthodologie Thomas More (Rapport 35, février 2026)
================================================================================

Analyse de l'orientation idéologique des chaînes d'information en continu
(BFMTV, CNEWS, LCI, France Info) via l'API Gemini.

Usage :
    python pipeline_thomas_more.py --config config.yaml
    python pipeline_thomas_more.py --config config.yaml --sample 50
    python pipeline_thomas_more.py --config config.yaml --resume
    python pipeline_thomas_more.py --config config.yaml --aggregate-only

Prérequis :
    pip install google-genai pandas pyyaml tqdm

Pour activer venv:
    .venv/Scripts/activate

Auteur : Thomas Delclite
Date   : Mars 2026
"""

import argparse
import json
import logging
import os
import sys
import time
from datetime import datetime
from pathlib import Path
from typing import Optional

import pandas as pd
import yaml
from tqdm import tqdm

# ---------------------------------------------------------------------------
# Configuration du logging
# ---------------------------------------------------------------------------
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    handlers=[
        logging.FileHandler("pipeline.log", encoding="utf-8"),
        logging.StreamHandler(sys.stdout),
    ],
)
logger = logging.getLogger(__name__)

# ===========================================================================
# SECTION 1 : PROMPTS (reproduits du PDF Thomas More, Rapport 35)
# ===========================================================================
# Source : institut-thomas-more.org/wp-content/uploads/2026/02/
#          202602-ITM-Rapport35-Promptsdanalyses.pdf
# Les variables [transcript], [themes] et le titre sont injectés à l'exécution.

PROMPT_BASE = """Votre tâche est d'analyser un contenu audiovisuel (transcription) et d'évaluer l'orientation \
politique de l'intention du chroniqueur ou de l'émetteur du message sur une échelle de 0 à 100, où :
0 = aucune opinion de nature politique détectable = orientation = "neutre"
1 à 100 = biais détecté, plus la valeur est élevée, plus le biais est fort
orientation = "gauche" ou "droite" selon la direction idéologique de l'émetteur.

L'objectif est d'identifier les biais idéologiques à travers :
● les faits mis en avant,
● les thèmes abordés,
● les éléments de langage,
● la détection et l'interprétation des outils rhétoriques (ton, ironie, satire) pour déterminer \
l'intention réelle de l'émetteur.
et d'expliquer clairement le choix de la note.

⚠ Faire très attention au ton, à l'ironie, au sous-entendu. Si l'ironie ou la satire est détectée, la \
note doit refléter l'intention critique de l'émetteur et non l'idéologie (ou le contenu brut) qui est la \
cible de la moquerie. Si aucune ironie n'est détectée, la note reflète le contenu idéologique direct.

Structure de sortie attendue :
Renvoyer exclusivement un objet JSON sous la forme :
{
  "note": <0-100>,
  "orientation": "neutre",
  "analyse": {
    "analyse_factuelle": {
      "faits_identifies": ["Fait 1", "Fait 2"]
    },
    "themes_abordes": [
      {"theme": "Thème 1", "description": "Description concise", "orientation_politique": "Interprétation"}
    ],
    "elements_de_langage": [
      {"element": "Élément 1", "analyse": "Impact idéologique ou orientation révélée"}
    ],
    "validation_coherence": {
      "explication": "Justification concise reliant les biais détectés à la note et à l'orientation"
    }
  }
}

Rappel des Règles :
Note (0–100) :
● 0 = aucun biais politique détectable = orientation = "neutre"
● 1–20 = biais léger
● 21–50 = biais marqué
● 51-70 = Biais partisan
● 71–100 = biais très fort / clairement orienté

Orientation :
⚠ OBLIGATOIREMENT une de ces trois valeurs exactes, sans texte supplémentaire :
● "gauche"
● "droite"
● "neutre"
⚠ Ne jamais ajouter de texte après ces valeurs.
⚠ Si le biais est très faible mais détectable, choisir "gauche" ou "droite" selon la direction, pas "neutre".
⚠ "neutre" est réservé aux cas où AUCUN biais n'est détectable (note = 0).

Analyse : Concise, Factuelle, Sensible au ton et à l'ironie.
Justification claire dans validation_coherence.explication.

[transcript]"""

PROMPT_DETECT_PERSO = """Contexte :
Vous recevez en entrée la transcription textuelle (ou le titre + transcription) d'une séquence \
audio (émission, chronique, extrait radio).

Objectif :
Identifier toutes les personnalités politiques françaises *contemporaines* et toutes les \
formations/partis politiques français mentionnées, que ce soit :
● explicitement dans la séquence,
● implicitement via le titre ou par allusion claire et non ambiguë.

Définitions strictes :
1) Personnalité politique (FRANCE UNIQUEMENT)
Est considérée comme personnalité politique toute personne française (vivante ou décédée) \
appartenant à au moins une des catégories suivantes :
● Président, ancien président, Premier ministre, ancien Premier ministre.
● Membre actuel du gouvernement ou ancien ministre.
● Député, sénateur, député européen, élu local majeur.
● Dirigeant, cadre national ou porte-parole d'un parti politique français.
● Candidat déclaré à une élection nationale.

2) Formation politique (FRANCE UNIQUEMENT)
Réservé aux entités suivantes :
● Partis politiques français officiellement constitués.
● Groupes parlementaires identifiés dans le texte.

Méthode d'identification :
● Inclure les mentions explicites (nom prononcé).
● Inclure les mentions implicites si et seulement si l'allusion est claire et attribuable sans ambiguïté.
● Ne pas extrapoler ou inventer une affiliation implicite.

Structure de sortie attendue :
Renvoyer exclusivement un tableau JSON (array) :
[
  {
    "type": "<'personnalité' ou 'formation'>",
    "nom": "<Nom complet ou nom du parti>",
    "courant_politique": "<Affiliation exacte en octobre 2025>",
    "attitude_journaliste": <Valeur entre -10 et +10>,
    "justification": "<court paragraphe justifiant la note>",
    "citation": "<citation exacte tirée de la chronique>",
    "pondération": "<faible | moyenne | forte>",
    "mention_mode": "<explicite | implicite>"
  }
]
Si aucune personnalité ou formation n'est détectée, renvoyer un tableau vide : []

Barème d'attitude / ton du journaliste :
● -10 à -9 = hostilité très nette
● -5 à -8 = critique marquée
● -2 à -4 = légère critique
● 0 = neutre (strictement factuel)
● +2 à +4 = légère complaisance
● +5 à +8 = forte complaisance
● +9 à +10 = complaisance totale

⚠ N'attribuer une critique ou une complaisance qu'en présence d'indices lexicaux explicites.
⚠ Les propos rapportés ne reflètent jamais l'attitude du journaliste sauf si celui-ci approuve ou commente.

Pondération des mentions :
● Forte = sujet principal, omniprésent.
● Moyenne = rôle secondaire mais récurrent.
● Faible = simple citation ou anecdote.

Consignes de format :
● Toujours un seul tableau JSON, sans aucun texte extérieur.
● Toujours inclure toutes les clés, même si certaines valeurs sont nulles.
● Ne jamais inclure d'entités non françaises ou non politiques.

"""

PROMPT_DETECT_THEMES = """Vous êtes un assistant chargé d'extraire jusqu'à 3 thématiques dominantes d'une \
transcription (émission, chronique) et de les classer impérativement dans l'une des thématiques \
définies par les préoccupations des Français.

Thématiques (liste exhaustive) :
1. La santé
2. La lutte contre la délinquance
3. L'éducation
4. La hausse des prix, l'inflation
5. Le relèvement des salaires et du pouvoir d'achat
6. La lutte contre le terrorisme
7. La réduction de la dette publique
8. La lutte contre la précarité
9. La lutte contre l'immigration clandestine
10. Les retraites
11. La maîtrise du niveau des impôts
12. La sauvegarde des services publics
13. La lutte contre le chômage
14. Le logement
15. L'amélioration de la situation dans les banlieues
16. La protection de l'environnement / dérèglement climatique
17. La guerre Russie–Ukraine
18. La guerre Israël–Hamas / Hezbollah
19. L'Europe et l'Union européenne
20. Autre

Instructions :
● Identifiez jusqu'à 3 thématiques dominantes (2-4 mots chacune)
● Pour chaque thématique, rattachez-la à une thématique parmi la liste ci-dessus en reprenant exactement l'intitulé
● Si une thématique ne correspond à aucune de la liste, attribuez « Autre » comme theme
● Si la transcription traite 1 ou 2 sujets seulement, renvoyez une liste de longueur correspondante
● Ne créez jamais de nouvelle thématique et reprenez exactement l'intitulé de la thématique choisie

⚠ IMPORTANT ⚠
NE CREEZ JAMAIS DE NOUVELLES THÉMATIQUES. Si le sous-thème ne peut pas être rangé dans la liste, placez-le dans 'Autre'.
Le sous-thème (2-4 mots) doit être spécifique.

Retournez uniquement une liste JSON :
[
  { "theme": "<Thématique ou Autre>", "sous-thème": "<Sous-thématique 1>" },
  { "theme": "<Thématique ou Autre>", "sous-thème": "<Sous-thématique 2>" }
]

Si aucun thème n'est détecté, renvoyez un tableau vide : []

Consignes de format :
● Toujours un seul tableau JSON, sans aucun texte extérieur.
● Toujours inclure les deux clés "theme" et "sous-thème" pour chaque entrée.
● Les intitulés de thèmes doivent correspondre exactement à la liste fournie.

Transcription :
[transcript]"""

PROMPT_DETECT_THEMES_BIAS = """Vous recevez une transcription et une liste de thèmes **déjà identifiés** dans cette transcription.

Votre tâche est d'analyser le traitement idéologique de chaque thème fourni et d'évaluer \
l'orientation politique de l'intention du chroniqueur ou de l'émetteur du message pour chaque \
thème sur une échelle de 0 à 100, où :
● 0 = aucun biais politique détectable = orientation = "neutre"
● 1 à 100 = biais détecté, plus la valeur est élevée, plus le biais est fort
● orientation = "gauche" ou "droite" selon la direction idéologique de l'émetteur pour ce thème.

⚠ IMPORTANT ⚠
Les thèmes vous sont fournis. Vous ne devez PAS détecter de nouveaux thèmes, mais analyser \
uniquement le traitement idéologique des thèmes fournis.

⚠ Faire très attention au ton, à l'ironie, au sous-entendu.
⚠ Ne jamais renvoyer une note de 0 si un biais, même léger, est détectable pour un thème.

Rappel des Règles pour la note (0–100) :
● 0 = aucun biais politique détectable = orientation = "neutre"
● 1–20 = biais léger
● 21–50 = biais marqué
● 51-70 = Biais partisan
● 71–100 = biais très fort / clairement orienté

Orientation :
⚠ OBLIGATOIREMENT une de ces trois valeurs exactes : "gauche", "droite", "neutre"
⚠ "neutre" est réservé aux cas où AUCUN biais n'est détectable (note = 0).

Structure de sortie attendue :
[
  {
    "theme": "<Thématique exacte du thème fourni>",
    "sous-thème": "<Sous-thématique exacte du thème fourni>",
    "note": <0-100>,
    "orientation": "neutre"
  }
]

⚠ Vous devez analyser TOUS les thèmes fournis, dans l'ordre.
⚠ Conservez exactement les valeurs "theme" et "sous-thème" telles qu'elles sont fournies.
⚠ Si aucun thème n'est fourni, renvoyez un tableau vide : []

Thèmes à analyser :
[themes]

Transcription :
[transcript]"""


# ===========================================================================
# SECTION 2 : GESTION DE L'API GEMINI
# ===========================================================================

class GeminiClient:
    """Client pour l'API Gemini avec retry et rate limiting."""

    def __init__(self, api_key: str, model_name: str = "gemini-2.5-flash",
                 max_retries: int = 3, requests_per_minute: int = 10):
        try:
            from google import genai
        except ImportError:
            logger.error("Package google.genai non installé. "
                         "Exécutez : pip install google-genai")
            sys.exit(1)

        self.client = genai.Client(api_key=api_key)        
        self.model_name = model_name
        self.max_retries = max_retries
        self.min_interval = 60.0 / requests_per_minute  # secondes entre requêtes
        self.last_request_time = 0.0
        self.total_calls = 0
        self.total_errors = 0

    def _rate_limit(self):
        """Attend si nécessaire pour respecter le rate limit."""
        elapsed = time.time() - self.last_request_time
        if elapsed < self.min_interval:
            time.sleep(self.min_interval - elapsed)
        self.last_request_time = time.time()

    def call(self, prompt: str) -> str:
        """Appel API avec retry exponentiel."""
        for attempt in range(1, self.max_retries + 1):
            try:
                self._rate_limit()
                response = self.client.models.generate_content(
                    model=self.model_name,
                    contents=prompt,
                )
                self.total_calls += 1
                return response.text
            except Exception as e:
                self.total_errors += 1
                wait = 2 ** attempt
                logger.warning(f"Erreur API (tentative {attempt}/{self.max_retries}): {e}")
                logger.warning(f"Attente de {wait}s avant retry...")
                time.sleep(wait)

        raise RuntimeError(f"Échec après {self.max_retries} tentatives")


# ===========================================================================
# SECTION 3 : PARSING DES RÉPONSES JSON
# ===========================================================================

def parse_json_response(raw_text: str) -> object:
    """
    Extrait le JSON d'une réponse qui peut contenir des backticks markdown
    ou du texte superflu avant/après le JSON.
    """
    text = raw_text.strip()

    # Retirer les blocs ```json ... ```
    if "```json" in text:
        text = text.split("```json", 1)[1]
        text = text.split("```", 1)[0]
    elif "```" in text:
        text = text.split("```", 1)[1]
        text = text.split("```", 1)[0]

    text = text.strip()

    # Trouver le premier { ou [
    for i, c in enumerate(text):
        if c in "{[":
            text = text[i:]
            break

    try:
        return json.loads(text)
    except json.JSONDecodeError as e:
        logger.error(f"Échec du parsing JSON: {e}")
        logger.debug(f"Texte brut: {raw_text[:500]}")
        return None


# ===========================================================================
# SECTION 4 : FONCTIONS D'ANALYSE (une par prompt)
# ===========================================================================

def analyse_base(client: GeminiClient, transcript: str) -> Optional[dict]:
    """Prompt 1 : orientation globale."""
    prompt = PROMPT_BASE.replace("[transcript]", transcript)
    raw = client.call(prompt)
    result = parse_json_response(raw)
    if result and isinstance(result, dict):
        # Validation minimale
        if "note" in result and "orientation" in result:
            return result
    logger.warning("Réponse base invalide")
    return None


def detect_personnalites(client: GeminiClient, titre: str, transcript: str) -> Optional[list]:
    """Prompt 2 : détection des personnalités et formations."""
    prompt = PROMPT_DETECT_PERSO + f"\nTitre : {titre}\n\n{transcript}"
    raw = client.call(prompt)
    result = parse_json_response(raw)
    if result is not None and isinstance(result, list):
        return result
    logger.warning("Réponse detect_perso invalide")
    return None


def detect_themes(client: GeminiClient, transcript: str) -> Optional[list]:
    """Prompt 3a : détection des thèmes."""
    prompt = PROMPT_DETECT_THEMES.replace("[transcript]", transcript)
    raw = client.call(prompt)
    result = parse_json_response(raw)
    if result is not None and isinstance(result, list):
        return result
    logger.warning("Réponse detect_themes invalide")
    return None


def analyse_themes_bias(client: GeminiClient, themes_json: str,
                        transcript: str) -> Optional[list]:
    """Prompt 3b : biais par thème."""
    prompt = (PROMPT_DETECT_THEMES_BIAS
              .replace("[themes]", themes_json)
              .replace("[transcript]", transcript))
    raw = client.call(prompt)
    result = parse_json_response(raw)
    if result is not None and isinstance(result, list):
        return result
    logger.warning("Réponse themes_bias invalide")
    return None


# ===========================================================================
# SECTION 5 : PIPELINE PRINCIPAL
# ===========================================================================

def process_video(client: GeminiClient, video: dict) -> dict:
    """
    Exécute les 4 prompts pour une vidéo.

    Paramètres
    ----------
    video : dict avec les clés video_id, titre, transcript

    Retourne
    --------
    dict avec les résultats des 4 analyses
    """
    vid = video["video_id"]
    titre = video.get("titre", "")
    transcript = video["transcript"]

    logger.info(f"[{vid}] Début de l'analyse...")

    # --- Étape 1 : Orientation globale ---
    logger.info(f"[{vid}] Prompt 1/4 : base")
    res_base = analyse_base(client, transcript)

    # --- Étape 2 : Personnalités ---
    logger.info(f"[{vid}] Prompt 2/4 : detect_perso")
    res_perso = detect_personnalites(client, titre, transcript)

    # --- Étape 3a : Thèmes ---
    logger.info(f"[{vid}] Prompt 3/4 : detect_themes")
    res_themes = detect_themes(client, transcript)

    # --- Étape 3b : Biais par thème ---
    res_bias = None
    if res_themes and len(res_themes) > 0:
        logger.info(f"[{vid}] Prompt 4/4 : themes_bias")
        themes_json = json.dumps(res_themes, ensure_ascii=False)
        res_bias = analyse_themes_bias(client, themes_json, transcript)
    else:
        logger.info(f"[{vid}] Prompt 4/4 : skipped (aucun thème détecté)")

    logger.info(f"[{vid}] Analyse terminée.")

    return {
        "video_id": vid,
        "timestamp": datetime.now().isoformat(),
        "base": res_base,
        "personnalites": res_perso,
        "themes": res_themes,
        "themes_bias": res_bias,
    }


# ===========================================================================
# SECTION 6 : SAUVEGARDE DES RÉSULTATS
# ===========================================================================

def save_result(result: dict, output_dir: Path):
    """Sauvegarde un résultat individuel en JSON."""
    vid = result["video_id"]
    filepath = output_dir / f"{vid}.json"
    with open(filepath, "w", encoding="utf-8") as f:
        json.dump(result, f, ensure_ascii=False, indent=2)


def get_processed_ids(output_dir: Path) -> set:
    """Récupère les video_id déjà traités (pour le mode resume)."""
    processed = set()
    if output_dir.exists():
        for f in output_dir.glob("*.json"):
            processed.add(f.stem)
    return processed


def aggregate_results(output_dir: Path) -> pd.DataFrame:
    """
    Agrège tous les résultats JSON en un DataFrame résumé.
    Appelée en fin de pipeline pour produire les tableaux d'analyse.
    """
    records = []
    for filepath in sorted(output_dir.glob("*.json")):
        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)

        vid = data["video_id"]
        base = data.get("base") or {}
        themes = data.get("themes") or []
        themes_bias = data.get("themes_bias") or []
        persos = data.get("personnalites") or []

        records.append({
            "video_id": vid,
            "note_base": base.get("note"),
            "orientation_base": base.get("orientation"),
            "nb_personnalites": len(persos),
            "nb_themes": len(themes),
            "themes_list": json.dumps(themes, ensure_ascii=False),
            "themes_bias_list": json.dumps(themes_bias, ensure_ascii=False),
            "personnalites_list": json.dumps(persos, ensure_ascii=False),
        })

    return pd.DataFrame(records)


def flatten_personnalites(output_dir: Path) -> pd.DataFrame:
    """Produit un DataFrame avec une ligne par mention de personnalité."""
    rows = []
    for filepath in sorted(output_dir.glob("*.json")):
        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)
        vid = data["video_id"]
        for p in (data.get("personnalites") or []):
            rows.append({
                "video_id": vid,
                "type": p.get("type"),
                "nom": p.get("nom"),
                "courant_politique": p.get("courant_politique"),
                "attitude": p.get("attitude_journaliste"),
                "ponderation": p.get("pondération", p.get("ponderation")),
                "mention_mode": p.get("mention_mode"),
                "justification": p.get("justification"),
                "citation": p.get("citation"),
            })
    return pd.DataFrame(rows)


def flatten_themes(output_dir: Path) -> pd.DataFrame:
    """Produit un DataFrame avec une ligne par thème détecté (avec biais)."""
    rows = []
    for filepath in sorted(output_dir.glob("*.json")):
        with open(filepath, "r", encoding="utf-8") as f:
            data = json.load(f)
        vid = data["video_id"]

        themes = data.get("themes") or []
        bias = data.get("themes_bias") or []

        # Associer les thèmes et leurs biais
        bias_map = {}
        for b in bias:
            key = (b.get("theme", ""), b.get("sous-thème", b.get("sous-theme", "")))
            bias_map[key] = b

        for t in themes:
            theme_name = t.get("theme", "")
            sous_theme = t.get("sous-thème", t.get("sous-theme", ""))
            b = bias_map.get((theme_name, sous_theme), {})
            rows.append({
                "video_id": vid,
                "theme": theme_name,
                "sous_theme": sous_theme,
                "note_bias": b.get("note"),
                "orientation_bias": b.get("orientation"),
            })
    return pd.DataFrame(rows)


# ===========================================================================
# SECTION 7 : CHARGEMENT DES DONNÉES
# ===========================================================================

def load_corpus(filepath: str) -> pd.DataFrame:
    """
    Charge le corpus depuis un fichier CSV, Excel ou Parquet.
    Colonnes attendues : video_id, chaine, titre, date, transcript, duree_sec
    """
    path = Path(filepath)
    ext = path.suffix.lower()

    if ext == ".csv":
        df = pd.read_csv(path, encoding="utf-8")
    elif ext in (".xlsx", ".xls"):
        df = pd.read_excel(path)
    elif ext == ".parquet":
        df = pd.read_parquet(path)
    else:
        raise ValueError(f"Format non supporté : {ext}")

    # Vérification des colonnes requises
    required = {"video_id", "transcript"}
    missing = required - set(df.columns)
    if missing:
        raise ValueError(f"Colonnes manquantes : {missing}")

    # Nettoyage minimal
    df["transcript"] = df["transcript"].fillna("").astype(str)
    df = df[df["transcript"].str.len() > 50]  # Exclure les transcriptions vides

    logger.info(f"Corpus chargé : {len(df)} vidéos depuis {filepath}")
    return df


# ===========================================================================
# SECTION 8 : CONFIGURATION
# ===========================================================================

DEFAULT_CONFIG = {
    "api_key": "VOTRE_CLE_API_GEMINI",
    "model": "gemini-1.5-pro",
    "corpus_path": "corpus.csv",
    "output_dir": "resultats",
    "max_retries": 3,
    "requests_per_minute": 10,
}


def load_config(config_path: str) -> dict:
    """Charge la configuration depuis un fichier YAML."""
    if not os.path.exists(config_path):
        # Créer un fichier de config par défaut
        with open(config_path, "w", encoding="utf-8") as f:
            yaml.dump(DEFAULT_CONFIG, f, default_flow_style=False, allow_unicode=True)
        logger.info(f"Fichier de configuration créé : {config_path}")
        logger.info("Éditez ce fichier avec votre clé API avant de relancer.")
        sys.exit(0)

    with open(config_path, "r", encoding="utf-8") as f:
        config = yaml.safe_load(f)

    return {**DEFAULT_CONFIG, **config}


# ===========================================================================
# SECTION 9 : POINT D'ENTRÉE
# ===========================================================================

def main():
    parser = argparse.ArgumentParser(
        description="Pipeline de réplication de la méthodologie Thomas More"
    )
    parser.add_argument(
        "--config", default="config.yaml",
        help="Chemin du fichier de configuration YAML (défaut: config.yaml)"
    )
    parser.add_argument(
        "--sample", type=int, default=None,
        help="Nombre de vidéos à traiter (pour un test rapide)"
    )
    parser.add_argument(
        "--resume", action="store_true",
        help="Reprendre le traitement en sautant les vidéos déjà analysées"
    )
    parser.add_argument(
        "--aggregate-only", action="store_true",
        help="Uniquement agréger les résultats existants (pas d'appels API)"
    )
    args = parser.parse_args()

    # --- Chargement de la configuration ---
    config = load_config(args.config)
    output_dir = Path(config["output_dir"])
    output_dir.mkdir(parents=True, exist_ok=True)

    # --- Mode agrégation seule ---
    if args.aggregate_only:
        logger.info("Mode agrégation uniquement")
        df_summary = aggregate_results(output_dir)
        df_perso = flatten_personnalites(output_dir)
        df_themes = flatten_themes(output_dir)

        df_summary.to_csv(output_dir / "resume_global.csv", index=False, encoding="utf-8")
        df_perso.to_csv(output_dir / "personnalites.csv", index=False, encoding="utf-8")
        df_themes.to_csv(output_dir / "themes.csv", index=False, encoding="utf-8")

        logger.info(f"Résumé : {len(df_summary)} vidéos")
        logger.info(f"Personnalités : {len(df_perso)} mentions")
        logger.info(f"Thèmes : {len(df_themes)} détections")
        logger.info(f"Fichiers CSV sauvegardés dans {output_dir}/")
        return

    # --- Chargement du corpus ---
    df = load_corpus(config["corpus_path"])

    # --- Échantillonnage optionnel ---
    if args.sample:
        df = df.sample(n=min(args.sample, len(df)), random_state=42)
        logger.info(f"Échantillon de {len(df)} vidéos")

    # --- Mode resume ---
    processed = set()
    if args.resume:
        processed = get_processed_ids(output_dir)
        logger.info(f"Mode resume : {len(processed)} vidéos déjà traitées")

    # --- Initialisation du client API ---
    if config["api_key"] == "VOTRE_CLE_API_GEMINI":
        logger.error("Clé API non configurée ! Éditez config.yaml")
        sys.exit(1)

    client = GeminiClient(
        api_key=config["api_key"],
        model_name=config["model"],
        max_retries=config["max_retries"],
        requests_per_minute=config["requests_per_minute"],
    )

    # --- Boucle principale ---
    errors = []
    n_total = len(df)
    n_skipped = 0

    logger.info(f"Démarrage du pipeline : {n_total} vidéos à traiter")
    logger.info(f"Modèle : {config['model']}")
    logger.info(f"Rate limit : {config['requests_per_minute']} requêtes/min")
    logger.info(f"Sortie : {output_dir}/")

    for idx, row in tqdm(df.iterrows(), total=n_total, desc="Analyse"):
        vid = str(row["video_id"])

        # Skip si déjà traité
        if vid in processed:
            n_skipped += 1
            continue

        try:
            video_dict = {
                "video_id": vid,
                "titre": str(row.get("titre", row.get("title", ""))),
                "transcript": str(row["transcript"]),
            }
            result = process_video(client, video_dict)
            save_result(result, output_dir)

        except Exception as e:
            logger.error(f"[{vid}] ERREUR : {e}")
            errors.append({"video_id": vid, "error": str(e)})

    # --- Résumé final ---
    logger.info("=" * 60)
    logger.info("PIPELINE TERMINÉ")
    logger.info(f"  Vidéos traitées : {n_total - n_skipped - len(errors)}")
    logger.info(f"  Vidéos skippées (resume) : {n_skipped}")
    logger.info(f"  Erreurs : {len(errors)}")
    logger.info(f"  Appels API total : {client.total_calls}")
    logger.info(f"  Erreurs API total : {client.total_errors}")
    logger.info("=" * 60)

    # Sauvegarder les erreurs
    if errors:
        df_errors = pd.DataFrame(errors)
        df_errors.to_csv(output_dir / "erreurs.csv", index=False, encoding="utf-8")
        logger.info(f"Erreurs sauvegardées dans {output_dir}/erreurs.csv")

    # --- Agrégation finale ---
    logger.info("Agrégation des résultats...")
    df_summary = aggregate_results(output_dir)
    df_perso = flatten_personnalites(output_dir)
    df_themes = flatten_themes(output_dir)

    df_summary.to_csv(output_dir / "resume_global.csv", index=False, encoding="utf-8")
    df_perso.to_csv(output_dir / "personnalites.csv", index=False, encoding="utf-8")
    df_themes.to_csv(output_dir / "themes.csv", index=False, encoding="utf-8")

    logger.info(f"Fichiers CSV finaux dans {output_dir}/")
    logger.info("Terminé !")


if __name__ == "__main__":
    main()
