#!/usr/bin/env bash
# =============================================================================
# convert_media.sh — prépare les médias de la page « soil fauna » pour le web
#
# Pourquoi : les navigateurs ne lisent pas les .avi et n'affichent pas les .tif.
# Ce script crée, SANS toucher aux originaux :
#   images/animation/web/<nom>.mp4        (H.264, sans son, léger, démarrage rapide)
#   images/photos_animals/web/<nom>.webp  (max 900 px de large)
#
# Utilisation (depuis la racine du dépôt emmabelaud.github.io, terminal MSYS2) :
#   pacman -S mingw-w64-ucrt-x86_64-ffmpeg      # une seule fois, si ffmpeg absent
#   bash convert_media.sh
#
# Relançable sans risque : les fichiers déjà convertis sont ignorés (option -n).
# Les noms avec espaces sont normalisés ("formicidae solenopsis.tif" devient
# "formicidae_solenopsis.webp"). Si deux fichiers donnent le même nom
# (ex. "insecta larva.tif" et "insecta_larva.tif"), le second est ignoré.
# =============================================================================
set -euo pipefail
shopt -s nullglob

cd "$(dirname "$0")/images"
mkdir -p animation/web photos_animals/web

# --- Vidéos : .avi -> .mp4 ---------------------------------------------------
# crf : qualité (18 = quasi sans perte, 28 = très compressé). 26 = bon compromis.
for f in animation/*.avi; do
  out="animation/web/$(basename "${f%.avi}").mp4"
  echo "Vidéo  : $f -> $out"
  ffmpeg -nostdin -loglevel error -n -i "$f" \
    -an -c:v libx264 -preset slow -crf 26 -pix_fmt yuv420p \
    -vf "scale='min(1280,iw)':-2" -movflags +faststart "$out"
done

# --- Photos : .tif -> .webp ----------------------------------------------------
for f in photos_animals/*.tif; do
  base="$(basename "${f%.tif}")"
  base="${base// /_}"      # espaces -> underscores
  base="${base,,}"         # minuscules
  out="photos_animals/web/${base}.webp"
  echo "Photo  : $f -> $out"
  ffmpeg -nostdin -loglevel error -n -i "$f" -frames:v 1 \
    -vf "scale='min(900,iw)':-2" -quality 82 "$out"
done

echo "Terminé."
