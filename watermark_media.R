# =============================================================================
# watermark_media.R : médias du site avec mention de licence incrustée
#
# Ce que fait ce script (à lancer depuis la racine du projet, ouvert avec le
# fichier .Rproj dans RStudio) :
#   - photos : .tif -> .webp (900 px max) avec la mention en bas, au centre
#   - vidéos : .avi -> .mp4 (1280 px max) avec la mention en bas à gauche
#     (positions modifiables : position_video et position_photo)
# Il REMPLACE le script de conversion précédent : il repart des ORIGINAUX
# (meilleure qualité qu'un ré-encodage des .mp4 déjà réduits).
# Les fichiers produits écrasent ceux des dossiers images/.../web/.
# =============================================================================
library(av)
library(magick)

# ---- [MODIFIER] réglages ----------------------------------------------------
mention   <- "© Emma Belaud · CC BY-NC-SA 4.0"   # si le © s'affiche mal : "(c) Emma Belaud ..."
opacite   <- 0.7        # 1 = mention opaque, 0.4 = très discrète
filigrane_photos <- TRUE
filigrane_videos <- TRUE

# Position de la mention. Valeurs possibles : "southwest" (bas gauche),
# "south" (bas centre), "southeast" (bas droite), "northwest", "north", "northeast".
position_video <- "southwest"   # à gauche : les barres d'échelle sont à droite
position_photo <- "south"       # au centre : reste visible après le recadrage carré

# Où sont les originaux maintenant ? Une fenêtre te demande de choisir les dossiers.
dossier_avi <- choose.dir(caption = "Dossier des vidéos .avi ORIGINALES")
dossier_tif <- choose.dir(caption = "Dossier des photos .tif ORIGINALES")

sortie_video <- "images/animation"
sortie_photo <- "images/photos_animals"
dir.create(sortie_video, showWarnings = FALSE, recursive = TRUE)
dir.create(sortie_photo, showWarnings = FALSE, recursive = TRUE)

# ---- Fonction : ajoute la mention sur une image ----------------------------
# gravity : emplacement de la mention (voir position_video / position_photo).
# La taille du texte suit la largeur de l'image. Une ombre sombre sous le texte
# blanc le rend lisible sur fond clair comme sur fond sombre.
filigrane <- function(img, texte, gravity = "southeast") {
  largeur <- image_info(img)$width
  taille  <- max(12, round(largeur * 0.017))
  marge   <- round(largeur * 0.012)
  blanc   <- sprintf("rgba(255,255,255,%s)", opacite)
  noir    <- sprintf("rgba(0,0,0,%s)", opacite * 0.8)
  img <- image_annotate(img, texte, gravity = gravity, size = taille,
                        color = noir,
                        location = sprintf("+%d+%d", marge + 1, marge - 1))
  image_annotate(img, texte, gravity = gravity, size = taille,
                 color = blanc,
                 location = sprintf("+%d+%d", marge, marge))
}

# ---- Photos : .tif -> .webp ------------------------------------------------
tifs <- list.files(dossier_tif, pattern = "\\.tiff?$", full.names = TRUE, ignore.case = TRUE)
vus  <- character()
for (f in tifs) {
  nom <- tolower(gsub(" ", "_", tools::file_path_sans_ext(basename(f))))
  if (nom %in% vus) next            # doublon "insecta larva" / "insecta_larva"
  vus <- c(vus, nom)
  message("Photo : ", basename(f))
  img <- image_read(f)[1]
  img <- image_convert(img, colorspace = "sRGB")
  img <- image_resize(img, "900x>")
  if (filigrane_photos) img <- filigrane(img, mention, gravity = position_photo)
  image_write(img, file.path(sortie_photo, paste0(nom, ".webp")),
              format = "webp", quality = 82)
}

# ---- Vidéos : .avi -> images -> mention -> .mp4 ----------------------------
filtre_even <- "scale=w='trunc(iw/2)*2':h='trunc(ih/2)*2',format=yuv420p"  # dimensions paires (obligatoire en H.264)
avis <- list.files(dossier_avi, pattern = "\\.avi$", full.names = TRUE, ignore.case = TRUE)
for (f in avis) {
  nom <- tools::file_path_sans_ext(basename(f))
  message("Vidéo : ", nom)
  
  fps <- av_media_info(f)$video$framerate
  if (is.null(fps) || !is.finite(fps) || fps <= 0) fps <- 5
  
  tmp <- file.path(tempdir(), nom)
  dir.create(tmp, showWarnings = FALSE)
  images <- av_video_images(f, destdir = tmp, format = "jpg")
  
  for (im in images) {                       # réduit puis marque chaque image
    img <- image_resize(image_read(im), "1280x>")
    if (filigrane_videos) img <- filigrane(img, mention, gravity = position_video)
    image_write(img, im, format = "jpeg", quality = 88)
  }
  
  av_encode_video(images, file.path(sortie_video, paste0(nom, ".mp4")),
                  framerate = fps, vfilter = filtre_even, verbose = FALSE)
  unlink(tmp, recursive = TRUE)
}

# ---- Contrôle : taille des fichiers produits (en Mo) -------------------------
produits <- list.files(c(sortie_video, sortie_photo), full.names = TRUE)
print(data.frame(fichier = basename(produits), Mo = round(file.size(produits) / 1e6, 2)))
