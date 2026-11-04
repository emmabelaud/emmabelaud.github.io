# =============================================================================
# watermark_media.R : médias du site avec mention de licence incrustée
#
# À lancer depuis la racine du projet (ouvert via le .Rproj dans RStudio).
#   - photos : .tif -> .webp (900 px max), mention en bas au centre
#   - vidéos : .avi -> .mp4 (1280 px max), mention en bas à gauche
# Le script part des ORIGINAUX et ne traite que les fichiers dont la version
# web n'existe pas encore dans le dossier cible (sauf si forcer <- TRUE).
# =============================================================================
library(av)
library(magick)

# ---- [MODIFIER] réglages ----------------------------------------------------
mention  <- "© Emma Belaud · CC BY-NC-SA 4.0"   # si le © s'affiche mal : "(c) Emma Belaud ..."
opacite  <- 0.85
filigrane_photos <- TRUE
filigrane_videos <- TRUE

# "southwest", "south", "southeast", "northwest", "north", "northeast"
position_video <- "southwest"
position_photo <- "south"

# FALSE : ne traite que ce qui n'existe pas encore dans le dossier cible.
# TRUE  : retraite tout et écrase l'existant (à faire UNE fois pour ajouter
#         la mention aux fichiers déjà produits sans filigrane).
forcer <- FALSE

# Dossiers cibles : doivent être CEUX que lisent tes .qmd
# (si tes pages pointent vers images/animation/web, change-le ici).
sortie_video <- "images/animation"
sortie_photo <- "images/photos_animals"

# Dossiers d'originaux : écris le chemin, ou laisse NULL pour avoir une fenêtre.
dossier_avi <- NULL   # ex. "F:/DOC/originaux/avi"
dossier_tif <- NULL   # ex. "F:/DOC/originaux/tif"
if (is.null(dossier_avi)) dossier_avi <- choose.dir(caption = "Dossier des vidéos .avi ORIGINALES")
if (is.null(dossier_tif)) dossier_tif <- choose.dir(caption = "Dossier des photos .tif ORIGINALES")
stopifnot(!is.na(dossier_avi), !is.na(dossier_tif))

dir.create(sortie_video, showWarnings = FALSE, recursive = TRUE)
dir.create(sortie_photo, showWarnings = FALSE, recursive = TRUE)

# ---- Utilitaire : faut-il traiter ce fichier ? -------------------------------
a_traiter <- function(cible) forcer || !file.exists(cible)

# ---- Fonction : ajoute la mention sur une image ------------------------------
# Texte blanc avec ombre sombre : lisible sur fond clair comme sombre.
filigrane <- function(img, texte, gravity = "southeast") {
  largeur <- image_info(img)$width
  taille  <- max(14, round(largeur * 0.02))
  marge   <- round(largeur * 0.012)
  blanc   <- sprintf("rgba(255,255,255,%s)", opacite)
  noir    <- "rgba(0,0,0,0.9)"
  img <- image_annotate(img, texte, gravity = gravity, size = taille,
                        color = noir,  location = sprintf("+%d+%d", marge + 1, marge - 1))
  image_annotate(img, texte, gravity = gravity, size = taille,
                 color = blanc, location = sprintf("+%d+%d", marge, marge))
}

# ---- Photos : .tif -> .webp --------------------------------------------------
tifs <- list.files(dossier_tif, pattern = "\\.tiff?$", full.names = TRUE, ignore.case = TRUE)
vus  <- character()
for (f in tifs) {
  nom <- tolower(gsub(" ", "_", tools::file_path_sans_ext(basename(f))))
  if (nom %in% vus) next                      # doublon "insecta larva" / "insecta_larva"
  vus <- c(vus, nom)
  cible <- file.path(sortie_photo, paste0(nom, ".webp"))
  if (!a_traiter(cible)) { message("Photo déjà présente, ignorée : ", nom); next }
  message("Photo : ", basename(f))
  tryCatch({
    img <- image_read(f)[1]
    img <- image_convert(img, colorspace = "sRGB")
    img <- image_resize(img, "900x>")
    if (filigrane_photos) img <- filigrane(img, mention, gravity = position_photo)
    image_write(img, cible, format = "webp", quality = 82)
  }, error = function(e) message("  ÉCHEC photo ", nom, " : ", conditionMessage(e)))
}

# ---- Vidéos : .avi -> images PNG -> mention -> .mp4 --------------------------
filtre_even <- "scale=w='trunc(iw/2)*2':h='trunc(ih/2)*2',format=yuv420p"
ffmpeg <- Sys.which("ffmpeg")   # "" si ffmpeg n'est pas installé / pas dans le PATH
if (!nzchar(ffmpeg))
  message("ffmpeg introuvable : encodage direct par 'av' (sans passe de recompression).")

avis <- list.files(dossier_avi, pattern = "\\.avi$", full.names = TRUE, ignore.case = TRUE)
for (f in avis) {
  nom   <- tools::file_path_sans_ext(basename(f))
  cible <- file.path(sortie_video, paste0(nom, ".mp4"))
  if (!a_traiter(cible)) { message("Vidéo déjà présente, ignorée : ", nom); next }
  message("Vidéo : ", nom)
  
  tmp <- file.path(tempdir(), nom)
  unlink(tmp, recursive = TRUE)
  dir.create(tmp, showWarnings = FALSE)
  
  tryCatch({
    fps <- av_media_info(f)$video$framerate
    if (is.null(fps) || !is.finite(fps) || fps <= 0) fps <- 5
    
    # 1. Extraction des images
    images <- av_video_images(f, destdir = tmp, format = "png")
    
    # 2. Redimensionnement + mention sur chaque image
    for (im in images) {
      img <- image_read(im)
      img <- image_convert(img, colorspace = "sRGB")
      img <- image_resize(img, "1280x>")
      if (filigrane_videos) img <- filigrane(img, mention, gravity = position_video)
      image_write(img, im, format = "png")
    }
    
    # 3. Encodage
    brut <- file.path(tempdir(), paste0(nom, "_brut.mp4"))
    av_encode_video(images, brut, framerate = fps, vfilter = filtre_even, verbose = FALSE)
    
    ok <- FALSE
    if (nzchar(ffmpeg)) {
      statut <- system2(ffmpeg, c(
        "-y", "-i", shQuote(brut),
        "-c:v", "libx264", "-profile:v", "main", "-level", "4.0",
        "-pix_fmt", "yuv420p", "-crf", "23", "-preset", "slow",
        "-movflags", "+faststart", "-an", shQuote(cible)),
        stdout = FALSE, stderr = FALSE)
      ok <- identical(as.integer(statut), 0L) && file.exists(cible)
      if (!ok) message("  ffmpeg a échoué, repli sur l'encodage 'av'.")
    }
    if (!ok) file.copy(brut, cible, overwrite = TRUE)
    unlink(brut)
  }, error = function(e) message("  ÉCHEC vidéo ", nom, " : ", conditionMessage(e)))
  
  unlink(tmp, recursive = TRUE)
}

# ---- Contrôle : fichiers produits (en Mo) ------------------------------------
produits <- list.files(c(sortie_video, sortie_photo), full.names = TRUE)
print(data.frame(fichier = basename(produits), Mo = round(file.size(produits) / 1e6, 2)))
