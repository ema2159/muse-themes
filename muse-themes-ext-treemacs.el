;;; muse-themes-ext-treemacs.el --- description -*- lexical-binding: t; no-byte-compile: t -*-
;;; Commentary:
;; Muse themes Treemacs theme settings and configurations.


;;; Code:
(defgroup muse-themes-treemacs nil
  "Options for muse's treemacs theme"
  :group 'muse-themes)


;;
;;; Variables

(defcustom muse-themes-treemacs-enable-variable-pitch t
  "If non-nil, display the labels for files, folders and projects with the variable-pitch face."
  :type 'boolean
  :group 'muse-themes-treemacs)

(defcustom muse-themes-treemacs-line-spacing 1
  "Line-spacing for treemacs buffer."
  :type 'integer
  :group 'muse-themes-treemacs)


;;
;;; Library

(defun muse-themes-hide-fringes ()
  "Remove fringes in currnent window."
  (when (display-graphic-p)
    (set-window-fringes nil 0 0)))

(defun muse-themes-setup-tab-width (&rest _)
  "Set `tab-width' to 1, so tab characters don't ruin formatting."
  (setq tab-width 1))

(defun muse-themes-setup-line-spacing ()
  "Set `line-spacing' in treemacs buffers."
  (setq line-spacing muse-themes-treemacs-line-spacing))

(defun muse-themes-hide-modeline ()
"Function for hiding the modeline."
  (setq mode-line-format nil))

(defun muse-themes-enable-treemacs-variable-pitch-labels (&rest _)
  "Function for enabling variable pitch faces for the labels."
  (when muse-themes-treemacs-enable-variable-pitch
    (dolist (face '(treemacs-root-face
                    treemacs-git-unmodified-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-ignored-face
                    treemacs-git-untracked-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face
                    treemacs-tags-face))
      (let ((faces (face-attribute face :inherit nil)))
        (set-face-attribute
         face nil :inherit
         `(variable-pitch ,@(delq 'unspecified (if (listp faces) faces (list faces)))))))))

(defun muse-themes-fix-treemacs-icons-dired-mode ()
  "Set `tab-width' to 1 in `dired-mode' if `treemacs-icons-dired-mode' is active."
  (if treemacs-icons-dired-mode
      (add-hook 'dired-mode-hook #'muse-themes-setup-tab-width nil t)
    (remove-hook 'dired-mode-hook #'muse-themes-setup-tab-width t)))


;;
;;; Bootstrap

(with-eval-after-load 'treemacs
  (unless (require 'all-the-icons nil t)
    (error "The package all-the-icons isn't installed"))

  (add-hook 'treemacs-mode-hook #'muse-themes-setup-tab-width)
  (add-hook 'treemacs-mode-hook #'muse-themes-setup-line-spacing)

  ;; Fix #293: tabs messing up formatting in `treemacs-icons-dired-mode'
  (add-hook 'treemacs-icons-dired-mode-hook #'muse-themes-fix-treemacs-icons-dired-mode)

  ;; The modeline isn't useful in treemacs
  (add-hook 'treemacs-mode-hook #'muse-themes-hide-modeline)

  ;; Disable fringes (and reset them everytime treemacs is selected because it
  ;; may change due to outside factors)
  (add-hook 'treemacs-mode-hook #'muse-themes-hide-fringes)
  (advice-add #'treemacs-select-window :after #'muse-themes-hide-fringes)

  ;; variable-pitch labels for files/folders
  (muse-themes-enable-treemacs-variable-pitch-labels)
  (advice-add #'load-theme :after #'muse-themes-enable-treemacs-variable-pitch-labels)

  ;; minimalistic atom-inspired icon theme
  (treemacs-create-theme "muse"
    :config
    (let ((face-spec '(:inherit font-lock-doc-face :slant normal)))
      (treemacs-create-icon
       :icon (format " %s\t" (all-the-icons-octicon "repo" :v-adjust -0.1 :face face-spec))
       :extensions (root))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
       :extensions (dir-open))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "file-directory" :v-adjust 0 :face face-spec))
       :extensions (dir-closed))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-down" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "package" :v-adjust 0 :face face-spec)) :extensions (tag-open))
      (treemacs-create-icon
       :icon (format "%s\t%s\t"
                     (all-the-icons-octicon "chevron-right" :height 0.75 :v-adjust 0.1 :face face-spec)
                     (all-the-icons-octicon "package" :v-adjust 0 :face face-spec))
       :extensions (tag-closed))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "tag" :height 0.9 :v-adjust 0 :face face-spec))
       :extensions (tag-leaf))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "flame" :v-adjust 0 :face face-spec))
       :extensions (error))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "stop" :v-adjust 0 :face face-spec))
       :extensions (warning))
      (treemacs-create-icon
       :icon (format "%s\t" (all-the-icons-octicon "info" :height 0.75 :v-adjust 0.1 :face face-spec))
       :extensions (info))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-media" :v-adjust 0 :face face-spec))
       :extensions ("png" "jpg" "jpeg" "gif" "ico" "tif" "tiff" "svg" "bmp"
                    "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "mkv"
                    "wav" "mp3" "ogg" "midi"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-code" :v-adjust 0 :face face-spec))
       :extensions ("yml" "yaml" "sh" "zsh" "fish" "c" "h" "cpp" "cxx" "hpp"
                    "tpp" "cc" "hh" "hs" "lhs" "cabal" "py" "pyc" "rs" "el"
                    "elc" "clj" "cljs" "cljc" "ts" "tsx" "vue" "css" "html"
                    "htm" "dart" "java" "kt" "scala" "sbt" "go" "js" "jsx"
                    "hy" "json" "jl" "ex" "exs" "eex" "ml" "mli" "pp" "dockerfile"
                    "vagrantfile" "j2" "jinja2" "tex" "racket" "rkt" "rktl" "rktd"
                    "scrbl" "scribble" "plt" "makefile" "elm" "xml" "xsl" "rb"
                    "scss" "lua" "lisp" "scm" "sql" "toml" "nim" "pl" "pm" "perl"
                    "vimrc" "tridactylrc" "vimperatorrc" "ideavimrc" "vrapperrc"
                    "cask" "r" "re" "rei" "bashrc" "zshrc" "inputrc" "editorconfig"
                    "gitconfig"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "book" :v-adjust 0 :face face-spec))
       :extensions ("lrf" "lrx" "cbr" "cbz" "cb7" "cbt" "cba" "chm" "djvu"
                    "doc" "docx" "pdb" "pdb" "fb2" "xeb" "ceb" "inf" "azw"
                    "azw3" "kf8" "kfx" "lit" "prc" "mobi" "exe" "or" "html"
                    "pkg" "opf" "txt" "pdb" "ps" "rtf" "pdg" "xml" "tr2"
                    "tr3" "oxps" "xps"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
       :extensions ("md" "markdown" "rst" "log" "org" "txt"
                    "CONTRIBUTE" "LICENSE" "README" "CHANGELOG"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-binary" :v-adjust 0 :face face-spec))
       :extensions ("exe" "dll" "obj" "so" "o" "out"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-pdf" :v-adjust 0 :face face-spec))
       :extensions ("pdf"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-zip" :v-adjust 0 :face face-spec))
       :extensions ("zip" "7z" "tar" "gz" "rar" "tgz"))
      (treemacs-create-icon
       :icon (format "  %s\t" (all-the-icons-octicon "file-text" :v-adjust 0 :face face-spec))
       :extensions (fallback))))

  (treemacs-load-theme "muse"))

;;;###autoload
(defun muse-themes-treemacs-config ()
  "Install muse-themes' treemacs configuration.

Includes an Atom-esque icon theme and highlighting based on filetype.")

(provide 'muse-themes-ext-treemacs)
;;; muse-themes-ext-treemacs.el ends here
