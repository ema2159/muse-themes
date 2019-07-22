;;; doom-themes.el --- an opinionated pack of modern color-themes -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2019 Emmanuel Bustos, Ian Pan
;;
;; Authors: Emmanuel Bustos <http://github/ema2159>, Ian Pan <http://github/ianpan870102>
;; Maintainers: Emmanuel Bustos <ema2159@gmail.com>, Ian Pan <>
;; Created: Jul 22, 2019
;; Version: 2.1.6
;; Keywords: dark light blue one theme icons faces
;; Homepage: https://github.com/ema2159/muse-themes
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Muse Themes is an opinionated UI plugin and pack of themes that uses the core of
;; Doom Themes.  It ports and implements awesome color themes like:
;;
;; Flagship themes
;;
;; Additional themes
;;
;; ## Install
;;
;;   `M-x package-install RET muse-themes`
;;
;; A comprehensive configuration example:
;;
;;   (require 'muse-themes)
;;
;;   ;; Global settings (defaults)
;;   (setq muse-themes-enable-bold t    ; if nil, bold is universally disabled
;;         muse-themes-enable-italic t) ; if nil, italics is universally disabled
;;
;;   ;; Load the theme (muse-one, muse-molokai, etc); keep in mind that each
;;   ;; theme may have their own settings.
;;   (load-theme 'muse-one t)
;;
;;   ;; Enable flashing mode-line on errors
;;   (muse-themes-visual-bell-config)
;;
;;   ;; Enable custom neotree theme
;;   (muse-themes-neotree-config)  ; all-the-icons fonts must be installed!
;;
;;; Code:

(require 'cl-lib)
(require 'muse-themes-base)

(defgroup muse-themes nil
  "Options for muse-themes."
  :group 'faces)

(defcustom muse-themes-padded-modeline nil
  "Default value for padded-modeline setting for themes that support it."
  :group 'muse-themes
  :type '(or integer boolean))

;;
(defcustom muse-themes-enable-bold t
  "If nil, bold will be disabled across all faces."
  :group 'muse-themes
  :type 'boolean)

(defcustom muse-themes-enable-italic t
  "If nil, italics will be disabled across all faces."
  :group 'muse-themes
  :type 'boolean)


;;
;;; API

(defvar muse-themes--colors nil)
(defvar muse--min-colors '(257 256 16))
(defvar muse--quoted-p nil)
(defvar muse-themes--faces nil)

(defun muse-themes--colors-p (item)
  (declare (pure t) (side-effect-free t))
  (when item
    (cond ((listp item)
           (let ((car (car item)))
             (cond ((memq car '(quote muse-color)) nil)

                   ((memq car '(backquote \`))
                    (let ((muse--quoted-p t))
                      (muse-themes--colors-p (cdr item))))

                   ((eq car '\,)
                    (let (muse--quoted-p)
                      (muse-themes--colors-p (cdr item))))

                   ((or (muse-themes--colors-p car)
                        (muse-themes--colors-p (cdr-safe item)))))))

          ((and (symbolp item)
                (not (keywordp item))
                (not muse--quoted-p)
                (not (equal (substring (symbol-name item) 0 1) "-"))
                (assq item muse-themes--colors))))))

(defun muse-themes--apply-faces (new-faces &optional default-faces)
  (declare (pure t) (side-effect-free t))
  (let ((default-faces (or default-faces muse-themes-base-faces))
        (faces (make-hash-table :test #'eq :size (+ (length default-faces) (length new-faces))))
        (directives (make-hash-table :test #'eq)))
    (dolist (spec (append (mapcar #'copy-sequence default-faces) new-faces))
      (if (listp (car spec))
          (cl-destructuring-bind (face action &optional arg) (car spec)
            (unless (assq face new-faces)
              (puthash face (list action arg (cdr spec))
                       directives)))
        (puthash (car spec) (cdr spec) faces)))
    (cl-loop for face being the hash-keys of directives
             for (action target spec) = (gethash face directives)
             unless (memq action '(&inherit &extend &override))
             do (error "Invalid operation (%s) for '%s' face" action face)
             if (eq (car spec) 'quote)
             do (error "Can't extend literal face spec (for '%s')" face)
             ;; TODO Add &all/&light/&dark extension support
             else if (memq (car spec) '(&all &light &dark))
             do (error "Can't extend face with &all, &light or &dark specs (for '%s')" face)
             else do
             (puthash face
                      (let ((old-spec (gethash (or target face) faces))
                            (plist spec))
                        ;; remove duplicates
                        (while (keywordp (car plist))
                          (setq old-spec (plist-put old-spec (car plist) (cadr plist))
                                plist (cddr plist)))
                        old-spec)
                      faces))
    (let (results)
      (maphash (lambda (face plist)
                 (when (keywordp (car plist))
                   ;; TODO Clean up duplicates in &all/&light/&dark blocks
                   (dolist (prop (append (unless muse-themes-enable-bold   '(:weight normal :bold nil))
                                         (unless muse-themes-enable-italic '(:slant normal :italic nil))))
                     (when (and (plist-member plist prop)
                                (not (eq (plist-get plist prop) 'inherit)))
                       (plist-put plist prop
                                  (if (memq prop '(:weight :slant))
                                      (quote 'normal))))))
                 (push (cons face plist) results))
               faces)
      (nreverse results))))

(defun muse-themes--colorize (item type)
  (declare (pure t) (side-effect-free t))
  (when item
    (let ((muse--quoted-p muse--quoted-p))
      (cond ((listp item)
             (cond ((memq (car item) '(quote muse-color))
                    item)
                   ((eq (car item) 'muse-ref)
                    (muse-themes--colorize
                     (apply #'muse-ref (cdr item)) type))
                   ((let* ((item (append item nil))
                           (car (car item))
                           (muse--quoted-p
                            (cond ((memq car '(backquote \`)) t)
                                  ((eq car '\,) nil)
                                  (t muse--quoted-p))))
                      (cons car
                            (cl-loop
                             for i in (cdr item)
                             collect (muse-themes--colorize i type)))))))

            ((and (symbolp item)
                  (not (keywordp item))
                  (not muse--quoted-p)
                  (not (equal (substring (symbol-name item) 0 1) "-"))
                  (assq item muse-themes--colors))
             `(muse-color ',item ',type))

            (item)))))

(defun muse-themes--build-face (face)
  (declare (pure t) (side-effect-free t))
  `(list
    ',(car face)
    ,(let ((face-body (cdr face)))
       (cond ((keywordp (car face-body))
              (let ((real-attrs face-body)
                    defs)
                (if (muse-themes--colors-p real-attrs)
                    (dolist (cl muse--min-colors `(list ,@(nreverse defs)))
                      (push `(list '((class color) (min-colors ,cl))
                                   (list ,@(muse-themes--colorize real-attrs cl)))
                            defs))
                  `(list (list 't (list ,@real-attrs))))))

             ((memq (car-safe (car face-body)) '(quote backquote \`))
              (car face-body))

             ((let (all-attrs defs)
                (dolist (attrs face-body `(list ,@(nreverse defs)))
                  (cond ((eq (car attrs) '&all)
                         (setq all-attrs (append all-attrs (cdr attrs))))

                        ((memq (car attrs) '(&dark &light))
                         (let ((bg (if (eq (car attrs) '&dark) 'dark 'light))
                               (real-attrs (append all-attrs (cdr attrs) '())))
                           (cond ((muse-themes--colors-p real-attrs)
                                  (dolist (cl muse--min-colors)
                                    (push `(list '((class color) (min-colors ,cl) (background ,bg))
                                                 (list ,@(muse-themes--colorize real-attrs cl)))
                                          defs)))

                                 ((push `(list '((background ,bg)) (list ,@real-attrs))
                                        defs)))))))))))))


;;
;;; Color helper functions

;; Shamelessly *borrowed* from solarized
;;;###autoload
(defun muse-name-to-rgb (color)
  "Retrieves the hexidecimal string repesented the named COLOR (e.g. \"red\")
for FRAME (defaults to the current frame)."
  (cl-loop with div = (float (car (tty-color-standard-values "#ffffff")))
           for x in (tty-color-standard-values (downcase color))
           collect (/ x div)))

;;;###autoload
(defun muse-blend (color1 color2 alpha)
  "Blend two colors (hexidecimal strings) together by a coefficient ALPHA (a
float between 0 and 1)"
  (when (and color1 color2)
    (cond ((and color1 color2 (symbolp color1) (symbolp color2))
           (muse-blend (muse-color color1) (muse-color color2) alpha))

          ((or (listp color1) (listp color2))
           (cl-loop for x in color1
                    when (if (listp color2) (pop color2) color2)
                    collect (muse-blend x it alpha)))

          ((and (string-prefix-p "#" color1) (string-prefix-p "#" color2))
           (apply (lambda (r g b) (format "#%02x%02x%02x" (* r 255) (* g 255) (* b 255)))
                  (cl-loop for it    in (muse-name-to-rgb color1)
                           for other in (muse-name-to-rgb color2)
                           collect (+ (* alpha it) (* other (- 1 alpha))))))

          (color1))))

;;;###autoload
(defun muse-darken (color alpha)
  "Darken a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between
0 and 1)."
  (cond ((and color (symbolp color))
         (muse-darken (muse-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (muse-darken c alpha)))

        ((muse-blend color "#000000" (- 1 alpha)))))

;;;###autoload
(defun muse-lighten (color alpha)
  "Brighten a COLOR (a hexidecimal string) by a coefficient ALPHA (a float between 0 and 1)."
  (cond ((and color (symbolp color))
         (muse-lighten (muse-color color) alpha))

        ((listp color)
         (cl-loop for c in color collect (muse-lighten c alpha)))

        ((muse-blend color "#FFFFFF" (- 1 alpha)))))

;;;###autoload
(defun muse-color (name &optional type)
  "Retrieve a specific color named NAME (a symbol) from the current theme.
It passes TYPE as the prop argument to the `plist-get` function."
  (let ((colors (if (listp name)
                    name
                  (cdr-safe (assq name muse-themes--colors)))))
    (and colors
         (cond ((listp colors)
                (let ((i (or (plist-get '(256 1 16 2 8 3) type) 0)))
                  (if (> i (1- (length colors)))
                      (car (last colors))
                    (nth i colors))))
               (t colors)))))

;;;###autoload
(defun muse-ref (face prop &optional class)
  "TODO"
  (let ((spec (or (cdr (assq face muse-themes--faces))
                  (error "Couldn't find the '%s' face" face))))
    (when (memq (car spec) '(quote backquote \`))
      (user-error "Can't fetch the literal spec for '%s'" face))
    (when class
      (setq spec (cdr (assq class spec)))
      (unless spec
        (error "Couldn't find the '%s' class in the '%s' face"
               class face)))
    (unless (plist-member spec prop)
      (error "Couldn't find the '%s' property in the '%s' face%s"
             prop face (if class (format "'s '%s' class" class) "")))
    (plist-get spec prop)))


;;
;;; Defining themes

(defun muse-themes-prepare-facelist (custom-faces)
  "Return an alist of face definitions for `custom-theme-set-faces'.

Faces in EXTRA-FACES override the default faces."
  (declare (pure t) (side-effect-free t))
  (setq muse-themes--faces (muse-themes--apply-faces custom-faces))
  (mapcar #'muse-themes--build-face muse-themes--faces))

(defun muse-themes-prepare-varlist (vars)
  "Return an alist of variable definitions for `custom-theme-set-variables'.

Variables in EXTRA-VARS override the default ones."
  (declare (pure t) (side-effect-free t))
  (cl-loop for (var val) in (append muse-themes-base-vars vars)
           collect `(list ',var ,val)))

;;;###autoload
(defun muse-themes-set-faces (theme &rest faces)
  "Customize THEME (a symbol) with FACES.

If THEME is nil, it applies to all themes you load. FACES is a list of Muse
theme face specs. These is a simplified spec. For example:

  (muse-themes-set-faces 'user
    '(default :background red :foreground blue)
    '(muse-modeline-bar :background (if -modeline-bright modeline-bg highlight))
    '(muse-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
    '(muse-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
    '(muse-modeline-buffer-project-root :foreground green :weight 'bold))"
  (declare (indent defun))
  (apply #'custom-theme-set-faces
         (or theme 'user)
         (eval
          `(let* ((bold   ,muse-themes-enable-bold)
                  (italic ,muse-themes-enable-italic)
                  ,@(cl-loop for (var . val) in muse-themes--colors
                             collect `(,var ',val)))
             (list ,@(mapcar #'muse-themes--build-face faces))))))

(defmacro def-muse-theme (name docstring defs &optional extra-faces extra-vars)
  "Define a MUSE theme, named NAME (a symbol)."
  (declare (doc-string 2))
  (let ((muse-themes--colors defs))
    `(let* ((bold   muse-themes-enable-bold)
            (italic muse-themes-enable-italic)
            ,@defs)
       (setq muse-themes--colors
             (list ,@(cl-loop for (var val) in defs
                              collect `(cons ',var ,val))))
       (deftheme ,name ,docstring)
       (custom-theme-set-faces
        ',name ,@(muse-themes-prepare-facelist extra-faces))
       (custom-theme-set-variables
        ',name ,@(muse-themes-prepare-varlist extra-vars))
       (unless bold (set-face-bold 'bold nil))
       (unless italic (set-face-italic 'italic nil))
       (provide-theme ',name))))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'muse-themes)
;;; muse-themes.el ends here
