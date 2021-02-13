;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alex Meng"
      user-mail-address "imxiangpeng@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;;(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'spacemacs-light)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/keeping/")
(setq +org-capture-journal-file "org/journal-2021.org")
;;(setq org-roam-directory "~/keeping")
;;; Recommendation for Windows users for performance
;;; https://github.com/org-roam/org-roam/issues/1289#issuecomment-744046148
(setq org-roam-db-update-method 'immediate)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq undo-limit 80000000)                          ; Raise undo-limit to 80Mb

(setq evil-want-fine-undo t)                        ; By default while in insert all changes are one big blob. Be more granular
(setq auto-save-default t)                          ; Nobody likes to loose work, I certainly don't
(setq inhibit-compacting-font-caches t)             ; When there are lots of glyphs, keep them in memory

;;(delete-selection-mode 1)                             ; Replace selection when inserting text
(display-time-mode 1)                                   ; Enable time in the mode-line

(setq line-spacing 0.3)                                   ; seems like a nice line spacing balance.

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;(setq gc-cons-threshold 20000000)
;;(setq large-file-warning-threshold 200000000)

;;(setq kill-whole-line t)
;;(setq confirm-kill-emacs nil)

;;(use-package! org-roam
;;      :ensure t
;;      :hook
;;      (after-init . org-roam-mode)
;;      :custom
;;      (org-roam-directory "~/keeping/roam")
;;      :bind (:map org-roam-mode-map
;;              (("C-c n l" . org-roam)
;;               ("C-c n f" . org-roam-find-file)
;;               ("C-c n g" . org-roam-graph))
;;              :map org-mode-map
;;              (("C-c n i" . org-roam-insert))
;;              (("C-c n I" . org-roam-insert-immediate))))

(after!  org-roam
  ;; fixed bug in windows, force using immediate
  (setq org-roam-db-update-method 'immediate)
  ;; mxp, 20210212, as we set roam directory to ~/keeping,
  ;; but we also want all roam notes in ~/keeping/roam
  ;; so we adjust the capture directory, prefixed with roam/
  (setq org-roam-directory "~/keeping/")
  ;;(setq org-roam-db-location "~/keeping/roam")
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "roam/${slug}"
           :head "#+title: ${title}\n"
           ;;:immediate-finish t
           :unnarrowed t)
          ("p" "private" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "roam/private/${slug}"
           :head "#+title: ${title}\n"
           ;;:immediate-finish t
           :unnarrowed t)))
    (setq org-roam-dailies-directory "daily/")
    (setq org-roam-dailies-capture-templates
          '(("d" "default" entry
             #'org-roam-capture--get-point
             "* %?"
             :file-name "roam/daily/%<%Y-%m-%d>"
             :head "#+title: %<%Y-%m-%d>\n\n"))))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! ox-hugo
  :after ox)
