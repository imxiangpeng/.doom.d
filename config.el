;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(load-library "find-lisp")

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
(setq doom-font (font-spec :family "Hack Nerd Font" :size 18))
(setq doom-big-font (font-spec :family "Hack Nerd Font" :size 25))

(setq doom-upgrade-url "git@github.com:doomemacs/doomemacs")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)
(setq doom-theme 'spacemacs-light)

;; exit without confirm
(setq confirm-kill-emacs nil)

(setq system-time-locale "C")

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/keeping/")
;;(setq org-attach-id-dir "~/keeping/attachments")
;; we prefer to store attach in current dir
(setq org-attach-id-dir "./data")
(setq org-agenda-files (find-lisp-find-files "~/keeping/" "\.org$"))
(setq +org-capture-journal-file "org/journal-2025.org")
(setq +org-capture-todo-file "org/todo-2025.org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq undo-limit 80000000)                          ; Raise undo-limit to 80Mb

(setq evil-want-fine-undo t)                        ; By default while in insert all changes are one big blob. Be more granular
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq auto-save-default t)                          ; Nobody likes to loose work, I certainly don't
(setq inhibit-compacting-font-caches t)             ; When there are lots of glyphs, keep them in memory

;;(delete-selection-mode 1)                             ; Replace selection when inserting text
(display-time-mode 1)                                   ; Enable time in the mode-line

(setq line-spacing 0.3)                                   ; seems like a nice line spacing balance.

(setq company-idle-delay nil)

(setq default-directory "~")

(setq projectile-enable-caching t)
;; mxp, 20220218, do not auto delete projectile's cache
;; we enlarge the limit size
(setq doom-projectile-cache-limit 90000000)

(setq gc-cons-threshold 20000000)
(setq large-file-warning-threshold 200000000)

;;(setq kill-whole-line t)


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

;; disable markdown's auto indent when new line
;; it causing too much space in empty line
(setq markdown-indent-on-enter nil)

;; max window size, when startup
(add-hook! 'window-setup-hook #'toggle-frame-maximized)
;;(add-hook! 'window-setup-hook
;;  (lambda()
;;    (setq menu-bar-mode t)))
;; using pretty mode for org
;;(add-hook! 'org-mode-hook #'+org-pretty-mode)

;; projectile using default alien index method
(after! projectile
  ;; mxp, 20220509, alien is fast, because it will not filter & sort
  ;; while hybrid is slow much when project is very large
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'default)
  (setq projectile-globally-ignored-file-suffixes
        (append projectile-globally-ignored-file-suffixes '(".bak" ".swp" ".lock" ".bin" ".a" ".so" ".html" ".ts")))
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories '("out" ".git" "prebuilts" "tests"))))

(after! org
  (setq org-log-done t)
  (setq org-log-into-drawer t)
  (setq org-index-mode t)
  (setq org-file-apps
      '(("\\.pdf\\'" . "zathura %s")))
  )

;; must use setq-default to adjust buffer local var
(after! org-download
  (setq org-download-method 'directory)
  (setq-default org-download-image-dir "./assets")
  (setq-default org-download-heading-lvl nil))

;; mxp, 20250108, org-mode now support yank-media
;; which adjust file handler to org-dd-local-file-handler
;; in dnd-protocol-alist
;; but it leading org-download broken when draging files
;; we prefer using org-download, so we use advice to
;; override default org-setup-yank-dnd-handlers in org-mode's code
(defun org--setup-yank-dnd-handlers-advice (&rest _args)
  "Prevent `org-setup-yank-dnd-handlers` from overwriting `dnd-protocol-alist`.")
;; Add advice to override the behavior of `org-setup-yank-dnd-handlers`
(advice-add 'org-setup-yank-dnd-handlers :override #'org--setup-yank-dnd-handlers-advice)

(setq org-roam-v2-ack t)

(after! org-roam
  ;; fixed bug in windows, force using immediate
  (setq org-roam-db-update-method 'immediate)
  ;; mxp, 20210212, as we set roam directory to ~/keeping,
  ;; but we also want all roam notes in ~/keeping/roam
  ;; so we adjust the capture directory, prefixed with roam/
  (setq org-roam-directory "~/keeping/roam")
  ;; default org-roam-buffer-width is 0.33
  (setq org-roam-buffer-width 0.20)
  ;;(setq org-roam-db-location "~/keeping/roam")
  (setq +org-roam-open-buffer-on-find-file nil))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! ox-hugo
  :after ox
  :init
  ;; markmap only support yaml meta information
  (setq org-hugo-front-matter-format "yaml"))

;; disable persistent undo history
(when (modulep! :editor undo +tree)
  (after! undo-tree
    (setq undo-tree-auto-save-history nil)))

;;(use-package! ggtags
;;  :hook ((c-mode . ggtags-mode)
;;         (c++-mode . ggtags-mode)
;;         (java-mode . ggtags-mode))
;;  :init
;;  :config)

;;(use-package! counsel-gtags
;;  :hook ((c-mode . counsel-gtags-mode)
;;         (c++-mode . counsel-gtags-mode)
;;         (java-mode . counsel-gtags-mode))
;;  :init
;;  :config
;;  (define-key!
;;    [remap +lookup/definition]    #'counsel-gtags-find-definition
;;    [remap +lookup/references]    #'counsel-gtags-find-reference))

(use-package! rime
:custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-disable-predicates
      '(rime-predicate-evil-mode-p
        rime-predicate-after-ascii-char-p ;; after any asscii
        rime-predicate-space-after-cc-p ;; after zh with space
        rime-predicate-current-uppercase-letter-p ;; upcase
        rime-predicate-in-code-string-p
        rime-predicate-punctuation-line-begin-p
        rime-predicate-punctuation-after-space-cc-p
        rime-predicate-punctuation-after-ascii-p
        rime-predicate-prog-in-code-p))

  (rime-inline-predicate
        '(rime-predicate-space-after-ascii-p)))

(after! lsp-clangd
  (setq lsp-clients-clangd-args
        '("-j=1"
          "--background-index"
          "--clang-tidy"
          "--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

(use-package! dts-mode
  :mode ("\\.dts\\'" "\\.dtsi\\'")
  :config
  (setq dts-indent-level 4))

(defun org-hugo-export-markmap ()
  "Export current Org file to Markdown using ox-hugo,
  then generate and show a mind map in the browser."
  (interactive)
  (let* ((org-file (buffer-file-name))
         (md-file (org-hugo-export-to-md nil nil))
         (html-file (replace-regexp-in-string "\\.md$" ".html" md-file)))
    (if md-file
        (progn
          (message "Exported to: %s" md-file)
          (shell-command (format "markmap --no-open %s -o %s" md-file html-file))
          (browse-url html-file))
      (message "Error exporting to Markdown."))))

(global-set-key (kbd "C-c h m") 'org-hugo-export-markmap)



(load! "+bindings")
