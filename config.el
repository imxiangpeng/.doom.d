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
(setq projectile-fd-executable "fd")
(setq projectile-auto-discover nil)
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
  (setq org-latex-compile 'xelatex)
  (setq org-file-apps
      '(("\\.pdf\\'" . "zathura %s")))
  )

(after! ox-latex
  ;;(setq org-latex-with-hyperref nil)
  (setq org-latex-default-packages-alist
        '(("" "fontspec" nil) ; 加载 fontspec 包
          ("" "xunicode" nil) ; 支持 Unicode
          ("" "xltxtra" nil)  ; 额外的 LaTeX 支持
          ("AUTO" "inputenc" t) ; 自动检测输入编码
          ("T1" "fontenc" t)    ; 使用 T1 字体编码
          ("" "graphicx" t)     ; 支持图片
          ("" "hyperref" nil)   ; 支持超链接
          ("" "newfloat" nil)
          ("" "minted" nil)
          ("" "listings" nil)
          ("" "xcolor" nil)))


  ;;(setq org-latex-src-block-backend 'listings)
  (setq org-latex-src-block-backend 'minted)

  ;; clear page after title
  (setq org-latex-title-command "\\maketitle\n\\clearpage")

  (setq org-latex-pdf-process
        '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (setq org-latex-default-class "article")

  ;; 设置默认字体
  ;; use fc-match serif/sans to lookup default font from system
  ;; see: https://github.com/emacsmirror/org/blob/master/lisp/ox-latex.el
  (setq org-latex-classes
        '(("article"
           "\\documentclass[12pt]{article}
\\usepackage[a4paper,margin=1in,footskip=0.40in]{geometry}
\\usepackage{fontspec}
% \\setmainfont{Times New Roman} % 设置英文主字体
% \\setsansfont{} % 设置英文无衬线字体
% \\setmonofont{} % 设置英文等宽字体
\\usepackage{xeCJK} % 支持中文字体
\\setCJKmainfont{SimSun} % 设置中文主字体（宋体）
\\setCJKsansfont{FangSong} % 设置中文无衬线字体（仿宋）
\\setCJKmonofont{FangSong} % 设置中文等宽字体（楷体）
\\usepackage[hidelinks]{hyperref}
\\usepackage{tocloft}
\\renewcommand{\\cftsecleader}{\\cftdotfill{\\cftdotsep}}
\\renewcommand{\\contentsname}{\\centerline{\\bfseries 目录}} % 修改目录标题
% \\renewcommand{\\cfttoctitlefont}{\\hfill\\Large\\bfseries} % 目录标题样式
% \\setlength{\\cftsecindent}{0pt}
\\setlength{\\parindent}{0pt}  % 取消段落缩进
\\setlength{\\parskip}{1em}     % 设置段落间距
\\usepackage{xcolor}
\\usepackage{minted}
\\setminted{
  bgcolor=gray!10, % 设置背景色
  frame=none,        % 添加边框
  % framerule=1pt,       % 边框线宽度
  % framesep=5pt,        % 边框与代码的间距
  % style=emacs,        % 设置样式
  fontsize=\\footnotesize,    % 设置字体大小 tiny/scriptsize/footnotesize/small/normalsize/large
  linenos=false,       % 显示行号
  breaklines=true,     % 自动换行
  breakautoindent=false,
  breaksymbolleft={},
}"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))))

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
  (setq org-hugo-front-matter-format "yaml")
  (setq org-hugo-headline-anchor nil))

;; disable persistent undo history
(when (modulep! :editor undo +tree)
  (after! undo-tree
    (setq undo-tree-auto-save-history nil)))

;; always use rg never use git grep
(after! dumb-jump
  (setq dumb-jump-force-searcher 'rg))

;; using name ripgrep not rg
(after! xref
  (setq xref-search-program 'ripgrep))


;; mxp, 20250215, emacs's xref support global directly
;; we can use  gd/gD to lookup definition & referenceso
;; we only use global generate GPATH
;; so no need use this ggtags now
;; (use-package! ggtags
;;   :hook ((c-mode . ggtags-mode)
;;          (c++-mode . ggtags-mode)
;;          (java-mode . ggtags-mode))
;;   :init
;;   :config
;;   ;; do not abreviate file name early ...
;;   (setq ggtags-global-abbreviate-filename 128
;;         ggtags-auto-jump-to-match nil
;;         ;; highlight slow
;;         ggtags-highlight-tag nil))

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
        '("-j=2"
          "--background-index"
          ;;"--clang-tidy"
          ;;"--completion-style=detailed"
          "--header-insertion=never"
          "--header-insertion-decorators=0"))
  (set-lsp-priority! 'clangd 2))

;; support user own env path
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(after! eglot
  (set-eglot-client! 'cc-mode '("clangd" "-j=3" "--clang-tidy"))
  (set-eglot-client! 'sh-mode '("bash-language-server" "start")))

(use-package! dts-mode
  :mode ("\\.dts\\'" "\\.dtsi\\'")
  :config
  (setq dts-indent-level 4))

;; 主要修改点：
;; 1. 链接倾向于使用 []()
;; 2. 图片倾向于使用 ![]()
;; 3. 裸链接继续保持 <>
(defun org-hugo-export-markmap ()
  "Export current Org file to Markdown using ox-hugo,
  then generate and show a mind map in the browser.
  Temporarily toggles org-hugo--prefer-md-link-style."
  (interactive)
  (require 'ox-hugo)
  (let ((original-value org-hugo--prefer-md-link-style))
    (unwind-protect
        (progn
          (setq org-hugo--prefer-md-link-style t)
          (let* ((md-file (org-hugo-export-to-md nil nil))
                 (html-file (replace-regexp-in-string "\\.md$" ".html" md-file)))
            (if md-file
                (progn
                  (message "Exported to: %s" md-file)
                  (shell-command (format "markmap --no-open %s -o %s" md-file html-file))
                  (browse-url html-file))
              (message "Error exporting to Markdown."))))
      (setq org-hugo--prefer-md-link-style original-value))))

(global-set-key (kbd "C-c h m") 'org-hugo-export-markmap)

(load! "+bindings")
