;; I prefer using default binding in insert mode
(when (modulep! :editor evil +everywhere)
  (map!
    :gi "C-p" #'previous-line
    :gi "C-n" #'next-line
    :gi "C-b" #'backward-char
    :gi "C-f" #'forward-char
    ;gi "C-k" #'kill-line
    :i "C-y" #'yank
    ;; evil bind C-x C-s to company-files
    :gi "C-x C-s" #'save-buffer))

;;(map! (:map evil-normal-state-map "M-." nil))
;;(map!
;;   (:after counsel-gtags
;;     :nv "gc"    #'counsel-gtags-create-tags
;;     :gi "M-."    #'counsel-gtags-find-definition
;;     :gi "M-,"    #'counsel-gtags-go-backward
;;     :nv "gu"    #'counsel-gtags-update-tags
;;     :nv "gf"    #'counsel-gtags-find-file
;;     :nv "gd"    #'counsel-gtags-find-definition
;;     :nv "g."    #'counsel-gtags-dwim
;;     :nv "gr"    #'counsel-gtags-find-reference
;;     :nv "gs"    #'counsel-gtags-find-symbol
;;     :nv "g,"    #'counsel-gtags-go-backward
;;     :nv "god"    #'counsel-gtags-find-definition-other-window
;;     :nv "gor"    #'counsel-gtags-find-reference-other-window
;;     :nv "gos"    #'counsel-gtags-find-symbol-other-window
;;     :nv "gof"    #'counsel-gtags-find-file-other-window))

