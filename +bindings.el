
;; I prefer using default binding in insert mode
(when (featurep! :editor evil +everywhere)
  (map!
    :gi "C-p" #'previous-line
    :gi "C-n" #'next-line
    :gi "C-b" #'backward-char
    :gi "C-f" #'forward-char
    ;gi "C-k" #'kill-line
    ;; evil bind C-x C-s to company-files
    :gi "C-x C-s" #'save-buffer))
