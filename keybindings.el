;; ** Keybindings voor eigen zoekfuncties
;; todo: <f2> i : zoek met helm-swoop het index-bestand van mijn aantekeningen
;; (define-key global-map (kbd "<f2> s") 'nielius-grep-org-files)
;; (define-key global-map (kbd "<f2> d") 'nielius-grep-studie-files)
;; (define-key global-map (kbd "<f2> f") (lambda () (interactive) (helm-find-1 "~/Dropbox/")))
;; (define-key global-map (kbd "<f2> b") (lambda () (interactive) (helm-find-1 "~/md/"))) ; wiskundeboeken

(spacemacs/set-leader-keys
  "os" 'nielius-grep-studie-files
  "od" 'nielius-grep-org-files
  "of" (lambda () (interactive) (helm-find-1 "~/doc/"))
  "op" (lambda () (interactive) (helm-find-1 "~/Dropbox/emacs/spacemacs/private/")) ; configure private layers
  "ob" (lambda () (interactive) (helm-find-1 "~/md/"))
  "oh" 'niels-go-home-and-open
  "oe" 'open-org-export-in-broswer ; eigenlijk hoeft dit natuurlijk alleen in org-files
  "oH" (lambda () (interactive) (find-file "~/doc/org/home.org"))
  "ol" (lambda () (interactive) (ace-link)
         (spacemacs/toggle-maximize-buffer)) ; open link and immediately maximize
   "wn" 'make-frame)

(spacemacs/set-leader-keys-for-major-mode 'org-mode
  "io" 'helm-okular ; insert org link; eigenlijk deel van een andere layer?
  "iO" 'okular-dbus-insert-default-link ; komt ook van andere layer
  "oq" 'org-latex-export-to-pdf-cd) ; q voor quick export

(define-key evil-insert-state-map (kbd "M-p") 'yas-expand)
