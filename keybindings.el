;; ** Keybindings voor eigen zoekfuncties
;; todo: <f2> i : zoek met helm-swoop het index-bestand van mijn aantekeningen
;; (define-key global-map (kbd "<f2> s") 'nielius-grep-org-files)
;; (define-key global-map (kbd "<f2> d") 'nielius-grep-studie-files)
;; (define-key global-map (kbd "<f2> f") (lambda () (interactive) (helm-find-1 "~/Dropbox/")))
;; (define-key global-map (kbd "<f2> b") (lambda () (interactive) (helm-find-1 "~/md/"))) ; wiskundeboeken

(require 'helm-find) ;; is this executed? otherwise, I need to put this in a defun that replaces the commands for of, op, ob (or use progn?)

(spacemacs/set-leader-keys
  ; "os" 'nielius-grep-studie-files ;; gebruikte ik niet echt
  "os" 'create-link-to-desktop
  "od" 'nielius-grep-org-files
  "of" (lambda () (interactive) (helm-find-1 "~/doc/"))
  "op" (lambda () (interactive) (helm-find-1 "~/.spacemacs.d/private/")) ; configure private layers
  "ob" (lambda () (interactive) (helm-find-1 "~/md/"))
  "oB" (lambda () (interactive) (helm-find-1 "~/library/"))
  "oh" 'niels-go-home-and-open
  "oe" 'open-org-export-in-broswer ; eigenlijk hoeft dit natuurlijk alleen in org-files
  "oH" (lambda () (interactive) (find-file "~/doc/org/home.org"))
  "ol" (lambda () (interactive) (ace-link)
         (spacemacs/toggle-maximize-buffer)) ; open link and immediately maximize
  ; "wn" 'make-frame ; dit is nu SPC w F
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode ; works only in org-mode, using SPC m or ,
  "io" 'helm-okular ; insert org link; eigenlijk deel van een andere layer?
  "iO" 'okular-dbus-insert-default-link ; komt ook van andere layer
  "oh" 'open-the-exported-html
  "oq" 'org-latex-export-to-pdf-cd) ; q voor quick export


(define-key evil-insert-state-map (kbd "M-p") 'yas-expand)


;; Keybindings for markdown mode
(spacemacs/set-leader-keys-for-major-mode 'markdown-mode "v" 'open-this-file-in-browser)


;; Reftex-keybindings
;; ~~~~~~~~~~~
;; Also see https://www.gnu.org/software/emacs/manual/html_node/reftex/Key-Bindings.html
;; I found the correct mode by running describe-keymap and searching.
;; Also useful: helm-descbinds, describe-bindings.
(eval-after-load 'reftex-toc
  '(progn
     (define-key reftex-toc-mode-map  "j" 'reftex-toc-next)
     (define-key reftex-toc-mode-map "k" 'reftex-toc-previous)))

;; Maar dit is eigenlijk niet helemaal wat ik wil.
;; Ik wil gewoon in evil-mode zitten en een paar handige dingen van reftex.
;; Misschien als inspiratie hoe dired werkt?
