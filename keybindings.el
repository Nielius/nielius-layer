;; ** Keybindings voor eigen zoekfuncties
;; todo: <f2> i : zoek met helm-swoop het index-bestand van mijn aantekeningen
;; (define-key global-map (kbd "<f2> s") 'nielius-grep-org-files)
;; (define-key global-map (kbd "<f2> d") 'nielius-grep-studie-files)
;; (define-key global-map (kbd "<f2> f") (lambda () (interactive) (helm-find-1 "~/Dropbox/")))
;; (define-key global-map (kbd "<f2> b") (lambda () (interactive) (helm-find-1 "~/md/"))) ; wiskundeboeken

(require 'helm-find) ;; is this executed? otherwise, I need to put this in a defun that replaces the commands for of, op, ob (or use progn?)

;; Override some general keybindings
(define-key evil-normal-state-map (kbd "gf") 'nielius-ffap)
(define-key evil-normal-state-map (kbd "<up>") 'm/scrollup)
(define-key evil-normal-state-map (kbd "<up>") 'm/scrolldown)

(define-key evil-normal-state-map (kbd "<mouse-2>") 'my-eval-curr-sexp)

(spacemacs/set-leader-keys
  "er" 'eval-region
  "ec" 'nielius-eval-curr-sexp
  "ee" 'eval-last-sexp ; more useful than I thought
  )


;; Keybindings for finding files
(spacemacs/set-leader-keys
  ; "os" 'nielius-grep-studie-files ;; gebruikte ik niet echt
  "os" 'create-link-to-desktop
  "od" 'nielius-grep-org-files
  "of" (lambda () (interactive) (helm-find-1 "~/doc/"))
  "oF" 'baloo-helm-search
  "op" (lambda () (interactive) (helm-find-1 "~/.spacemacs.d/private/")) ; configure private layers
  "ob" (lambda () (interactive) (helm-find-1 "~/md/"))
  "oB" (lambda () (interactive) (helm-find-1 "~/library/"))
  "oe" 'nielius-sh-execute-region-or-line ; send current line or region to inferior shell; 'e' for execute; compare to the global "SPC e e" for evaluation sexps anywhere; see also nielius-xargs-on-region
  "oH" (lambda () (interactive) (find-file "~/doc/org/home.org"))
  "ol" (lambda () (interactive) (ace-link)
         (spacemacs/toggle-maximize-buffer)) ; open link and immediately maximize
  ; "wn" 'make-frame ; dit is nu SPC w F
  "oo" 'nielius-xargs-xdg-open-smart
  "oy" 'buffer-file-name-to-kill-ring
  ;; Relativize the filename at point is useful in combination with buffer-file-name-to-kill-ring.
  "or" (lambda () (interactive (find-file "~/doc/notes/agenda/.latest_docs.md")))
  "ox" 'nielius-xargs-on-region
  )

(spacemacs/set-leader-keys-for-major-mode 'org-mode ; works only in org-mode, using SPC m or ,
  "io" 'helm-okular ; insert org link; eigenlijk deel van een andere layer?
  "iO" 'okular-dbus-insert-default-link ; komt ook van andere layer
  "oh" 'open-the-exported-html
  "oq" 'org-latex-export-to-pdf-cd) ; q voor quick export


(define-key evil-insert-state-map (kbd "M-p") 'yas-expand)

;; Keybindings for markdown mode
(spacemacs/set-leader-keys-for-major-mode 'markdown-mode "v" 'open-this-file-in-browser)
(spacemacs/set-leader-keys-for-major-mode 'markdown-mode "p" 'markdown-paste-as-relative-link)


;; Global keybindings
(evil-global-set-key 'normal (kbd "~") 'evil-my-paste-at-mark)


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
