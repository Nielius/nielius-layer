(require 'helm-find) ;; is this executed? otherwise, I need to put this in a defun that replaces the commands for of, op, ob (or use progn?)

;; Override some general key bindings
(define-key evil-normal-state-map (kbd "gf") 'nielius-ffap)
(define-key evil-normal-state-map (kbd "<mouse-2>") 'eval-last-sexp)

;; Global key bindings for evaluating emacs lisp in any file type.
(spacemacs/set-leader-keys
  "er" 'eval-region
  "ec" 'nielius-eval-curr-sexp
  "ee" 'eval-last-sexp ; more useful than I thought
  )


;; Global key bindings
(evil-global-set-key 'normal (kbd "~") 'evil-my-paste-at-mark)
(define-key evil-insert-state-map (kbd "M-p") 'yas-expand)


;; Key bindings for eyebrowse-switch-to-window-config-n with n an integer
;;
;; For every 1 <= n <= 9, map SPC n to switch to eyebrowse window configuration n
(mapc
 (lambda (n)
   (spacemacs/set-leader-keys
     (number-to-string n)
     (symbol-function
      (intern-soft
       (concat "eyebrowse-switch-to-window-config-" (number-to-string n))))))
 (number-sequence 1 9))


;; Generally useful global shortcuts available under `SPC o'
(spacemacs/set-leader-keys
  "os" 'create-link-to-desktop ; `s' for symlink
  "oy" 'buffer-file-name-to-kill-ring ; better: SPC f y y  (spacemacs/copy-file-path)
  ;; Key bindings to execute pieces of shell code or to open files. Compare to
  ;; `SPC e e' for evaluation of sexps anywhere.
  "oe" 'nielius-sh-execute-region-or-line ; `e' for execute
  "oo" 'nielius-xargs-xdg-open-smart ; `o' for open
  "ox" 'nielius-xargs-on-region ; `x' for xargs
  )


;; Key bindings for markdown mode
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
