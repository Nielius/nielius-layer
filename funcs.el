;; * Search functions
;; ---
;; zoek in org-bestanden
;; eventuele verbetering: zoek alleen in bepaalde directories (niet heel ~/)

;; Wat wil ik?
;; - zoek bestandsnamen
;;   - alle bestandsnamen
;;   - alleen org
;; - zoek in bestanden (eigenlijk aan org-bestanden)
;;   - met helm
;;   - niet interactief


(defun nielius--get-org-files ()
  "Return a list of all org files on the system."
  ;; misschien zou dit eigenlijk opgeslagen moeten worden? ipv steeds opnieuw
  ;; berekend? dat heeft een speciale naam
  (append
   (directory-files-recursively "~/doc/studie/" ".*\\.org$")
   (directory-files-recursively "~/doc/org/" "^log.*\\.org$")
   (directory-files-recursively "~/doc/org/" "^dagplanning.*\\.org$")))

(defun nielius-search-org-files (search-string)
  (interactive "sSearch for: ")
  (find-grep
   (format "find ~/doc -name \"*.org\" -type f -exec grep -inH -e \"%s\" {} + ")
   ;; This was the old search format I used, when all my files were still in ~/Dropbox/... :
   ;; (format "find ~/ -not \\( -path ~/Dropbox/.dropbox.cache -prune \\) -not \\( -path ~/Dropbox/config/okular-docdata/ -prune \\) -name \"*.org\" -type f -exec grep -inH -e \"%s\"  {} +"
   ;;                   search-string)

             ))
;; voor info over find-command:
;; http://stackoverflow.com/a/16595367
(defun nielius-search-studie-files (search-string)
  (interactive "sSearch for: ")
  (find-grep (format "find ~/doc/studie -name \"*.org\" -type f -exec grep -inH -e \"%s\"  {} +"
                     search-string)))

(defun nielius-grep-studie-files (arg)
  "Zoek in studiebestanden.
Zonder prefix: gebruik helm-do-grep.
Met prefix: gebruik find-grep."
  (interactive "P")
  (if arg
      (call-interactively #'nielius-search-studie-files)
    (helm-do-grep-1
     (nielius--get-org-files))))

(defun nielius-grep-org-files (arg)
  "Zoek in org-bestanden.
Zonder prefix: gebruik helm-do-grep.
Met prefix: gebruik find-grep."
  (interactive "P")
  (if arg
      (call-interactively #'nielius-search-org-files)
    (helm-do-grep-1
     (directory-files-recursively "~/doc" ".*\\.org$"))))

(defun nielius-open-org-files ()
  "Use helm to open one of the org files on the system. FIXME"
  ;; misschien moet ik gewoon helm sources bouwen?
  (interactive)
  (helm :sources 'helm-source-findutils
        :buffer "*helm find*"
        :ff-transformer-show-only-basename nil
        :case-fold-search helm-file-name-case-fold-search)
  )


;; * misc custom commands
;; ---
(defun my-ediff-backup ()
  "Apply ediff-files to the current file and its #-surrounded backup (e.g. file.txt and #file.txt#)."
  (interactive)
  (let ((backup-file-name
         (make-auto-save-file-name)
         ;; (concat
         ;;                   (file-name-directory
         ;;                    buffer-file-name)
         ;;                   "#"
         ;;                   (file-name-nondirectory
         ;;                    buffer-file-name)
         ;;                   "#")
                          ))
    (ediff-files
     buffer-file-name
     backup-file-name)
    (delete-file backup-file-name)))


(setq org-latex-my-export-dir "/home/niels/tmp/tmpdir/")
(defun org-latex-export-to-pdf-cd (display-in-okular &rest r)
  (interactive "p")
  (let ((orig-dir default-directory))
    ;;   (message "Default directory before cd is %s" orig-dir)
    (if (file-accessible-directory-p  org-latex-my-export-dir)
     (cd org-latex-my-export-dir)
     (user-error (format  "Directory %s does not exist!" org-latex-my-export-dir)))
    ;;    (message "Original directory after cd is %s" orig-dir)
    (apply #'org-latex-export-to-pdf r)
    (if display-in-okular
        (start-process "org-latex-export-okular-process" nil "okular"
                       (concat ;; lijkt niet nodig directory te doen... gek...
                        ;; was ook al zo als ik eerst cd orig-dir deed
                        (file-name-base buffer-file-name)
                        ".pdf")))
    (cd orig-dir)
    ))


;; doet dit hetzelfde als org-latex-convert-region-to-latex?
;; als ik zoiets wil opzetten, zie ook org-export-as in ox.el (dit export iig een string);
;; het lijkt erop dat het alleen goed te doen is door zelf een buffer te creeëren
;; (defun org-latex-export-inline (arg)
;;   "Replace the current org region with its latex export.
;; With prefix: do not replace current region, only yank new export.
;; Seems to do the same thing as org-latex-convert-region-to-latex..."
;;   (interactive "P")
;;   (let ((org-export-show-temporary-export-buffer nil))
;;     (org-latex-export-as-latex nil nil nil t))
;;   (if (not arg)
;;       (delete-region (region-beginning) (region-end)))
;;   (insert-buffer-substring-as-yank "*Org LATEX Export*"))



(defun niels-go-home-and-open ()
  "Open the \"home screen\", start a search and open the link
  that is at point after the search."
  (interactive)
  (let
      ((startwin (selected-window))
       (org-link-frame-setup '((file . find-file))) ; het is idioot dat het zo
                                        ; moet, maar dit zorgt er
                                        ; hopelijk voor dat de link
                                        ; in een ander frame wordt
                                        ; geopend
       )
    (with-selected-window startwin
      (find-file "~/doc/org/home.org")
      ;; (spacemacs/toggle-centered-buffer-mode) ; dit gaf problemen
      (goto-char 0)
      (isearch-forward) ; ik zou natuurlijk ook b.v. helm-swoop kunnen gebruiken
      (org-open-at-point))))



;; Keyboard macro om links die ik via chrome heb gekregen mooi te maken.
;; kun je uitvoeren met M-x of met call macro
;; ook misschien handig: apply-macro-to-region-lines to a key
;; ook grappig: You can create a macro out of your last 100 keystrokes. Type C-x C-k l.
;; repeat met: C-x z (z z z z z z )


(defun prettify-chrome-links ()
  (interactive)
                                        ; "slecht" geschreven: doe eerst dat, doe dan dit
  ;; delete-region
  ;; delete-and-extract-region
  ;; point-at-bol
  ;; point-at-eol
  ;; (beginning-of-line)
  ;; (insert "[[")
  ;; (end-of-line)
  ;; (insert "][")
  ;; (join-line 1)
  ;; (end-of-line)
  ;; (insert "]]")
  (let
      ((link (delete-and-extract-region (point-at-bol 2) (point-at-eol 2)))
       (description (delete-and-extract-region (point-at-bol) (point-at-eol 1))))
    (insert (format "[[%s][%s]]" link description)))
  (join-line 1))

(defun listify-chrome-links (beg end)
  "Turn the selected region of links into a nice list of org-mode links."
  (interactive (list (region-beginning) (region-end)))
  (if (= beg end) ; i.e., if no region is actually selected
      (listify-chrome-links--helper (split-string (current-kill 0) "\n+")) ; insert a listified version of what's in the kill ring
    (let
        ((links (delete-and-extract-region beg end)))
      ;; just feed a list of (descr link descr link ...) into the helper
      (listify-chrome-links--helper (split-string links "\n+"))
      )))

(defun listify-chrome-links--helper (link-list)
  (let
      ((description (car link-list))
       (link (car (cdr link-list)))
       (rest (cdr (cdr link-list))))
    (if (and description link) ; i.e., if we're not at the end of a list
        (progn
          (insert (format "- [[%s][%s]]\n" link description))
          (listify-chrome-links--helper rest)))))

(defun listify-at-newlines (arg beg end)
  "Convert all plain lines in region to a plain list with
checkboxes. Code at https://emacs.stackexchange.com/a/5617 due to
Dan, but adapted by me (NudB)."
  (interactive "P\nr")
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (dotimes (_ (- (line-number-at-pos end) (line-number-at-pos beg)))
      (if arg
          (insert "- [ ] ")
        (insert "- "))
      (indent-according-to-mode)
      (forward-line 1))))


(defun open-org-export-in-broswer ()
  "Open the HTML export of the current org-mode file in a
browser. Assumes a very specific organisation: the org-mode files
are supposed to be in ~/doc and this entire directory is supposed
to be exported to ~/Dropbox/org-export/"
  (interactive)
  (let
      ((directory-name (file-relative-name (file-name-directory (buffer-file-name)) "~/doc"))
       (file-name (concat (file-name-base (buffer-file-name)) ".html")))
    (browse-url-default-browser
     (concat
      "file://"
      (expand-file-name "~/Dropbox/org-export/")
      directory-name
      file-name)
     )))


;; * Start-up screen
;; ** New frame met organiser
(defun niels-open-organiser ()
  "Open een nieuw frame met allemaal organiser-informatie"
  (interactive)
  (let*
      ((organiser-frame (make-frame '((name . "organiser"))))
       (w1 (car (window-list organiser-frame)))
       (w2 (split-window w1 4 'below))
       (w3 (split-window w2 nil 'right))
       (w4 (split-window w3 (* 4 (/ (window-total-height w3) 5)) 'below))
       (org-link-frame-setup '((file . find-file)))
       (weeknr (string-to-int (shell-command-to-string "echo -n $(date +%V)")))
       (week-string (shell-command-to-string "echo -n W$(date +%V)")))
    (with-selected-window w1
      (org-open-file "~/Dropbox/org/lessen.org" nil nil "Projectenoverzicht")
      ()
      (forward-line (+ 10 weeknr))
      (recenter 3))
					; aka kalendar
    (with-selected-window w3
      (org-open-file "~/Dropbox/org/lessen.org" nil nil "Begonnen")
					; begonnen habits
      (recenter 0))
    (with-selected-window w2
      (org-open-file "~/Dropbox/org/dagplanning.org" nil nil week-string)
      (org-goto-local-search-headings
       (shell-command-to-string "LC_TIME='nl_NL.UTF-8' date '+%a'")  nil nil)
      (recenter 0))
    (with-selected-window w4
      (org-open-file "~/Dropbox/org/log.org" nil nil week-string)
      (recenter 0))))

(defun niels-startup-screen ()
  (let ((week-string (shell-command-to-string "echo -n W$(date +%V)")))
    (org-open-file "~/Dropbox/org/dagplanning.org" nil nil week-string)
    (delete-other-windows)))

(defun create-link-to-desktop (&optional file-to-link-to)
  (interactive)
  (let*
      ((src (if file-to-link-to
                file-to-link-to
              (buffer-file-name)))
       (month (format-time-string "%Y-%m"))
       (tgt (concat "~/Desktop/maanden/" month "/" (file-name-nondirectory src))))
    (if (not (file-exists-p tgt))
        (progn
          (make-symbolic-link src tgt)
          (message (format "Link to %s made at %s" src tgt)))
      (message (format "A link to %s already exists" tgt)))))
