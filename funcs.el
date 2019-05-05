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


;;; Utility functions
(defun insert-string-at-point (str pt)
  "Insert the string STR at the point PT. PT is an integer that
specifies the position; it is fed to `goto-char'."
  (save-excursion
    (goto-char pt)
    (insert str)))

(defun replace-region-in-buffer (beg end str)
  "Replace the region between BEG and END in the current buffer with the string
  STR."
  (delete-region beg end)
  (insert-string-at-point str beg))


;;; General purpose functions
(defun m/scrolldown ()
  (interactive)
  (scroll-up 1))
(defun m/scrollup ()
  (interactive)
  (scroll-down 1))

(defun nielius-eval-curr-sexp ()
  "Evaluate (as emacs lisp) the current sexp, where the sexp is
determined by `sp-get-enclosing-sexp`."
  (interactive)
  (pcase-let
      (( `(_ ,beg _ ,end) (sp-get-enclosing-sexp)))
    (eval-region beg end)))

(defun buffer-file-name-to-kill-ring ()
  "Save the name of the current buffer to the kill ring."
  (interactive)
  (kill-new buffer-file-name))

;; relies on my small library
(defun relativize-filename (beg end)
  "Relativize the filename that is selected."
  (interactive "r")
  (let ((filename (buffer-substring-no-properties beg end)))
    (replace-region-in-buffer beg end
                              (file-relative-name filename (file-name-directory
                                                                    (buffer-file-name))))))

(defun markdown-paste-as-relative-link (&optional filename)
  "Pastes the filename in the kill ring as a relative link.
Useful in combination with `buffer-file-name-to-kill-ring'.

If the optional argument FILENAME is given, then make a link to that file,
instead of to the filename in the kill ring."
  (interactive)
  (let
      ((filename
        (if filename ; if the optional argument is given,
            filename ; use that
          (substring-no-properties ; otherwise, kill ring
           (car kill-ring)))))
    (unless (eolp) (forward-char)) ; necessary to get evil's normal paste-after effect
    (insert-for-yank
     (concat
      "["
      (file-name-base filename)
      "]("
      (s-trim
       (file-relative-name filename
                           (file-name-directory (buffer-file-name))))
      ")"))))

(defun relativize-filename-at-point ()
  "Relativize the filename at the point, using `thing-at-point' to get the filename at the point."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (relativize-filename (car bounds) (cdr bounds))))


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
    (helm-do-grep-1 '("~/doc") t nil '("*.md"))))

(defun nielius-open-org-files ()
  "Use helm to open one of the org files on the system. FIXME"
  ;; misschien moet ik gewoon helm sources bouwen?
  (interactive)
  (helm :sources 'helm-source-findutils
        :buffer "*helm find*"
        :ff-transformer-show-only-basename nil
        :case-fold-search helm-file-name-case-fold-search)
  )



(defun nielius-insert-doc-file-as-markdown-link ()
  "Use helm to search for a doc file and insert a relative
markdown link to that file."
  (interactive)
  (let
      ((helm-type-file-actions (helm-make-actions "Insert file as link"  'my-helm-insert-as-markdown-link)))
    (helm-find-1 "~/doc")))




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


;; 
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
  (if (= beg end)
      ;; if no region specified, just insert what is in the kill ring
      (listify-chrome-links--helper (split-string (current-kill 0) "\n+")) ; insert a listified version of what's in the kill ring
      ;; otherwise, convert the links in the region
      (let
          ((links (delete-and-extract-region beg end)))
          ;; just feed a list of (descr link descr link ...) into the helper
          (listify-chrome-links--helper (split-string links "\n+"))
        )))

(defun listify-chrome-links--helper (link-list)
  "Turns a list of tuples (description, link) into strings"
  (let
      ((description (car link-list))
       (link (car (cdr link-list)))
       (rest (cdr (cdr link-list))))
    (if (and description link) ; i.e., if we're not at the end of a list
        (progn
          (if (derived-mode-p 'org-mode)
              (insert (format "- [[%s][%s]]\n" link description))
              ;; otherwise, assume markdown
            (insert (format "- [%s](%s)\n" description link))
            )
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

;; 
(defun open-the-exported-html ()
  "Open the HTML export of the current org-mode file in a
browser. Assumes a very specific organisation: the org-mode files
are supposed to be in /home/niels/doc and this entire directory is supposed
to be exported to /home/niels/proj/org-publish/"
  (interactive)
  (let*
      ((org-dir "/home/niels/doc/")
       (export-dir "/home/niels/proj/org-publish/"))
    (if (and ;; just checking if the file is an org-file in our doc-directory
         (string= (file-name-extension buffer-file-name) "org")
         (string-prefix-p org-dir (file-name-directory buffer-file-name)))
        (let*
            ;; directory-name-in-org-dir is going to be the subdirectory in org-dir, e.g.,
            ;; for the file '~/doc/comp/alexa.org' it is going to be 'comp/'
            ((directory-name-in-org-dir (file-relative-name (file-name-directory buffer-file-name) org-dir))
             (html-file-name-nondirectory (concat (file-name-base (buffer-file-name)) ".html"))
             (html-file-name (concat export-dir directory-name-in-org-dir html-file-name-nondirectory)))
          (browse-url-default-browser (concat "file://" (expand-file-name html-file-name))))
      ;; If the file is not in the right directory...
      (message "This file is not an org file in the standard org directory!"))))

;; See this link:
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/File-Name-Components.html
;; for a lot of useful file name operations.


(defun open-this-file-in-browser (&optional new-window-q)
  "Open the current file in firefox.
With prefix argument, open new window."
  (interactive)
  (browse-url-firefox (buffer-file-name) new-window-q)) ; the t is to open in new window



;; 
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
       (tgtdir (concat "~/Desktop/maanden/" month "/"))
       (tgt (concat tgtdir (file-name-nondirectory src))))
    ;; make the target dir (tgtdir) if it doesn't exist yet
    (unless (file-exists-p tgtdir)
      (make-directory tgtdir))
    (if (not (file-exists-p tgt))
        (progn
          (make-symbolic-link src tgt)
          (message (format "Link to %s made at %s" src tgt)))
      (message (format "A link to %s already exists" tgt)))))

;; 
;; Function to quickly open file at point

;; It requires ffap.el.
(autoload 'ffap-file-at-point "ffap.el")
(defun nielius-ffap ()
  "My own \"find file at point\". Does not prompt to ask if the file was correct.

One possible addition would be to use the prefix-key to enter the normal find-file-at-point."
  (interactive)
  (let
      ((filename (ffap-file-at-point)))
    (find-file filename)))


(defun nielius-xdg-open-this-line ()
  (interactive)
  (let ((filename (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (message (format "Trying to open %s." filename))
    (start-process "nielius-xdg-open-this-line" nil "xdg-open" filename)
    ))

(defun nielius-xargs-xdg-open-smart (beg end)
  (interactive (list (region-beginning) (region-end)))
  (if (use-region-p) ; i.e., if no region selected
    (nielius-xargs-on-region "xdg-open {}" beg end)
    (nielius-xdg-open-this-line)))

(defun nielius-xargs-on-region (cmd beg end)
  (interactive  "sCommand: \nr"  )
  (shell-command-on-region beg end (format "xargs -I{} -d '\n' %s" cmd)))

(defun nielius-sh-execute-region-or-line (replace beg end)
  "Pipe the region (if using or region) or the current line to
bash. With a prefix argument, replace line/region with output;
otherwise, display output in a temporary buffer."
  (interactive (list current-prefix-arg (region-beginning) (region-end) ))
  (let ((reg
         (if (use-region-p)
             (cons beg end)
           (cons (line-beginning-position) (line-end-position)))))
    ;; in theory, you could insert in current buffer, but not replace the selection/line
    (shell-command-on-region (car reg) (cdr reg) "bash" replace replace)))


;; 
;; Helm insert markdown links

(defun nielius-helm--insert-markdown-links-action (&optional _ignore)
  "Simple helm action that runs `markdown-paste-as-relative-link' on marked candidates.
Code inspired by `helm-find-many-files'."
  (interactive)
  (let ((helm--reading-passwd-or-string t) ; don't know why; took this from helm-find-many-files
        (candidates (helm-marked-candidates)))
    (if (> (length candidates) 1)
        ;; If we get more than 1 candidate, insert links to all candidates as a markdown list.
        (loop for cand in candidates
              do
              (progn
                (insert "- ")
                (markdown-paste-as-relative-link cand)
                (insert "\n")))
      (mapc 'markdown-paste-as-relative-link (helm-marked-candidates)))))


(defun nielius-helm--insert-markdown-links-windows ()
  "Code inspired by `spacemacs/helm-find-buffers-windows'. I
think it should just do nothing and execute `nielius-helm--insert-markdown-links-action'
on all marked helm candidates (i.e., in the lambda, `candidate' is simply ignored)."
  (interactive)
  (helm-exit-and-execute-action
   (lambda (candidate)
     (nielius-helm--insert-markdown-links-action ))))


(defun nielius-helm-insert-markdown-links-from-doc ()
  "Search for files in my document folder with helm and insert
them as markdown links."
  (interactive)
  (let
      ((helm-type-file-actions
        '(("Make relative markdown link to file" . nielius-helm--insert-markdown-links-action)))
       ;; (helm-find-map my-helm-find-map)
       ;; (spacemacs/helm-find-files-windows 'nielius-helm--insert-markdown-links-action)
       )
    (cl-letf (((symbol-function 'spacemacs/helm-find-files-windows) #'nielius-helm--insert-markdown-links-windows))
      (helm-find-1 "~/doc"))))


;; Add the action to helm's list, so that I can use it with any helm find file.
;; TODO: fix this; this be evaluated after helm is loaded, for example;
;; otherwise my entire config is not loaded
;; (setf
;;  (alist-get "Insert as markdown link" helm-type-file-actions)
;;  'nielius-helm--insert-markdown-links-action)



;; 

(defun evil-my-paste-at-mark (char)
  "Paste from the kill ring at the given mark."
  (interactive (list (read-char)))
  (save-excursion
    (evil-goto-mark char)
    (yank)))
