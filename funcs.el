;; 
;;; Utility functions for elisp code

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


;; 
;;; General purpose emacs functions
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
  "Save the name of the current buffer to the kill ring.
Better to use `spacemacs/copy-file-path'."
  (interactive)
  (kill-new buffer-file-name))

(defun relativize-filename (beg end)
  "Relativize the filename that is selected.
Useful to replace absolute links in markdown files or org files
by relative links."
  (interactive "r")
  (let ((filename (buffer-substring-no-properties beg end)))
    (replace-region-in-buffer beg end
                              (file-relative-name filename (file-name-directory
                                                                    (buffer-file-name))))))

(defun relativize-filename-at-point ()
  "Relativize the filename at the point (obtained via `thing-at-point').
Useful to replace absolute links in markdown files or org files
by relative links."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'filename)))
    (relativize-filename (car bounds) (cdr bounds))))

;; TODO: move this to md-agenda
(defun markdown-paste-as-relative-link (&optional filename)
  "Pastes the filename in the kill ring as a relative link.
Useful in combination with `spacemacs/copy-fily-path', which
copies the path to the file that is being visited in the current
buffer. This makes it easier to use simple markdown files as a
kind of personal wiki.

If the optional argument FILENAME is given, then make a link to
that file, instead of to the filename in the kill ring."
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


(defun open-this-file-in-browser (&optional new-window-q)
  "Open the current file in firefox.
With prefix argument, open new window."
  (interactive)
  (browse-url-firefox (buffer-file-name) new-window-q))


(defun create-link-to-desktop (&optional file-to-link-to)
  "Create a linux symbolic link to the currently visited file in the directory
`~/Desktop/maanden/<currentmonth>/'."
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
;; Functions to prettify links from chrome


(defun prettify-chrome-links ()
  "This function turns something of the form

   Description of a link
   http://some-link.com/wherever

into an org-mode-link. Execute it with the cursor on the
description of the link."
  (interactive)
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
  "Turn the selected region of links (of the form
`<description>\n<link>\n', with different links separated by
blank lines)into a nice list of org-mode links."
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
;; Smart open/execute functions

;; It requires ffap.el.
(autoload 'ffap-file-at-point "ffap.el")
(defun nielius-ffap ()
  "My own \"find file at point\". Does not prompt to ask if the file was
  correct, so it is quicker."
  (interactive)
  (let
      ((filename (ffap-file-at-point)))
    (find-file filename)))


(defun nielius-xdg-open-this-line ()
  "Open the file on the current line with `xdg-open' (i.e., open
it with a default application determined by the (Linux) desktop
system)."
  (interactive)
  (let ((filename (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (message (format "Trying to open %s." filename))
    (start-process "nielius-xdg-open-this-line" nil "xdg-open" filename)
    ))

(defun nielius-xargs-xdg-open-smart (beg end)
  "If a region is selected, apply xargs to the region.
If no region is selected, open the file with `xdg-open'
(through the function `nielius-xdg-open-this-line')."
  (interactive (list (region-beginning) (region-end)))
  (if (use-region-p) ; i.e., if a region is selected
    (nielius-xargs-on-region nil "xdg-open {}" beg end)
    (nielius-xdg-open-this-line)))

(defun nielius-xargs-on-region (replace cmd beg end)
  "Do xargs on the region. Use newline as the delimiter (i.e.,
`xargs -d '\n'`) and specify the input with `{}'."
  (interactive  "P\nsCommand: \nr")
  (print replace)
  (shell-command-on-region beg end (format "xargs -I{} -d '\n' %s" cmd) replace replace))

(defun nielius-sh-execute-region-or-line (replace beg end)
  "Pipe the region (if using or region) or the current line (if
not using region) to bash. With a prefix argument, replace
line/region with output; otherwise, display output in a temporary
buffer."
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


;; 
;;; Functions for mouse organisation state

(defun evil-my-paste-at-mark (char)
  "Paste from the kill ring at the given mark."
  (interactive (list (read-char)))
  (save-excursion
    (evil-goto-mark char)
    (yank)))

(defun last-key ()
  "Return the last key pressed."
  (car (reverse (append (recent-keys) nil))))

(defun nielius-layer--move-line-to-last-pressed-key ()
  "Kill the current line and paste it at the mark of the key that is last pressed.

This is not intended to be used as a standalone function, but
works with the my-mouse-organisation-state transient state."
  (interactive)
  (let ((last-key-pressed (last-key)))
    (when (or
           ;; if marker exists, everything fine
           (evil-get-marker last-key-pressed)
           ;; else, ask user if he wants to make a new marker and continue
           (let
               ((label (read-string "Marked doesn't exist. Label for new marker (empty for abort): ")))
             (if (not (string= label ""))
                 ;; then insert label at start of file and set the marker there
                 (progn
                   ;; make the label at the start of the document and return marker-char-maybe
                   (save-excursion
                     (goto-char (point-min))
                     (insert (format "%s\n" label))
                     (evil-set-marker last-key-pressed)
                     (insert "\n\n"))
                   t)
               ;; otherwise, abort
               nil)))
      ;; actual work:
      (progn
        (kill-region
         (- (line-beginning-position) 1)
         (line-end-position))
        (evil-my-paste-at-mark last-key-pressed)
        ;; These next two lines are supposed to make the behaviour similar to evil's 'dd'.
        (evil-next-line)
        (evil-first-non-blank)))))

(defun nielius-layer--generate-all-keybindings-for-my-mouse-organisation-state ()
  "A helper function that generates all necessary keybindings for
the transient state called my-mouse-organisation-state."
  (mapcar
   (lambda (i)
     (let
         ((binding (char-to-string (+ ?a i))))
       (list binding 'nielius-layer--move-line-to-last-pressed-key (format "Move to %s" binding))))
   (number-sequence 0 25)))

(eval
 `(spacemacs|define-transient-state my-mouse-organisation-state
   :title "Mouse based organisation"
   :doc "Move lines to marks by using the mouse and pressing the character for the mark."
   :foreign-keys run
   :bindings
   ,@(nielius-layer--generate-all-keybindings-for-my-mouse-organisation-state)
   ("q" nil "Exit"
    :exit t)))


;; 
;; Functions for quick copy
;;
;; Uses my shell-script mergecopy

(setq nielius-mergecopy-process nil)

(defun nielius-mergecopy ()
  (interactive)
  (let ((shell-script-to-run
         "while xclip -o -sel c && echo -e \"\\nNEWCOPYSTARTSHERE\"; do xclip -i -quiet -sel c <> /dev/null >&0 2>&0; done "))
    (message "Running: %s" shell-script-to-run)
    (setq nielius-merge-copy-process
          (start-process "mergecopy" "*mergecopy*" "sh" "-c" shell-script-to-run))))

(defun nielius-mergecopy-stop ()
  (interactive)
  (delete-process nielius-merge-copy-process))
