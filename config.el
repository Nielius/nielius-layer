;; The following code adds two functions to (most? all? at least some)
;; Helm actions that work on files
(with-eval-after-load 'helm
  (setf
   (alist-get "Insert as markdown link" helm-find-files-actions nil nil #'equal)
   'nielius-helm--insert-markdown-links-action)
  (setf
   (alist-get "Append to this file and delete current file" helm-find-files-actions nil nil #'equal)
   'append-this-file-to-other-and-delete))
