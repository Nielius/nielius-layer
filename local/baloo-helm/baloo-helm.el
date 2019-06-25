;;
;; Some of the code is inspired by helm-ag.el

;; When editing, this is useful:
(makunbound 'baloo-helm--helm-source)
(defvar baloo-helm--helm-source
  (helm-build-async-source "Baloo Desktop Search"
    :candidates-process 'baloo-helm--helm-candidate-process
    :requires-pattern 3 ; only starts after 3 characters have been entered?
    :action helm-type-file-actions
    :keymap helm-find-files-map
    ))

(defun baloo-helm--helm-candidate-process ()
  "Produces helm candidates by running `baloosearch' with the current helm input."
  ;; helm-ag uses set-process-sentinel (see helm-ag--do-ag-candidate-process),
  ;; but I don't understand why exactly. Maybe to format the results? It doesn't
  ;; seem te be necessary here.
  (start-process "baloo-helm-baloosearch" nil "baloosearch" helm-input))

;; This is the main user function in this file
(defun baloo-helm-search ()
  "Search your desktop with baloo and helm."
  (interactive)
  (helm
   :sources '(baloo-helm--helm-source)
   :buffer "*baloo-helm*"))
