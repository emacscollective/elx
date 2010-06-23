;; TODO maybe delete these packages:
;; - write-subdirs-el
;; - wcy-display-line-number

;; TODO provide the feature:
;; - xmlunicode
;; - xmlchars

;; TODO did't bother to check yet:
;; - whizzytex
;; - wanderlust

;; TODO for backward compat, but this information ain't used:
;; - hilit19
;; - help+20
;; - hfy-emacs20)

;; (defun elx-package-features* (name repo rev &optional verify)
;;   (let (required required-hard required-soft
;; 	provided provided-full bundled dropped
;; 	(include (elx-get repo "include" t))
;; 	(exclude (elx-get repo "exclude" t))
;; 	(exclude-path (elx-get repo "exclude-path"))
;; 	(prefixes (nconc (list name)
;; 			 (elx-get repo "prefix")
;; 			 (when (string-match "\\(.+\\)-mode$" name)
;; 			   (list (match-string 1 name))))))
;;     (dolist (file (elx-elisp-files (cons repo rev) nil t))
;;       (lgit-with-file repo rev file
;; 	(setq provided (elx--buffer-provided)
;; 	      required (elx--buffer-required)))
;;       ;; (when (> (length provided) 1)
;;       ;; 	(elx-msg "Multiple features: %s" provided))
;;       (dolist (prov provided)
;; 	(cond ((member prov elx-features-exclude-xemacs)
;; 	       (push prov dropped))
;; 	      ((member* file exclude-path
;; 			:test (lambda (file rgx)
;; 				(string-match rgx file)))
;; 	       (push prov dropped))
;; 	      ((member prov exclude)
;; 	       (push prov bundled))
;; 	      ((let ((id (or prov
;; 			     ;; If FILE does not provide a feature
;; 			     ;; we still have to check whether the
;; 			     ;; required features are relevant, so
;; 			     ;; match against the filename instead.
;; 			     (when (string-match
;; 				    "\\([^/]+?\\)\\.el\\(\\.in\\)?$"
;; 				    file)
;; 			       (intern (match-string 1 file))))))
;; 		 (or (member id include)                ; forced include
;; 		     (member* (symbol-name id) prefixes ; has valid prefix
;; 			      :test (lambda (feature pre)
;; 				      (string-match
;; 				       (concat "^" (regexp-quote pre))
;; 				       feature)))))
;; 	       (if (not prov)
;; 		   (elx-msg "No features [1]")
;; 		 ;; Test whether another package also provides this feature.
;; 		 (let ((elt (assoc prov elx-features-provided)))
;; 		   (if elt
;; 		       (unless (equal (cdr elt) name)
;; 			 (elx-msg "!!! Feature %s provided by %s and %s"
;; 				  prov (cdr elt) name))
;; 		     ;; Store feature/package association.
;; 		     (aput 'elx-features-provided prov name)
;; 		     ;; Store provided feature.
;; 		     (push prov provided-full))))
;; 	       ;; Store required features.
;; 	       (setq required-hard
;; 		     (nconc (copy-list (nth 0 required)) required-hard))
;; 	       (setq required-soft
;; 		     (nconc (copy-list (nth 1 required)) required-soft))
;; 	       )
;; 	      ((not prov)
;; 	       (elx-msg "No feature [2]"))
;; 	      (t
;; 	       (elx-msg "Bad prefix: %s" prov)
;; 	       ))
;; 	))
;;     ;; (list required-hard required-soft
;;     ;; 	  provided-full bundled dropped)
;;     nil))




;; (defun elx-package-features* (name repo rev)
;;   (let (required required-hard required-soft
;; 	provided provided-repo bundled
;; 	(include (mapcar #'intern (elx--git-get repo "elm.include")))
;; 	(exclude (mapcar #'intern (elx--git-get repo "elm.exclude")))
;; 	(exclude-path (elx--git-get repo "elm.exclude-path"))
;; 	(prefixes (cons name (elx-get repo "prefix"))))
;;     ;; Collect features.
;;     (dolist (file (elx-elisp-files (cons repo rev)))
;;       (lgit-with-file repo rev file
;; 	(setq provided (elx--buffer-provided)
;; 	      required (elx--buffer-required)))
;;       (when (> (length provided) 1)
;; 	(elx-msg "Multiple features: %s" provided))
;;       (dolist (prov (elx-provided (cons repo rev)))
;; 	(cond ((or (member  prov exclude)
;; 		   (member* file exclude-path
;; 			    :test (lambda (file path)
;; 				    (string-match path file))))
;; 	       (push prov bundled))
;; 	      ((let ((id (or prov
;; 			     ;; If FILE does not provide a feature
;; 			     ;; we still have to check whether the
;; 			     ;; required features are relevant, so
;; 			     ;; match against the filename instead.
;; 			     (when (string-match
;; 				    "\\([^/]+?\\)\\.el\\(\\.in\\)?$"
;; 				    file)
;; 			       (intern (match-string 1 file))))))
;; 		 (or (member id include)                ; forced include
;; 		     (member* (symbol-name id) prefixes ; has valid prefix
;; 			      :test (lambda (feature pre)
;; 				      (string-match
;; 				       (concat "^" (regexp-quote pre))
;; 				       feature)))))
;; 	       (when prov
;; 		 (push prov provided-repo))
;; 	       (setq required-hard
;; 		     (nconc (copy-list (nth 0 required)) required-hard))
;; 	       (setq required-soft
;; 		     (nconc (copy-list (nth 1 required)) required-soft))))))
;;     ;; Add provides to `elx-features-provided', check for conflicts.
;;     (dolist (prov provided-repo)
;;       (let ((elt (assoc prov elx-features-provided)))
;; 	(if elt
;; 	    (unless (equal (cdr elt) name)
;; 	      (elm-log "Feature %s provided by %s and %s"
;; 		       prov (cdr elt) name))
;; 	  (aput 'elx-features-provided prov name))))
;;     ;; Cleanup and return features.
;;     (list (sort (remove-duplicates provided-repo) #'string<)
;; 	  (sort (remove-duplicates required-hard) #'string<)
;; 	  (sort (remove-duplicates required-soft) #'string<))))


(progn (elm-recreate-feture-list)
       (elm-map-packages
	 (lambda (name)
	   (elx-package-features name
				 (elm-package-repository name)
				 (car (elm-package-vendors name))))))

(elm-recreate-keyword-list)

(elm-recreate-epkgs)

(elx-package-mainfile '("/home/devel/emacs/mirror/pkgs/ljupdate/" . "master"))

(elm-update-version-epkgs "elm" "master" t)

(elx-package-mainfile '("/home/devel/emacs/mirror/pkgs/ecukes/" . "master"))

(elx-package-mainfile '("/home/devel/emacs/mirror/pkgs/ecukes/" . "master"))

(elx-package-mainfile '("/home/devel/emacs/mirror/pkgs/37emacs/" . "master"))
(elx-elisp-files '("/home/devel/emacs/mirror/pkgs/37emacs/" . "master"))

(elm-update-version-epkgs "edit-server" "master")

(elx-package-mainfile '("/home/devel/emacs/mirror/pkgs/edit-server/" . v1.3))
(elx-elisp-files '("/home/devel/emacs/mirror/pkgs/edit-server/" . v1.3))


(elm-map-packages
  (lambda (name)
    (let ((mains (cdr (lgit (elm-package-repository name) 1
			    "config --get-all elm.mainfile"))))
      (when mains
	(message "%s: %s" name mains)))))


(elx-with-mainfile '("/home/devel/emacs/mirror/pkgs/edit-server/" . "v1.3") nil
  (message "%s %s %s %s"
	   mainfile
	   (buffer-file-name)
	   (elx-created)
	   (elx-created mainfile)
	   ))

(lgit-with-file
    "/home/devel/emacs/mirror/pkgs/edit-server/"
    "v1.3"
    "servers/edit_server.el"
  (buffer-file-name))

