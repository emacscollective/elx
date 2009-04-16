;;; elx.el --- extract information from Emacs Lisp libraries

;; Copyright (C) 2008, 2009  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Updated: 20090416
;; Version: 0.0.8
;; Homepage: https://github.com/tarsius/elx
;; Keywords: libraries

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library extracts information from Emacs Lisp libraries.

;; This extends library `lisp-mnt', which is only suitable for libraries
;; that closely follow the header conventions.  Unfortunately there are
;; many libraries that do not - this library tries to cope with that.

;; This library is also able to extract some values that `lisp-mnt' can't.
;; Most notably this library can extract the features required and
;; provided by a file or group of files.  Libraries `load-hist' and
;; `lib-requires' provide similar functionality by inspecting
;; `load-history' and thus require the inspected library to be loaded.

;; This library on the other hand uses regexp search in the respective
;; files making it unnecessary to load them first.  This means that the
;; `require' and `provide' have to satisfy certain restrictions (see
;; `elx-required' and `elx-provided') and features required or provided
;; by other means are not recognized.  But this is very rare, and this
;; library has the advantage that it can be used even when the required
;; features can't be loaded (because the providing libraries are not
;; available) or when one doesn't want to load them for security reasons.

;;; Code:

(require 'cl)
(require 'dconv)
(require 'vcomp)
(require 'lisp-mnt)

(defgroup lib-info nil
  "Extract information from Emacs Lisp libraries."
  :group 'emacs
  :link '(url-link :tag "Homepage" "https://github.com/tarsius/elx"))

(defmacro elx-with-file (file &rest body)
  "Execute BODY in a buffer containing the contents of FILE.
If FILE is nil or equal to `buffer-file-name' execute BODY in the
current buffer.  Move to beginning of buffer before executing BODY."
  (declare (indent 1) (debug t))
  (let ((filesym (make-symbol "file")))
    `(let ((,filesym ,file))
       (if (and ,filesym (not (equal ,filesym buffer-file-name)))
	   (with-temp-buffer
	     (insert-file-contents ,filesym)
	     (with-syntax-table emacs-lisp-mode-syntax-table
	       (goto-char (point-min))
	       ,@body))
	 (save-excursion
	   (with-syntax-table emacs-lisp-mode-syntax-table
	     (goto-char (point-min))
	     ,@body))))))

(defun elx-header (header &optional multiline)
  "Return the contents of the header named HEADER, a string.
Or if MULTILINE is non-nil return a list of strings, one per
continuation line."
  (if multiline
      (lm-header-multiline header)
    (save-excursion
      (lm-header header))))

;;; Extract Various.

(defun elx-summary (&optional file standardize)
  "Return the summary of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.
If STANDARDIZE is non-nil remove trailing period and upcase first word."
  (let ((summary
	 (elx-with-file file
	   (when (and (looking-at lm-header-prefix)
		      (progn (goto-char (match-end 0))
			     (looking-at "[^ ]+[ \t]+--+[ \t]+\\(.*\\)")))
	     (let ((summary (match-string-no-properties 1)))
	       (if (string-match "[ \t]*-\\*-.*-\\*-" summary)
		   (substring summary 0 (match-beginning 0))
		 summary))))))
    (unless (member summary '(nil ""))
      (when standardize
	(when (string-match "\\.$" summary)
	  (setq summary (substring summary 0 -1)))
	(when (string-match "^[a-z]" summary)
	  (setq summary
		(concat (upcase (substring summary 0 1))
			(substring summary 1)))))
      summary)))

(defun elx-keywords (&optional file)
  "Return list of keywords given in file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil."
  (elx-with-file file
    (let ((keywords (elx-header "keywords" t)))
      (when keywords
	(split-string
	 (replace-regexp-in-string
	  "\\(\t\\|\s\\)+" "\s"
	  (replace-regexp-in-string
	   "," ""
	   (downcase (mapconcat #'identity keywords " "))))
	 " ")))))

(defun elx-commentary (&optional file)
  "Return the commentary in file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.

Return the value as a string, which leading semicolons and one space
removed.  In the file, the commentary section starts with the tag
`Commentary' or `Documentation' and ends just before the next section.
If the commentary section is absent, return nil."
  (elx-with-file file
    (let ((start (lm-section-start lm-commentary-header t)))
      (when start
	(let ((string (replace-regexp-in-string
		       "\\`[\n\t\s]*" ""
		       (replace-regexp-in-string
			"[\n\t\s]*\\'" ""
			(replace-regexp-in-string
			 "^;+ ?" ""
			 (buffer-substring-no-properties
			  start (lm-commentary-end)))))))
	  (when (string-match "[^\s\t\n]" string)
	    string))))))

;;; Extract License.

(defcustom elx-license-search
  (let ((r "[\s\t\n;]+")
	(c " General Public License as published by the Free Software Foundation.? \\(either \\)?version"))
    `(("GPL-3"    . ,(replace-regexp-in-string " " r (concat "GNU" c " 3")))
      ("GPL-2"    . ,(replace-regexp-in-string " " r (concat "GNU" c " 2")))
      ("GPL-1"    . ,(replace-regexp-in-string " " r (concat "GNU" c " 1")))
      ("LGPL-3"   . ,(replace-regexp-in-string " " r (concat "GNU Lesser" c " 3")))
      ("LGPL-2.1" . ,(replace-regexp-in-string " " r (concat "GNU Lesser" c " 2.1")))
      ("AGPL-3"   . ,(replace-regexp-in-string " " r (concat "GNU Affero" c " 3")))
      ("MIT"           . "^;\\{2,4\\}.* mit license")
      ("as-is"         . "^;\\{2,4\\}.* provided \"as[- ]is")
      ("public-domain" . "^;\\{2,4\\}.* in the public[- ]domain")))
  "List of regexp to common license string mappings.
Used by function `elx-license'.  Each entry has the form
\(LICENSE . REGEXP) where LICENSE is used instead of matches of REGEXP."
  :group 'lib-info
  :type '(repeat (cons (string :tag "use")
		       (regexp :tag "for regexp"))))

(defcustom elx-license-replace
  '(("GPL-3"    .  "gpl-?v?3")
    ("GPL-2"    .  "gpl-?v?2")
    ("GPL-1"    .  "gpl-?v?1")
    ("LGPL-3"   . "lgpl-?v?3")
    ("LGPL-2.1" . "lgpl-?v?2.1")
    ("AGPL-3"   . "agpl-?v?3")
    ("MIT"      . "mit")
    ("as-is"    . "as-?is")
    ("public-domain" . "public[- ]domain"))
  "List of string to common license string mappings.
Used by function `elx-license'.  Each entry has the form
\(LICENSE . REGEXP) where LICENSE is used instead of matches of REGEXP."
  :group 'lib-info
  :type '(repeat (cons (string :tag "use")
		       (regexp :tag "for regexp"))))

(defun elx-license (&optional file)
  "Return the license of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.

The license is extracted from the \"License\" header or if that is missing
by searching the file header for text matching entries in `elx-license-regexps'.

The extracted license string might be modified using `elx-license-mappings'
before it is returned ensuring that each known license is always represented
the same."
  (elx-with-file file
    (let ((license (elx-header "License")))
      (unless license
	(let ((regexps elx-license-search)
	      (case-fold-search t)
	      (elt))
	  (while (and (not license)
		      (setq elt (pop regexps)))
	    (when (re-search-forward (cdr elt) (lm-code-start) t)
	      (setq license (car elt)
		    regexps nil)))))
      (when license
	(let (elt (mappings elx-license-replace))
	  (while (setq elt (pop mappings))
	    (when (string-match (cdr elt) license)
	      (setq license (car elt)
		    mappings nil))))
	(when (string-match "^[-_.a-zA-Z0-9]+$" license)
	  license)))))

;;; Extract Dates.

(defun elx-date--id-header (&optional file)
  (elx-with-file file
    (when (re-search-forward "\\$[I]d: [^ ]+ [^ ]+ \\([^ ]+\\)"
			     (lm-code-mark) t)
      (match-string-no-properties 1))))

(defun elx-date--first-copyright (&optional file)
  (elx-with-file file
    (let ((lm-copyright-prefix "^\\(;+[ \t]\\)+Copyright \\((C) \\)?"))
      (when (lm-copyright-mark)
	(cadr (lm-crack-copyright))))))

(defun elx-date--last-copyright (&optional file)
  (elx-with-file file
    (let ((lm-copyright-prefix "^\\(;+[ \t]\\)+Copyright \\((C) \\)?"))
      (when (lm-copyright-mark)
	(let ((last (car (last (lm-crack-copyright)))))
	  last)))))

(defun elx-date--time-stamp-header (&optional file)
  (let ((value (elx-header "time-stamp")))
    (when (and value
	       (string-match "[\"<]\\([-0-9]+\\)[\s\t].+[\">]" value))
      (match-string 1 value))))

(defun elx-updated (file)
  (elx-with-file file
    (or (dconv-convert-date (elx-header "\\(last-\\)?updated"))
	(dconv-convert-date (elx-header "modified"))
	(dconv-convert-date (elx-header "\\$date"))
	(dconv-convert-date (elx-date--id-header))
	(dconv-convert-date (elx-date--time-stamp-header))
	(dconv-convert-date (elx-date--last-copyright)))))

(defun elx-created (&optional file)
  (elx-with-file file
    (or (dconv-convert-date (lm-creation-date))
	(dconv-convert-date (elx-date--first-copyright)))))

;;; Extract Version.

(defun elx-version--no-colon (&optional file)
  (elx-with-file file
    (when (re-search-forward ";+[\s\t]+Version[\s\t]+\\([\s\t]+\\)"
			     (lm-code-mark) t)
      (match-string-no-properties 1))))

(defun elx-version--id-header (&optional file)
  (elx-with-file file
    (when (re-search-forward "\\$[I]d: [^ ]+ \\([^ ]+\\) "
			     (lm-code-mark) t)
      (match-string-no-properties 1))))

(defun elx-version--revision-header (&optional file)
  (elx-with-file file
    (when (re-search-forward "\\$Revision: +\\([^ ]+\\) "
			     (lm-code-mark) t)
      (match-string-no-properties 1))))

(defun elx-version--variable (file)
  (elx-with-file file
    (when (re-search-forward
	   (concat "(def\\(var\\|const\\) "
		   (file-name-sans-extension
		    (file-name-nondirectory file))
		   "[-:]version \"\\([-_.0-9a-z]+\\)\"")
	   (lm-code-mark) t)
      (match-string-no-properties 2))))

(defun elx-version--do-standardize (version)
  (mapc (lambda (elt)
	  (setq version (replace-regexp-in-string
			 (car elt) (cdr elt) version t t 1)))
	'(("[^_]\\(alpha\\)\\([0-9]+\\)?$" . "_alpha")
	  ("[^_]\\(beta\\)\\([0-9]+\\)?$" . "_beta")
	  ("[^_]\\(pre\\)\\([0-9]+\\)?$" . "_pre")
	  ("[^_]\\(rc\\)\\([0-9]+\\)?$" . "_rc")
	  ("\\(^[vV]\\)\\.?" . "")))
  (elx-version--do-verify version))

(defun elx-version--do-verify (version)
  (if (and version (vcomp-version-p version))
      version
    (dconv-convert-date version)))

(defun elx-version--greater (version old-version)
  (when (and version old-version
	     (vcomp-compare version old-version #'<))
    (error "New version is smaller than old version: %s %s"
	   version old-version))
  (elx-version--do-verify
   (if version
       (if (equal version old-version)
	   (if (string-match "[^a-z][a-z]$" old-version)
	       (concat (substring old-version 0 -1)
		       (char-to-string (1+ (string-to-char
					    (substring old-version -1)))))
	     (concat old-version "a"))
	 version)
     (if old-version
	 (number-to-string (1+ (string-to-number old-version)))
       "0001"))))

(defun elx-version (file &optional standardize)
  "Return the version of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name'.

Return the value of header \"Version\".  If header \"Update\\( #\\)?\" is
also defined append it's value after a period.  If \"Update\\( #\\)?\" is
defined but \"Version\" is not assume 0 for \"Version\".

If optional STANDARDIZE is non-nil verify and possible convert the version
using function `elx-version--do-standardize' (which see).

If the file fails to properly define the version and you absolutely need
something else than nil try function `elx-version+' or even `elx-version>'
and complain to the respective author."
  (elx-with-file file
    (let ((version (or (elx-header "version")
		       (elx-version--no-colon)))
	  (update (elx-header "update\\( #\\)?")))
      (cond ((not version))
	    ((string-match "\\$[I]d: [^ ]+ \\([^ ]+\\) " version)
	     (setq version (match-string-no-properties 1 version)))
	    ((string-match "\\$Revision: +\\([^ ]+\\) "  version)
	     (setq version (match-string-no-properties 1 version)))
	    ((string-match "\\([-_.0-9a-z]+\\)[\s\t].+"  version)
	     (setq version (match-string-no-properties 1 version))))
      (when update
	(setq version (concat (or version "0") "." update)))
      (elx-version--do-verify (if (and version standardize)
				  (elx-version--do-standardize version)
				version)))))

(defun elx-version+ (file &optional standardize)
  "Return _a_ version string for file FILE.
Or the current buffer if FILE is equal to `buffer-file-name'.

If the file properly defines a version extract it using `elx-version'.
Otherwise try several known ways in which people have defined the version
in Emacs Lisp libraries.

If optional STANDARDIZE is non-nil verify and possible convert the version
using function `elx-version--do-standardize' (which see).

If this function returns nil then the author of FILE sucks badly at
writing library headers and if you can absolutely not live with that use
`elx-version>' instead."
  (let ((version (elx-version file standardize)))
    (if version
	version
      (elx-with-file file
	(setq version (or (elx-version--variable file)
			  (elx-version--id-header)
			  (elx-version--revision-header))))
      (elx-version--do-verify
       (if (and version standardize)
	   (elx-version--do-standardize version)
	 version)))))

(defun elx-version> (file old-version &optional standardize)
  "Return _a_ version string for the file FILE.
Or the current buffer if FILE is equal to `buffer-file-name'.

If no version can be found return a pseudo version like \"0001\".

If OLD-VERSION is non-nil the new version has to be greater.  If it is
smaller this is an error.  If it is equal increase it.  E.g. \"0.1\" becomes
\"0.1a\" but if OLD-VERSION appears to be a pseudo version like \"0001\" use
something like \"0002\" instead.

If optional STANDARDIZE is non-nil verify and possible convert the version
using function `elx-version--do-standardize' (which see).

Also see functions `elx-version' and `elx-version+' for less aggressive
approches and more aggressive doc-strings."
  ;; FIXME doc-string might be wrong for some side cases.
  (elx-version--greater (or (elx-version+ file standardize)
			    (elx-updated file))
			old-version))

(defun elx-version-internal (file &optional standardize)
  "Return the version string of the file FILE.
Or the current buffer if FILE is equal to `buffer-file-name'.

Only use this for files that are distributed with GNU Emacs otherwise use
function `elx-version'.

If optional STANDARDIZE is non-nil verify and possibly convert the version
using function `elx-version--do-standardize' (which see).

If the file defines a version extract it using function `elx-version' and
if that fails using function `elx-version--variable'.  If that fails return
the value of variable `emacs-version'."
  (or (elx-version file t)
      (let ((version (elx-version--variable file)))
	(elx-version--do-verify
	 (if (and version standardize)
	     (elx-version--do-standardize version)
	   version)))
      emacs-version))

(defun elx-version-internal> (file old-version &optional standardize)
  (elx-version--greater (elx-version-internal file standardize) old-version))

;;; Extract People.

(defun elx-crack-address (x)
  "Split up an email address X into full name and real email address.
The value is a cons of the form (FULLNAME . ADDRESS)."
  (cond ((string-match "\\(.+\\) [(<]\\(.*\\)[>)]" x)
	 (cons (match-string 1 x)
	       (match-string 2 x)))
	((string-match "\\S-+@\\S-+" x)
	 (cons nil x))
	(t
	 (cons x nil))))

(defun elx-authors (&optional file)
  "Return the author list of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.
Each element of the list is a cons; the car is the full name,
the cdr is an email address."
  (elx-with-file file
    (let ((authorlist (elx-header "authors?" t)))
      (mapcar 'elx-crack-address authorlist))))

(defun elx-maintainer (&optional file)
  "Return the maintainer of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.
The return value has the form (NAME . ADDRESS)."
  (elx-with-file file
    (let ((maint (elx-header "maintainer")))
      (if maint
	  (elx-crack-address maint)
	(car (lm-authors))))))

(defun elx-adapted-by (&optional file)
  "Return the person how adapted file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.
The return value has the form (NAME . ADDRESS)."
  (elx-with-file file
    (let ((adapter (elx-header "adapted-by")))
      (when adapter
	(elx-crack-address adapter)))))

;;; Extract Features.

(defconst elx-provided-regexp "\
\(\\(?:cc-\\)?provide[\s\t\n]'\
\\([^(),\s\t\n]+\\)\\(?:[\s\t\n]+'\
\(\\([^(),]+\\))\\)?)")

(defun elx-buffer-provided (buffer)
  (let (features)
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward elx-provided-regexp nil t)
	  (unless (save-match-data
		    (or (nth 3 (syntax-ppss))   ; in string
			(nth 4 (syntax-ppss)))) ; in comment
	    (dolist (feature (cons (match-string 1)
				   (when (match-string 2)
				     (split-string (match-string 2) " " t))))
	      (add-to-list 'features (intern feature))))))
      (sort features #'string<))))

(defun elx-provided (source)
  "Return a list of the features provided by SOURCE.

SOURCE has to be a file, directory or list of files and/or directories.

If SOURCE is a directory return all features provided by Emacs lisp files
inside SOURCE and recursively all subdirectories.  Files not ending in
\".el\" and directories starting with a period are ignored, except when
explicetly passed to this function.

This will only find features required exactly like:
\(require 'FEATURE [nil|\"PATH\" [nil|t]])."
  (delete-duplicates
   (sort (cond ((listp source)
		(mapcan #'elx-provided source))
	       ((file-directory-p source)
		(mapcan (lambda (elt)
			  (when (or (file-directory-p elt)
				    (string-match "\\.el$" elt))
			    (elx-provided elt)))
			(directory-files source t "^[^\\.]" t)))
	       (t
		(elx-with-file source
		  (elx-buffer-provided (current-buffer)))))
	 #'string<)
   :test #'equal))

(defconst elx-required-regexp "\
\(\\(?:cc-\\)?require[\s\t\n]'\
\\([^(),\s\t\n]+\\)\
\\(?:\\(?:[\s\t\n]+\\(?:nil\\|\".*\"\\)\\)\
\\(?:[\s\t\n]+\\(?:nil\\|\\(t\\)\\)\\)?\\)?)")

(defun elx-buffer-required (buffer &optional provided)
  (let (required-hard
	required-soft)
    (with-current-buffer buffer
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward elx-required-regexp nil t)
	  (let ((feature (intern (match-string 1))))
	    (cond ((save-match-data
		      (or (nth 3 (syntax-ppss))    ; in string
			  (nth 4 (syntax-ppss))))) ; in comment
		  ((match-string 2)
		   (unless (or (member feature required-hard)
			       (member feature required-soft))
		     (push feature required-soft)))
		  ((not (member feature required-hard))
		   (setq required-soft (remove feature required-soft))
		   (push feature required-hard))))))
      (list (sort required-hard #'string<)
	    (sort required-soft #'string<)))))

(defun elx-required (source &optional provided)
  "Return the features required by SOURCE.
The returned value has the form ((HARD-REQUIRED...) (SOFT-REQUIRED...)).

SOURCE has to be a file, directory or list of files and/or directories.

If SOURCE is a directory return all features required by Emacs lisp files
inside SOURCE and recursively all subdirectories.  Files not ending in
\".el\" and directories starting with a period are ignored, except when
explicetly passed to this function.

If optional PROVIDED is a list of features only return required features
that are not members of PROVIDED.  If PROVIDED is t then it is expanded to
the features provided by SOURCE.

This will only find features provided exactly like:
\(provide 'FEATURE '(SUBFEATURE...))."
  (when (eq provided t)
    (setq provided (elx-provided source)))
  (delete-duplicates
   (sort (cond ((listp source)
		(mapcan (lambda (elt)
			  (elx-required elt provided))
			source))
	       ((file-directory-p source)
		(mapcan (lambda (source)
			  (when (or (file-directory-p source)
				    (string-match "\\.el$" source))
			    (elx-required source provided)))
			(directory-files source t "^[^\\.]" t)))
	       (t
		(elx-with-file source
		  (elx-buffer-required (current-buffer) provided))))
	 #'string<)
   :test #'equal))

(provide 'elx)
;;; elx.el ends here
