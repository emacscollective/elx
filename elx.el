;;; elx.el --- extract information from Emacs Lisp libraries

;; Copyright (C) 2008-2012  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Version: 0.8.3
;; Homepage: https://github.com/tarsius/elx
;; Keywords: docs, libraries, packages

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

;; This package extracts information from Emacs Lisp libraries.  It
;; extends `lisp-mnt', which is only suitable for libraries that closely
;; follow the header conventions.  Unfortunately there are many libraries
;; that do not - this library tries to cope with that.

;;; Code:

(require 'lisp-mnt)

(defgroup elx nil
  "Extract information from Emacs Lisp libraries."
  :group 'maint
  :link '(url-link :tag "Homepage" "https://github.com/tarsius/elx"))

(defun elx-summary (&optional file sanitize)
  "Return the one-line summary of file FILE.
If optional FILE is nil return the summary of the current buffer
instead.  When optional SANITIZE is non-nil a trailing period is
removed and the first word is upcases."
  (lm-with-file file
    (let ((summary-match
	   (lambda ()
	     (and (looking-at lm-header-prefix)
		  (progn (goto-char (match-end 0))
			 ;; There should be three dashes after the
			 ;; filename but often there are only two or
			 ;; even just one.
			 (looking-at "[^ ]+[ \t]+-+[ \t]+\\(.*\\)"))))))
      (if (or (funcall summary-match)
	      ;; Some people put the -*- specification on a separate
	      ;; line, pushing the summary to the second or third line.
	      (progn (forward-line) (funcall summary-match))
	      (progn (forward-line) (funcall summary-match)))
	  (let ((summary (match-string-no-properties 1)))
	    (unless (equal summary "")
	      ;; Strip off -*- specifications.
	      (when (string-match "[ \t]*-\\*-.*-\\*-" summary)
		(setq summary (substring summary 0 (match-beginning 0))))
	      (when sanitize
		(when (string-match "\\.$" summary)
		  (setq summary (substring summary 0 -1)))
		(when (string-match "^[a-z]" summary)
		  (setq summary
			(concat (upcase (substring summary 0 1))
				(substring summary 1)))))
	      summary))))))

(defcustom elx-remap-keywords nil
  "List of keywords that should be replaced or dropped by `elx-keywords'.
If function `elx-keywords' is called with a non-nil SANITIZE
argument it checks this variable to determine if keywords should
be dropped from the return value or replaced by another.  If the
cdr of an entry is nil then the keyword is dropped; otherwise it
will be replaced with the keyword in the cadr."
  :group 'elx
  :type '(repeat (list string (choice (const  :tag "drop" nil)
				      (string :tag "replacement")))))

(defvar elx-keywords-regexp "^[- a-z]+$")

(defun elx-keywords-list (&optional file)
  "Return list of keywords given in file FILE, or current buffer if FILE is nil."
  (lm-with-file file
    (let (keywords)
      (dolist (line (lm-header-multiline "keywords"))
	(dolist (keyword (split-string
			  (downcase line)
			  (concat "\\("
				  (if (string-match-p "," line)
				      ",[ \t]*"
				    "[ \t]+")
				  "\\|[ \t]+and[ \t]+\\)")
			  t))
	  (setq keyword (intern keyword))
	  (let ((remap (assoc keyword elx-remap-keywords)))
	    (and remap (setq keyword (cadr remap))))
	  (and keyword
	       (string-match elx-keywords-regexp (symbol-name keyword))
	       (add-to-list 'keywords keyword))))
      (sort keywords 'string<))))

(defun elx-commentary (&optional file)
  "Return the commentary in file FILE, or current buffer if FILE is nil.

Return the commentary as a normalized string.  The commentary section
starts with the tag `Commentary' or `Documentation' and ends just before
the next section.  Leading and trailing whitespace is removed from the
returned value but it always ends with exactly one newline. On each line
the leading semicolons and exactly one space are removed, likewise
leading \"\(\" is replaced with just \"(\".  Lines consisting only of
whitespace are converted to empty lines."
  (lm-with-file file
    (let ((start (lm-section-start lm-commentary-header t)))
      (when start
	(goto-char start)
	(let ((commentary (buffer-substring-no-properties
			   start (lm-commentary-end))))
	  (mapc (lambda (elt)
		  (setq commentary (replace-regexp-in-string
				    (car elt) (cdr elt) commentary)))
		'(("^;+ ?"        . "")
		  ("^\\\\("       . "(")
		  ("^\n"        . "")
		  ("^[\n\t\s]\n$" . "\n")
		  ("\\`[\n\t\s]*" . "")
		  ("[\n\t\s]*\\'" . "")))
	  (when (string-match "[^\s\t\n]" commentary)
	    (concat commentary "\n")))))))

(defun elx-wikipage (&optional file)
  "Extract the Emacswiki page of the specified package."
  (let ((page (lm-with-file file
		(lm-header "Doc URL"))))
    (and page
	 (string-match
	  "^<?http://\\(?:www\\.\\)?emacswiki\\.org.*?\\([^/]+\\)>?$"
	  page)
	 (match-string 1 page))))

;;; Extract License.

(defcustom elx-license-search
  (let* ((r "[\s\t\n;]+")
	 (l "^;\\{1,4\\} ")
	 (g (concat " General Public Licen[sc]e"
		    "\\( as published by the Free Software Foundation\\)?.?"))
	 (c (concat g " \\(either \\)?version"))
	 (d "Documentation"))
    `(("GPL-3"      . ,(replace-regexp-in-string " " r (concat "GNU" c " 3")))
      ("GPL-2"      . ,(replace-regexp-in-string " " r (concat "GNU" c " 2")))
      ("GPL-1"      . ,(replace-regexp-in-string " " r (concat "GNU" c " 1")))
      ("GPL"        . ,(replace-regexp-in-string " " r (concat "GNU" g)))
      ("LGPL-3"     . ,(replace-regexp-in-string " " r (concat "GNU Lesser"  c " 3")))
      ("LGPL-2.1"   . ,(replace-regexp-in-string " " r (concat "GNU Lesser"  c " 2.1")))
      ("LGPL-2"     . ,(replace-regexp-in-string " " r (concat "GNU Library" c " 2")))
      ("AGPL-3"     . ,(replace-regexp-in-string " " r (concat "GNU Affero"  c " 3")))
      ("FDL-2.1"    . ,(replace-regexp-in-string " " r (concat "GNU Free " d c " 1.2")))
      ("FDL-1.1"    . ,(replace-regexp-in-string " " r (concat "GNU Free " d c " 1.1")))
      ("EPL-1.1"    . ,(replace-regexp-in-string " " r
			"Erlang Public License,? Version 1.1"))
      ("Apache-2.0" . ,(replace-regexp-in-string " " r
			"Apache License, Version 2.0"))
      ("GPL"        . ,(replace-regexp-in-string " " r (concat
			"Everyone is granted permission to copy, modify and redistribute "
			".*, but only under the conditions described in the "
			"GNU Emacs General Public License.")))
      ("GPL"        . ,(concat l "GPL'ed as under the GNU license"))
      ("GPL"        . ,(concat l "GPL'ed under GNU's public license"))
      ("GPL-2"      . ,(concat l ".* GPL v2 applies."))
      ("GPL-2"      . ,(concat l "The same license/disclaimer for "
				 "XEmacs also applies to this package."))
      ("GPL-3"      . ,(concat l "Licensed under the same terms as Emacs."))
      ("MIT"        . ,(concat l ".* mit license"))
      ("as-is"      . ,(concat l ".* \\(provided\\|distributed\\) "
				 "\\(by the author \\)?"
				 "[\"`']\\{0,2\\}as[- ]is[\"`']\\{0,2\\}"))
      ("public-domain" . ,(concat l ".*in\\(to\\)? the public[- ]domain"))
      ("public-domain" . "^;+ +Public domain.")))
  "List of regexp to common license string mappings.
Used by function `elx-license'.  Each entry has the form
\(LICENSE . REGEXP) where LICENSE is used instead of matches of REGEXP.
Unambitious expressions should come first and those that might produce
false positives last."
  :group 'elx
  :type '(repeat (cons (string :tag "use")
		       (regexp :tag "for regexp"))))

(defcustom elx-license-replace
  '(("GPL-3"      .  "gpl[- ]?v?3")
    ("GPL-2"      .  "gpl[- ]?v?2")
    ("GPL-1"      .  "gpl[- ]?v?1")
    ("GPL"        .  "gpl")
    ("LGPL-3"     . "lgpl[- ]?v?3")
    ("LGPL-2.1"   . "lgpl[- ]?v?2.1")
    ("AGPL-3"     . "agpl[- ]?v?3")
    ("FDL-2.1"    .  "fdl[- ]?v?2.1")
    ("FDL-2.1"    .  "fdl[- ]?v?2.1")
    ("EPL-1.1"    .  "epl[- ]?v?1.1")
    ("EPL-1.1"    .  "erlang-1.1")
    ("Apache-2.0" .  "apache-2.0")
    ("MIT"        .  "mit")
    ("as-is"      .  "as-?is")
    ("public-domain" . "public[- ]domain"))
  "List of string to common license string mappings.
Used by function `elx-license'.  Each entry has the form
\(LICENSE . REGEXP) where LICENSE is used instead of matches of REGEXP."
  :group 'elx
  :type '(repeat (cons (string :tag "use")
		       (regexp :tag "for regexp"))))

(defun elx-license (&optional file)
  "Return the license of file FILE, or current buffer if FILE is nil.

The license is extracted from the \"License\" header or if that is missing
by searching the file header for text matching entries in `elx-license-regexps'.

The extracted license string might be modified using `elx-license-mappings'
before it is returned ensuring that each known license is always represented
the same.  If the extracted license does not match \"^[-_.a-zA-Z0-9]+$\"
return nil."
  (lm-with-file file
    (let ((license (lm-header "License")))
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

(defcustom elx-license-url
  '(("GPL-3"         . "http://www.fsf.org/licensing/licenses/gpl.html")
    ("GPL-2"         . "http://www.gnu.org/licenses/old-licenses/gpl-2.0.html")
    ("GPL-1"         . "http://www.gnu.org/licenses/old-licenses/gpl-1.0.html")
    ("LGPL-3"        . "http://www.fsf.org/licensing/licenses/lgpl.html")
    ("LGPL-2.1"      . "http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html")
    ("LGPL-2.0"      . "http://www.gnu.org/licenses/old-licenses/lgpl-2.0.html")
    ("AGPL-3"        . "http://www.fsf.org/licensing/licenses/agpl.html")
    ("FDL-1.2"       . "http://www.gnu.org/licenses/old-licenses/fdl-1.2.html")
    ("FDL-1.1"       . "http://www.gnu.org/licenses/old-licenses/fdl-1.1.html")
    ("Apache-2.0"    . "http://www.apache.org/licenses/LICENSE-2.0.html")
    ("EPL-1.1"       . "http://www.erlang.org/EPLICENSE")
    ("MIT"           . "http://www.emacsmirror.org/licenses/MIT.html)")
    ("as-is"         . "http://www.emacsmirror.org/licenses/as-is.html)")
    ("public-domain" . "http://www.emacsmirror.org/licenses/public-domain.html)"))
  "List of license to canonical license url mappings.
Each entry has the form (LICENSE . URL) where LICENSE is a license string
and URL the canonical url to the license.  Where no canonical url is known
use a page on the Emacsmirror instead."
  :group 'elx
  :type '(repeat (cons (string :tag "License")
		       (string :tag "URL"))))

(defun elx-license-url (license)
  "Return the canonical url to LICENSE.
The license is looked up in the variable `elx-license-url'.
If no matching entry exists return nil."
  (cdr (assoc license elx-license-url)))

;;; Extract Dates.

(defun elx-created (&optional file)
  (lm-with-file file
    (or (elx--date-1 (lm-creation-date))
	(elx--date-1 (elx--date-copyright)))))

(defun elx-updated (&optional file)
  (lm-with-file file
    (elx--date-1 (lm-header "\\(last-\\)?updated"))))

(defun elx--date-1 (string)
  (when (stringp string)
    (let ((ymd "\
\\([0-9]\\{4,4\\}\\)\\(?:[-/.]?\
\\([0-9]\\{1,2\\}\\)\\(?:[-/.]?\
\\([0-9]\\{1,2\\}\\)?\\)?\\)")
	  (dmy "\
\\(?3:[0-9]\\{1,2\\}\\)\\(?:[-/.]?\\)\
\\(?2:[0-9]\\{1,2\\}\\)\\(?:[-/.]?\\)\
\\(?1:[0-9]\\{4,4\\}\\)"))
      (or (elx--date-2 string ymd t)
	  (elx--date-2 string dmy t)
	  (let ((a (elx--date-3 string))
		(b (or (elx--date-2 string ymd nil)
		       (elx--date-2 string dmy nil))))
	    (cond ((not a) b)
		  ((not b) a)
		  ((> (length a) (length b)) a)
		  ((> (length b) (length a)) b)
		  (t a)))))))
  
(defun elx--date-2 (string regexp anchored)
  (when (string-match (if anchored (format "^%s$" regexp) regexp) string)
    (let ((m  (match-string 2 string))
	  (d  (match-string 3 string)))
      (concat (match-string 1 string)
	      (and m d (concat (if (= (length m) 2) m (concat "0" m))
			       (if (= (length d) 2) d (concat "0" d))))))))

(defun elx--date-3 (string)
  (let ((time (mapcar (lambda (e) (or e 0))
		      (butlast (parse-time-string string)))))
    (unless (= (nth 5 time) 0)
      (format-time-string
       (if (and (> (nth 4 time) 0)
		(> (nth 3 time) 0))
	   "%Y%m%d"
	 ;; (format-time-string "%Y" (encode-time x x x 0 0 2012))
	 ;; => "2011"
	 (setcar (nthcdr 3 time) 1)
	 (setcar (nthcdr 4 time) 1)
	 "%Y")
       (apply 'encode-time time)
       t))))

;; FIXME implement range extraction in lm-crack-copyright
(defun elx--date-copyright ()
  (let ((lm-copyright-prefix "^\\(;+[ \t]\\)+Copyright \\((C) \\)?"))
    (when (lm-copyright-mark)
      (cadr (lm-crack-copyright)))))

;;; Extract Version.

(declare-function vcomp-version-p "vcomp" (string))
(declare-function vcomp-normalize "vcomp" (version))
(declare-function vcomp-prefixed-version-p "vcomp" (string &optional prefix))

(defun elx-version (&optional file raw)
  "Return the version of file FILE.
Or of the current buffer if FILE is equal to `buffer-file-name' or nil.

Return the value of header \"Version\".  If header \"Update\\( #\\)?\" is
also defined append it's value after a period.  If \"Update\\( #\\)?\" is
defined but \"Version\" is not assume 0 for \"Version\".

Unless optional RAW is non-nil verify and possible convert the version
using function `vcomp-normalize' (which see)."
  (require 'vcomp)
  (lm-with-file file
    (let ((version (lm-header "version"))
	  (update  (lm-header "update\\( #\\)?")))
      (when update
	(setq version (concat (or version "0") "." update)))
      (if raw
	  version
	(when (and version
		   (setq version (vcomp-prefixed-version-p version)))
	  (vcomp-normalize version))))))

;;; Extract People.

(defcustom elx-remap-names nil
  "List of names that should be replaced or dropped by `elx-crack-address'.
If function `elx-crack-address' is called with a non-nil SANITIZE argument
it checks this variable to determine if names should be dropped from the
return value or replaced by another.  If the cdr of an entry is nil then
the keyword is dropped; otherwise it will be replaced with the keyword in
the cadr."
  :group 'elx
  :type '(repeat (list string (choice (const  :tag "drop" nil)
				      (string :tag "replacement")))))

(defun elx-crack-address (x)
  "Split up an email address X into full name and real email address.
The value is a cons of the form (FULLNAME . ADDRESS)."
  (let (name mail)
    (cond ((string-match (concat "\\(.+\\) "
				 "?[(<]\\(\\S-+@\\S-+\\)[>)]") x)
	   (setq name (match-string 1 x)
		 mail (match-string 2 x)))
	  ((string-match (concat "\\(.+\\) "
				 "[(<]\\(?:\\(\\S-+\\) "
				 "\\(?:\\*?\\(?:AT\\|[.*]\\)\\*?\\) "
				 "\\(\\S-+\\) "
				 "\\(?:\\*?\\(?:DOT\\|[.*]\\)\\*? \\)?"
				 "\\(\\S-+\\)\\)[>)]") x)
	   (setq name (match-string 1 x)
		 mail (concat (match-string 2 x) "@"
			      (match-string 3 x) "."
			      (match-string 4 x))))
	  ((string-match (concat "\\(.+\\) "
				 "[(<]\\(?:\\(\\S-+\\) "
				 "\\(?:\\*?\\(?:AT\\|[.*]\\)\\*?\\) "
				 "\\(\\S-+\\)[>)]\\)") x)
	   (setq name (match-string 1 x)
		 mail (concat (match-string 2 x) "@"
			      (match-string 3 x))))
	  ((string-match (concat "\\(\\S-+@\\S-+\\) "
				 "[(<]\\(.*\\)[>)]") x)
	   (setq name (match-string 2 x)
		 mail (match-string 1 x)))
	  ((string-match "\\S-+@\\S-+" x)
	   (setq mail x))
	  (t
	   (setq name x)))
    (setq name (and (stringp name)
		    (string-match "^ *\\([^:0-9<@>]+?\\) *$" name)
		    (match-string 1 name)))
    (setq mail (and (stringp mail)
		    (string-match
		     (concat "^\\s-*\\("
			     "[a-z0-9!#$%&'*+/=?^_`{|}~-]+"
			     "\\(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+\\)*@"
			     "\\(?:[a-z0-9]\\(?:[a-z0-9-]*[a-z0-9]\\)?\.\\)+"
			     "[a-z0-9]\\(?:[a-z0-9-]*[a-z0-9]\\)?"
			     "\\)\\s-*$") mail)
		    (downcase (match-string 1 mail))))
    (let ((elt (assoc name elx-remap-names)))
      (when elt
	(setq name (cadr elt))))
    (when (or name mail)
      (cons name mail))))

(defun elx-authors (&optional file)
  "Return the author list of file FILE.
Or of the current buffer if FILE is equal to `buffer-file-name'
or is nil.  Each element of the list is a cons; the car is the
full name, the cdr is an email address."
  (lm-with-file file
    (let (authors)
      (dolist (a (lm-header-multiline "authors?"))
	(when a
	  (setq a (elx-crack-address a))
	  (when a
	    (setq authors (cons a authors)))))
      (nreverse authors))))

(defun elx-maintainer (&optional file)
  "Return the maintainer of file FILE, or current buffer if FILE is nil.
The return value has the form (NAME . ADDRESS)."
  (lm-with-file file
    (let ((maint (lm-header "maintainer")))
      (if maint
	  (elx-crack-address maint)
	(car (elx-authors))))))

(defun elx-adapted-by (&optional file)
  "Return the adapter of file FILE, or current buffer if FILE is nil.
The return value has the form (NAME . ADDRESS)."
  (lm-with-file file
    (let ((adapter (lm-header "adapted-by")))
      (when adapter
	(elx-crack-address adapter)))))

(provide 'elx)
;;; elx.el ends here
