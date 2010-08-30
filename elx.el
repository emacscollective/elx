;;; elx.el --- extract information from Emacs Lisp libraries

;; Copyright (C) 2008, 2009, 2010  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Created: 20081202
;; Updated: 20100830
;; Version: 0.5-git
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

;; Some of the features implemented here will hopefully be merged into
;; `lisp-mnt.el' once I find the time to create patches.

;;; Code:

(require 'cl)
(require 'dconv)
(require 'vcomp)
(require 'lisp-mnt)
(require 'lgit nil t)

(defgroup elx nil
  "Extract information from Emacs Lisp libraries."
  :group 'maint
  :link '(url-link :tag "Homepage" "https://github.com/tarsius/elx"))

(defmacro elx-with-file (file &rest body)
  "Execute BODY in a buffer containing the contents of FILE.
If FILE is nil or equal to `buffer-file-name' execute BODY in the
current buffer.  Move to beginning of buffer before executing BODY."
  (declare (indent 1) (debug t))
  (let ((filesym (gensym "file")))
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

;; This is almost identical to `lm-header-multiline' and will be merged
;; into that function.
;;
(defun elx-header-multiline (header)
  "Return the contents of the header named HEADER, with continuation lines.
The returned value is a list of strings, one per line."
  (save-excursion
    (goto-char (point-min))
    (let ((res (lm-header header)))
      (when res
	(setq res (list res))
	(forward-line 1)
	(while (and (or (looking-at (concat lm-header-prefix "[\t ]+"))
			(and (not (looking-at
				   (lm-get-header-re
				    "\\sw\\(\\sw\\|\\s_\\|\\s-\\)*")))
			     (looking-at lm-header-prefix)))
		    (goto-char (match-end 0))
		    (looking-at ".+"))
	  (setq res (cons (match-string-no-properties 0) res))
	  (forward-line 1)))
      (nreverse res))))

(defun elx-header (header &optional multiline seperator)
  "Return the contents of the header named HEADER, a string.
Or if MULTILINE and/or SEPERATOR is non-nil return a list of strings,
one per continuation line and/or substring split by SEPERATOR."
  (let ((value (if multiline
		   (elx-header-multiline header)
		 (save-excursion
		   (list (lm-header header))))))
    (when seperator
      (setq value (mapcan (lambda (string)
			    (when string
			      (split-string string seperator t)))
			  value)))
    (if (or seperator multiline)
	value
      (car value))))

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


(defcustom elx-remap-keywords nil
  "List of keywords that should be replaced or dropped by `elx-keywords'.
If function `elx-keywords' is called with a non-nil SANITIZE argument it
checks this variable to determine if keywords should be dropped from the
return value or replaced by another.  If the cdr of an entry is nil then
the keyword is dropped; otherwise it will be replaced with the keyword in
the cadr."
  :group 'elx
  :type '(repeat (list string (choice (const  :tag "drop" nil)
				      (string :tag "replacement")))))

(defun elx-keywords (&optional file sanitize)
  "Return list of keywords given in file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil."
  ;; TODO merge changes from `lisp-maint'.
  (elx-with-file file
    (let ((keywords (elx-header "keywords" t)))
      (when keywords
	(mapcan
	 ;; Filter some nonsense.
	 (lambda (str)
	   (when (string-match "^[-a-z]+$" str)
	     (let ((elt (assoc str elx-remap-keywords)))
	       (if elt
		   (when (cadr elt)
		     (list (cadr elt)))
		 (list str)))))
	 (split-string
	  (replace-regexp-in-string
	   "\\(\t\\|\s\\)+" "\s"
	   (replace-regexp-in-string
	    "," ""
	    (downcase (mapconcat #'identity keywords " "))))
	  " "))))))

(defsubst elx-commentary-start (&optional afterp)
  "Return the buffer location of the `Commentary' start marker.
If optional AFTERP is non-nil return the locations after the
commentary header itself."
  (lm-section-start lm-commentary-header t))

(defsubst elx-commentary-end ()
  "Return the buffer location of the `Commentary' section end.
This even works when no other section follows the commentary section
like when the actual code is not prefixed with the \"Code\" seciton tag."
  (goto-char (elx-commentary-start t))
  (min (lm-section-end lm-commentary-header)
       (1- (or (re-search-forward "^[\s\t]*[^;\n]" nil t) (point-max)))))

(defun elx-commentary (&optional file)
  "Return the commentary in file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.

Return the commentary as a normalized string.  The commentary section
starts with the tag `Commentary' or `Documentation' and ends just before
the next section.  Leading and trailing whitespace is removed from the
returned value but it always ends with exactly one newline. On each line
the leading semicolons and exactly one space are removed, likewise
leading \"\(\" is replaced with just \"(\".  Lines only consisting only of
whitespace are converted to empty lines."
  (elx-with-file file
    (let ((start (elx-commentary-start t)))
      (when start
	(let ((commentary (buffer-substring-no-properties
			   start (elx-commentary-end))))
	  (mapc (lambda (elt)
		  (setq commentary (replace-regexp-in-string
				    (car elt) (cdr elt) commentary)))
		'(("^;+ ?"        . "")
		  ("^\\\\("       . "(")
		  ("^[\n\t\s]\n$" . "\n")
		  ("\\`[\n\t\s]*" . "")
		  ("[\n\t\s]*\\'" . "")))
	  (when (string-match "[^\s\t\n]" commentary)
	    (concat commentary "\n")))))))

;;; Extract Pages.

(defcustom elx-wiki-directory
  (convert-standard-filename "~/.emacs.d/wikipages/")
  "The directory containing the Emacswiki pages.

This variable is used by function `elx-wikipage' when determining the page
on the Emacswiki about a given package.

It's value should be a directory containing all or a subset of pages from
the Emacswiki all at the top-level.  You can create such a directory by
cloning eigher the svn or git repository described at
http://www.emacswiki.org/emacs/SVN_repository and
http://www.emacswiki.org/emacs/Git_repository respectively."
  :group 'elx
  :type 'directory)

(defun elx-wikipage (file &optional name pages urlp)
  "Extract the page on the Emacswiki for the specified package.

The page is extracted from the respective header of FILE which should be
the package's mainfile.  If this fails the package's NAME is matched
against a list of pages known to exist on the Emacswiki.  While either
FILE and NAME may be nil at least one has to be non-nil.

This list can be specified explicitly using the optional PAGES argument
which either has to be a list of filenames or a directory containing the
pages at the toplevel.  If PAGES is nil the contents of
`elx-wiki-directory' are used.

The name of the package can be passed explicitly using the optional NAME
argument otherwise it is derived from FILE.  If NAME is set FILE may be
omitted.

Both the package's name and the pages it is being matched against are
downcased for comparison.  Also a trailing plus sign in the package's name
is replaced with \"plus\" and dashes appearing anywhere in it are removed.

If optional URLP is specified and non-nil return the url of the page
otherwise only the name.

There is no guarantee that this will always return the package's page on
the Emacswiki when such a page exists or that false-positives do not ever
occur."
  (or (when file
	(elx-with-file file
	  (elx-header "\\(?:x-\\)?\\(?:emacs\\)?wiki-?page")))
      (flet ((match (name)
		    (car (member* name pages :test 'equal :key 'downcase))))
	(unless (consp pages)
	  (setq pages (directory-files (or pages elx-wiki-directory)
				       nil "^[^.]" t)))
	(setq name (downcase
		    (replace-regexp-in-string "\\+$" "plus"
		     (replace-regexp-in-string "-" ""
		      (or name
			  (file-name-sans-extension
			   (file-name-nondirectory file)))))))
	(let ((page (or (match name)
			(match (if (string-match "mode$" name)
				   (substring name 0 -4)
				 (concat name "mode"))))))
	  (when page
	    (concat (when urlp "http://www.emacswiki.org/emacs/") page))))))

(defun elx-homepage (&optional file)
  "Extract the homepage of the specified package."
  (elx-with-file file
    (elx-header "\\(?:x-\\)?\\(?:homepage\\|?url\\)")))

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
  "Return the license of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.

The license is extracted from the \"License\" header or if that is missing
by searching the file header for text matching entries in `elx-license-regexps'.

The extracted license string might be modified using `elx-license-mappings'
before it is returned ensuring that each known license is always represented
the same.  If the extracted license does not match \"^[-_.a-zA-Z0-9]+$\"
return nil."
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
  (cdar (member* license elx-license-url :key 'car :test 'equal)))

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
	;; FIXME \/ seams to have problems with ranges like 2009-2010
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

(defun elx-updated (&optional file)
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

(defun elx-version (file &optional standardize)
  "Return the version of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name'.

Return the value of header \"Version\".  If header \"Update\\( #\\)?\" is
also defined append it's value after a period.  If \"Update\\( #\\)?\" is
defined but \"Version\" is not assume 0 for \"Version\".

If optional STANDARDIZE is non-nil verify and possible convert the version
using function `vcomp-normalize' (which see).

If the file fails to properly define the version and you absolutely need
something else than nil try function `elx-version+' or even `elx-version>'
and complain to the respective author."
  (elx-with-file file
    (let ((version (elx-header "version"))
	  (update  (elx-header "update\\( #\\)?")))
      (when update
	(setq version (concat (or version "0") "." update)))
      (if standardize
	  (when (and version (vcomp-version-p version))
	    (vcomp-normalize version))
	version))))

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

(defun elx-crack-address (x &optional sanitize)
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

(defun elx-authors (&optional file sanitize)
  "Return the author list of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.
Each element of the list is a cons; the car is the full name,
the cdr is an email address."
  (elx-with-file file
    (mapcan (lambda (elt)
	      (when elt
		(setq elt (elx-crack-address elt) sanitize)
		(when elt
		  (list elt))))
	    (elx-header "authors?" t ", +"))))

(defun elx-maintainer (&optional file sanitize)
  "Return the maintainer of file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.
The return value has the form (NAME . ADDRESS)."
  (elx-with-file file
    (let ((maint (elx-header "maintainer" nil ", +")))
      (if maint
	  (elx-crack-address (car maint) sanitize)
	(car (elx-authors))))))

(defun elx-adapted-by (&optional file sanitize)
  "Return the person how adapted file FILE.
Or the current buffer if FILE is equal to `buffer-file-name' or is nil.
The return value has the form (NAME . ADDRESS)."
  (elx-with-file file
    (let ((adapter (elx-header "adapted-by" nil ", +")))
      (when adapter
	(elx-crack-address (car adapter) sanitize)))))

;;; Extract Features.

(defvar elx-features-provided nil
  "List of features and the providing packages.

Each element is a cons cell whose car is a feature symbol and whose cdr is
the providing package, a string.  This variable has to be set for function
`elx-required-packages' to work correctly; you are responsible to do that
yourself.")

(defvar elx-features-emacs nil
  "List of features provided by Emacs.")

(defvar elx-features-xemacs nil
  "List of features provided by XEmacs only.
This excludes all features also provided by GNU Emacs.")

(defvar elx-features-drop-deps nil)

(defcustom elx-xemacs-elisp "/unknown/"
  "Directory contain XEmacs' lisp files."
  :group 'elx
  :type 'directory)

(defun elx-update-emacs-features ()
  (message "Updating Emacs features...")
  (setq elx-features-emacs
	(elx-provided (file-name-directory (find-library-name "version"))))
  (message "Updating Emacs features...done"))

(defun elx-update-xemacs-features ()
  (unless elx-features-emacs
    (elx-update-emacs-features))
  (message "Updating XEmacs features...")
  (let ((exclusive '(xemacs
		     ;; FIXME Apparently these are also provided by XEmacs.
		     ;; Verify that this is the case (maybe they were
		     ;; provided by older versions?).
		     atomic-extents auc-menu balloon-help func-menu un-define)))
    (dolist (feature (elx-provided elx-xemacs-elisp))
      (unless (member feature elx-features-emacs)
	(push feature exclusive)))
    (setq elx-features-xemacs (sort exclusive #'string<)))
  (message "Updating XEmacs features...done"))

(defconst elx-provided-regexp "\
\(\\(?:cc-\\|silentcomp-\\)?provide[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\\(?:[\s\t\n]+'\
\(\\([^(),]+\\))\\)?)")

(defun elx--sanitize-provided (provided &optional drop)
  (let (sanitized)
    (dolist (feature provided)
      (unless (or (member feature sanitized)
		  (when drop
		    (member feature elx-features-xemacs)))
	(push feature sanitized)))
    (sort sanitized #'string<)))

(defun elx--buffer-provided (&optional buffer)
  (let (features)
    (with-current-buffer (or buffer (current-buffer))
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
      (elx--sanitize-provided features))))

(defun elx-provided (source &optional drop)
  "Return a list of the features provided by SOURCE.

SOURCE has to be a file, directory or list of files and/or directories.

If SOURCE is a directory return all features provided by Emacs lisp files
inside SOURCE and recursively all subdirectories.  Files not ending in
\".el\" and directories starting with a period are ignored, except when
explicitly passed to this function.

If library `lgit' is loaded SOURCE can also be a cons cell whose car is
the path to a git repository (which may be bare) and whose cdr has to be
an existing revision in that repository.

This function finds provided features using `elx-provided-regexp'."
  (if (or (atom source) (listp (cdr source)))
      (elx--sanitize-provided
       (elx-provided-1 source) drop)
    (elx-provided-git (car source) (cdr source) drop)))

(defun elx-provided-1 (source)
  (cond ((listp source)
	 (mapcan #'elx-provided source))
	((file-symlink-p source)
	 nil)
	((file-regular-p source)
	 (elx-with-file source
	   (elx--buffer-provided)))
	(t
	 (let ((default-directory source))
	   (mapcan #'elx-provided-1 (elx-elisp-files source))))))

(defun elx-provided-git (repo rev &optional drop)
  (elx--sanitize-provided
   (mapcan (lambda (file)
	     (lgit-with-file repo rev file
	       (elx--buffer-provided)))
	   (elx-elisp-files-git repo rev))
   drop))

(defconst elx-required-regexp "\
\(\\(?:cc-\\)?require[\s\t\n]+'\
\\([^(),\s\t\n]+\\)\
\\(?:\\(?:[\s\t\n]+\\(?:nil\\|\".*\"\\)\\)\
\\(?:[\s\t\n]+\\(?:nil\\|\\(t\\)\\)\\)?\\)?)")

(defun elx--sanitize-required-1 (required &optional exclude drop)
  (let (sanitized)
    (dolist (feature required)
      (unless (or (member feature sanitized)
		  (member feature exclude)
		  (when drop
		    (or (member feature elx-features-xemacs)
			(member feature elx-features-drop-deps))))
	(push feature sanitized)))
    (sort sanitized #'string<)))

(defun elx--sanitize-required (required &optional provided drop)
  (let (hard soft)
    (dolist (requ required)
      (setq hard (append (nth 0 requ) hard)
	    soft (append (nth 1 requ) soft)))
    (setq hard (elx--sanitize-required-1 hard provided drop)
	  soft (elx--sanitize-required-1 soft (append provided hard) drop))
    (if soft
	(list hard soft)
      (when hard
	(list hard)))))

(defun elx--lookup-required-1 (feature)
  "Return a string representing the package that provides FEATURE."
  (if (member feature elx-features-emacs)
      "emacs"
    (cdr (assoc feature elx-features-provided))))

(defun elx--lookup-required (required)
  "Return the packages providing all features in list REQUIRED.
The returned value has the form: ((PACKAGE FEATURE...)...)."
  (let (packages)
    (dolist (feature required)
      (let ((package (elx--lookup-required-1 feature))
	    (feature-name (symbol-name feature)))
	(when (and (not package)
		   (string-match "-autoloads?$" feature-name))
	  (setq package (elx--lookup-required-1
			 (intern (substring feature-name 0
					    (match-beginning 0))))))
	(let ((entry (assoc package packages)))
	  (if entry
	      (unless (memq feature (cdr entry))
		(setcdr entry (sort (cons feature (cdr entry))
				    'string<)))
	    (push (list package feature) packages)))))
    (sort* packages
	   (lambda (a b)
	     (cond ((null a) nil)
		   ((null b) t)
		   (t (string< a b))))
	   :key 'car)))

(defun elx--buffer-required (&optional buffer)
  "Return the features required by BUFFER's (or current buffer's) content."
  (let (required-hard
	required-soft)
    (with-current-buffer (or buffer (current-buffer))
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
      (elx--sanitize-required (list (list required-hard required-soft))))))

(defun elx-required (source &optional provided drop)
  "Return the features required by SOURCE.

The returned value has the form:

  ((HARD-REQUIRED...)
   [(SOFT-REQUIRED...)])

Where HARD-REQUIREDs and SOFT-REQUIREDs are symbols.  If no features are
required nil is returned.

SOURCE has to be a file, directory or list of files and/or directories.

If SOURCE is a directory return all features required by Emacs lisp files
inside SOURCE and recursively all subdirectories.  Files not ending in
\".el\" and directories starting with a period are ignored, except when
explicetly passed to this function.

If library `lgit' is loaded SOURCE can also be a cons cell whose car is
the path to a git repository (which may be bare) and whose cdr has to be
an existing revision in that repository.

If optional PROVIDED is provided and non-nil is has to be a list of
features, t or a function.  If it is t call `elx-provided' with SOURCE as
only argument and use the returned list of features.  Members of this list
of features are not included in the return value.

This function finds required features using `elx-required-regexp'."
  (if (or (atom source) (listp (cdr source)))
      (elx--sanitize-required
       (elx-required-1 source) (or provided (elx-provided source) drop))
    (elx-required-git (car source) (cdr source) provided drop)))

(defun elx-required-1 (source)
  (cond ((listp source)
	 (mapcar #'elx-required-1 source))
	((file-symlink-p source)
	 nil)
	((file-regular-p source)
	 (elx-with-file source
	   (elx--buffer-required)))
	(t
	 (let ((default-directory source))
	   (mapcar #'elx-required-1 (elx-elisp-files source t))))))

(defun elx-required-git (repo rev &optional provided drop)
  (elx--sanitize-required
   (mapcar (lambda (file)
	     (lgit-with-file repo rev file
	       (elx--buffer-required)))
	   (elx-elisp-files-git repo rev))
   provided drop))

(defun elx-required-packages (source &optional provided drop)
  "Return the packages required by SOURCE.

The returned value has the form:

  (((HARD-REQUIRED-PACKAGE FEATURE...)...)
   [((SOFT-REQUIRED-PACKAGE FEATURE...)...)])

Where HARD-REQUIRED-PACKAGE and SOFT-REQUIRED-PACKAGE are strings and
FEATURE is a symbol.  If SOURCE has no external dependencies required nil.

SOURCE has to be a file, directory, list of files and/or directories.

If SOURCE is a directory return all packages required by Emacs lisp files
inside SOURCE and recursively all subdirectories.  Files not ending in
\".el\" and directories starting with a period are ignored, except when
explicetly passed to this function.

If library `lgit' is loaded SOURCE can also be a cons cell whose car is
the path to a git repository (which may be bare) and whose cdr has to be
an existing revision in that repository.

The return value does not include features provided by SOURCE.  If
optional PROVIDED is not provided or nil the features provided by SOURCE
are determined using `elx-provided'.  Otherwise PROVIDED has to be a list
of features which are assumed to be the features provided by SOURCE.

Note that this function uses the value of variable `elx-features-provided'
to determine what package provides a feature.  You are responsible to
setup that variable yourself.

This function finds required features using `elx-required-regexp'."
  (let* ((required (elx-required source (or provided
					    (elx-provided source))))
	 (hard (elx--lookup-required (nth 0 required)))
	 (soft (elx--lookup-required (nth 1 required))))
    (if soft
	(list hard soft)
      (when hard
	(list hard)))))

;;; List Emacs Lisp Files.

(defconst elx-elisp-files-suffix "\\.el\\(\\.in\\)?$")
(defconst elx-elisp-files-exclude "\\(\\`\\|/\\)\\.")

(defun elx-elisp-files (source &optional all)
  "Return a list of Emacs lisp files inside directory SOURCE.

The returned paths are relative to SOURCE.  Whether a file is an Emacs
lisp files is determinded by it's suffix.  Files whose basename or any
path component begins with a dot are excluded unless optional ALL is
non-nil.

If library `lgit' is loaded SOURCE can also be a cons cell whose car is
the path to a git repository (which may be bare) and whose cdr has to be
an existing revision in that repository."
  (if (atom source)
      (elx-elisp-files-1 source all)
    (elx-elisp-files-git (car source) (cdr source) all)))

(defun elx-elisp-files-1 (source &optional all)
  (mapcan (lambda (file)
	    (cond ((unless all
		     (string-match elx-elisp-files-exclude file))
		   nil)
		  ((file-directory-p file)
		   (mapcar (lambda (child)
			     (concat (file-name-as-directory file) child))
			   (elx-elisp-files-1 file all)))
		  ((string-match "\\.el$" file)
		   (list file))))
	  (directory-files source)))

(defun elx-elisp-files-git (repo rev &optional all)
  (mapcan (lambda (file)
	    (when (and (string-match elx-elisp-files-suffix file)
		       (or all (not (string-match elx-elisp-files-exclude
						  file))))
	      (list file)))
   (lgit repo "ls-tree -r --name-only %s" rev)))

;;; Extract Package Information.

(defun elx-package-mainfile (source)
  "Return the mainfile of the package inside SOURCE.

The returned path is relative to SOURCE which has to be a directory
containing all libraries belonging to some package.

If the package has only one file matching \"\\\\.el\\\\(\\\\.in\\\\)$\" return that
file unconditionally.  Otherwise return the file which provides the feature
matching the basename of SOURCE, or if no such file exists the file
that provides the feature matching the basename of SOURCE with \"-mode\"
added to or removed from the end, whatever makes sense.  Case is ignored.

If library `lgit' is loaded SOURCE can also be a cons cell whose car is
the path to a git repository (which may be bare) and whose cdr has to be
an existing revision in that repository.

If SOURCE is a cons cell and the mainfile can not be determined as
described above the value of the git variable \"elm.mainfile\" is used."
  (let ((files (elx-elisp-files source))
	(name (regexp-quote
	       (file-name-nondirectory
		(directory-file-name
		 (if (atom source)
		     source
		   (replace-regexp-in-string
		    "\\.git/?$" "" (car source))))))))
    (if (= 1 (length files))
	(car files)
      (flet ((match (feature)
		    (car (member* (format "^\\(.+?/\\)?%s\\.el$"
					  (regexp-quote feature))
				  files :test 'string-match))))
	(or (match name)
	    (match (if (string-match "-mode$" name)
		       (substring name 0 -5)
		     (concat name "-mode")))
	    (when (consp source)
	      (let ((mainfile (cadr (lgit (car source) 1
					  "config elm.mainfile"))))
		(when (and mainfile (member mainfile files))
		  mainfile))))))))

(defmacro elx-with-mainfile (source mainfile &rest body)
  "Execute BODY in a buffer containing the contents of SOURCE's mainfile.

If MAINFILE is non-nil use that as mainfile otherwise determine the
mainfile by applying `elx-package-mainfile' to SOURCE.

SOURCE has to be the mainfile itself (in which case it doesn't make much
sense to specify MAINFILE also) or a directory containing a package
consisting of one or more Emacs Lisp files.  This directory may also
contain auxiliary files.

If library `lgit' is loaded SOURCE can also be a cons cell whose car is
the path to a git repository (which may be bare) and whose cdr has to be
an existing revision in that repository."
  (declare (indent 2) (debug t))
  `(let ((source ,source)
	 (mainfile ,mainfile))
     (unless mainfile
       (setq mainfile
	     (if (and (atom source) (file-regular-p source))
		 source
	       (elx-package-mainfile source))))
     (unless mainfile
       (error "The mainfile can not be determined"))
     (if (consp source)
	 (lgit-with-file (car source) (cdr source) mainfile ,@body)
       (elx-with-file (if (file-name-absolute-p mainfile)
			  mainfile
			(concat source mainfile))
	 ,@body))))

(defun elx-package-features (name repo rev &optional dependencies associate)
  "Process features of the package named NAME.

REPO is the path to the git repository containing the package; it may be
bare.  REV has to be an existing revision in that repository.

If optional DEPENDENCIES is non-nil return a list of the form:

  ((PROVIDED-FEATURE...)
   ((PROVIDING-PACKAGE HARD-REQUIRED-FEATURE...)...)
   ((PROVIDING-PACKAGE HARD-REQUIRED-FEATURE...)...))

Otherwise return nil.

If optional ASSOCIATE is non-nil associate the provided features with the
package in the value of variable `elx-provided-features', if appropriate.

Also see the source comments of this function for more information."
  (let (required provided bundled
	;; Sometimes features provided by a packages repository have to be
	;; excluded from the list of features provided by the package.
	;; This usually is the case when a package bundles libraries that
	;; originate from another package.  Note that this is sometimes
	;; done even for bundled packages are not mirrored themselves (yet).
	;;
	;; This does not result in unresolved dependencies (also see below),
	;; on the opposite the package actually providing the bundled
	;; features is not even added to the list of dependencies so we can
	;; be sure the bundled libraries are loaded which might differ from
	;; those of the package they originate from.
	;;
	;; If no other package depends on the same features (or does also
	;; bundle them) this is a good solution.  However when other
	;; packages also require the packages providing these features
	;; the libraries in the original packages conflict with the
	;; bundled libraries - we simply can't do anything about that and
	;; must hope the file that gets loaded based in it's position in
	;; `load-path' works for all packages that depend on it.
	;;
	;; Features are excluded by setting the git variables "elm.exclude"
	;; (can be specified multiple times, matching features are excluded)
	;; and "elm.exclude-path" (files whose path match are excluded) in
	;; the packages repository.
	(exclude (mapcar #'intern (cdr (lgit repo 1 "config --get-all %s"
					     "elm.exclude"))))
	(exclude-path (cadr (lgit repo 1 "config elm.exclude-path"))))
    (dolist (file (elx-elisp-files-git repo rev))
      (dolist (prov (lgit-with-file repo rev file
		      (elx--buffer-provided)))
	(if (or (member prov exclude)
		(and exclude-path (string-match exclude-path file)))
	    (push prov bundled)
	  (when prov
	    (push prov provided))
	  (when dependencies
	    ;; Even if some of the features provided by this file are
	    ;; excluded do not exclude the required features if at least
	    ;; one of the provided features is not excluded.  We do this
	    ;; because the file might be legitimately belong the the
	    ;; package but might never-the-less illegitimately provide
	    ;; a foreign feature to indicate that is a drop-in replacement
	    ;; or whatever.
	    ;;
	    ;; If multiple features are provided and not excluded then the
	    ;; required features are added multiple times to the list of
	    ;; required features at this point but that is not a problem
	    ;; as duplicates are later removed.
	    ;;
	    ;; Since it is rare that a file provides multiple features, we
	    ;; don't care if we extract the dependencies multiple times.
	    (push (lgit-with-file repo rev file (elx--buffer-required))
		  required)))))
    (setq provided (elx--sanitize-provided provided t))
    (when associate
      ;; If and only if optional argument ASSOCIATE is non-nil add
      ;; associations for the provided features to the value of variable
      ;; `elx-features-provided' unless another package is already
      ;; associated with the feature and the current package does not win
      ;; based on a comparison of it's package name with the feature name.
      ;; In case of conflict and regardless which package wins a warning
      ;; is shown.
      ;;
      ;; The value of `elx-features-provided' is not updated always
      ;; updated when this function is called because is called for all
      ;; versions as well as the tips of all vendor branches and these
      ;; different revisions might differ in what features they provide.
      ;; If the caller of this function could not control whether
      ;; associations are updated or not could seemingly randomly change
      ;; depending on what revision was last processed.
      ;;
      ;; Since Emacs provides no way to specify what version of a package
      ;; another package depends on a particular revision had to be
      ;; choosen whose provided features are recorded to calculate the
      ;; dependencies of other packages.  The latest tagged revision
      ;; of the "main" vendor or if no tagged revision exists it's tip
      ;; has been chosen for this purpose, but this is controlled by the
      ;; callers of this function not itself.
      (dolist (prov provided)
	(let ((elt (assoc prov elx-features-provided)))
	  (if elt
	      (unless (equal (cdr elt) name)
		(elm-log "Feature %s provided by %s and %s"
			 prov (cdr elt) name)
		(when (eq (intern name) prov)
		  (aput 'elx-features-provided prov name)))
	    (aput 'elx-features-provided prov name)))))
    (when dependencies
      ;; This function usually is called to update/create revision epkgs
      ;; and to updated the value of variable `elx-features-provided' by
      ;; side-effect.  However in some cases we only need to do the latter
      ;; so it is possible to skip the step of determine the dependencies.
      ;; In this case we also return nil.
      (setq required (elx--sanitize-required required
						  provided t))
      (let ((hard (elx--lookup-required (nth 0 required)))
	    (soft (elx--lookup-required (nth 1 required))))
	;; If the package providing a particular feature can not be
	;; determined and the providing library also isn't bundled report
	;; a warning here.
	(dolist (dep (cdr (assoc nil hard)))
	  (unless (memq dep bundled)
	    (elm-log "%s: hard required %s not available" name dep)))
	(dolist (dep (cdr (assoc nil soft)))
	  (unless (memq dep bundled)
	    (elm-log "%s: soft required %s not available" name dep)))
	(list provided hard soft)))))

(defun elx-package-metadata (name repo rev &optional branch)
  "Return the metadata of the specified revision of the package named NAME.

REPO is the path to the git repository containing the package; it may be
bare.  REV has to be an existing revision in that repository.

If optional BRANCH is specified it should be the vendor branch containing
REV.  It is only used to get the vendor homepage from the git config if
necessary."
  (let ((features (elx-package-features name repo rev t)))
    (elx-with-mainfile (cons repo rev) nil
      (let ((wikipage (elx-wikipage mainfile name nil t)))
	(list :summary (elx-summary nil t)
	      :created (elx-created)
	      :updated (elx-updated)
	      :license (elx-license)
	      :authors (elx-authors nil t)
	      :maintainer (elx-maintainer nil t)
	      :adapted-by (elx-adapted-by nil t)
	      :provided (car features)
	      :required (unless (equal (cdr features) '(nil nil))
			  (if (equal (cddr features) '(nil))
			      (list (cadr features))
			    (cdr features)))
	      :keywords (elx-keywords mainfile t)
	      :homepage (or (elx-homepage)
			    (cadr (lgit repo 1 "config branch.%s.elm-webpage"
					(or branch rev)))
			    (when (equal (or branch rev) "emacswiki")
			      wikipage))
	      :wikipage wikipage
	      :commentary (elx-commentary mainfile))))))

(provide 'elx)
;;; elx.el ends here
