;;; org-kiwix.el --- Org Mode link support -*- lexical-binding: t; -*-

;;; Time-stamp: <2021-01-14 02:14:33 stardiviner>

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: kiwix wikipedia
;; Homepage: https://github.com/stardiviner/kiwix.el
;; Created: 23th July 2016
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.3"))

;; Copyright (C) 2019-2020  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support Org-mode
;;
;; - [[wikipedia:(library):query]]
;; - [[wikipedia:query]]
;;
;; links:
;; - wikipedia:(zh):%E7%A6%85%E5%AE%97
;; - wikipedia:(en):linux
;; - wikipedia:linux
;;
;; - parameter `link' will be (en):linux" or linux".
;;
;; elisp regexp: "\\(?:(\\(.*\\)):\\)?\\([^] \n\t\r]*\\)"
;; - non capturing group (\(?:...\)) for optional library
;; - group 1: library (en or zh)
;; - group 2: link? (match everything but ], space, tab, carriage return, linefeed by using [^] \n\t\r]*)
;; for open wiki search query with local application database.

;; Usage:
;;
;; (add-hook 'org-load-hook #'org-kiwix-setup-link)

;;; Code:

(require 'kiwix)

(autoload 'org-link-set-parameters "org")
(autoload 'org-store-link-props "org")

(defun org-kiwix--chinese-string-p (string)
  "Return t if STRING is a Chinese string."
  (if (string-match (format "\\cC\\{%s\\}" (length string)) string)
      t
    nil))

(defun org-kiwix-get-library (link)
  "Get library from Org-mode `LINK'."
  (let ((library (catch 'args-out-of-range
                   (when (string-match "(\\([^)].*\\)):\\(.*\\)" link)
                     (match-string 1 link)))))
    (or library
        (cond
         ((org-kiwix--chinese-string-p link)
          (kiwix-select-library "zh"))
         ((string-match-p "[a-zA-Z\ ]+" link)
          ;; convert between libraries full name and abbrev.
          (kiwix-select-library "en"))
         (t (kiwix-select-library))))))

(defun org-kiwix-open-link (link)
  "Open LINK in external Wikipedia program."
  ;; The regexp: (library):query
  ;; - query : should not exclude space
  ;; match link spec: "(library):query" with regexp "([^).]*):?:.*"
  ;; (string-match "\\(?:(\\(.*\\)):\\)?\\([^]\n\t\r]*\\)"  link)
  (string-match "(\\([^)].*\\)):\\(.*\\)" link)
  (let* ((library (org-kiwix-get-library link))
         (query (cond
                 ((org-kiwix--chinese-string-p link) link)
                 ((string-match-p "(\\([^)].*\\)):\\(.*\\)" link)
                  (match-string 2 link))
                 (t link)))
         (url (concat
               kiwix-server-url
               "/" library "/A/"
               ;; query need to be convert to URL encoding: "禅宗" https://zh.wikipedia.org/wiki/%E7%A6%85%E5%AE%97
               (url-encode-url
                ;; convert space to underline: "Beta distribution" "Beta_distribution"
                (replace-regexp-in-string
                 " " "_"
                 ;; only capitalize the first word. like: "meta-circular interpreter" -> "Meta-circular interpreter"
                 (kiwix-capitalize-first query)
                 nil nil))
               ".html")))
    ;; (prin1 (format "library: %s, query: %s, url: %s" library query url))
    (browse-url url)))

(defun org-kiwix-export-link (link description format)
  "Export the Wikipedia LINK with DESCRIPTION for FORMAT from Org files."
  (when (string-match "\\(?:(\\(.*\\)):\\)?\\([^] \n\t\r]*\\)" link)
    (let* ((library (org-kiwix-get-library link))
           (query (url-encode-url (or (match-string 2 link) description)))
           ;; "http://en.wikipedia.org/wiki/Linux"
           ;;         --
           ;;          ^- library: en, zh
           (path (concat "http://" library ".wikipedia.org/wiki/" query))
           (desc (or (match-string 2 link) description)))
      (when (stringp path)
        (cond
         ((eq format 'html) (format "<a href=\"%s\">%s</a>" path desc))
         ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
         (t path))))))

(defun org-kiwix-store-link ()
  "Store a link to a Wikipedia link."
  ;; [C-c o C-l l] `org-store-link'
  ;; remove those interactive functions. use normal function instead.
  (when (eq major-mode 'wiki-mode)
    (let* ((query (read-string "Wikipedia Query with Kiwix: "))
           (library (kiwix-select-library))
           (link (concat "wikipedia:" "(" library "):" query)))
      (org-store-link-props :type "wikipedia"
                            :link link
                            :description query)
      link)))

(defun org-kiwix-complete-link (&optional arg)
  "Use kiwix AJAX request to provide available completion keywords."
  (let* ((query (or arg (read-from-minibuffer "Search keyword: ")))
         (library (kiwix-select-library))
         (keywords (kiwix-ajax-search-hints query library)))
    (concat "wikipedia:"
            "(" library "):"
            (completing-read "Available keywords: " keywords))))

;;;###autoload
(defun org-kiwix-setup-link ()
  "Setup Org link for org-kiwix."
  (org-link-set-parameters "wikipedia" ; NOTE: use `wikipedia' for future backend changing.
                           :follow #'org-kiwix-open-link
                           :store #'org-kiwix-store-link
                           :export #'org-kiwix-export-link
                           :complete #'org-kiwix-complete-link)
  (add-hook 'org-store-link-functions 'org-kiwix-store-link))



(provide 'org-kiwix)

;;; org-kiwix.el ends here
