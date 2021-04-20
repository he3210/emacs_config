;;; kiwix.el --- Searching offline Wikipedia through Kiwix.  -*- lexical-binding: t; -*-
;; -*- coding: utf-8 -*-

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: kiwix wikipedia
;; Homepage: https://github.com/stardiviner/kiwix.el
;; Created: 23th July 2016
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.4") (request "0.3.0"))

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

;; This currently only works for GNU/Linux, not tested for Mac OS X and Windows.

;;;; Kiwix installation
;;
;; https://github.com/stardiviner/kiwix.el/#install

;;;; Config:
;;
;; (use-package kiwix
;;   :ensure t
;;   :after org
;;   :bind (:map document-prefix ("w" . kiwix-at-point))
;;   :init (setq kiwix-server-use-docker t
;;               kiwix-server-port 8080
;;               kiwix-default-library "wikipedia_zh_all_2015-11.zim"))

;;;; Usage:
;;
;; 1. [M-x kiwix-launch-server] to launch Kiwix server.
;; 2. [M-x kiwix-at-point] to search the word under point or the region selected string.

;;; Code:


(require 'cl-lib)
(require 'request)
(require 'subr-x)
(require 'thingatpt)
(require 'json)

(declare-function helm "helm")
(declare-function helm-build-async-source "helm")
(declare-function ivy-read "ivy")


(defgroup kiwix nil
  "Kiwix customization options."
  :group 'kiwix)

(defcustom kiwix-server-use-docker nil
  "Using Docker container for kiwix-serve or not?"
  :type 'boolean
  :safe #'booleanp)

(defcustom kiwix-server-port 8000
  "Specify default kiwix-serve server port."
  :type 'number
  :safe #'numberp)

(defcustom kiwix-server-url "http://127.0.0.1"
  "Specify Kiwix server URL."
  :type 'string)

(defcustom kiwix-server-command
  (cond
   ((file-executable-p "/usr/bin/kiwix-serve") "/usr/bin/kiwix-serve")
   ((eq system-type 'gnu/linux) "/usr/lib/kiwix/bin/kiwix-serve")
   ((eq system-type 'darwin)
    (warn "You need to specify Mac OS X Kiwix path. And send a PR to my repo."))
   ((eq system-type 'windows-nt)
    (warn "You need to specify Windows Kiwix path. And send a PR to my repo.")))
  "Specify kiwix server command."
  :type 'string)

(defun kiwix-dir-detect ()
  "Detect Kiwix profile directory exist."
  (let ((kiwix-dir "~/.www.kiwix.org/kiwix"))
    (if (and (file-directory-p kiwix-dir) (file-readable-p kiwix-dir))
        kiwix-dir
      (warn "ERROR: Kiwix profile directory \"~/.www.kiwix.org/kiwix\" is not accessible.")
      nil)))

(defcustom kiwix-default-data-profile-name
  (when (kiwix-dir-detect)
    (car (directory-files "~/.www.kiwix.org/kiwix" nil ".*\\.default\\'")))
  "Specify the default Kiwix data profile path."
  :type 'string)

(defcustom kiwix-default-data-dir
  (when (kiwix-dir-detect)
    (concat "~/.www.kiwix.org/kiwix/" kiwix-default-data-profile-name))
  "Specify the default Kiwix data directory."
  :type 'string
  :safe #'stringp)

(defcustom kiwix-default-library-dir
  (file-name-directory (concat kiwix-default-data-dir "/data/library/library.xml"))
  "Kiwix libraries path."
  :type 'string
  :safe #'stringp)

(defcustom kiwix-default-completing-read (cond
                                          ((fboundp 'selectrum-read) 'selectrum)
                                          ((fboundp 'ivy-read) 'ivy)
                                          ((fboundp 'helm) 'helm)
                                          (t t))
  "Kiwix default completion frontend.
Currently `selectrum', Ivy (`ivy') and Helm (`helm') all supported.
Set it to ‘t’ will use Emacs built-in ‘completing-read’."
  :type 'symbol
  :safe #'symbolp)

(defcustom kiwix-default-browser-function browse-url-browser-function
  "Set default browser for open kiwix query result URL."
  :type '(choice
          (const :tag "browse-url default function" browse-url-default-browser)
          (const :tag "EWW" eww-browse-url)
          (const :tag "EAF web browser" eaf-open-browser)
          (const :tag "Firefox web browser" browse-url-firefox)
          (const :tag "Google Chrome web browser" browse-url-chrome)
          (const :tag "Conkeror web browser" browse-url-conkeror)
          (const :tag "xwidget browser" xwidget-webkit-browse-url))
  :safe #'symbolp)

(defun kiwix--get-library-name (file)
  "Extract library name from library file."
  (replace-regexp-in-string "\\.zim\\'" "" file))

(defun kiwix-get-libraries ()
  "Check out all available Kiwix libraries."
  (when (kiwix-dir-detect)
    (mapcar #'kiwix--get-library-name
            (directory-files kiwix-default-library-dir nil ".*\\.zim\\'"))))

(defvar kiwix-libraries (kiwix-get-libraries)
  "A list of Kiwix libraries.")

(defun kiwix-libraries-refresh ()
  "A helper function to refresh available Kiwx libraries."
  (setq kiwix-libraries (kiwix-get-libraries)))

(defvar kiwix--selected-library nil
  "Global variable of currently select library used in anonymous function.
Like in function `kiwix-ajax-search-hints'.")

;; - examples:
;; - "wikipedia_en_all" - "wikipedia_en_all_2016-02"
;; - "wikipedia_zh_all" - "wikipedia_zh_all_2015-17"
;; - "wiktionary_en_all" - "wiktionary_en_all_2015-17"
;; - "wiktionary_zh_all" - "wiktionary_zh_all_2015-17"
;; - "wikipedia_en_medicine" - "wikipedia_en_medicine_2015-17"

(defun kiwix-select-library (&optional filter)
  "Select Kiwix library name."
  (kiwix-libraries-refresh)
  (completing-read "Kiwix library: " kiwix-libraries nil t filter))

(defcustom kiwix-default-library "wikipedia_en_all.zim"
  "The default kiwix library when library fragment in link not specified."
  :type 'string
  :safe #'stringp)

(defcustom kiwix-mode-prefix nil
  "Specify kiwix-mode keybinding prefix before loading."
  :type 'kbd)


;; launch Kiwix server
;;;###autoload
(defun kiwix-launch-server ()
  "Launch Kiwix server."
  (interactive)
  (let ((library-path kiwix-default-library-dir))
    (if kiwix-server-use-docker
        (start-process
         "kiwix-server"
         " *kiwix server*"
         "docker"
         "container" "run" "-d"
         "--name" "kiwix-serve"
         "-v" (concat (file-name-directory library-path) ":" "/data")
         "kiwix/kiwix-serve"
         "--library" "library.xml")
      (start-process
       "kiwix-server"
       " *kiwix server*"
       kiwix-server-command
       "--port" (number-to-string kiwix-server-port)
       "--daemon"
       "--library" (concat library-path "library.xml")))))

(defun kiwix-capitalize-first (string)
  "Only capitalize the first word of STRING."
  (concat (string (upcase (aref string 0))) (substring string 1)))

(defun kiwix-query (query &optional selected-library)
  "Search `QUERY' in `LIBRARY' with Kiwix."
  (let* ((library (or selected-library (kiwix--get-library-name kiwix-default-library)))
         (url (concat (format "%s:%s" kiwix-server-url (number-to-string kiwix-server-port))
                      "/search?content=" library "&pattern=" (url-hexify-string query)))
         (browse-url-browser-function kiwix-default-browser-function))
    (browse-url url)))

(defun kiwix-docker-check ()
  "Make sure Docker image 'kiwix/kiwix-server' is available."
  (let ((docker-image (replace-regexp-in-string
                       "\n" ""
                       (shell-command-to-string
                        "docker image ls kiwix/kiwix-serve | sed -n '2p' | cut -d ' ' -f 1"))))
    (string-equal docker-image "kiwix/kiwix-serve")))

(defvar kiwix-server-available? nil
  "The kiwix-server current available?")

(defun kiwix-ping-server ()
  "Ping Kiwix server to set `kiwix-server-available?' global state variable."
  (and kiwix-server-use-docker
       (or (kiwix-docker-check)
           (async-shell-command "docker pull kiwix/kiwix-serve")))
  (let ((inhibit-message t))
    (request (format "%s:%s" kiwix-server-url (number-to-string kiwix-server-port))
      :type "GET"
      :sync t
      :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
      :error (cl-function
              (lambda (&rest args &key error-thrown &allow-other-keys)
                (setq kiwix-server-available? nil)
                (when (string-equal (cdr error-thrown) "exited abnormally with code 7\n")
                  (warn "kiwix.el failed to connect to host. exited abnormally with status code: 7."))))
      :success (cl-function
                (lambda (&key _data &allow-other-keys)
                  (setq kiwix-server-available? t)))
      :status-code '((404 . (lambda (&rest _) (message (format "Endpoint %s does not exist." url))))
                     (500 . (lambda (&rest _) (message (format "Error from  %s." url))))))))

(defun kiwix-ajax-search-hints (input &optional selected-library)
  "Instantly AJAX request to get available Kiwix entry keywords
list and return a list result."
  (kiwix-ping-server)
  (when (and input kiwix-server-available?)
    (let* ((library (or selected-library
                        (kiwix--get-library-name (or kiwix--selected-library
                                                     kiwix-default-library))))
           (ajax-api (format "%s:%s/suggest?content=%s&term="
                             kiwix-server-url (number-to-string kiwix-server-port)
                             library))
           (ajax-url (concat ajax-api input))
           (data (request-response-data
                  (let ((inhibit-message t))
                    (request ajax-url
                      :type "GET"
                      :sync t
                      :headers '(("Content-Type" . "application/json"))
                      :parser #'json-read
                      :success (cl-function
                                (lambda (&key data &allow-other-keys)
                                  data)))))))
      (if (vectorp data) (mapcar #'cdar data)))))

(defun kiwix--get-thing-at-point ()
  "Get region select text or symbol at point."
  (if (use-region-p)
      (buffer-substring
       (region-beginning) (region-end))
    (thing-at-point 'symbol)))

;;;###autoload
(defun kiwix-at-point ()
  "Search for the symbol at point with `kiwix-query'."
  (interactive)
  (unless (kiwix-ping-server)
    (kiwix-launch-server))
  (if kiwix-server-available?
      (progn
        (setq kiwix--selected-library (kiwix-select-library))
        (let* ((library kiwix--selected-library)
               (query (pcase kiwix-default-completing-read
                        ('selectrum
                         (require 'selectrum)
                         (selectrum-read
                          "Kiwix related entries: "
                          (lambda (input)
                            (apply #'kiwix-ajax-search-hints
                                   input `(,kiwix--selected-library)))
                          :require-match nil))
                        ('ivy
                         (require 'ivy)
                         (ivy-read
                          "Kiwix related entries: "
                          (lambda (input)
                            (apply #'kiwix-ajax-search-hints
                                   input `(,kiwix--selected-library)))
                          :predicate nil
                          :require-match nil
                          :initial-input (kiwix--get-thing-at-point)
                          :preselect nil
                          :def nil
                          :history nil
                          :keymap nil
                          :update-fn 'auto
                          :sort t
                          :dynamic-collection t
                          :caller 'ivy-done))
                        ('helm
                         (require 'helm)
                         (helm
                          :source (helm-build-async-source "kiwix-helm-search-hints"
                                    :candidates-process
                                    (lambda (input)
                                      (apply #'kiwix-ajax-search-hints
                                             input `(,kiwix--selected-library))))
                          :input (kiwix--get-thing-at-point)
                          :buffer "*helm kiwix completion candidates*"))
                        (_
                         (completing-read
                          "Kiwix related entries: "
                          ;; FIXME: This needs work!
                          (completion-table-dynamic
                           (lambda (input)
                             (apply #'kiwix-ajax-search-hints
                                    input `(,kiwix--selected-library))))
                          nil nil
                          (kiwix--get-thing-at-point))))))
          (message (format "library: %s, query: %s" library query))
          (if (or (null library)
                  (string-empty-p library)
                  (null query)
                  (string-empty-p query))
              (error "Your query is invalid")
            (kiwix-query query library))))
    (warn "kiwix-serve is not available, please start it at first."))
  (setq kiwix-server-available? nil))

;;===============================================================================

(defun kiwix-mode-enable ()
  "Enable kiwix-mode."
  )

(defun kiwix-mode-disable ()
  "Disable kiwix-mode."
  )

(defvar kiwix-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "kiwix-mode map.")

;;;###autoload
(define-minor-mode kiwix-mode
  "Kiwix global minor mode for searching Kiwix serve."
  :global t
  :lighter " Kiwix"
  (if kiwix-mode (kiwix-mode-enable) (kiwix-mode-disable)))


(provide 'kiwix)

;;; kiwix.el ends here
