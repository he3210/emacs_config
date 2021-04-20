;;; kiwix-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kiwix" "kiwix.el" (0 0 0 0))
;;; Generated autoloads from kiwix.el

(autoload 'kiwix-launch-server "kiwix" "\
Launch Kiwix server." t nil)

(autoload 'kiwix-at-point "kiwix" "\
Search for the symbol at point with `kiwix-query'." t nil)

(defvar kiwix-mode nil "\
Non-nil if Kiwix mode is enabled.
See the `kiwix-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `kiwix-mode'.")

(custom-autoload 'kiwix-mode "kiwix" nil)

(autoload 'kiwix-mode "kiwix" "\
Kiwix global minor mode for searching Kiwix serve.

If called interactively, enable Kiwix mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kiwix" '("kiwix-")))

;;;***

;;;### (autoloads nil "org-kiwix" "org-kiwix.el" (0 0 0 0))
;;; Generated autoloads from org-kiwix.el

(autoload 'org-kiwix-setup-link "org-kiwix" "\
Setup Org link for org-kiwix." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-kiwix" '("org-kiwix-")))

;;;***

;;;### (autoloads nil nil ("kiwix-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kiwix-autoloads.el ends here
