;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "he.org" user-emacs-directory))    ;; org-babel-load-file函数从org文件加载eamcs lisp代码块（先根据org文件生成el文件，然后加载el文件）
