
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'init-func)
(require 'init-packages)
(require 'init-ui)
(require 'init-better-defaults)
(require 'init-org)
(require 'init-keybindings)


;; 把定制化的一些配置写到单独的一个el文件中
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(load-file custom-file)
