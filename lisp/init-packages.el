;; 如果版本号大于 24，使用包管理，并且可以添加包的源
(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  )

(require 'cl)
;; add whatever packages you want here

;; 定义一个存放 packages 的列表，根据 window-system 来添加不同的包
(cond ((not window-system)        ;; 终端 emacs 下加载的 packages
       (defvar he/packages '(
                             color-theme-solarized  ;; solarized-theme 终端模式下会出现颜色混乱，使用这个 solarized 配色来解决这个问题。参考：https://emacs-china.org/t/spacemacs-powerline-separator/1008
                             ) "Default packages"))
      ((equal window-system 'ns)  ;; mac GUI emacs 下加载的 packages
       (defvar he/packages '(
                             solarized-theme        ;; GUI 模式下使用这个 solarized 配色。终端下使用该配色方案会出现颜色错乱
                             ) "Default packages")))
;; 继续向列表 he/packages 中添加公共的包
(setq he/packages (append he/packages '(
                                        company
		                                hungry-delete
		                                swiper
		                                counsel
		                                smartparens
		                                js2-mode
		                                nodejs-repl
		                                exec-path-from-shell
		                                popwin
		                                reveal-in-osx-finder
		                                web-mode
		                                expand-region
		                                iedit
		                                org-pomodoro
		                                helm-ag
		                                htmlize
                                        ;; org-bullets
		                                )))
(setq package-selected-packages he/packages)

(defun he/packages-installed-p ()
  (loop for pkg in he/packages
	    when (not (package-installed-p pkg)) do (return nil)
	    finally (return t)))

(unless (he/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg he/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; let emacs could find execuable
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; 使用 hungry-delete，行末去掉空白
(require 'hungry-delete)
(global-hungry-delete-mode)

;; 配置 smartparens
;; (smartparens-global-mode t)
;; (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)    ;; emacs-lisp-mode 下，输入 ' 时，不会自动多输入一个 '
(require 'smartparens-config)  ;; 默认配置

;; 配置 swiper、counsel，即候选列表插件
;; 需要同时安装 swiper、counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

;; config js2-mode for .js files
;; config web-mode for .html
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	     ("\\.html\\'" . web-mode))
       auto-mode-alist))

(global-company-mode t)           ;; 在所有 major-mode 中启用 company-mode

;; config for popwin
;; 新打开某些 buffer 时，光标会跳转到该 buffer。例如打开帮助文档 buffer
(require 'popwin)
(popwin-mode t)

;; 打开代码折叠功能。C-c @ C-c 触发折叠
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)

;; config for web-mode
;; 配置不同语言在 html 中的缩进
(defun my-web-mode-indent-setup()
  (setq web-mode-markup-indent-offset 2)    ;; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)       ;; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)      ;; web-mode, js code in html file
  )
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

;; 缩进 2 个空格和 4 个空格之间进行切换
(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
	    (setq js-indent-level (if (= js-indent-level 2) 4 2))
	    (setq js2-basic-offset (if (= js2-basic-offset 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
	         (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
	         (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))
  (setq indent-tabs-mode nil))

;; config for iedit
(require 'iedit)

;; config for org-pomodoro
(require 'org-pomodoro)

;; org-bullets
;;(require 'org-bullets)
;;(add-hook 'org-mode-hook 'org-bullets-mode)

(provide 'init-packages)
