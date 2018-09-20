;; 如果版本号大于24，使用包管理，并且可以添加包的源
(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  )

(require 'cl)
;; add whatever packages you want here
(defvar he/packages '(
		      company
		      hungry-delete
		      ;; solarized 主题，Mac 下终端启动 emacs 时会出现颜色错乱，解决方案：https://emacs-china.org/t/spacemacs-powerline-separator/1008
		      ;; color-theme-solarized 和 color-theme-sanityinc-solarized 可以解决这个问题
;;		      solarized-theme        ;; 终端下启动 emacs 时，solarized 会出现颜色错乱，使用 color-theme-solarized 替换它
                      color-theme-solarized  ;; 用于替换 solarized-theme
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
;;		      org-bullets
		      ) "Default packages")
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

;; 使用hungry-delete，行末去掉空白
(require 'hungry-delete)
(global-hungry-delete-mode)

;; 配置smartparens
(smartparens-global-mode t)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)    ;; emacs-lisp-mode下，输入'时，不会自动多输入一个'

;; 配置swiper、counsel，即候选列表插件
;; 需要同时安装swiper、counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)

;; config js2-mode for .js files
;; config web-mode for .html
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode)
	 ("\\.html\\'" . web-mode))
       auto-mode-alist))

(global-company-mode t)          ;; 在所有major-mode中启用company-mode

;;;;;; 定制 solarized 主题，该插件使用 color-theme-solarized 替换了
;;;; make the fringe stand out from the background
;;(setq solarized-distinct-fringe-background t)
;;;; Don't change the font for some headings and titles
;;(setq solarized-use-variable-pitch nil)
;;;; make the modeline high contrast
;;(setq solarized-high-contrast-mode-line t)
;;;; Use less bolding
;;(setq solarized-use-less-bold t)
;;;; Use more italics
;;(setq solarized-use-more-italic t)
;;;; Use less colors for indicators such as git:gutter, flycheck and similar
;;(setq solarized-emphasize-indicators t)
;;;; Don't change size of org-mode headlines (but keep other size-changes)
;;(setq solarized-scale-org-headlines nil)
;;;; Avoid all font-size changes
;;(setq solarized-height-minus-1 1)
;;(setq solarized-height-plus-1 2)
;;(setq solarized-height-plus-2 1)
;;(setq solarized-height-plus-3 1)
;;(setq solarized-height-plus-4 1)
;;(load-theme 'solarized-light t)  ;; 使用emacs主题

;; color-theme-solarized 配置
(setq solarized-termcolors 16)     ;; 在终端下打开不能设置为 256，会出现颜色错乱
(setq solarized-degrade nil)       ;; 在 GUI 模式下，会强制 Solarized 使用 256 降级颜色模式来测试近似颜色值的准确性
(setq solarized-bold nil)
(setq solarized-underline nil)
(setq solarized-italic nil)
(setq solarized-contrast "normal") ;; 对比度：normal/low/high
(setq solarized-visibility "normal")  ;; 使用时显示的特殊字符（如尾随空格，制表符，换行符）：normal/low/high
(setq solarized-broken-srgb t)     ;; Emacs bug＃8402 导致Mac上的颜色处理不正确。设置为 t，Solarized 会使用其他颜色来解决它
(load-theme 'solarized t)

;; config for popwin
;; 新打开某些buffer时，光标会跳转到该buffer。例如打开帮助文档buffer
(require 'popwin)
(popwin-mode t)

;; config for web-mode
;; 配置不同语言在html中的缩进
(defun my-web-mode-indent-setup()
  (setq web-mode-markup-indent-offset 2)    ;; web-mode, html tag in html file
  (setq web-mode-css-indent-offset 2)       ;; web-mode, css in html file
  (setq web-mode-code-indent-offset 2)      ;; web-mode, js code in html file
  )
(add-hook 'web-mode-hook 'my-web-mode-indent-setup)

;; 缩进2个空格和4个空格之间进行切换
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
