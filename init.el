;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'init-packages)

(tool-bar-mode -1)               ;; 关闭工具栏
(scroll-bar-mode -1)             ;; 关闭scroll-bar
(electric-indent-mode t)         ;; 使用自动对齐
(global-linum-mode t)            ;; 在所有major-mode中显示行号
(setq inhibit-splash-screen t)   ;; 关闭启动页面
(setq ring-bell-function 'ignore);; 关闭响铃
(setq make-backup-files nil)     ;; 不生成备份文件
(setq-default cursor-type 'box)  ;; 光标设为box类型
(delete-selection-mode t)        ;; 修改光标选中的文本时，会先删掉选中文本，然后才会编辑
(global-hl-line-mode t)          ;; 突出显示当前行
(global-auto-revert-mode t)      ;; 如果文件在外部被修改了，并且emacs没有未保存的内容，emacs就会重新加载文件
(setq auto-save-default nil)     ;; 禁止生成自动保存的文件，例如：#filename#

;; abbrev-mode
;; 用法：输入缩写+非ASCII码即可输入全名
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ("8email" "shi_zhonghe@163.com")
					    ))


;; org-mode agenda
;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/org"))
;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)

;; 阅读源码快捷键
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)






(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)




(setq initial-frame-alist (quote ((fullscreen . maximized))))    ;; 启动emacs时全屏

;; 向emacs-lisp-mode-hook添加show-paren-mode，当启动emacs-lisp时会运行show-paren-mode
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)    ;; 匹配括号


;; 设置recentf
(require 'recentf)               ;; 引入requiref文件
(recentf-mode 1)                 ;; 退出emacs时会保存一个最近打开的文件列表
(setq recentf-max-menu-items 20) ;; recentf列表最大长度
(global-set-key (kbd "<f3>") 'recentf-open-files)   ;; 列出最近打开的文件列表

;; 下面2行代码会使当前Major-mode中的
;; #+BEGIN_SRC org
;; #+END_SRC
;; 之间的代码使用Org-mode进行高亮
(require 'org)
(setq org-src-fontify-natively t)

;; emacs配置文件快捷键
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-my-init-file)    ;; 使用快捷键调用函数


;;;; 以下是通过M-x customize-group 定制的配置
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-external-variable ((t (:foreground "dark gray")))))
