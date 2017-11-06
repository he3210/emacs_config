(setq ring-bell-function 'ignore);; 关闭响铃
(global-auto-revert-mode t)      ;; 如果文件在外部被修改了，并且emacs没有未保存的内容，emacs就会重新加载文件
(global-linum-mode t)            ;; 在所有major-mode中显示行号
(setq make-backup-files nil)     ;; 不生成备份文件
(setq auto-save-default nil)     ;; 禁止生成自动保存的文件，例如：#filename#
(delete-selection-mode t)        ;; 修改光标选中的文本时，会先删掉选中文本，然后才会编辑
(electric-indent-mode t)         ;; 使用自动对齐


;; abbrev-mode
;; 用法：输入缩写+非ASCII码即可输入全名
(setq-default abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ("8email" "shi_zhonghe@163.com")
					    ))

;; 设置recentf
(recentf-mode 1)                 ;; 退出emacs时会保存一个最近打开的文件列表
(setq recentf-max-menu-items 20) ;; recentf列表最大长度

;; 向emacs-lisp-mode-hook添加show-paren-mode，当启动emacs-lisp时会运行show-paren-mode
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)    ;; 匹配括号



(provide 'init-better-defaults)
