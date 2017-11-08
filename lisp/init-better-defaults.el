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


(defun indent-buffer()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))    ;; point-min表示buffer开始位置，point-max表示buffer末尾位置

(defun indent-region-or-buffer()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  ;; save-excursion 函数保存当前buffer和当前光标位置，执行完其包含的代码段后恢复buffer和光标位置
  ;; region-active-p 如果Transient-Mark Mode打开，并且mark处于激活状态（即处于选中状态下），该函数返回非空
  ;; region-beginning 函数返回选中区域起始位置，region-end函数返回末尾位置
  (save-excursion
    (if(region-active-p)
	(progn
	  (indent-region (region-beginning) (region-end))
	  (message "Indented selected region."))
      (progn
	(indent-buffer)
	(message "Indented buffer.")))))


;; 设置hippie-expand自动补全的尝试补全选项
;; 例如：try-expand-dabbrev代表本buffer所有单词，try-expand-dabbrev-all-buffers代表全部buffer中的所有单词。使用这些单词的作为补全候选项
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))


(fset 'yes-or-no-p 'y-or-n-p)             ;; yes or no 改为 y or n
(setq dired-recursive-deletes 'always)    ;; dired-mode删除一个目录时，递归删除
(setq dired-recursive-copies 'always)     ;; dired-mode拷贝一个目录时，递归拷贝

;; 不生成新的dired buffer
(put 'dired-find-alternate-file 'disabled nil)

;; 加载dired-x后可以使用快捷键 C-x C-j 直接打开当前buffer文件所在目录
(require 'dired-x)

;; dired尝试猜测默认的目录
;; 例如：两个dired buffer窗口，在一个dired窗口按C进行复制文件，复制时给出的默认目标目录为另外一个dired buffer所在的目录
(setq dired-dwim-target t)


(provide 'init-better-defaults)
