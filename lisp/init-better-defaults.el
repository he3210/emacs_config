(setq ring-bell-function 'ignore);; 关闭响铃
(global-auto-revert-mode t)      ;; 如果文件在外部被修改了，并且emacs没有未保存的内容，emacs就会重新加载文件
(global-linum-mode t)            ;; 在所有major-mode中显示行号
(setq make-backup-files nil)     ;; 不生成备份文件
(setq auto-save-default nil)     ;; 禁止生成自动保存的文件，例如：#filename#
(delete-selection-mode t)        ;; 修改光标选中的文本时，会先删掉选中文本，然后才会编辑
(electric-indent-mode t)         ;; 使用自动对齐
(set-language-environment "UTF-8") ;; 文本编码默认设置为utf-8


;; abbrev-mode
;; 用法：输入缩写+非ASCII码即可输入全名
;;(setq-default abbrev-mode t)
;;(define-abbrev-table 'global-abbrev-table '(
;;					    ("8email" "shi_zhonghe@163.com")
;;					    ))

;; 设置recentf
(recentf-mode 1)                 ;; 退出emacs时会保存一个最近打开的文件列表
(setq recentf-max-menu-items 20) ;; recentf列表最大长度

;; 增强pair高亮。例如：对于代码 (recentf-mode 1) 光标移到括号内部，括号依然高亮
;; define-advice表示当运行函数show-paren-function时，先运行下面的代码
;; cond 类似C语言中的switch，save-excusion 表示保存当前buffer和光标位置，事后恢复
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

(defun hidden-dos-eol()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun remove-dos-eol()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

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


(defun occur-dwim ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
	    (buffer-substring-no-properties
	     (region-beginning)
	     (region-end))
	  (let ((sym (thing-at-point 'symbol)))
	    (when (stringp sym)
	      (regexp-quote sym))))
	regexp-history)
  (call-interactively 'occur))
(global-set-key (kbd "M-s o") 'occur-dwim)

;; imenu
(defun js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
			       ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
			       ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
			       ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))
(add-hook 'js2-mode-hook
	  (lambda ()
	    (setq imenu-create-index-function 'js2-imenu-make-index)))


(provide 'init-better-defaults)
