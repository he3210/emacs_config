;; 阅读源码快捷键
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)


;; for swiper and counsel
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-c p f") 'counsel-git)             ;; 打开git版本控制中的一个指定的文件

(global-set-key (kbd "<f3>") 'recentf-open-files)         ;; 列出最近打开的文件列表
(global-set-key (kbd "<f2>") 'open-my-init-file)          ;; f2打开init.el文件
(global-set-key (kbd "C-c a") 'org-agenda)                ;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)  ;; 改进文本对齐快捷键。该快捷键原先是调用indent-region
(global-set-key (kbd "s-/") 'hippie-expand)               ;; 自动补全增强，当company-mode补全不理想时，使用该快捷键进行补全

;; dired mode使用会车进入一个目录时，复用dired buffer，不另外生成一个新的dired buffer
;; 函数with-eval-after-load表示当一个feature或file加载后才会执行后续代码
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))


(provide 'init-keybindings)
