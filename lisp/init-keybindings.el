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
(global-set-key (kbd "C-c p f") 'counsel-git)     ;; 打开git版本控制中的一个指定的文件

(global-set-key (kbd "<f3>") 'recentf-open-files)   ;; 列出最近打开的文件列表
(global-set-key (kbd "<f2>") 'open-my-init-file)    ;; f2打开init.el文件
(global-set-key (kbd "C-c a") 'org-agenda)          ;; 设置 org-agenda 打开快捷键


(provide 'init-keybindings)
