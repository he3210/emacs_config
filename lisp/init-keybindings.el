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
(global-set-key (kbd "C-c p f") 'counsel-git)             ;; 打开 git 版本控制中的一个指定的文件
(global-set-key (kbd "C-c p g") 'counsel-git-grep)        ;; 在 git 仓库中搜索
(global-set-key (kbd "M-s i") 'counsel-imenu)             ;; 列出当前 buffer 中的所有函数名

(global-set-key (kbd "<f3>") 'recentf-open-files)         ;; 列出最近打开的文件列表
(global-set-key (kbd "<f2>") 'open-my-init-file)          ;; f2 打开 init.el 文件
(global-set-key (kbd "C-c a") 'org-agenda)                ;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c c") 'org-capture)               ;; GTD 模版快捷键
(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)  ;; 改进文本对齐快捷键。该快捷键原先是调用 indent-region
(global-set-key (kbd "s-/") 'hippie-expand)               ;; 自动补全增强，当 company-mode 补全不理想时，使用该快捷键进行补全
(global-set-key (kbd "C-c p s") 'helm-do-ag-project-root) ;; 在当前 git 仓库进行实时搜索，然后输入 pattern。然后按 <C-c C-e> 进入编辑模式，可以编辑搜索结果
(global-set-key (kbd "C-c e n") 'export-my-notes-test)    ;; 导出 notes 笔记本到 html，测试环境
(global-set-key (kbd "C-c e N") 'export-my-notes)         ;; 生产环境
(global-set-key (kbd "C-c g") 'google-translate-smooth-translate)  ;; 选中内容，然后按该快捷键进行 google 翻译。在 mini buffer 中按 C-n 或 C-p 来选择翻译方向

;; dired mode 使用会车进入一个目录时，复用 dired buffer，不另外生成一个新的 dired buffer
;; 函数 with-eval-after-load 表示当一个 feature 或 file 加载后才会执行后续代码
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; 切换 web-mode 下的缩进，2 空格或4 空格
(global-set-key (kbd "C-c t i") 'my-toggle-web-indent)


;; expand-region
;; 按 <C-c f> 扩大选中区域，按 <C-c b> 缩小选中区域
(global-set-key (kbd "C-c f") 'er/expand-region)
(global-set-key (kbd "C-c b") 'er/contract-region)

;; 选中光标下的单词，并同时选中在该 buffer 中的所有该单词
(global-set-key (kbd "C-c i") 'iedit-mode)

;; company-mode 下使用 <C-n> <C-p> 选择补全项（原先快捷键是 <M-n> <M-p>）
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


(provide 'init-keybindings)
