(tool-bar-mode -1)               ;; 关闭工具栏
(scroll-bar-mode -1)             ;; 关闭scroll-bar
(setq inhibit-splash-screen t)   ;; 关闭启动页面
(setq-default cursor-type 'box)  ;; 光标设为box类型
(setq initial-frame-alist (quote ((fullscreen . maximized))))    ;; 启动emacs时全屏
(global-hl-line-mode t)          ;; 突出显示当前行


(provide 'init-ui)
