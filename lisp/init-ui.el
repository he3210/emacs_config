(tool-bar-mode -1)               ;; 关闭工具栏
(scroll-bar-mode -1)             ;; 关闭scroll-bar
(menu-bar-mode -1)               ;; 关闭菜单栏。terminal 下的 emacs 会有菜单栏
(setq inhibit-splash-screen t)   ;; 关闭启动页面
(setq-default cursor-type 'box)  ;; 光标设为box类型
(set-cursor-color "#e56232")     ;; 设置光标颜色
(setq initial-frame-alist (quote ((fullscreen . maximized))))    ;; 启动emacs时全屏
(global-hl-line-mode t)          ;; 突出显示当前行
(set-face-attribute 'default nil :height 130)   ;; 设置字体大小。130 表示 13pt，单位为 0.1pt


(provide 'init-ui)
