(tool-bar-mode -1)               ;; 关闭工具栏
;;(scroll-bar-mode -1)             ;; 关闭 scroll-bar，新版本没有该配置了
(menu-bar-mode -1)               ;; 关闭菜单栏。terminal 下的 emacs 会有菜单栏
(setq inhibit-splash-screen t)   ;; 关闭启动页面
(setq-default cursor-type 'box)  ;; 光标设为 box 类型
(set-cursor-color "#e56232")     ;; 设置光标颜色
(setq initial-frame-alist (quote ((fullscreen . maximized))))    ;; 启动 emacs 时全屏
(global-hl-line-mode t)          ;; 突出显示当前行
(set-face-attribute 'default nil :height 130)   ;; 设置字体大小。130 表示 13pt，单位为 0.1pt。只在 GUI 下有效
(display-time-mode t)            ;; 显示时间
(setq display-time-24hr-format t);; 时间使用 24 小时制
(which-function-mode)            ;; 在模式栏中显示当前光标所在函数
(column-number-mode t)           ;; 状态栏显示光标所在列号(默认不显示)
(line-number-mode t)             ;; 状态栏显示光标所在行号(默认显示)

;; 增强pair高亮。例如：对于代码 (recentf-mode 1) 光标移到括号内部，括号依然高亮
;; define-advice 表示当运行函数 show-paren-function 时，先运行下面的代码
;; cond 类似 C 语言中的 switch，save-excusion 表示保存当前 buffer 和光标位置，事后恢复
(show-paren-mode t)  ;; 当光标在括号上时显示另一半匹配的括号
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))

;; solarized 主题配置
(cond ((not window-system)        ;; 终端 emacs 下加载的 solarized 主题
       (setq solarized-termcolors 16                   ;; 在终端下打开不能设置为 256，会出现颜色错乱
             solarized-degrade nil                     ;; 在 GUI 模式下，会强制 Solarized 使用 256 降级颜色模式来测试近似颜色值的准确性
             solarized-bold nil
             solarized-underline nil
             solarized-italic nil
             solarized-contrast "normal"               ;; 对比度：normal/low/high
             solarized-visibility "normal"             ;; 使用时显示的特殊字符（如尾随空格，制表符，换行符）：normal/low/high
             solarized-broken-srgb t))                 ;; Emacs bug＃8402 导致Mac上的颜色处理不正确。设置为 t，Solarized 会使用其他颜色来解决它
      ((equal window-system 'ns)  ;; GUI emacs 下加载的主题
       (setq solarized-distinct-fringe-background t    ;; make the fringe stand out from the background
             solarized-use-variable-pitch nil          ;; Don't change the font for some headings and titles
             solarized-high-contrast-mode-line t       ;; make the modeline high contrast
             solarized-use-less-bold t                 ;; Use less bolding
             solarized-use-more-italic t               ;; Use more italics
             solarized-emphasize-indicators nil        ;; Use less colors for indicators such as git:gutter, flycheck and similar
             solarized-scale-org-headlines nil         ;; Don't change size of org-mode headlines (but keep other size-changes)
             solarized-height-minus-1 1.0              ;; Avoid all font-size changes
             solarized-height-plus-1 1.0
             solarized-height-plus-2 1.0
             solarized-height-plus-3 1.0
             solarized-height-plus-4 1.0))
      )
(load-theme 'solarized t)

(provide 'init-ui)
