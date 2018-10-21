;; 打开emacs配置文件函数
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun export-my-notes-internal()
  ;; 配色问题。需要设置为 256 色。否则，在终端下的 emacs 中执行该函数，导出的代码块颜色混乱
  (setq solarized-termcolors 256)
  (load-theme 'solarized t)

  ;; 导出 notes 到 html
  (load-file "~/.emacs.d/lisp/init-org.el")  ;; 需要重新加载 init-org.el，否则 css 等文件修改后无法重新发布
  (org-publish-project "notes" t)

  ;; 导出完毕后，配色再改回来，防止 solarized 在终端中颜色混乱
  (setq solarized-termcolors 16)
  (load-theme 'solarized t)
  )

(defun export-my-notes-test()
  (interactive)
  (setq *site-template-directory* "~/notes/org/templates-test")
  (export-my-notes-internal)
  )

(defun export-my-notes()
  (interactive)
  (setq *site-template-directory* "~/notes/org/templates")
  (export-my-notes-internal)
  )

;; bounds 是含有 2 个元素的列表，表示取值范围。例如 (1 1.9)。用 (a b) 表示
;; el 是一个数值
;; 该函数用于判断 el 是否在区间 [a, b] 内
;; car 函数返回列表的第一个元素
;; cadr 函数删除列表的第一个元素，并返回结果列表的第一个元素
(defun in-interval (bounds el)
  (and (>= el (car bounds)) (<= el (cadr bounds))))

(provide 'init-func)
