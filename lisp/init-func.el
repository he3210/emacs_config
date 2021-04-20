;; 打开 emacs 配置文件函数
(defun open-my-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun read-html-template (template-file)
  (with-temp-buffer
    (insert-file-contents (concat *site-template-directory* "/" template-file))
    (buffer-string)))

;; 调整 solarized 配色
(defun reload-solarized-termcolors(color)
  (setq solarized-termcolors color)
  (load-theme 'solarized t)
  )

(defun export-my-notes-internal(is-force)
  ;; 配色问题。需要设置为 256 色。否则，在终端下的 emacs 中执行该函数，导出的代码块颜色混乱
  (if(not window-system)
      (reload-solarized-termcolors 256))

  (save-excursion (org-publish-project "notes" is-force nil)) ;; 导出 notes 到 html。is-force 表示是否强制导出，nil 表示不使用异步

  ;; 导出完毕后，配色再改回来，防止 solarized 在终端中颜色混乱
  (if(not window-system)
      (reload-solarized-termcolors 16))
  )

(defvar *call-export-my-notes-count* 0 "run export-my-notes-internal count")
(defun export-my-notes-test()
  (interactive)
  (if (and (> *call-export-my-notes-count* 0) (equal *site-template-directory* "~/notes/org/templates-test"))
      (progn  ;; 如果连续 2 次导出到测试环境，说明第二次导出时已经时测试环境了，所以就可以使用缓存文件而不必强制重新导出所有文件了
        (export-my-notes-internal nil)
        )
    (progn  ;; 否则，修改环境变量，并强制重新导出所有文件
      (setq *site-template-directory* "~/notes/org/templates-test")
      (setq org-html-head (read-html-template "html-head.html"))
      (setq org-html-preamble (read-html-template "preamble.html"))
      (setq org-html-postamble (read-html-template "postamble.html"))
      (export-my-notes-internal t)
      )
    )
  (cl-incf *call-export-my-notes-count*)  ;; 自增 1
  )

(defun export-my-notes()
  (interactive)
  (if (and (> *call-export-my-notes-count* 0) (equal *site-template-directory* "~/notes/org/templates"))
      (progn  ;; 如果连续 2 次导出到生产环境，说明第二次导出时已经时生产环境了，所以就可以使用缓存文件而不必强制重新导出所有文件了
        (export-my-notes-internal nil)
        )
    (progn  ;; 否则修改环境变量，强制导出
      (setq *site-template-directory* "~/notes/org/templates")
      (setq org-html-head (read-html-template "html-head.html"))
      (setq org-html-preamble (read-html-template "preamble.html"))
      (setq org-html-postamble (read-html-template "postamble.html"))
      (export-my-notes-internal t)
      )
    )
  (cl-incf *call-export-my-notes-count*)
  )

;; bounds 是含有 2 个元素的列表，表示取值范围。例如 (1 1.9)。用 (a b) 表示
;; el 是一个数值
;; 该函数用于判断 el 是否在区间 [a, b] 内
;; car 函数返回列表的第一个元素
;; cadr 函数删除列表的第一个元素，并返回结果列表的第一个元素
(defun in-interval (bounds el)
  (and (>= el (car bounds)) (<= el (cadr bounds))))

(provide 'init-func)
