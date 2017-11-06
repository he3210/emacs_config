;; 下面2行代码会使当前Major-mode中的
;; #+BEGIN_SRC org
;; #+END_SRC
;; 之间的代码使用Org-mode进行高亮
(require 'org)
(setq org-src-fontify-natively t)


;; org-mode agenda
;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/org"))

(provide 'init-org)
