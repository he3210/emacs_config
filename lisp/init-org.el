
(require 'org)
(setq org-src-fontify-natively t)   ;; 对 begin_src 中的代码语法高亮

(setq org-startup-indented t)       ;; 使 org 文档根据大纲进行缩进

;;;; todo  org-agenda  org-capture  org-refile 配置
;; todo 关键字
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "DELAY" "|" "DONE")))

;; todo 关键字配色
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#FF4500" :weight bold))
	("DOING" . (:foreground "#33cc33" :weight bold))
	("DELAY" . (:foreground "black" :weight bold))
        ("DONE" . (:foreground "#27AE60" :weight bold))
	))

;; inbox.org 用于搜集突发临时事件、灵感等，临时记录下来。然后去做其它事情，不至于打断当前工作流程
;; gtd.org   我的工作内容，使用了 datetree 作为大纲。每天早上都会记录当天要做的事情
;; someday   保存我有空闲时间要做的事情。比如：搭建我的博客系统

;; %i 表示 Mark set 选中的内容插入到该位置
;; %^{Description} 表示会提示输入一个 Description，得到 Description 后，它会写入到文档的 %^{Description} 位置
;; %? 模版创建好后，光标会跳转到该位置
;; %U 当前包含日期和时间的未激活的 timestamp，在 org 中使用 [] 括起来，例如：[2018-08-13 Mon 19:12]
;; %T 当前包含日期和时间激活的 timestamp，在 org 中使用 <> 括起来，例如: <2018-03-04 Sun 19:26>
;; 激活(active)和未激活(inactive)的 timestamp 的区别在于，后者不会出现在 org-agenda 中
;; %^g 会提示为该 org node 输入 tag
;; :empty-lines 1  在该位置插入一个空行
(setq org-capture-templates '(
			      ("i" "inbox" entry (file+headline "~/gtd/inbox.org" "inbox")
                               "* TODO [#B] %U %i%?" :empty-lines 1)
                              ("s" "someday" entry (file+headline "~/gtd/someday.org" "some day")
                               "* TODO [#C] %U %i%?" :empty-lines 1)
			      ("g" "GTD" entry (file+datetree "~/gtd/gtd.org")
			       "* TODO [#B] %U %i%?" :empty-lines 1)
			      ))

;; 为 org-refile 函数设置目标文件
(setq org-refile-targets '(
                           ("~/gtd/someday.org" :level . 1)
			   ("~/gtd/gtd.org" :maxlevel . 3)
			   ))

;; Parse a time string D and return a date to pass to the datetree functions
(defun he/org-read-datetree-date (d)
  (let ((dtmp (nthcdr 3 (parse-time-string d))))
    (list (cadr dtmp) (car dtmp) (caddr dtmp))))

;; refile 一个 entry 到 gtd.org 文件
(defun he/org-refile-to-datetree (&optional bfn)
  (interactive)
  (require 'org-datetree)
  (let* ((bfn (or bfn (find-file-noselect (expand-file-name "~/gtd/gtd.org"))))
	 (datetree-date (he/org-read-datetree-date (org-read-date t nil))))
    (org-refile nil nil (list nil (buffer-file-name bfn) nil
			      (with-current-buffer bfn
				(save-excursion
				  (org-datetree-find-date-create datetree-date)
				  (point)))))))

;; org-mode agenda
;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/gtd/inbox.org"
                         "~/gtd/gtd.org"
                         "~/gtd/someday.org"))


;;;; 以下配置为 org 文档的导出配置
(require 'ox-html)
(require 'ox-publish)
(require 'htmlize)

(setq org-export-with-entities t)   ;; 导出时是否进行转义。查看转义字符命令：M-x org-entities-help。例如：将 org 文档中的 \vbar 转义成 html 中的 |

;; HTML模板目录
(defvar *site-template-directory* "~/notes/org/templates")

(defun read-html-template (template-file)
  (with-temp-buffer
    (insert-file-contents (concat *site-template-directory* "/" template-file))
    (buffer-string)))

(setq org-publish-project-alist
      '(
	("org-notes"
         :base-directory "~/notes/org"
         :base-extension "org"
         :publishing-directory "~/notes/html"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
	 :language "zh-CN"              ;; 设置为 zh-CN 会影响一些东西。比如：目录会显示为汉字
	 :section-numbers t             ;; 是否为标题编号
	 :with-toc t                    ;; 是否创建 table of contents
	 :with-latex t                  ;; 是否可以使用 latex
	 :html-doctype "html5"          ;; 导出 html5
	 :with-sub-superscript {}       ;; 禁用 _ 转义成下标，^转义成上标。但加 {} 就可以转义了
;;	 :html-link-home "file:///Users/he/notes/html/index.html"
;;	 :html-link-up "file:///Users/he/notes/html/index.html"
	 :author "时中贺"
	 :email "shi_zhonghe@163.com"
	 :preserve-breaks t             ;; 是否保留换行符。如果设置为 nil，导出后就会多行文本显示在一行
;;	 :creator
	 :html-head-include-default-style nil  ;; 取消默认的 css
	 :html-head-include-scripts nil        ;; 取消默认的 javascript 代码
         ;; css 文件如果修改了，就需要重新加载该 el 文件，这样才能看到 html 样式的变化
;;	 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"file:///Users/he/notes/html/css/style.css\" />"
;;	 :html-head-extra "<script src=\"file:///Users/he/notes/html/css/default.js\" type=\"text/javascript\"></script>"
	 :exclude "test*\\|.*\.test\.org"      ;; test 为前缀的文件和文件夹都不导出 html
	 :include ("./test/math.org" "./test/worg.org" "./test/o-blog.org")          ;; 虽然 math.org 在 test 文件夹里，但依然会导出到 html，显然 include 比 exclude 优先
	 )
        ("static"
         :base-directory "~/notes/org"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc"
         :publishing-directory "~/notes/html"
         :recursive t
         :publishing-function org-publish-attachment)
        ("notes" :components ("org-notes" "static"))
	))

;;; 设置CSS样式
(setq org-html-head (read-html-template "html-head.html"))
;;; 取消默认的CSS
;;(setq org-html-head-include-default-style nil)
;;; 取消默认的Javascript代码
;;(setq org-html-head-include-scripts nil)
;;; XXX 用org-html-head可以设置<head>部分
(setq org-html-preamble (read-html-template "preamble.html"))
(setq org-html-postamble (read-html-template "postamble.html"))

;;; 设置Mathjax库的路径
(add-to-list 'org-html-mathjax-options '(path "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_HTML"))

(provide 'init-org)
