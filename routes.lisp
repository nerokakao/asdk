(in-package :asdk)

(defun init-routes (host)
  ;;route regex uri test
  (push (tbnl:create-regex-dispatcher "^/route/[a-z]$" 'routes) (dispatch-table host))
  ;;js assets folder setting
  (push (tbnl:create-folder-dispatcher-and-handler "/js/"
						   (merge-pathnames "www/js/"
								    (gethash "app-root-path" *conf-k-v*)))
	(dispatch-table host))
  ;;css assets folder setting
  (push (tbnl:create-folder-dispatcher-and-handler "/css/"
						   (merge-pathnames "www/css/"
								    (gethash "app-root-path" *conf-k-v*)))
	(dispatch-table host))
  ;;img assets folder setting
  (push (tbnl:create-folder-dispatcher-and-handler "/img/"
						   (merge-pathnames "www/img/"
								    (gethash "app-root-path" *conf-k-v*)))
	(dispatch-table host))
  ;;login html
  (push (tbnl:create-static-file-dispatcher-and-handler "/login-page"
							(merge-pathnames "www/login.html"
									 (gethash "app-root-path" *conf-k-v*)))
	(dispatch-table host))
  ;;login
  (push (tbnl:create-regex-dispatcher "^/login$" 'login) (dispatch-table host))
  "init ok")

