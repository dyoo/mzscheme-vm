#lang s-exp "../../lang/wescheme.rkt"

;; http://dev.opera.com/articles/view/introduction-html5-video/

(void
 (big-bang 0
	   (stop-when (lambda (w) true))
	   (to-draw-page (lambda (w) 
			   (list (js-elt "video" '(("src" "http://people.opera.com/patrickl/articles/introduction-html5-video/videos/turkish.ogv") ("controls" true))))))))
				 
