(defconst gunnar-mobimeo-packages '(org-jira))

(defun gunnar-mobimeo/post-init-org-jira ()
  (setq jiralib-url "https://jira.mobimeo.com")
  (setq jiralib-token
        '("Cookie" . "JSESSIONID=A1018EB2B746DC0E220B311CB06D3F4F; atlassian.xsrf.token=BHKG-6Y3R-2MU5-S6N1_e52abf974ebc1ca7626bb4ae895e11a99053e10d_lin; slack.inapp.links.first.clicked.gunnar.bastkowski=false; _opensaml_req_ss%3Amem%3A6a38756c83d6d2061a0b606e2ee3f93b1e24305ff4f38747cc08e3ef67f58a73=_4f3aaf6efbbc2f41f503eed1f1e61823; _opensaml_req_ss%3Amem%3A0a423b9315e4bded87238bf103308b6661a011e506a233ab5d54e479e5a5e977=_647014d2a3d2032a4a4529793b271c60; _opensaml_req_ss%3Amem%3Ad483e3a8baaca82b1b8551edd3be5ab92c0116a295ff568b0f2244d75f31b2ad=_da4a996939965f862a3e084bc621a51d; _opensaml_req_ss%3Amem%3A3f5a3d2a930063ca7e4ce457caad1212cf0d3e8ec5a0a4ec13e441bbd94720f0=_b60de4c53b96c2d4bdfa2f3ac3d96093; _opensaml_req_ss%3Amem%3A9d21667706ac4efe9aab4348598c52b859c3adaed040d4731d7296eda490e3aa=_8d1b2adde66070e1373e2024cf1d3891; _opensaml_req_ss%3Amem%3A0146ab8551a745eb9ae9824fad61a41ec0d5697a28643da22852c210d7b55b32=_1713b1e8f6f08917ba09af660d335857; _opensaml_req_ss%3Amem%3A3e8e8fa80aff958dc3f37616c74db6234b7a1575c4fafe9ac887c1088e114f85=_194fe7ee262d6f9a7b6a5336fed3fdf4; _opensaml_req_ss%3Amem%3Ae1758e20a8153f60841947aa481f49399918601d36cac0f3f7575cec3efd622b=_ed29e03141e76ce83aa78109a9d757b8; _opensaml_req_ss%3Amem%3A2a61df1debea45448385aa9ee66a1bc9db4d03ea4ebcf9353ef38b2d2f8ca0a3=_7db865f7d198ff58e481ebbb8e6a2c75; _opensaml_req_ss%3Amem%3A805e9a4de80fd71f1b5af8a714576be61606986e41919be2b0bda44aa5501a11=_84f20c80aa7970e5be8987eb415067ba; _opensaml_req_ss%3Amem%3A015f7a812bec0470fe6d9b79466456926a16a5d98a3ad7de57bf540cdbba2c33=_3ee27764ce13e8c883188647ccf52ee1; _opensaml_req_ss%3Amem%3A28565b674be1eb168c109640df01e179b83433c89bfef29c4df9fd835720ab92=_6bfbc53c880c6a106141ef249992b0a2; _opensaml_req_ss%3Amem%3A769d52efdf2488d034a73ecdf2742d5bfa02b39ce3401dbb46a8c344bc551e24=_2009a70f0140891ae62a25d38634a603; _opensaml_req_ss%3Amem%3Acce2f5fef1219819b15ef781d230ce8334ceeec539f5001d4c6610f519623c85=_d97b8f00750410685fc9997b89c84ae8; _opensaml_req_ss%3Amem%3A7ffac02018e0a4fcc34c16b322743a2039f27a367dd4cb8f9dca7cfd3fe494da=_04075010f3b9b52139485e071a73a992; _opensaml_req_ss%3Amem%3A27ab8dbbf5499b4db0a02465a7e8c3e933b08be6d42be000c0344f15a88090c7=_3752c59098822176fd645c57972c535e; _opensaml_req_ss%3Amem%3Ae0912d929a12834e26b616da43623035f12a491c5bb208ba05ef68b0470195f5=_027557a60a418391b31d800c63d4cc71; _opensaml_req_ss%3Amem%3Abdc5e7a0559d23ebc03c9c780ab6fda6f845830eac9edcd689609734a324164c=_f198caf3267e5f160aecb0a92407f50f; _opensaml_req_ss%3Amem%3A897ce84c5bce74265b4391cfd1163899a8e7dd4ab3d3f38967beb6bf54edced5=_e1064027869c3366433e84e14330f29d; _shibsession_70726f645f6a69726168747470733a2f2f6a6972612e6d6f62696d656f2e636f6d=_84b1e70678a9c98f1e06866acfb8a8dc"))
  ;; (setq jiralib-token
  ;;       (let ((found (nth 0 (auth-source-search :max 1
  ;;                                               :host (url-host (url-generic-parse-url jiralib-url))
  ;;                                               :port 80
  ;;                                               :require '(:user :secret)
  ;;                                               :create nil)))
  ;;             user secret)
  ;;         (when found
  ;;           (setq user (plist-get found :user)
  ;;                 secret
  ;;                 (let ((sec (plist-get found :secret)))
  ;;                   (if (functionp sec)
  ;;                       (funcall sec)
  ;;                     sec)))
  ;;           `("Authorization" . , (format "Basic %s" (base64-encode-string (concat user ":" secret)))))))

  (spacemacs/set-leader-keys "ajpg" 'org-jira-get-projects)
  (spacemacs/set-leader-keys "ajib" 'org-jira-browse-issue)
  (spacemacs/set-leader-keys "ajig" 'org-jira-get-issues)
  (spacemacs/set-leader-keys "ajih" 'org-jira-get-issues-headonly)
  (spacemacs/set-leader-keys "ajiu" 'org-jira-update-issue)
  (spacemacs/set-leader-keys "ajiw" 'org-jira-progress-issue)
  (spacemacs/set-leader-keys "ajin" 'org-jira-progress-issue-next)
  (spacemacs/set-leader-keys "ajia" 'org-jira-assign-issue)
  (spacemacs/set-leader-keys "ajia" 'org-jira-assign-issue)
  (spacemacs/set-leader-keys "ajir" 'org-jira-refresh-issue)
  (spacemacs/set-leader-keys "ajiR" 'org-jira-refresh-issues-in-buffer)
  (spacemacs/set-leader-keys "ajic" 'org-jira-create-issue)
  (spacemacs/set-leader-keys "ajik" 'org-jira-copy-current-issue-key)
  (spacemacs/set-leader-keys "ajsc" 'org-jira-create-subtask)
  (spacemacs/set-leader-keys "ajsg" 'org-jira-get-subtasks)
  (spacemacs/set-leader-keys "ajcu" 'org-jira-update-comment)
  (spacemacs/set-leader-keys "ajwu" 'org-jira-update-worklogs-from-org-clocks)
  (spacemacs/set-leader-keys "ajtj" 'org-jira-todo-to-jira)
  (spacemacs/set-leader-keys "ajif" 'org-jira-get-issues-by-fixversion)
)
