Press C-x 1 again to restore windows!

Install:
Test it out with M-x zygospore-toggle-delete-other-windows RET (twice, on a frame with multiple windows)

To make it permanent, add this line to your init.el:
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
