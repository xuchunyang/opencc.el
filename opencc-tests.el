;;; opencc-tests.el --- Tests for opencc.el          -*- lexical-binding: t; -*-

;;; Code:

(require 'opencc)
(require 'ert)

(ert-deftest opencc ()
  (should (string= "開放中文轉換" (opencc "开放中文转换")))
  (should (string= "开放中文转换" (opencc "開放中文轉換" 't2s))))

;;; opencc-tests.el ends here
