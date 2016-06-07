(require 'opencc)
(require 'ert)

(ert-deftest opencc-sample ()
  (should (string= "開放中文轉換" (opencc-sample))))

(ert-deftest opencc ()
  (should (string= "開放中文轉換" (opencc "开放中文转换")))
  (should (string= "开放中文转换" (opencc "開放中文轉換" 't2s))))
