(require 'opencc)
(require 'ert)

(ert-deftest opencc-sample ()
  (should (string= "開放中文轉換" (opencc-sample))))
