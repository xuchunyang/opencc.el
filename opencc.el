;;; opencc.el --- OpenCC binding                     -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; Keywords: Chinese

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; OpenCC 是一个简繁转换工具，预设的配置文件有
;;
;; s2t   Simplified Chinese to Traditional Chinese 简体到繁体
;; t2s   Traditional Chinese to Simplified Chinese 繁体到简体
;; s2tw  Simplified Chinese to Traditional Chinese (Taiwan Standard) 简体到台湾正体
;; tw2s  Traditional Chinese (Taiwan Standard) to Simplified Chinese 台湾正体到简体
;; s2hk  Simplified Chinese to Traditional Chinese (Hong Kong Standard) 简体到香港繁体（香港小学学习字词表标准）
;; hk2s  Traditional Chinese (Hong Kong Standard) to Simplified Chinese 香港繁体（香港小学学习字词表标准）到简体
;; s2twp Simplified Chinese to Traditional Chinese (Taiwan Standard) with Taiwanese idiom 简体到繁体（台湾正体标准）并转换为台湾常用词汇
;; tw2sp Traditional Chinese (Taiwan Standard) to Simplified Chinese with Mainland Chinese idiom 繁体（台湾正体标准）到简体并转换为中国大陆常用词汇"
;;
;;
;; 在 `opencc.el' 中「配置文件」由 CONFIG 参数指定，类型为 String 或 Symbol。

;;; Code:

(require 'opencc-core nil t)

;;;###autoload
(defun opencc-use-api (text config)
  "Use the OpenCC C API to convert text with CONFIG (a string)."
  (setq config (if (stringp config)
                   config
                 (symbol-name config)))
  (opencc-core text config))

(defconst opencc-temp-file (make-temp-file "opencc-"))

;;;###autoload
(defun opencc-use-cli (text config)
  "Use opencc(1) to convert TEXT with CONFIG (a string)."
  (setq config (if (stringp config)
                   config
                 (symbol-name config)))
  (with-temp-buffer
    (insert text)
    (write-region nil nil opencc-temp-file nil 0)
    (delete-region 1 (point))
    ;; WARNNING: opencc(1) returns 0 even with invalid config
    (if (zerop (call-process "opencc" nil t nil
                             "--config" config "--input" opencc-temp-file))
        (buffer-string)
      (error "Error: %s" (buffer-string)))))

(defvar opencc-function
  (if (file-exists-p (expand-file-name "opencc-core.so"))
      #'opencc-use-api
    #'opencc-use-cli))

;;;###autoload
(defun opencc (text &optional config)
  "Convert TEXT with CONFIG (`s2t' if not passed or without prefix argument)."
  (interactive
   (let ((text
          (read-string
           (if current-prefix-arg
               "简繁转化: " "简繁转化 (简 -> 繁): ")
           (when (use-region-p)
             (buffer-substring (region-beginning) (region-end)))))
         (config (when current-prefix-arg
                   (completing-read
                    "转化方式: "
                    (list "s2t" "t2s" "s2tw" "tw2s" "s2hk" "hk2s" "s2twp" "tw2sp")))))
     (list text config)))
  (unless config
    (setq config "s2t"))
  (setq config (if (stringp config)
                   config
                 (symbol-name config)))
  (let ((result (funcall opencc-function text config)))
    (when (called-interactively-p)
      (message "%s" result))
    result))

;;;###autoload
(defun opencc-replace-in-region (start end &optional config)
  (interactive
   (let ((config (when current-prefix-arg
                   (completing-read
                    "转化方式: "
                    (list "s2t" "t2s" "s2tw" "tw2s" "s2hk" "hk2s" "s2twp" "tw2sp")))))
     (list (region-beginning) (region-end) config)))
  (unless config (setq config "s2t"))
  (let* ((text (buffer-substring-no-properties start end))
         (converted (opencc text config)))
    (delete-region start end)
    (insert converted)))

(provide 'opencc)
;;; opencc.el ends here
