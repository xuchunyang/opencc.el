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

;;

;;; Code:

;; (add-to-list 'load-path
;;              (file-name-directory (or #$ (expand-file-name (buffer-file-name)))))

(require 'opencc-core)

(defun opencc (text &optional config)
  "Convert TEXT according to CONFIG (`s2t' if not passed).

CONFIG is a symbol or a string.
s2t   Simplified Chinese to Traditional Chinese 简体到繁体
t2s   Traditional Chinese to Simplified Chinese 繁体到简体
s2tw  Simplified Chinese to Traditional Chinese (Taiwan Standard) 简体到台湾正体
tw2s  Traditional Chinese (Taiwan Standard) to Simplified Chinese 台湾正体到简体
s2hk  Simplified Chinese to Traditional Chinese (Hong Kong Standard) 简体到香港繁体（香港小学学习字词表标准）
hk2s  Traditional Chinese (Hong Kong Standard) to Simplified Chinese 香港繁体（香港小学学习字词表标准）到简体
s2twp Simplified Chinese to Traditional Chinese (Taiwan Standard) with Taiwanese idiom 简体到繁体（台湾正体标准）并转换为台湾常用词汇
tw2sp Traditional Chinese (Taiwan Standard) to Simplified Chinese with Mainland Chinese idiom 繁体（台湾正体标准）到简体并转换为中国大陆常用词汇"
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
  (let* ((config (format "%s" (or config "s2t")))
         (result (opencc-core text config)))
    (when (called-interactively-p)
      (message "%s" result))
    result))

(provide 'opencc)
;;; opencc.el ends here
