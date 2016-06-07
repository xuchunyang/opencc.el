# opencc.el

[OpenCC](https://github.com/BYVoid/OpenCC) 是一个中文简繁转换工具，`opencc.el` 是它的 Emacs 接口。我写 `opencc.el` 的最初目的是为了了解 Emacs 25 新增的 [dynamic modules](http://diobla.info/blog-archive/modules-tut.html) 功能。

## Demo 示例

``` emacs-lisp
;; 简体 -> 繁体
(opencc "简繁转换" 's2t)
    ⇒ "簡繁轉換"

;; 繁体 -> 简体
(opencc "簡繁轉換" 't2s)
    ⇒ "简繁转换"
```

## Interface 接口

### `(opencc-use-api text config)`

依据配置 CONFIG 转化 TEXT，利用 [OpenCC 的 C API](http://byvoid.github.io/OpenCC/1.0.2/group__opencc__c__api.html) 和 Emacs dynamic modules 实现。

*dynamic module 需要 Emacs 25 并需要在 configure 时用 `--with-modules` 开启支持*

### `(opencc-use-cli text config)`

依据配置 CONFIG 转化 TEXT，利用 OpenCC 的命令行工具 `opencc(1)` 实现。

## Command 命令

### `(opencc text &optional config)`

依据配置 CONFIG 转化 TEXT，显示转化结果，如果 CONFIG 未指定或者 <kbd>M-x opencc</kbd> 时没有指定 prefix argument 的话，预设成 `s2t`（简体到繁体）。

转换方式由变量 `opencc-function` 指定。

### `(opencc-replace-in-region start end &optional config)`

在选定区域里依据配置 CONFIG 执行转化，替换为转化结果。如果 CONFIG 未指定或者 <kbd>M-x opencc</kbd> 时没有指定 prefix argument 的话，预设成 `s2t`（简体到繁体）。

## Config 配置

| 配置 | 描述
|-------|--------------------------------------------------------------------------------------------------------------------------------------------|
|`s2t`  | Simplified Chinese to Traditional Chinese 简体到繁体                                                                                       |
|`t2s`  | Traditional Chinese to Simplified Chinese 繁体到简体                                                                                       |
|`s2tw` | Simplified Chinese to Traditional Chinese (Taiwan Standard) 简体到台湾正体                                                                 |
|`tw2s` | Traditional Chinese (Taiwan Standard) to Simplified Chinese 台湾正体到简体                                                                 |
|`s2hk` | Simplified Chinese to Traditional Chinese (Hong Kong Standard) 简体到香港繁体（香港小学学习字词表标准）                                    |
|`hk2s` | Traditional Chinese (Hong Kong Standard) to Simplified Chinese 香港繁体（香港小学学习字词表标准）到简体                                    |
|`s2twp`| Simplified Chinese to Traditional Chinese (Taiwan Standard) with Taiwanese idiom 简体到繁体（台湾正体标准）并转换为台湾常用词汇            |
|`tw2sp`| Traditional Chinese (Taiwan Standard) to Simplified Chinese with Mainland Chinese idiom 繁体（台湾正体标准）到简体并转换为中国大陆常用词汇 |

## Links 相关链接

* OpenCC 项目主页 https://github.com/BYVoid/OpenCC
* OpenCC C API http://byvoid.github.io/OpenCC/1.0.2/group__opencc__c__api.html
* Emacs dynamic modules 的介绍 http://diobla.info/blog-archive/modules-tut.html
