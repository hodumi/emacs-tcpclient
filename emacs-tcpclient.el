;;; emacs-tcpclient.el --- Generic TCP client with binary support and pluggable reply handler

;; Copyright (C) 2026  

;; Author: Yusuke KUDO <ajsmithy00@gmail.com>
;; Keywords: tools
;; URL: https://github.com/hodumi/elauncher
;; Version: 0.1

;;; Commentary:

;; EmacsからTCPサーバへ接続するプログラムです。


;;; Code:




(require 'cl-lib)

;;----------------------------------------------------------
;; Utility: Logging
;;----------------------------------------------------------

;;;###autoload
(defun tcp-client--log (buffer fmt &rest args)
  "BUFFER にタイムスタンプ付きでログを追記する。"
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert (format "[%s] " (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert (apply #'format fmt args))
        (insert "\n")))))

;;;###autoload
(defun tcp-client--hex-string (str)
  "STR（raw bytes）を 16 進表現の文字列にして返す。"
  (let ((i 0)
        (len (length str))
        (res ""))
    (while (< i len)
      (setq res (concat res (format "%02X " (aref str i))))
      (setq i (1+ i)))
    (string-trim-right res)))

(defun tcp-client--make-log-buffer-name (host port)
  (format "*tcp-client-%s:%s*" host port))


;;----------------------------------------------------------
;; Main: Connect
;;----------------------------------------------------------

;;;###autoload
(defun tcp-client-connect (host port &optional name reply-handler)
  "HOST:PORT に TCP 接続してプロセスを返す。
REPLY-HANDLER は (lambda (proc raw-data decoded-text) ...) の形で返信データを返す関数。"
  (let* ((bufname (tcp-client--make-log-buffer-name host port))
         (logbuf (get-buffer-create bufname))
         (proc-name (or name (format "tcp-client-%s:%s" host port)))
         (proc (open-network-stream proc-name logbuf host port
                                    :type 'plain :coding 'no-conversion)))

    ;; 初期化ログ
    (with-current-buffer logbuf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "TCP client log for %s:%s\n" host port))
        (insert (format "Process name: %s\n\n" proc-name))
        (setq buffer-read-only t)))

    ;; プロセスプロパティ
    (process-put proc 'tcp-client-log-buffer logbuf)
    (process-put proc 'tcp-client-reply-handler reply-handler)

    (set-process-query-on-exit-flag proc nil)
    (set-process-filter proc #'tcp-client--process-filter)
    (set-process-sentinel proc #'tcp-client--process-sentinel)

    (tcp-client--log logbuf "Connected to %s:%s (process %s)" host port proc-name)
    proc))


;;----------------------------------------------------------
;; Process filter / sentinel
;;----------------------------------------------------------
;;;###autoload
(defun tcp-client--process-filter (proc data)
  "受信データ DATA をログに記録し、必要なら返信する。"
  (let* ((logbuf (process-get proc 'tcp-client-log-buffer))
         (hex (tcp-client--hex-string data))
         decoded reply)

    ;; cp932 デコード
    (condition-case nil
        (setq decoded (decode-coding-string data 'cp932))
      (error (setq decoded nil)))

    ;; ログ
    (when logbuf
      (tcp-client--log logbuf "RECV (hex): %s" hex)
      (tcp-client--log logbuf "RECV (cp932): %s"
                       (or decoded "<decode error>")))

    ;; ★ 返信ロジックを呼び出す
    (setq reply (tcp-client-handle-received-data proc data decoded))

    ;; ★ 返信がある場合は送信
    (when reply
      (process-send-string proc reply)
      (when logbuf
        (tcp-client--log logbuf "AUTO-REPLY (hex): %s"
                         (tcp-client--hex-string reply))))))

;;;###autoload
(defun tcp-client--process-sentinel (proc event)
  "プロセス状態変化をログに残す。"
  (let ((logbuf (process-get proc 'tcp-client-log-buffer)))
    (when logbuf
      (tcp-client--log logbuf "Process event: %s" (string-trim event)))))


;;----------------------------------------------------------
;; Reply handler wrapper
;;----------------------------------------------------------
;;;###autoload
(defun tcp-client-handle-received-data (proc raw-data decoded)
  "PROC に紐づく reply-handler を呼び出し、その戻り値を返す。
戻り値は送信すべきバイト列（string）か nil。"
  (let ((handler (process-get proc 'tcp-client-reply-handler)))
    (when handler
      (funcall handler proc raw-data decoded))))


;;----------------------------------------------------------
;; Manual send functions
;;----------------------------------------------------------
;;;###autoload
(defun tcp-client-send-bytes (proc bytes)
  "PROC に生のバイト列を送信する。"
  (let* ((logbuf (process-get proc 'tcp-client-log-buffer))
         (data (cond
                ((stringp bytes) bytes)
                ((and (listp bytes)
                      (cl-every (lambda (x) (and (integerp x) (<= 0 x 255))) bytes))
                 (apply #'string bytes))
                (t (error "bytes must be a string or list of 0-255 integers"))))
         (hex (tcp-client--hex-string data)))
    (process-send-string proc data)
    (when logbuf
      (tcp-client--log logbuf "SENT (hex): %s" hex))))
;;;###autoload
(defun tcp-client-send-text (proc text)
  "PROC に TEXT を cp932 でエンコードして送信する。"
  (let* ((logbuf (process-get proc 'tcp-client-log-buffer))
         (data (encode-coding-string text 'cp932))
         (hex (tcp-client--hex-string data)))
    (process-send-string proc data)
    (when logbuf
      (tcp-client--log logbuf "SENT (cp932): %s" text)
      (tcp-client--log logbuf "SENT (hex): %s" hex))))


;;----------------------------------------------------------
;; Disconnect
;;----------------------------------------------------------
;;;###autoload
(defun tcp-client-disconnect (proc)
  "PROC を切断する。"
  (let ((logbuf (process-get proc 'tcp-client-log-buffer)))
    (when (process-live-p proc)
      (delete-process proc)
      (when logbuf
        (tcp-client--log logbuf "Disconnected.")))))

(provide 'emacs-tcpclient)
;;; tcp-client.el ends here
