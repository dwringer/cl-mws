(in-package :common-lisp-user)

(defpackage :cl-mws
  (:use :common-lisp
	:cl-base64
	:cl-ppcre
	:drakma
	:ironclad)
  (:shadowing-import-from :ironclad
			  :null)
  (:export :*mws-credentials*
	   :mws-request))


(in-package :cl-mws)

(defparameter *mws-credentials*
  ;;; NOTE: This variable is a placeholder.  It is strongly recommended
  ;;;       NOT to store credentials in this file, but to load them to
  ;;;       cl-mws:*mws-credentials* from an encrypted source when they
  ;;;       are to be used.
  '((:my-store . ((:access-key     . "")
		  (:secret-key     . "")
		  (:seller-id      . "")
		  (:marketplace-id . "")))
    ;;  . . .
    ))

(defparameter *main-host* "https://mws.amazonservices.com")

(defparameter *api-versions*
  '((:feeds                         . "2009-01-01")
    (:finances                      . "2015-05-01")
    (:fulfillment-inbound-shipment  . "2010-10-01")
    (:fulfillment-inventory         . "2010-10-01")
    (:fulfillment-outbound-shipment . "2010-10-01")
    (:merchant-fulfillment          . "2015-06-01")
    (:orders                        . "2013-09-01")
    (:products                      . "2011-10-01")
    (:recommendations               . "2013-04-01")
    (:reports                       . "2009-01-01")
    (:sellers                       . "2011-07-01")
    (:subscriptions                 . "2013-07-01")))
			       
(defparameter *api-paths*
  '((:feeds                         . "/Feeds/")
    (:finances                      . "/Finances/")
    (:fulfillment-inbound-shipment  . "/FulfillmentInboundShipment/")
    (:fulfillment-inventory         . "/FulfillmentInventory/")
    (:fulfillment-outbound-shipment . "/FulfillmentOutboundShipment/")
    (:merchant-fulfillment          . "/MerchantFulfillment/")
    (:orders                        . "/Orders/")
    (:products                      . "/Products/")
    (:recommendations               . "/Recommendations/")
    (:reports                       . "/Reports/")
    (:sellers                       . "/Sellers/")
    (:subscriptions                 . "/Subscriptions/")))


(defun aget (key alist &optional (test #'eql))
  "Shortcut to directly obtain value from an alist by key"
  (cdr (assoc key alist :test test)))


(defun time-to-utc (u-time)
  "Convert universal time to UTC equivalent on this machine"
  (multiple-value-bind (sec min hr day mon yr wkd dst tz)
      (decode-universal-time u-time)
    (declare (ignore sec min hr day mon yr wkd))
    (+ (get-universal-time)
       (* (- tz (if dst 1 0)) (* 60 60)))))


(defun iso-universal-time (u-time)
  "Make a given universal time into an ISO formatted string"
  (multiple-value-bind (sec min hr day mon yr wkd dst tz)
      (decode-universal-time u-time)
    (declare (ignore wkd dst tz))
    (let ((mdhms (list mon day hr min sec)))
      (multiple-value-bind (mon day hr min sec)
	  (apply #'values (mapcar #'(lambda (x) (format nil "~2,'0d" x)) mdhms))
	(reduce #'(lambda (a b) (format nil "~A~A" a b))
		(list yr "-" mon "-" day "T" hr ":" min ":" sec))))))


(defun get-utc-time ()
  "Return universal time as UTC"
  (time-to-utc (get-universal-time)))


(defun iso-time (&optional (utc? nil))
  "Return an ISO time string, optionally in UTC"
  (let ((time (get-universal-time)))
    (iso-universal-time (if utc? (time-to-utc time) time))))


(defun b64-hmac-sha256 (secret-string text-string)
  "Create a signature using Amazon's base64 encoded SHA256 HMAC standard"
  (let ((hmac (make-hmac (ascii-string-to-byte-array secret-string) :sha256)))
    (update-hmac hmac (ascii-string-to-byte-array text-string))
    (string-to-base64-string (map 'string #'code-char (hmac-digest hmac)))))


(defun required-mws-data (credentials &optional (include-alist nil))
  "Add merchant/seller required fields to data alist"
  (append include-alist
	  (list (cons "SignatureMethod" "HmacSHA256")
		(cons "SignatureVersion" "2")
		(cons "Timestamp" (format nil "~AZ" (iso-time t)))
		(cons "AWSAccessKeyId" (aget :access-key credentials))
		(cons "SellerId" (aget :seller-id credentials)))))


(defun safe-url-encode (string
			&optional (format *drakma-default-external-format*))
  "URL-encode but don't escape tildes or percent signs"
  (regex-replace-all "%25"
		     (regex-replace-all "%7E"
					(url-encode string format)
					"~")
		     "%"))


(defun create-mws-param-string (data)
  "Format the parameters of an MWS request as required before signing"
  (let ((access-key (assoc "AWSAccessKeyId" data :test #'string-equal))
	(remains (remove-if #'(lambda (x)
				(string-equal "AWSAccessKeyId"
					      (car x))) data)))
    (format nil "~{~A=~A~^&~}"
	    (apply #'append
		   (mapcar #'(lambda (x)
			       (list (car x)
				     (safe-url-encode (cdr x))))
			   (cons access-key
				 (sort remains
				       #'(lambda (a b)
					   (string-lessp (car a)
							 (car b))))))))))


(defun append-request-signature (parameter-string signature)
  "Properly append a signature to an MWS request parameter string"
  (format nil "~A&Signature=~A" parameter-string signature))


(defun sign-mws-request (method domain api credentials
			   &optional (data nil) (debug nil))
  "Sign a specified MWS HTTP request"
  (let ((path (aget api *api-paths*))
	(version (aget api *api-versions*))
	(params (create-mws-param-string
		 (required-mws-data credentials data))))
    (let ((string-to-sign
	   (format nil
		   (concatenate 'string
				"~{~A~^" (format nil "~C" #\linefeed) "~}")
		   (list method
			 (string-downcase
			  (regex-replace-all "https://" domain ""))
			 (concatenate 'string path version) params))))
      (when debug
	(format t (concatenate 'string
			       "===============~%"
			       "STRING TO SIGN:~%"
			       "---------------~%"
			       "~A~%"
			       "===============~%")
		string-to-sign))
      (append-request-signature
       params
       (url-encode
	(b64-hmac-sha256 (aget :secret-key credentials) string-to-sign)
	:utf-8)))))


(defun mws-request (api store action &optional (data nil))
  "Generate and submit a specified MWS HTTP request"
  (let* ((path (aget api *api-paths*))
	 (version (aget api *api-versions*))
	 (credentials (aget store *mws-credentials*))
         (act-data (append (list (cons "Version" version)
				 (cons "Action" action))
			   data))
	 (signed (sign-mws-request "POST"
				   *main-host*
				   api
				   credentials
				   act-data)))
    (http-request (concatenate 'string
			       *main-host*
			       path
			       version
			       "?"
			       signed)
		  :content-type "text/xml"
		  :accept nil
		  :method :POST
		  :user-agent "CL-MWS/v0 (Language=CL)"
		  :url-encoder #'safe-url-encode)))