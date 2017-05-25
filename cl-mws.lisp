(in-package :common-lisp-user)

(defpackage :cl-mws
  (:use :common-lisp
	:cl-base64
	:cl-ppcre
	:cxml
	:drakma
	:flexi-streams
	:ironclad)
  (:shadowing-import-from :flexi-streams
			  :octets-to-string
			  :string-to-octets)
  (:shadowing-import-from :cl
			  :null)
  (:export :*mws-credentials*
	   :*mws-endpoint*
	   :get-marketplace-id
	   :mws-request
	   :dom-from-xml-string
	   :sexp-from-xml-string
	   ;; PRODUCTS API:
	   :list-matching-products
	   :get-matching-product
	   :get-matching-product-for-id
	   :get-competitive-pricing-for-sku
	   :get-competitive-pricing-for-asin
	   :get-lowest-offer-listings-for-sku
	   :get-lowest-offer-listings-for-asin
	   :get-lowest-priced-offers-for-sku
	   :get-lowest-priced-offers-for-asin
	   :get-my-fees-estimate
	   :get-my-price-for-sku
	   :get-my-price-for-asin
	   :get-product-categories-for-sku
	   :get-product-categories-for-asin
	   ;; ORDERS API:
	   :list-orders
	   :list-orders-by-next-token
	   :get-order
	   :list-order-items
	   :list-order-items-by-next-token))


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

(defparameter *mws-endpoint* "https://mws.amazonservices.com")

(defparameter *mws-endpoints*
  '((:na . "https://mws.amazonservices.com")
    (:br . "https://mws.amazonservices.com")
    (:eu . "https://mws-eu.amazonservices.com")
    (:in . "https://mws.amazonservices.in")
    (:cn . "https://mws.amazonservices.com.cn")
    (:jp . "https://mws.amazonservices.com.jp")))

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

(defparameter *marketplace-ids*
  '((:ca . "A2EUQ1WTGCTBG2")
    (:mx . "A1AM78C64UM0Y8")
    (:us . "ATVPDKIKX0DER")
    (:br . "A2Q3Y263D00KWC")
    (:de . "A1PA6795UKMFR9")
    (:es . "A1RKKUPIHCS9HS")
    (:fr . "A13V1IB3VIYZZH")
    (:it . "APJ6JRA9NG5V4")
    (:uk . "A1F83G8C2ARO7P")
    (:in . "A21TJRUUN4KGV")
    (:jp . "A1VC38T7YXB528")
    (:cn . "AAHKV2X7AFYLW")))

(defparameter *item-conditions* '((:any . "Any")
				  (:new . "New")
				  (:used . "Used")
				  (:collectible . "Collectible")
				  (:refurbished . "Refurbished")
				  (:club . "Club")))


(defun aget (key alist &optional (test #'eql))
  "Shortcut to directly obtain value from an alist by key"
  (cdr (assoc key alist :test test)))


(defun time-to-utc (u-time)
  "Convert universal time to UTC equivalent on this machine"
  (multiple-value-bind (sec min hr day mon yr wkd dst tz)
      (decode-universal-time u-time)
    (declare (ignore sec min hr day mon yr wkd))
    (+ u-time  ; (get-universal-time)
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


(defun get-marketplace-id (&optional (marketplace :us))
  "Retrieve the desired marketplace ID for the specified marketplace"
  (aget marketplace *marketplace-ids*))


(defun set-mws-endpoint (&optional (region :na))
  "Retrieve the main endpoint URL for the specified region"
  (setf *mws-endpoint* (aget region *mws-endpoints*)))


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
  (regex-replace-all
	"%25" (regex-replace-all
	       "%7E" (url-encode (regex-replace-all
				  " " string "%20") format)
	       "~")
	"%"))


(defun create-mws-param-string (data)
  "Format the parameters of an MWS request as required before signing"
  (format nil "~{~A=~A~^&~}"
	  (apply #'append
		 (mapcar #'(lambda (x) (list (car x) (safe-url-encode (cdr x))))
			 (sort data #'(lambda (a b) (string< (car a) (car b))))))))


(defun append-request-signature (parameter-string signature)
  "Properly append a signature to an MWS request parameter string"
  (format nil "~A&Signature=~A" parameter-string signature))


(defun sign-mws-request (method domain api credentials
			   &key (data nil) (debug nil))
  "Sign a specified MWS HTTP request"
  (let ((path    (aget api *api-paths*))
	(version (aget api *api-versions*))
	(params  (create-mws-param-string
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


(defun mws-request (api store action &key (data nil) (debug nil) (as-body nil))
  "Generate and submit a specified MWS HTTP request"
  (let* ((credentials     (aget store *mws-credentials*))
	 (path            (aget api *api-paths*))
	 (version         (aget api *api-versions*))
         (act-data        (append (list (cons "Version" version)
					(cons "Action" action))
				  data))
	 (path-components (list *mws-endpoint* path version))
	 (signed          (sign-mws-request "POST"
					    *mws-endpoint*
					    api
					    credentials
					    :data act-data
					    :debug debug))
	 (content         (if as-body
			      (concatenate 'string "&" signed)
			      "")))
    (when (not as-body)
      (setf path-components (append path-components (list "?" signed))))
    (when (and debug as-body)
      (format t
	      (concatenate 'string
			   "====================~%"
			   "CONTENT:~%"
			   "--------------------~%"
			   "~A~%"
			   "====================~%")
	      content))
    (multiple-value-bind (body-or-stream
			  status-code
			  headers
			  uri
			  stream
			  must-close
			  reason-phrase)
	   (http-request (apply #'concatenate 'string path-components)
			 :accept nil
			 :content content
			 :content-type (if as-body
					   "application/x-www-form-urlencoded"
					   "text/xml")
			 :method :POST
			 :url-encoder #'safe-url-encode
			 :user-agent "CL-MWS/v0 (Language=CL)")
      (values
       (if as-body
	   (concatenate 'string (mapcar #'code-char
					(coerce body-or-stream 'list)))
	   body-or-stream)
       status-code
       headers
       uri
       stream
       must-close
       reason-phrase))))


(defun parse-xml-string (xml-string builder)
  "Parse an XML string into object defined by builder"
  (let ((seq (mapcar #'char-code (coerce xml-string 'list))))
    (with-input-from-sequence (stream seq)
      (parse-stream stream (funcall builder)))))


(defun dom-from-xml-string (xml-string)
  "Parse an XML string into a DOM instance"
  (parse-xml-string xml-string #'cxml-dom:make-dom-builder))


(defun sexp-from-xml-string (xml-string)
  "Parse an XML string into an S-expression"
  (parse-xml-string xml-string #'cxml-xmls:make-xmls-builder))


(defun increment-list-pairs (prefix name)
  "Make function to transform list to parameter-value pairs by name"
  (let ((i 1))
    #'(lambda (x)
	(prog1 (cons (format nil "~A.~A.~A" prefix name i) x)
	  (setf i (+ i 1))))))


(defun alists-as-key-lists (list-name item-name)
  "Make function to transform alists to numbered parameter-value lists"
  (let ((i 1))
    #'(lambda (alist)
	(prog1 (mapcar #'(lambda (cons-pair)
			   (let ((k (car cons-pair))
				 (v (cdr cons-pair)))
			     (cons (format nil "~A.~A.~A.~A"
					   list-name item-name i k) v)))
		       alist)
	  (setf i (+ i 1))))))


;;; PRODUCTS API: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-matching-products (store query
			       &optional
				 (query-context-id nil)
				 (marketplace (get-marketplace-id)))
  "Make a ListMatchingProducts request to the Products API"
  (mws-request :products store "ListMatchingProducts"
	       :data (append (list (cons "Query" query))
			     (if query-context-id
				 (list (cons "QueryContextId" query-context-id)
				       nil))
			     (list (cons "MarketplaceId" marketplace)))))


(defun get-matching-product (store asin-list
			     &optional (marketplace (get-marketplace-id)))
  "Make a GetMatchingProduct request to the Products API"
  (mws-request :products store "GetMatchingProduct"
	       :data (append (mapcar (increment-list-pairs "ASINList" "ASIN")
				     asin-list)
			     (list (cons "MarketplaceId" marketplace)))))


(defun get-matching-product-for-id (store id-list
				    &optional
				      (id-type "ASIN")
				      (marketplace (get-marketplace-id)))
  "Make a GetMatchingProductForId request to the Products API"
  (mws-request :products store "GetMatchingProductForId"
	       :data (append (list (cons "IdType" id-type))
			     (mapcar (increment-list-pairs "IdList" "Id")
				     id-list)
			     (list (cons "MarketplaceId" marketplace)))))


(defun get-competitive-pricing-for-sku (store seller-sku-list
					&optional
					  (marketplace (get-marketplace-id)))
  "UNTESTED - Make a GetCompetitivePricingForSKU request to the Products API"
  (mws-request :products store "GetCompetitivePricingForSKU"
	       :data (append (mapcar (increment-list-pairs "SellerSKUList"
							   "SellerSKU")
				     seller-sku-list)
			     (list (cons "MarketplaceId" marketplace)))))


(defun get-competitive-pricing-for-asin (store asin-list
					 &optional
					   (marketplace (get-marketplace-id)))
  "Make a GetCompetitivePricingForASIN request to the Products API"
  (mws-request :products store "GetCompetitivePricingForASIN"
	       :data (append (mapcar (increment-list-pairs "ASINList" "ASIN")
				     asin-list)
			     (list (cons "MarketplaceId" marketplace)))))


(defun get-lowest-offer-listings-for-sku (store seller-sku-list
					  &optional
					    (item-condition :any)
					    (marketplace (get-marketplace-id)))
  "UNTESTED - Make a GetLowestOfferListingsForSKU request to the Products API"
  (mws-request :products store "GetLowestOfferListingsForSKU"
	       :data (append (mapcar (increment-list-pairs "SellerSKUList"
							   "SellerSKU")
				     seller-sku-list)
			     (list (cons "MarketplaceId" marketplace))
			     (list (cons "ItemCondition"
					 (aget item-condition
					       *item-conditions*))))))


(defun get-lowest-offer-listings-for-asin (store asin-list
					   &optional
					     (item-condition :any)
					     (marketplace
					      (get-marketplace-id)))
  "Make a GetLowestOfferListingsForASIN request to the Products API"
  (mws-request :products store "GetLowestOfferListingsForASIN"
	       :data (append (mapcar (increment-list-pairs "ASINList" "ASIN")
				     asin-list)
			     (list (cons "MarketplaceId" marketplace))
			     (list (cons "ItemCondition"
					 (aget item-condition
					       *item-conditions*))))))


(defun get-lowest-priced-offers-for-sku (store seller-sku
					 &optional
					   (item-condition :any)
					   (marketplace (get-marketplace-id)))
  "UNTESTED - Make a GetLowestPricedOffersForSKU request to the Products API"
  (mws-request :products store "GetLowestPricedOffersForSKU"
	       :data (list (cons "MarketplaceId" marketplace)
			   (cons "ItemCondition" (aget item-condition
						       *item-conditions*))
			   (cons "SellerSKU" seller-sku))
	       :as-body t))


(defun get-lowest-priced-offers-for-asin (store asin
					  &optional
					    (item-condition :any)
					    (marketplace (get-marketplace-id)))
  "Make a GetLowestPricedOffersForASIN request to the Products API"
  (mws-request :products store "GetLowestPricedOffersForASIN"
	       :data (list (cons "MarketplaceId" marketplace)
			   (cons "ItemCondition" (aget item-condition
						       *item-conditions*))
			   (cons "ASIN" asin))
	       :as-body t))


(defun get-my-fees-estimate (store fees-estimate-request-list)
  "Make a GetMyFeesEstimate request to the Products API"
  (mws-request :products store "GetMyFeesEstimate"
	       :data (apply #'concatenate 'list
			    (mapcar (alists-as-key-lists
				     "FeesEstimateRequestList"
				     "FeesEstimateRequest")
				    fees-estimate-request-list))))


(defun get-my-price-for-sku (store seller-sku-list
			     &optional
			       (item-condition :all)
			       (marketplace (get-marketplace-id)))
  "UNTESTED - Make a GetMyPriceForSKU request to the Products API"
  (mws-request :products store "GetMyPriceForSku"
	       :data (append
		      (list (cons "MarketplaceId" marketplace)
			    (cons "ItemCondition" (aget item-condition
						       *item-conditions*)))
		      (mapcar (increment-list-pairs "SellerSKUList"
						    "SellerSKU")
			      seller-sku-list))))


(defun get-my-price-for-asin (store asin-list
			      &optional
				(item-condition :any)
				(marketplace (get-marketplace-id)))
  "Make a GetMyPriceForASIN request to the Products API"
  (mws-request :products store "GetMyPriceForASIN"
	       :data (append
		      (list (cons "MarketplaceId" marketplace)
			    (cons "ItemCondition" (aget item-condition
							*item-conditions*)))
		      (mapcar (increment-list-pairs "ASINList" "ASIN")
			      asin-list))))


(defun get-product-categories-for-sku (store seller-sku
				       &optional
					 (marketplace (get-marketplace-id)))
  "UNTESTED - Make a GetProductCategoriesForSKU request to the Products API"
  (mws-request :products store "GetProductCategoriesForSKU"
	       :data (list (cons "MarketplaceId" marketplace)
			   (cons "SellerSKU" seller-sku))))


(defun get-product-categories-for-asin (store asin
					&optional
					  (marketplace (get-marketplace-id)))
  "Make a GetProductCategoriesForASIN request to the Products API"
  (mws-request :products store "GetProductCategoriesForASIN"
	       :data (list (cons "MarketplaceId" marketplace)
			   (cons "ASIN" asin))))


;;; ORDERS API: ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun list-orders (store
		    &key
		      created-after
		      created-before
		      last-updated-after
		      last-updated-before
		      order-status-list
		      marketplace-id-list 
		      fulfillment-channel-list
		      payment-method-list
		      buyer-email
		      seller-order-id
		      max-results-per-page
		      tfm-shipment-status-list)
  "Make a ListOrders request to the Orders API"
  (when (null marketplace-id-list)
    (setf marketplace-id-list (list (get-marketplace-id))))
  (when (not (or (and (null last-updated-after)
		      (null created-after))
		 (and last-updated-after created-after)))
    (labels ((kvp (k v) (when v (list (cons k v))))
	     (lst (n i l) (when l (mapcar (increment-list-pairs n i) l))))
      (mws-request :orders store "ListOrders"
		   :data
		   (append
		    (kvp "CreatedAfter" created-after)
		    (kvp "CreatedBefore" created-before)
		    (kvp "LastUpdatedAfter" last-updated-after)
		    (kvp "LastUpdatedBefore" last-updated-before)
		    (lst "OrderStatus" "Status" order-status-list)
		    (lst "MarketplaceId" "Id" marketplace-id-list)
		    (lst "FulfillmentChannel" "Channel"
			 fulfillment-channel-list)
		    (lst "PaymentMethod" "Method" payment-method-list)
		    (kvp "BuyerEmail" buyer-email)
		    (kvp "SellerOrderId" seller-order-id)
		    (kvp "MaxResultsPerPage" max-results-per-page)
		    (lst "TFMShipmentSTatusList" "TFMShipmentStatus"
			 tfm-shipment-status-list))))))


(defun list-orders-by-next-token (store next-token)
  "UNTESTED - Make a ListOrdersByNextToken request to the Orders API"
  (mws-request :orders store "ListOrdersByNextToken"
	       :data (list (cons "NextToken" next-token))))


(defun get-order (store amazon-order-id)
  "UNTESTED - Make a GetOrder request to the Orders API"
  (mws-request :orders store "GetOrder"
	       :data (list (cons "AmazonOrderId" amazon-order-id))))


(defun list-order-items (store amazon-order-id)
  "Make a ListOrderItems request to the Orders API"
  (mws-request :orders store "ListOrderItems"
	       :data (list (cons "AmazonOrderId" amazon-order-id))))


(defun list-order-items-by-next-token (store amazon-order-id)
  "UNTESTED - Make a ListOrderItemsByNextToken request to the Orders API"
  (mws-request :orders store "ListOrderItemsByNextToken"
	       :data (list (cons "NextToken" next-token))))
