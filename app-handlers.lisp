;;;; Customizing acceptor behaviour

;;;; If you want to modify what acceptors do, you should subclass ACCEPTOR (or SSL-ACCEPTOR) and specialize
;;;; the generic functions that constitute their behaviour (see example below). The life of an acceptor looks
;;;; like this: It is started with the function START which immediately calls START-LISTENING and then applies
;;;; the function EXECUTE-ACCEPTOR to its taskmaster. This function will eventually call ACCEPT-CONNECTIONS
;;;; which is responsible for setting things up to wait for clients to connect. For each incoming connection
;;;; which comes in, HANDLE-INCOMING-CONNECTION is applied to the taskmaster which will either call
;;;; PROCESS-CONNECTION directly, or will create a thread to call it. PROCESS-CONNECTION calls
;;;; INITIALIZE-CONNECTION-STREAM before it does anything else, then it selects and calls a function which
;;;; handles the request, and finally it sends the reply to the client before it calls RESET-CONNECTION-STREAM.
;;;; If the connection is persistent, this procedure is repeated (except for the intialization step) in a loop
;;;; until the connection is closed. The acceptor is stopped with STOP.



;;;; Taskmasters

;;;; As a "normal" Hunchentoot user, you can completely ignore taskmasters and skip this section.
;;;; But if you're still reading, here are the dirty details: Each acceptor has a taskmaster associated with
;;;; it at creation time. It is the taskmaster's job to distribute the work of accepting and handling incoming
;;;; connections. The acceptor calls the taskmaster if appropriate and the taskmaster calls back into the
;;;; acceptor. This is done using the generic functions described in this and the previous section.
;;;; Hunchentoot comes with two standard taskmaster implementations - one
;;;; (which is the default used on multi-threaded Lisps) which starts a new thread for each incoming
;;;; connection and one which handles all requests sequentially. It should for example be relatively
;;;; straightforward to create a taskmaster which allocates threads from a fixed pool instead of creating
;;;; a new one for each connection.
;;;; 
;;;; You can control the resources consumed by a threaded taskmaster via two initargs.
;;;; :max-thread-count lets you set the maximum number of request threads that can be processes
;;;; simultaneously. If this is nil, the is no thread limit imposed. :max-accept-count lets you set
;;;; the maximum number of requests that can be outstanding (i.e. being processed or queued for processing).
;;;; If :max-thread-count is supplied and :max-accept-count is NIL, then a +HTTP-SERVICE-UNAVAILABLE+
;;;; error will be generated if there are more than the max-thread-count threads processing requests.
;;;; If both :max-thread-count and :max-accept-count are supplied, then max-thread-count must be less
;;;; than max-accept-count; if more than max-thread-count requests are being processed, then requests
;;;; up to max-accept-count will be queued until a thread becomes available. If more than max-accept-count
;;;; requests are outstanding, then a +HTTP-SERVICE-UNAVAILABLE+ error will be generated. In a
;;;; load-balanced environment with multiple Hunchentoot servers, it's reasonable to provide
;;;; :max-thread-count but leave :max-accept-count null. This will immediately result in
;;;; +HTTP-SERVICE-UNAVAILABLE+ when one server is out of resources, so the load balancer can try to
;;;; find another server. In an environment with a single Hunchentoot server, it's reasonable to provide
;;;; both :max-thread-count and a somewhat larger value for :max-accept-count. This will cause a server
;;;; that's almost out of resources to wait a bit; if the server is completely out of resources, then
;;;; the reply will be +HTTP-SERVICE-UNAVAILABLE+. The default for these values is 100 and 120, respectively. 



;;;; Request dispatch and handling

;;;; The main job of HANDLE-REQUEST is to select and call a function which handles the request,
;;;; i.e. which looks at the data the client has sent and prepares an appropriate reply to send back.
;;;; This is by default implemented as follows:

;;;; The ACCEPTOR class defines a ACCEPTOR-DISPATCH-REQUEST generic function which is used to
;;;; actually dispatch the request. This function is called by the default method of HANDLE-REQUEST.
;;;; Each ACCEPTOR-DISPATCH-REQUEST method looks at the request object and depending on its contents
;;;; decides to either handle the request or call the next method.

;;;; In order to dispatch a request, Hunchentoot calls the ACCEPTOR-DISPATCH-REQUEST generic functions.
;;;; The method for ACCEPTOR tries to serve a static file relative to it's ACCEPTOR-DOCUMENT-ROOT.
;;;; Application specific acceptor subclasses will typically perform URL parsing and dispatching according
;;;; to the policy that is required. 

;;;; The default method of HANDLE-REQUEST sets up standard logging and error handling
;;;; before it calls the acceptor's request dispatcher.

;;;; Request handlers do their work by modifying the reply object if necessary and by eventually
;;;; returning the response body in the form of a string or a binary sequence. As an alternative,
;;;; they can also call SEND-HEADERS and write directly to a stream.


;;;; Now do this


(in-package :asdk)
(defvar *app* nil)

(defclass app-acceptor (tbnl:acceptor)
  ())

;; called by (tbnl:handle-request ((*acceptor* acceptor) (*request* request)))
(defmethod tbnl:acceptor-dispatch-request ((acceptor app-acceptor) request)
  (fn)
  (funcall (funcall (tbnl:create-regex-dispatcher "/img1" 'fn) request))
  )

(defun fn ()
  "fn with aa~%")

