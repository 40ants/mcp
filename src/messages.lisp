(uiop:define-package #:40ants-mcp/messages
  (:use #:cl)
  (:import-from #:openrpc-server
                #:defmethod-rpc
                #:define-rpc-response)
  (:export ;; Core message types
           #:mcp-initialize-request
           #:mcp-initialize-response
           #:client-info
           #:server-info
           #:implementation
           
           ;; Tool-related messages
           #:list-tools-request
           #:list-tools-response
           #:call-tool-request
           #:call-tool-response
           #:tool-definition
           #:tool-input-schema
           
           ;; Resource-related messages
           #:list-resources-request
           #:list-resources-response
           #:read-resource-request
           #:read-resource-response
           #:resource-definition
           
           ;; Prompt-related messages
           #:list-prompts-request
           #:list-prompts-response
           #:get-prompt-request
           #:get-prompt-response
           #:prompt-definition
           
           ;; Notification messages
           #:ping-notification
           #:progress-notification
           #:log-notification
           
           ;; Utilities
           #:make-error-result
           #:make-success-result))
(in-package #:40ants-mcp/messages)

;; Core data structures

(defclass implementation ()
  ((name :initarg :name :reader implementation-name
         :documentation "Name of the implementation")
   (version :initarg :version :reader implementation-version
            :documentation "Version of the implementation"))
  (:documentation "Implementation details"))

(defclass client-info ()
  ((name :initarg :name :reader client-name
         :documentation "Name of the MCP client")
   (version :initarg :version :reader client-version
            :documentation "Version of the MCP client"))
  (:documentation "Information about the MCP client"))

(defclass server-info ()
  ((name :initarg :name :reader server-name
         :documentation "Name of the MCP server")
   (version :initarg :version :reader server-version
            :documentation "Version of the MCP server"))
  (:documentation "Information about the MCP server"))

;; Initialize Request/Response

(defclass mcp-initialize-request ()
  ((protocol-version :initarg :protocol-version 
                     :reader protocol-version
                     :documentation "MCP protocol version")
   (capabilities :initarg :capabilities 
                 :reader capabilities
                 :documentation "Client capabilities")
   (client-info :initarg :client-info 
                :reader client-info
                :type client-info
                :documentation "Information about the client"))
  (:documentation "MCP initialization request"))

(defclass mcp-initialize-response ()
  ((protocol-version :initarg :protocol-version 
                     :reader protocol-version
                     :documentation "MCP protocol version")
   (capabilities :initarg :capabilities 
                 :reader capabilities
                 :documentation "Server capabilities")
   (server-info :initarg :server-info 
                :reader server-info
                :type server-info
                :documentation "Information about the server")
   (instructions :initarg :instructions 
                 :reader instructions
                 :documentation "Optional instructions for the client"))
  (:documentation "MCP initialization response"))

;; Tool-related messages

(defclass tool-input-schema ()
  ((type :initarg :type :reader schema-type
         :documentation "Schema type (usually 'object')")
   (properties :initarg :properties :reader schema-properties
               :documentation "Tool input properties")
   (required :initarg :required :reader schema-required
             :documentation "Required properties"))
  (:documentation "JSON Schema for tool input"))

(defclass tool-definition ()
  ((name :initarg :name :reader tool-name
         :documentation "Name of the tool")
   (description :initarg :description :reader tool-description
                :documentation "Description of what the tool does")
   (input-schema :initarg :input-schema :reader tool-input-schema
                 :type tool-input-schema
                 :documentation "JSON Schema for tool input"))
  (:documentation "Definition of an available tool"))

(defclass list-tools-request ()
  ()
  (:documentation "Request to list available tools"))

(defclass list-tools-response ()
  ((tools :initarg :tools :reader tools
          :documentation "List of available tools"))
  (:documentation "Response containing available tools"))

(defclass call-tool-request ()
  ((name :initarg :name :reader tool-name
         :documentation "Name of the tool to call")
   (arguments :initarg :arguments :reader tool-arguments
              :documentation "Arguments for the tool"))
  (:documentation "Request to call a specific tool"))

(defclass call-tool-response ()
  ((content :initarg :content :reader response-content
            :documentation "Tool execution result")
   (is-error :initarg :is-error :reader is-error-p
             :initform nil
             :documentation "Whether this is an error response"))
  (:documentation "Response from tool execution"))

;; Resource-related messages

(defclass resource-definition ()
  ((uri :initarg :uri :reader resource-uri
        :documentation "URI identifying the resource")
   (name :initarg :name :reader resource-name
         :documentation "Human-readable name")
   (description :initarg :description :reader resource-description
                :documentation "Description of the resource")
   (mime-type :initarg :mime-type :reader resource-mime-type
              :documentation "MIME type of the resource"))
  (:documentation "Definition of an available resource"))

(defclass list-resources-request ()
  ()
  (:documentation "Request to list available resources"))

(defclass list-resources-response ()
  ((resources :initarg :resources :reader resources
              :documentation "List of available resources"))
  (:documentation "Response containing available resources"))

(defclass read-resource-request ()
  ((uri :initarg :uri :reader resource-uri
        :documentation "URI of the resource to read"))
  (:documentation "Request to read a specific resource"))

(defclass read-resource-response ()
  ((contents :initarg :contents :reader resource-contents
             :documentation "Contents of the resource"))
  (:documentation "Response containing resource contents"))

;; Prompt-related messages

(defclass prompt-definition ()
  ((name :initarg :name :reader prompt-name
         :documentation "Name of the prompt")
   (description :initarg :description :reader prompt-description
                :documentation "Description of the prompt")
   (arguments :initarg :arguments :reader prompt-arguments
              :documentation "Arguments schema for the prompt"))
  (:documentation "Definition of an available prompt"))

(defclass list-prompts-request ()
  ()
  (:documentation "Request to list available prompts"))

(defclass list-prompts-response ()
  ((prompts :initarg :prompts :reader prompts
            :documentation "List of available prompts"))
  (:documentation "Response containing available prompts"))

(defclass get-prompt-request ()
  ((name :initarg :name :reader prompt-name
         :documentation "Name of the prompt to get")
   (arguments :initarg :arguments :reader prompt-arguments
              :documentation "Arguments for the prompt"))
  (:documentation "Request to get a specific prompt"))

(defclass get-prompt-response ()
  ((description :initarg :description :reader prompt-description
                :documentation "Description of the prompt")
   (messages :initarg :messages :reader prompt-messages
             :documentation "List of messages in the prompt"))
  (:documentation "Response containing the prompt"))

;; Notification messages

(defclass ping-notification ()
  ()
  (:documentation "Ping notification to check connection"))

(defclass progress-notification ()
  ((progress :initarg :progress :reader progress-value
             :documentation "Progress value")
   (total :initarg :total :reader progress-total
          :documentation "Total value"))
  (:documentation "Progress notification"))

(defclass log-notification ()
  ((level :initarg :level :reader log-level
          :documentation "Log level (error, warn, info, debug)")
   (data :initarg :data :reader log-data
         :documentation "Log data")
   (logger :initarg :logger :reader log-logger
           :documentation "Logger name"))
  (:documentation "Log notification"))

;; Utility functions

(defun make-error-result (code message &optional data)
  "Create an error result structure"
  `((:is-error . t)
    (:error . ((:code . ,code)
               (:message . ,message)
               ,@(when data `((:data . ,data)))))))

(defun make-success-result (content)
  "Create a success result structure"
  `((:is-error . nil)
    (:content . ,content))) 