Example of NexJ Generic Server Interface invocation through REST interface with typed XML.

Get the NexJ WADL file from http://<host>:<port>/<context-root>/xml?wadl
e.g. wget --http-user=nexjsa --http-passwd=nexj "http://localhost:8080/nexj/xml?wadl" -O nexj.wadl

The Ruby WADL proxy generator library is available at http://www.crummy.com/software/wadl.rb/wadl.rb

NOTE: You will need to apply the following patch to wadl.rb for support of <param> references:

@@ -594,8 +594,8 @@
   in_document 'param'
   as_member 'param'
   as_collection 'params'
-  has_required :name
-  has_attributes :type, :default, :style, :path, :required, :repeating, :fixed
+  may_be_reference
+  has_attributes :id, :name, :type, :default, :style, :path, :required, :repeating, :fixed
   has_many Option
   has_many Link

@@ -876,7 +876,7 @@
   in_document 'method'
   as_collection 'http_methods'
   may_be_reference
-  has_required :id, :name
+  has_attributes :id, :name
   has_one RequestFormat
   has_one ResponseFormat

@@ -1196,7 +1196,7 @@
 class Application < HasDocs
   in_document 'application'
   has_one Resources
-  has_many HTTPMethod, RepresentationFormat, FaultFormat
+  has_many HTTPMethod, RepresentationFormat, FaultFormat, Param

   def Application.from_wadl(wadl)
     wadl = wadl.read if wadl.respond_to?(:read)
