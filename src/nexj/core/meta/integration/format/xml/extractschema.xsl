<!-- Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0 -->
<xs:stylesheet exclude-result-prefixes="xsd" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:xs="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:xalan="http://xml.apache.org/xslt">
	<xs:output indent="yes" xalan:indent-amount="3"/>
   <xs:param name="position" select="1"/>
	
	<xs:template match="/">
		<xs:apply-templates select="//xsd:schema[position()=$position]"/>
	</xs:template>

	<xs:template match="node()|@*">
		<xs:copy>
			<xs:apply-templates select="node()|@*"/>
		</xs:copy>
	</xs:template>

</xs:stylesheet>
