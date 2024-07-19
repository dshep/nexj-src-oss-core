<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
<xsl:param name="meta.url"/>
<xsl:param name="meta.base.url"/>
<xsl:param name="meta.connections.url"/>
<xsl:param name="keep.passwords"/>
<xsl:param name="clusterKeyStore"/>
<xsl:param name="clusterPassword"/>

<xsl:template match="/*">
	<xsl:copy>
	   <xsl:if test="not(@meta.url) and $meta.url">
	      <xsl:attribute name="meta.url"><xsl:value-of select="$meta.url"/></xsl:attribute>
	   </xsl:if>
	   <xsl:if test="not(@meta.base.url) and $meta.base.url">
	      <xsl:attribute name="meta.base.url"><xsl:value-of select="$meta.base.url"/></xsl:attribute>
	   </xsl:if>
	   <xsl:if test="not(@meta.connections.url) and $meta.connections.url">
	      <xsl:attribute name="meta.connections.url"><xsl:value-of select="$meta.connections.url"/></xsl:attribute>
	   </xsl:if>
	   <xsl:if test="not(@clusterPassword) and $clusterPassword">
         <xsl:attribute name="clusterPassword"><xsl:value-of select="$clusterPassword"/></xsl:attribute>
      </xsl:if>
      <xsl:if test="not(@clusterKeyStore) and $clusterKeyStore">
         <xsl:attribute name="clusterKeyStore"><xsl:value-of select="$clusterKeyStore"/></xsl:attribute>
      </xsl:if>
      <xsl:choose>
         <xsl:when test="$keep.passwords">
            <xsl:apply-templates select="node()|@*"/>
         </xsl:when>
         <xsl:otherwise>
            <xsl:apply-templates select="node()|@*" mode="remove.passwords"/>
         </xsl:otherwise>
      </xsl:choose>
	</xsl:copy>
</xsl:template>

<xsl:template match="/*/@user"/>
<xsl:template match="/*/@password"/>
<xsl:template match="/*/@keystorePassword" mode="remove.passwords"/>
<xsl:template match="/*/DataSourceConnections/RelationalDatabaseConnection/@user" mode="remove.passwords"/>
<xsl:template match="/*/DataSourceConnections/RelationalDatabaseConnection/@password" mode="remove.passwords"/>
<xsl:template match="/*/DataSourceConnections/RelationalDatabaseConnection/Fragments/Fragment/@user" mode="remove.passwords"/>
<xsl:template match="/*/DataSourceConnections/RelationalDatabaseConnection/Fragments/Fragment/@password" mode="remove.passwords"/>
<xsl:template match="/*/ChannelConnections/MessageQueueConnection[string-length(@factory)=0]/@user" mode="remove.passwords"/>
<xsl:template match="/*/ChannelConnections/MessageQueueConnection[string-length(@factory)=0]/@password" mode="remove.passwords"/>

<xsl:template match="node()|@*">
   <xsl:copy>
      <xsl:apply-templates select="node()|@*"/>
	</xsl:copy>
</xsl:template>

<xsl:template match="node()|@*" mode="remove.passwords">
   <xsl:copy>
      <xsl:apply-templates select="node()|@*" mode="remove.passwords"/>
   </xsl:copy>
</xsl:template>

</xsl:stylesheet>
