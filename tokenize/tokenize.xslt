<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns="http://www.w3.org/1999/xhtml">

  <xsl:output method="xml" encoding="UTF-8" indent="yes" />

  <xsl:variable name="all_linguas">de en fr</xsl:variable>

  <xsl:template match="noop">
    <xsl:call-template name="tokenize">
      <xsl:with-param name="linguas" select="$all_linguas" />
    </xsl:call-template>
  </xsl:template>

  <xsl:template name="tokenize">
    <xsl:param name="splitstr">
      <xsl:value-of select="' '" />
    </xsl:param>
    <xsl:param name="linguas" />
    <xsl:param name="lang">
      <xsl:if test="contains($linguas, $splitstr)">
        <xsl:value-of select="substring-before($linguas, $splitstr)" />
      </xsl:if>
    </xsl:param>
    <xsl:element name="option">
      <xsl:attribute name="value">
        <xsl:value-of select="$lang" />
      </xsl:attribute>
    </xsl:element>

  </xsl:template>
</xsl:stylesheet>
