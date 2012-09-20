<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
  xmlns="http://www.w3.org/1999/xhtml">

  <xsl:output method="xml" encoding="UTF-8" indent="yes" />

  <xsl:variable name="vocab" select="document('vocab.xml')" />
  <xsl:variable name="all_linguas">de en en_GB fr</xsl:variable>
  <xsl:variable name="splitstr">
    <xsl:value-of select="' '" />
  </xsl:variable>

  <xsl:template match="noop">
    <select>
      <xsl:call-template name="tokenize">
        <xsl:with-param name="linguas" select="$all_linguas" />
      </xsl:call-template>
    </select>
  </xsl:template>

  <xsl:template name="tokenize">
    <xsl:param name="linguas" />
    <xsl:param name="tail">
      <xsl:value-of select="substring-after($linguas, $splitstr)" />
    </xsl:param>
    <xsl:param name="lang">
      <xsl:value-of select="substring-before($linguas, $splitstr)" />
    </xsl:param>

    <xsl:choose>
      <xsl:when test="contains($tail, $splitstr)">
        <xsl:call-template name="create_option">
          <xsl:with-param name="value" select="$lang" />
          <xsl:with-param name="title" select="$vocab/vocab/item[@value=$lang]" />
        </xsl:call-template>

        <xsl:call-template name="tokenize">
          <xsl:with-param name="linguas" select="$tail" />
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="create_option">
          <xsl:with-param name="value" select="$tail" />
          <xsl:with-param name="title" select="$vocab/vocab/item[@value=$tail]" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:template>

  <xsl:template name="create_option">
    <xsl:param name="value" />
    <xsl:param name="title" />

    <xsl:element name="option">
      <xsl:attribute name="value">
        <xsl:value-of select="$value" />
      </xsl:attribute>
      <xsl:value-of select="$title" />
    </xsl:element>
  </xsl:template>

</xsl:stylesheet>
