<?xml version="1.0" encoding="UTF-8"?>

<xsl:stylesheet version="1.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:text="urn:oasis:names:tc:opendocument:xmlns:text:1.0"
  >

  <xsl:output method="text" />

  <!-- extract code examples -->
  <xsl:template match="text:p[@text:style-name='CodePACKT'] | text:p[@text:style-name='CodeEndPACKT']">
    <xsl:text>
    </xsl:text>
    <xsl:apply-templates select="text:s" />
    <xsl:value-of select="normalize-space(.)" />
  </xsl:template>

  <!--
       use the `c` attribute value for adding whitespace to indent the
       code
    -->
  <xsl:template match="text:s">
    <xsl:param name="n" select="@text:c"/>
    <xsl:for-each select="(//node())[$n >= position()]">
        <xsl:text> </xsl:text>
    </xsl:for-each>
  </xsl:template>

  <!-- adds whitespace before and after code -->
  <xsl:template match="text:p[@text:style-name='NormalPACKT']">
    <xsl:text>
    </xsl:text>
  </xsl:template>


  <!-- one to rule them all and ignore the rest of the text -->
  <xsl:template match="text()|@*"></xsl:template>

</xsl:stylesheet>
