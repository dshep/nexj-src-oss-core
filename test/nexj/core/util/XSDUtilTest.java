// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.util;

import java.io.IOException;

import junit.framework.TestCase;

import org.apache.xerces.xs.XSAttributeUse;
import org.apache.xerces.xs.XSComplexTypeDefinition;
import org.apache.xerces.xs.XSElementDeclaration;
import org.apache.xerces.xs.XSModel;
import org.apache.xerces.xs.XSObjectList;


public class XSDUtilTest extends TestCase
{

   /**
    * Constructor for XSDUtilTest.
    * @param name
    */
   public XSDUtilTest(String name)
   {
      super(name);
   }

   public void testValidatePattern()
   {
      try
      {
         XSModel model = XSDUtil.getXSModel(XSDUtilTest.class.getResource("attributetypes.xsd"));
   
         XSElementDeclaration elemDecl = model.getElementDeclaration("Metadata", null);
         XSComplexTypeDefinition complexDecl = XSDUtil.getComplexTypeDef(elemDecl);
         XSObjectList list = complexDecl.getAttributeUses();
         XSAttributeUse attrUse = (XSAttributeUse)list.item(0);
   
         assertTrue(XSDUtil.hasPattern(attrUse.getAttrDeclaration()));      
         assertNull(XSDUtil.validate("1.8", attrUse.getAttrDeclaration()));
         assertNotNull(XSDUtil.validate("abc", attrUse.getAttrDeclaration()));
         assertTrue(XSDUtil.getEnumeration(attrUse.getAttrDeclaration()).length == 0);
         assertFalse(XSDUtil.isBooleanAttribute(attrUse.getAttrDeclaration()));
               
         attrUse = (XSAttributeUse)list.item(1);
         assertTrue(XSDUtil.getEnumeration(attrUse.getAttrDeclaration()).length == 9);
         assertFalse(XSDUtil.isBooleanAttribute(attrUse.getAttrDeclaration()));

         attrUse = (XSAttributeUse)list.item(2);
         assertTrue(XSDUtil.isBooleanAttribute(attrUse.getAttrDeclaration()));
      }
      catch (IOException e)
      {
         fail();            
      }
   }
}
