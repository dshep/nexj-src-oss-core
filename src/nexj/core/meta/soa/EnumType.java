package nexj.core.meta.soa;

import java.util.Iterator;

import nexj.core.meta.xml.XMLSOAMetadataLoader;
import nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject;
import nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject;
import nexj.core.scripting.Pair;
import nexj.core.scripting.Symbol;
import nexj.core.util.HashHolder;

/**
 * An enumeration type for the information model.
 */
public class EnumType extends SOAObject implements GlobalObject
{
   // associations
   
   /**
    * The definition where this information model type is defined.
    */
   protected Definition m_definition;

   /**
    * A set of items in the enumeration.
    */
   protected HashHolder m_itemSet = new HashHolder(); // of type Symbol[]

   // constructors

   /**
    * Creates a new enumeration type.
    * @param definition The definition where this type is defined.
    */
   public EnumType(Definition definition)
   {
      m_definition = definition;
   }

   // operations

   /**
    * Adds an item to the enumeration.
    * @param sItem The item to add.
    * @return True if the item was added; false if an equivalent item already exists.
    */
   public boolean addItem(String sItem)
   {
      return m_itemSet.add(Symbol.define(sItem));
   }

   public Symbol getItem(String sItem)
   {
      return (Symbol)m_itemSet.get(Symbol.define(sItem));
   }

   public int getItemCount()
   {
      return m_itemSet.size();
   }

   public Iterator getItemIterator()
   {
      return m_itemSet.iterator();
   }

   /**
    * Generates the following code:
    *
    *    (define <definition QName>:enum:<name> (make-enumeration '(<items>)))
    *
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.SOAObject#getCode()
    */
   public Object getCode()
   {
      Pair items = Pair.fromIterator(m_itemSet.iterator());

      return Pair.list(
         Symbol.DEFINE,
         Symbol.define(getGlobalName()),
         Pair.list(
            XMLSOAMetadataLoader.MAKE_ENUMERATION,
            Pair.quote(items)
         )
      );
   }

   /**
    * @see nexj.core.meta.xml.XMLSOAMetadataLoader.GlobalObject#getGlobalName()
    */
   public String getGlobalName()
   {
      StringBuilder buf = new StringBuilder(m_definition.getGlobalName().length() + 6 + m_sName.length());

      buf.append(m_definition.getGlobalName());
      buf.append(":enum:");
      buf.append(m_sName);

      return buf.toString();
   }
}
