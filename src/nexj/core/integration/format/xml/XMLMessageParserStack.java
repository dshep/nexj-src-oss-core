// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.integration.format.xml;

import nexj.core.meta.Primitive;
import nexj.core.meta.integration.MessagePart;
import nexj.core.util.SysUtil;

/**
 * The parse stack.
 */
public class XMLMessageParserStack
{
   // constants

   /**
    * Parse stack element size.
    */
   protected final static int STACK_ITEM_SIZE = 5;

   // attributes

   /**
    * The parser stack top.
    */
   protected int m_nTop;

   // associations

   /**
    * The parse object stack: MessagePart[5*n], MessagePart[5*n+1], Object[5*n+2], Integer[5*n+3],
    * Object[5*n+4].
    * This is: part, base part, TO, index, value.
    */
   protected Object[] m_parseArray = new Object[5 * STACK_ITEM_SIZE];

   // operations

   /**
    * Removes all items from the stack.
    */
   public void clear()
   {
      m_nTop = -1;
   }

   /**
    * @return True if the stack is empty; false otherwise.
    */
   public boolean isEmpty()
   {
      return (m_nTop < 0);
   }

   /**
    * Gets the number of items on the stack.
    * @return The number of items on the stack.
    */
   public int getSize()
   {
      return m_nTop + 1;
   }

   /**
    * Pushes an item on the parse stack.
    * @param part The message part metadata to push.
    * @param basePart The base message part metadata to push.
    * @param obj The object to push.
    * @param nIndex The index to push.
    * @param value The message value to push.
    */
   public void push(MessagePart part, MessagePart basePart, Object obj, int nIndex, Object value)
   {
      int nSize = (++m_nTop + 1) * STACK_ITEM_SIZE;

      if (nSize > m_parseArray.length)
      {
         Object[] parseArray = new Object[nSize << 1];

         System.arraycopy(m_parseArray, 0, parseArray, 0, m_parseArray.length);
         m_parseArray = parseArray;
      }

      m_parseArray[nSize - 5] = part;
      m_parseArray[nSize - 4] = basePart;
      m_parseArray[nSize - 3] = obj;
      m_parseArray[nSize - 2] = Primitive.createInteger(nIndex);
      m_parseArray[nSize - 1] = value;
   }

   /**
    * Pushes an item on the parse stack.
    * @param part The message part metadata to push.
    * @param obj The object to push.
    * @param nIndex The index to push.
    */
   public void push(MessagePart part, Object obj, int nIndex)
   {
      push(part, null, obj, nIndex, null);
   }

   /**
    * Pops an item from the parse stack.
    */
   public void pop()
   {
      setTopObject(null);
      m_nTop--;
   }

   /**
    * Gets the message part metadata at a given level in the stack.
    * @param nLevel The stack level.
    * @return The message part metadata.
    */
   public MessagePart getMessagePart(int nLevel)
   {
      return (MessagePart)m_parseArray[nLevel * STACK_ITEM_SIZE];
   }

   /**
    * @return The message part metadata on the top of the stack.
    */
   public MessagePart getTopMessagePart()
   {
      return getMessagePart(m_nTop);
   }

   /**
    * Sets the top message part metadata.
    * @param part The message part metadata to set.
    */
   public void setTopMessagePart(MessagePart part)
   {
      m_parseArray[m_nTop * STACK_ITEM_SIZE] = part;
   }

   /**
    * Gets the object at a given level in the stack.
    * @param nLevel The stack level.
    * @return The object.
    */
   protected Object getObject(int nLevel)
   {
      return m_parseArray[nLevel * STACK_ITEM_SIZE + 2];
   }

   /**
    * Sets the object at the top of the stack.
    * @param obj The object to set.
    */
   public void setTopObject(Object obj)
   {
      m_parseArray[m_nTop * STACK_ITEM_SIZE + 2] = obj;
   }

   /**
    * @return The object on the top of the stack.
    */
   public Object getTopObject()
   {
      return getObject(m_nTop);
   }

   /**
    * Gets the index at a given level in the stack.
    * @param nLevel The stack level.
    * @return The index.
    */
   protected int getIndex(int nLevel)
   {
      return ((Integer)m_parseArray[nLevel * STACK_ITEM_SIZE + 3]).intValue();
   }

   /**
    * Sets the index at the top of the stack.
    * @param nIndex The index.
    */
   public void setTopIndex(int nIndex)
   {
      m_parseArray[m_nTop * STACK_ITEM_SIZE + 3] = Primitive.createInteger(nIndex);
   }

   /**
    * @return The index on the top of the stack.
    */
   public int getTopIndex()
   {
      return getIndex(m_nTop);
   }

   /**
    * Gets the value at a given level in the stack.
    * @param nLevel The stack level.
    * @return The value.
    */
   protected Object getValue(int nLevel)
   {
      return m_parseArray[nLevel * STACK_ITEM_SIZE + 4];
   }

   /**
    * Sets the value at the top of the stack.
    * @param value The value.
    */
   public void setTopValue(Object value)
   {
      m_parseArray[m_nTop * STACK_ITEM_SIZE + 4] = value;
   }

   /**
    * Gets the value at the top of the stack.
    * @return The value.
    */
   public Object getTopValue()
   {
      return getValue(m_nTop);
   }

   /**
    * Gets the base message part at a given level in the stack.
    * @param nLevel The stack level.
    * @return The base message part.
    */
   public MessagePart getBaseMessagePart(int nLevel)
   {
      return (MessagePart)m_parseArray[nLevel * STACK_ITEM_SIZE + 1];
   }

   /**
    * Gets the base message part at the top of the stack.
    * @return The base message part.
    */
   public MessagePart getTopBaseMessagePart()
   {
      return getBaseMessagePart(m_nTop);
   }

   /**
    * Sets the base message part at the top of the stack.
    * @param basePart The base message part.
    */
   public void setTopBaseMessagePart(MessagePart basePart)
   {
      m_parseArray[m_nTop * STACK_ITEM_SIZE + 1] = basePart;
   }

   /**
    * FOR DEBUGGING PURPOSES ONLY
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      if (isEmpty())
      {
         return "<empty>";
      }

      StringBuilder buf = new StringBuilder(256);

      for (int i = 0; i <= m_nTop; i++)
      {
         buf.append(getMessagePart(i).getFullPath());
         buf.append(": name=");
         buf.append(getBaseMessagePart(i).getFullPath());
         buf.append(", index=");
         buf.append(getIndex(i));
         buf.append(", obj=");
         buf.append(getObject(i));
         buf.append(", value=");
         buf.append(getValue(i));
         buf.append(SysUtil.LINE_SEP);
      }

      return buf.toString();
   }
}