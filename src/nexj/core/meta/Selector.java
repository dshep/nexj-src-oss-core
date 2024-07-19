// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.meta;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * A selector representing a message that can be sent to an object.
 */
public class Selector extends NamedMetadataObject
{
   // attributes

   /**
    * True if the message supports a variable number of arguments.
    */
   private boolean m_bVarArg;

   // associations

   /**
    * The containing class.
    */
   private Metaclass m_metaclass;

   /**
    * The member collection, indexed by argument count.
    */
   private List m_memberList = Collections.EMPTY_LIST; // of type Member

   // constructors
   
   /**
    * Creates a selector with a given name.
    * @param sName The selector name.
    */
   public Selector(String sName)
   {
      super(sName);
   }

   // operations
   
   /**
    * Sets the containing class.
    * @param metaclass The containing class to set.
    */
   public void setMetaclass(Metaclass metaclass)
   {
      verifyNotReadOnly();
      m_metaclass = metaclass;
   }

   /**
    * @return The containing class.
    */
   public Metaclass getMetaclass()
   {
      return m_metaclass;
   }
   
   /**
    * @return The variable argument flag.
    */
   public boolean isVarArg()
   {
      return m_bVarArg;
   }

   /**
    * Adds a new member with a specified number
    * of arguments to the selector.
    * @param The member to add.
    * @param nArgCount The number of arguments, including the vararg.
    * @param bVarArg True if the number of arguments is variable.
    * @throws MetadataException if a conflict with
    * the previous definitions occurs. 
    */
   public void addMember(Member member, int nArgCount, boolean bVarArg)
   {
      verifyNotReadOnly();
      
      if (!getName().equals(member.getName()))
      {
         throw new MetadataException("err.meta.memberNameMismatch",
               new Object[]{member.getName(), getName(), getMetaclass().getName()});
      }
      
      if (bVarArg)
      {
         if (--nArgCount < 0)
         {
            throw new MetadataException("err.meta.varArgCount",
               new Object[]{member.getName(), getMetaclass().getName()});
         }
      }

      if (m_memberList.size() == 0)
      {
         m_memberList = new ArrayList(nArgCount + 1);

         while (m_memberList.size() <= nArgCount)
         { 
            m_memberList.add(null);
         }
         
         m_bVarArg = bVarArg;
      }
      else
      {
         if (nArgCount < m_memberList.size() - 1)
         {
            if (bVarArg)
            {
               throw new MetadataException("err.meta.memberArgCountDup",
                     new Object[]{member.getName(), Primitive.createInteger(nArgCount), getMetaclass().getName()});
            }
         }
         else
         {
            if (m_bVarArg)
            {
               throw new MetadataException("err.meta.memberArgCountDup",
                     new Object[]{member.getName(), Primitive.createInteger(nArgCount), getMetaclass().getName()});
            }
            else
            {
               m_bVarArg = bVarArg;
            }
            
            while (m_memberList.size() <= nArgCount)
            { 
               m_memberList.add(null);
            }
         }

         if (m_memberList.get(nArgCount) != null)
         {
            throw new MetadataException("err.meta.memberArgCountDup",
               new Object[]{member.getName(), Primitive.createInteger(nArgCount), getMetaclass().getName()});
         }
      }

      m_memberList.set(nArgCount, member);
   }
   
   /**
    * Finds a member by argument count.
    * @param nArgCount The member argument count.
    * @return The member object, or null if not found.
    */
   public Member findMember(int nArgCount)
   {
      if (nArgCount >= m_memberList.size())
      {
         if (m_bVarArg)
         {
            return (Member)m_memberList.get(m_memberList.size() - 1);
         }
         else
         {
            return null;
         }
      }
      else
      {
         return (Member)m_memberList.get(nArgCount);
      }
   }

   /**
    * Gets a member by argument count.
    * @param nArgCount The member argument count.
    * @return The member object.
    * @throws MetadataLookupException if no such member is found.
    */
   public Member getMember(int nArgCount)
   {
      Member member = findMember(nArgCount);

      if (member != null)
      {
         return member;
      }

      throw new MetadataLookupException("err.meta.memberArgCount", String.valueOf(nArgCount), this);
   }

   /**
    * @return The maximum argument count.
    */
   public int getMaxArgCount()
   {
      return m_memberList.size() - 1;
   }

   /**
    * @return An iterator for the contained member objects. Can return null members.
    */
   public Iterator getMemberIterator()
   {
      return m_memberList.iterator();
   }

   /**
    * @see nexj.core.meta.MetadataObject#makeReadOnly()
    */
   public void makeReadOnly()
   {
      super.makeReadOnly();

      if (m_memberList instanceof ArrayList)
      {
         ((ArrayList)m_memberList).trimToSize(); // free unused memory
      }
   }
}