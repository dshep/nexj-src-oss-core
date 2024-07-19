// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.scripting.object;

import nexj.core.scripting.Function;
import nexj.core.scripting.Machine;
import nexj.core.scripting.ReadOnlyException;
import nexj.core.scripting.Symbol;

/**
 * Class member object implementation.
 */
public abstract class MemberObject extends NamedObject
{
   // attributes

   /**
    * The public visibility flag.
    * True means that the member is accessible through RPC.
    */
   protected boolean m_bPublic;

   // associations

   /**
    * The member holder class.
    */
   protected ClassObject m_holder;

   // constructors

   /**
    * Constructs the member.
    * @param classObject The class of this object.
    * @param holder The holder class. Can be null.
    * @param symbol The member symbol.
    */
   protected MemberObject(ClassObject classObject, ClassObject holder, Symbol symbol)
   {
      super(classObject, symbol);
      m_holder = holder;
   }

   // operations

   /**
    * Sets the holder class.
    * @param holder The holder class.
    */
   protected void setHolder(ClassObject holder)
   {
      m_holder = holder;
   }

   /**
    * @return The holder class.
    */
   public ClassObject getHolder()
   {
      return m_holder;
   }

   /**
    * Sets the public visibility flag.
    * @param bPublic The public visibility flag to set.
    */
   public void setPublic(boolean bPublic)
   {
      change();
      m_bPublic = bPublic;
   }

   /**
    * @return The public visibility flag.
    */
   public boolean isPublic()
   {
      return m_bPublic;
   }

   /**
    * @see ClassObject#change()
    */
   protected void change()
   {
      if (m_holder != null && m_holder.hasMember(this))
      {
         throw new ReadOnlyException("err.scripting.readOnlyMember",
            new Object[]{getName(), m_holder.getName()});
      }
   }

   /**
    * Exposes members from MemberObject.
    * @param classObject The destination class object.
    */
   public static void addMembers(ClassObject classObject)
   {
      NamedObject.addMembers(classObject);

      classObject.addMethod(":public?", 0, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            machine.returnValue(Boolean.valueOf(((MemberObject)machine.getArg(0, nArgCount)).isPublic()), nArgCount);

            return false;
         }
      });

      classObject.addMethod(":public", 1, false, new Function()
      {
         public boolean invoke(int nArgCount, Machine machine)
         {
            boolean bPublic = !Boolean.FALSE.equals(machine.getArg(1, nArgCount));

            ((MemberObject)machine.getArg(0, nArgCount)).setPublic(bPublic);
            machine.returnValue(Boolean.valueOf(bPublic), nArgCount);

            return false;
         }
      });

      ClassObject.addOptionsMethod(classObject, "err.scripting.memberOption");
   }
}
