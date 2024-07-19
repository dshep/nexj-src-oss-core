// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.core.rpc;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import nexj.core.util.PrintWriter;
import nexj.core.util.Printable;
import nexj.core.util.SysUtil;

/**
 * The object containing the server response.
 */
public class Response implements Serializable, Printable
{
   // constants

   /**
    * The Java stream unique identifier for the class.
    */
   private final static long serialVersionUID = -6666731432771459737L;

   // associations

   /**
    * Result objects corresponding to the request arg list.
    */
   private List m_resultList = new ArrayList(4); // of type Object

   /**
    * A list of collections of transfer objects corresponding
    * to the filter list objects.
    */
   private List m_eventList = new ArrayList(4); // of type Collection
   
   // operations

   /**
    * Adds a new result object to the response.
    * @param result The result object to add.
    */
   public void addResult(Object result)
   {
      m_resultList.add(result);
   }

   /**
    * Gets a result object by ordinal number.
    * @param nOrdinal The result object ordinal number (0-based).
    * @return The result object object.
    */
   public Object getResult(int nOrdinal)
   {
      return (Object)m_resultList.get(nOrdinal);
   }

   /**
    * @return The result object count.
    */
   public int getResultCount()
   {
      return m_resultList.size();
   }

   /**
    * @return An iterator for the contained result object objects.
    */
   public Iterator getResultIterator()
   {
      return m_resultList.iterator();
   }

   /**
    * Adds a new event to the response.
    * @param event The event to add.
    */
   public void addEvent(Collection event)
   {
      m_eventList.add(event);
   }

   /**
    * Gets an event by ordinal number.
    * @param nOrdinal The event ordinal number (0-based).
    * @return The event object.
    */
   public Collection getEvent(int nOrdinal)
   {
      return (Collection)m_eventList.get(nOrdinal);
   }

   /**
    * @return The event count.
    */
   public int getEventCount()
   {
      return m_eventList.size();
   }

   /**
    * @return An iterator for the contained event objects.
    */
   public Iterator getEventIterator()
   {
      return m_eventList.iterator();
   }

   /**
    * @see nexj.core.util.Printable#printOn(nexj.core.util.PrintWriter)
    */
   public void printOn(PrintWriter writer) throws IOException
   {
      if (writer.addObject(this))
      {
         writer.addIndent(1);
         writer.write("Response:");
         writer.write(SysUtil.LINE_SEP);
         writer.write("results(");
         writer.print(m_resultList.size());
         writer.write("):");

         for (int i = 0; i < m_resultList.size(); ++i)
         {
            writer.indent();
            writer.print(m_resultList.get(i));
         }

         writer.indent(-1);
         writer.write("events(");
         writer.print(m_eventList.size());
         writer.write("):");

         for (int i = 0; i < m_eventList.size(); ++i)
         {
            writer.indent();
            writer.print(m_eventList.get(i));
         }

         writer.addIndent(-1);
         writer.write(SysUtil.LINE_SEP);
      }
   }

   /**
    * @see java.lang.Object#toString()
    */
   public String toString()
   {
      return PrintWriter.toString(this);
   }
}
