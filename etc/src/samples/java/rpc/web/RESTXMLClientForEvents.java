// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package com.nexj.xml;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.JAXBException;
import javax.xml.ws.BindingProvider;

public class RESTXMLClientForEvents
{
   public static void main(String[] args) throws JAXBException, IOException
   {
      ObjectFactory factory = new ObjectFactory();
      PersonServer server = new GenericServer().getPersonServer();

      ((BindingProvider)server).getRequestContext().put(BindingProvider.USERNAME_PROPERTY, "nexjsa");
      ((BindingProvider)server).getRequestContext().put(BindingProvider.PASSWORD_PROPERTY, "nexj");

      Person person = factory.createPerson();

      person.firstName = "SOAP";
      person.lastName = "Box";
      person.keys = new ArrayList<String>(1);
      person.keys.add("homeAddress"); // explicitly set homeAddress attribute value to 'null' as opposed to unspecified

      // Invoke the server
      Person result = (Person)server.create(person);

      // Print the OID of created Person
      System.out.println("Created a Person with OID=" + result.oid);

      // Make a request to read all the Persons with name SOAP Box, returning the full name and the addresses with full names.
      List<Object> items = server.read6(expression("(fullName (addrs fullName))"), // let the server parse the text S-expression
                                        list(symbol("and"), // build the S-expression from objects
                                             list(symbol("="), attribute("firstName"), "SOAP"),
                                             list(symbol("="), attribute("lastName"), "Box")
                                             ),
                                        null,
                                        new Integer(-1),
                                        new Integer(0),
                                        Boolean.FALSE);

      // Print the results
      System.out.println("Read " + items.size() + " instance(s) of Person");

      for (int i = 0, nCount = items.size(); i < nCount; ++i)
      {
         person = (Person)items.get(i);

         System.out.println("[" + i + "]: " + person.fullName);
         System.out.println("   OID: " + person.oid);
         System.out.println("   addrs: ");

         if (person.addrs != null)
         {
            for (Address addr: person.addrs)
            {
                System.out.println("      " + addr.fullName);
            }
         }
      }
   }

   private static _Pair attribute(String assoc)
   {
      return list(symbol("@"), symbol(assoc));
   }

   private static _Expression expression(String text)
   {
      _Expression value = new _Expression();

       value.text = text;

       return value;
   }

   private static _Pair list(java.lang.Object ... args)
   {
      _Pair pair = null;

      for (int i = args.length - 1; i >= 0; --i)
      {
         pair = pair(args[i], pair);
      }

      return pair;
   }

   private static _Pair pair(java.lang.Object head, java.lang.Object tail)
   {
      _Pair value = new _Pair();

      value.head = head;
      value.tail = tail;

      return value;
   }

   private static _Symbol symbol(String name)
   {
      _Symbol value = new _Symbol();

      value.name = name;

      return value;
   }
}