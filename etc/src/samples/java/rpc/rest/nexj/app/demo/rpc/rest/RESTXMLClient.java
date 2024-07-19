// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.app.demo.rpc.rest;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;

public class RESTXMLClient
{
   public static void main(String[] args) throws JAXBException, IOException
   {
      HttpLocalhost8080NexjXml.Person service = new HttpLocalhost8080NexjXml.Person();
      ObjectFactory factory = new ObjectFactory();
      Person person = factory.createPerson();

      // Use a temporary OID (they start with "@") to represent the instance
      // in args as the same instance as in filter.instances
      person.setOid("@id1");
      person.setFirstName("SOAP");
      person.setLastName("Box");
      person.getKeys().add("initials");
      person.setEvent("create");

      person = service.postTextXmlAsPerson(person, "", "Basic bmV4anNhOm5leGo==");

      System.out.println("Created a Person with OID=" + person.getOid());

      List itemList = service.getAsCollection("(fullName (addrs fullName))",
         "(and (= (@ firstName) \"SOAP\") (= (@ lastName) \"Box\"))",
		   "()", -1, 0, "Basic bmV4anNhOm5leGo==").getItem();

      System.out.println("Read " + itemList.size() + " instance(s) of Person");

      for (int i = 0; i < itemList.size(); ++i)
      {
         person = (Person)itemList.get(i);

         System.out.println("[" + i + "]: " + person.getFullName());
         System.out.println("   addrs: ");

         for (Iterator itr = person.getAddrs().iterator(); itr.hasNext();)
         {
            System.out.println("      " + ((Address)itr.next()).getFullName());
         }
      }
   }
}
