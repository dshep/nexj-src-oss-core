// Copyright 2010 NexJ Systems Inc. This software is licensed under the terms of the Eclipse Public License 1.0
package nexj.app.demo.rpc.rest;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.xml.bind.JAXBException;

public class RESTXMLClientForEvents
{
   public static void main(String[] args) throws JAXBException, IOException
   {
      String sHTTPAuth = "Basic bmV4anNhOm5leGo==";
      HttpLocalhost8080NexjXml.Person service = new HttpLocalhost8080NexjXml.Person();
      ObjectFactory factory = new ObjectFactory();
      Person person = factory.createPerson();

      person.setFirstName("SOAP");
      person.setLastName("Box");
      person.getKeys().add("homeAddress"); // explicitly set homeAddress attribute value to 'null'

      person = service.postTextXmlAsPerson(person, "", sHTTPAuth);

      System.out.println("Created a Person with OID=" + person.getOid());

      HttpLocalhost8080NexjXml.Person.Read read = new HttpLocalhost8080NexjXml.Person.Read();
      List itemList = read.postAsObjectArray("(fullName (addrs fullName))",
                                             "(and (= (@ firstName) \"SOAP\") (= (@ lastName) \"Box\"))",
                                             null,
                                             new Integer(-1),
                                             new Integer(0),
                                             Boolean.FALSE,
                                             sHTTPAuth
                                            ).getItem();

      System.out.println("Read " + itemList.size() + " instance(s) of Person");

      for (int i = 0; i < itemList.size(); ++i)
      {
         person = (Person)itemList.get(i);

         System.out.println("[" + i + "]: " + person.getFullName());
         System.out.println("   OID: " + person.getOid());
         System.out.println("   addrs: ");

         for (Iterator itr = person.getAddrs().iterator(); itr.hasNext();)
         {
            System.out.println("      " + ((Address)itr.next()).getFullName());
         }
      }
   }
}
