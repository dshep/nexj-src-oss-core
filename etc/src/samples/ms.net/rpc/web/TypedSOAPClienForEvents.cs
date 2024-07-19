// Example of NexJ Generic Server Interface invocation through SOAP.
// NOTE: .NET Framework 2.0 features are used.
using NexJ;
using System;
using System.Net;
using System.Xml.Serialization;

public class TypedSOAPClienForEvents
{
   public static void Main(string[] args)
   {
      // Instantiate the proxy
      Person_Server server = new Person_Server();

      server.Credentials = new NetworkCredential("nexjsa", "nexj");
      server.PreAuthenticate = true;
      server.Url = "http://localhost:8080/nexj/web";

      Person person = new Person();

      person.firstName = "SOAP";
      person.lastName = "Box";
      person._keys = new string[] { "homeAddress" }; // explicitly set homeAddress attribute value to 'null' as opposed to unspecified

      // Invoke the server
      Person result = (Person)server.create(person);

      // Print the OID of created Person
      Console.WriteLine("Created a Person with OID=" + result._oid);

      // Make a request to read all the Persons with name SOAP Box, returning the full name and the addresses with full names.
      NexJ.Object[] items = server.read6(new _Expression("(fullName (addrs fullName))"), // let the server parse the text S-expression
                                         new _Expression("(and (= firstName \"SOAP\") (= lastName \"Box\"))"), // let the server parse the text S-expression
                                         null,
                                         -1, true,
                                         0, true,
                                         false, true);

      // Print the results
      Console.WriteLine("Read " + items.Length + " instance(s) of Person");

      for (int i = 0; i != items.Length; ++i)
      {
         Console.WriteLine("[" + i + "]: " + ((Person)items[i]).fullName);
         Console.WriteLine("   OID: " + ((Person)items[i])._oid);
         Console.WriteLine("   addrs: ");

         Address[] addrs = ((Person)items[i]).addrs;

         for (int k = 0; addrs != null && k != addrs.Length; ++k)
         {
            Console.WriteLine("      " + addrs[k].fullName);
         }
      }
   }
}
