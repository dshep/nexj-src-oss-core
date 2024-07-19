// Example of NexJ Generic Server Interface invocation through typed document/literal wrapped SOAP.

using NexJ;
using System;
using System.Net;
using System.Xml.Serialization;

public class TypedSOAPClient
{
   public static void Main(string[] args)
   {
      // Instantiate the proxy
      GenericServer server = new GenericServer();

      // Point the server to the correct URL
      server.Url = (args.Length > 0) ? args[0] : "http://localhost:8080/nexj/web";

      // Provide the user name and the password
      server.Credentials = new NetworkCredential(
         (args.Length > 1) ? args[1] : "nexjsa",
         (args.Length > 2) ? args[2] : "nexj");

      // Save an extra request which would result in HTTP 401 status
      server.PreAuthenticate = true;

      // Make a request to create an instance of Person
      Person person = new Person();

      // Use a temporary OID (they start with "@") to represent the instance
      // in args as the same instance as in filter.instances
      person._oid = "@id1";
      person.firstName = "SOAP";
      person.lastName = "Box";

      // Add to _keys any attributes that must be treated as null as opposed to unspecified
      person._keys = new string[]{"initials"};

      // Create a single request invocation
      _Invocation invocation = new _Invocation();

      // Specify the target object of the invocation
      invocation.@object = person;

      // Specify the event to invoke on the object, this value if present overrides the <_event> value of _TransferObject
      invocation.@event = "create";

      // Do not specify event arguments, as this event does not take any.
      // The elements in the parameters array correspond to those in the args array.
      // invocation.arg = new object[]{null};

      // Create a generic request, which is used to submit a batch of event invocations to the server
      _Request req = new _Request();

      // Specify that the transaction will be committed
      req.commit = true;

      // Specify the objects to invoke - just one in this example
      req.invocation = new _Invocation[]{invocation};

      // Add an instance filter to get the resulting OID.
      // It will be returned also in the result transfer object,
      // but this example illustrates the use of filters.
      _TransferObject filter = new _TransferObject();
      filter._class = "Person";
      filter._keys = new string[]{"instances"};
      filter._values = new object[]{new object[]{person}};

      req.filters = new _TransferObject[]{filter};

      // Invoke the server
      _Response resp = server.invoke(req);

      // Print the first elemet of the OID, assuming that it is a GUID
      Console.WriteLine("Created a Person with OID=" + new Guid(((Person)resp.events[0][0])._oid.PadLeft(32, '0').Substring(0, 32)));

      // Make a request to read all the Persons with name SOAP Box, returning the full name and the addresses with full names.
      _TransferObject tobj = new _TransferObject();
      tobj._class = "Person";
      tobj._event = "read";
      tobj._keys = new string[]{"attributes", "where", "orderBy", "count", "offset", "xlock"};
      tobj._values = new object[]
      {
         // One way to pass an S-expression is to let the server parse the text 
         new _Expression("(fullName (addrs fullName))"),
         // And another one is to build it from objects
         _Pair.list(_Symbol.AND,
            _Pair.list(_Symbol.EQ, _Pair.attribute("firstName"), "SOAP"),
            _Pair.list(_Symbol.EQ, _Pair.attribute("lastName"), "Box")
            ),
         null,
         -1,
         0,
         false
      };

      invocation = new _Invocation();
      invocation.@object = tobj;
      req = new _Request();
      req.invocation = new _Invocation[]{invocation};

      // Invoke the server
      resp = server.invoke(req);

      // Print the results
      object[] items = (object[])resp.results[0];

      Console.WriteLine("Read " + items.Length + " instance(s) of Person");

      for (int i = 0; i != items.Length; ++i)
      {
         Console.WriteLine("[" + i + "]: " + ((Person)items[i]).fullName);
         Console.WriteLine("   addrs: ");

         Address[] addrs = ((Person)items[i]).addrs;

         if (addrs != null)
         {
            for (int k = 0; addrs != null && k != addrs.Length; ++k)
            {
               Console.WriteLine("      " + addrs[k].fullName);
            }
         }
      }
   }
}
