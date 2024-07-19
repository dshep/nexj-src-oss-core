// Example of NexJ Generic Server Interface invocation through RPC/encoded SOAP.

using NexJ;
using System;
using System.Net;

public class GenericSOAPClient
{
   public static void Main(string[] args)
   {
      // Instantiate the proxy
      GenericServer server = new GenericServer();

      // Point the server to the correct URL
      server.Url = (args.Length > 0) ? args[0] : "http://localhost:7080/nexj/soap";

      // Provide the user name and the password
      server.Credentials = new NetworkCredential(
         (args.Length > 1) ? args[1] : "nexjsa",
         (args.Length > 2) ? args[2] : "nexj");

      // Save an extra request which would result in HTTP 401 status
      server.PreAuthenticate = true;

      // Make a request to create an instance of Person
      TransferObject tobj = new TransferObject(
         "Person", // Specify the object class
         "create", // Specify the event to invoke on the object
         null, // Do not specify the OID of the object, as we would like to create a new one
         // Specify attributes to set on the object instance before invoking the event.
         new string[]{"firstName", "lastName"}, // The keys contain the attribute names.
         new object[]{"SOAP", "Box"} // The values correspond to the keys at the same index in the array.
      );

      // Create a generic request, which is used to submit a batch of event invocations to the server
      Request req = new Request();

      // Specify that the transaction will be committed
      req.commit = true;

      // Specify the invocations to make - just one in this example
      req.invocations = new Invocation[]
         {
            new Invocation(
               // The object to invoke 
               tobj,
               // Do not specify an event name as it is already specified on the transfer object.
               // null,
               // Do not specify event arguments, as this event does not take any.
               // null,
               // The attributes to return: create and update events return the object itself.
               // The format is the same as with a read event attributes.
               Pair.list(Symbol.define("fullName"))
            )
         };

      // Add an optional filter to get the class names and OIDs of any created objects
      TransferObject filter = new TransferObject("Object" /* The superclass to match */);

      // If individual instances from the request have to be matched, pass them as an array in the instances key:
      // filter.@class = "Person";
      // NexJ.Array instances = new NexJ.Array();
      // instances.items = new object[]{tobj};
      // If particular attributes have to be retrieved by the filter, pass them in the attributes key.
      // The format is the same as with a read event attributes:
      // filter.keys = new string[]{"instances", "attributes"};
      // filter.values = new object[]{instances, new Expression("(firstName lastName fullName)")};
      // When an instance filter is used, the attributes that are the same as in the original
      // object from the request will not be returned.

      // Any number of filters can be specified
      req.filters = new TransferObject[]{filter};

      // Invoke the server
      Response resp = server.invoke(req);

      // Print the first element of the OID, assuming that it is a GUID
      Console.WriteLine("Created a Person with OID=" + new Guid((byte[])resp.events[0][0].oid.values[0]) +
         // Print the full name
         " and name=" + ((TransferObject)resp.results[0]).findValue("fullName"));

      // Make a request to read all the Persons with name SOAP Box, returning the full name and the addresses with full names.
      req = new Request(false, new Invocation[]
      {
         new Invocation(
            new TransferObject("Person"),
            "read", // Static events can also be invoked
            new object[]
            {
               // One way to pass an S-expression is to let the server parse the text 
               new Expression("(fullName (addrs fullName))"),
      
               // and another is to build it from objects
               Pair.list(Symbol.AND,
                  Pair.list(Symbol.EQ, Pair.attribute("firstName"), "SOAP"),
                  Pair.list(Symbol.EQ, Pair.attribute("lastName"), "Box")
                  ),
               null,
               -1,
               0,
               false
            },
            null
         )
      });

      // Another approach for invoking a static event is to use named arguments and not use the argument array:
      // tobj.keys = new string[]{"attributes", "where", "orderBy", "count", "offset", "xlock"};
      // tobj.values = new object[]{ ... };
      // The difference is in vararg event argument handling - with the latter approach,
      // the extra arguments will end up in (<key> . <value>) pairs.

      // Invoke the server
      resp = server.invoke(req);

      // Print the results
      object[] items = ((NexJ.Array)resp.results[0]).items;

      Console.WriteLine("Read " + items.Length + " instance(s) of Person");

      for (int i = 0; i != items.Length; ++i)
      {
         tobj = (TransferObject)items[i];

         Console.WriteLine("[" + i + "]: " + tobj.findValue("fullName"));
         Console.WriteLine("   addrs: ");

         object[] addrs = ((NexJ.Array)tobj.findValue("addrs")).items;

         for (int k = 0; k != addrs.Length; ++k)
         {
            Console.WriteLine("      " + ((TransferObject)addrs[k]).findValue("fullName"));
         }
      }
   }
}
