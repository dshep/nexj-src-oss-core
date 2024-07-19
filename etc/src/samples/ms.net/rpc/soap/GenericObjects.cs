// Convenience methods for the generic objects

namespace NexJ
{
   public partial class Pair
   {
      public Pair()
      {
      }

      public Pair(object head)
      {
         this.head = head;
      }

      public Pair(object head, object tail)
      {
         this.head = head;
         this.tail = tail;
      }

      public static Pair list(params object[] args)
      {
         Pair pair = null;

         for (int i = args.Length - 1; i >= 0; --i)
         {
            pair = new Pair(args[i], pair);
         }

         return pair;
      }

      public static Pair attribute(string assoc)
      {
         return list(Symbol.define("@"), Symbol.define(assoc));
      }
   }

   public partial class Symbol
   {
      public readonly static Symbol AND = new Symbol("and");
      public readonly static Symbol EQ = new Symbol("=");

      private Symbol()
      {
      }

      private Symbol(string name)
      {
         this.name = name;
      }

      public static Symbol define(string name)
      {
         return new Symbol(name);
      }
   }

   public partial class Expression
   {
      private Expression()
      {
      }

      public Expression(string text)
      {
         this.text = text;
      }
   }

   public partial class TransferObject
   {
      public TransferObject()
      {
      }

      public TransferObject(string className)
      {
         this.@class = className;
      }

      public TransferObject(string className, string eventName)
      {
         this.@class = className;
         this.@event = eventName;
      }

      public TransferObject(string className, string eventName, OID oid, string[] keys, object[] values)
      {
         this.@class = className;
         this.@event = eventName;
         this.oid = oid;
         this.keys = keys;
         this.values = values;
      }

      public object findValue(string name)
      {
         for (int i = 0, n = this.keys.Length; i != n; ++i)
         {
            if (this.keys[i] == name)
            {
               return this.values[i];
            }
         }

         return null;
      }
   }

   public partial class Invocation
   {
      public Invocation()
      {
      }

      public Invocation(TransferObject tobj)
      {
         this.@object = tobj;
      }

      public Invocation(TransferObject tobj, Pair attributes)
      {
         this.@object = tobj;
         this.attributes = attributes;
      } 

      public Invocation(TransferObject tobj, string eventName, object[] args, Pair attributes)
      {
         this.@object = tobj;
         this.@event = eventName;
         this.arguments = args;
         this.attributes = attributes;
      } 
   }

   public partial class Request
   {
      public Request()
      {
      }

      public Request(bool commit, Invocation[] invocations)
      {
         this.commit = commit;
         this.invocations = invocations;
      }
   }
};
