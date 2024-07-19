// Convenience methods for the generic objects

namespace NexJ
{
   public partial class _Pair
   {
      public _Pair()
      {
      }

      public _Pair(object head)
      {
         this.head = head;
      }

      public _Pair(object head, object tail)
      {
         this.head = head;
         this.tail = tail;
      }

      public static _Pair list(params object[] args)
      {
         _Pair pair = null;

         for (int i = args.Length - 1; i >= 0; --i)
         {
            pair = new _Pair(args[i], pair);
         }

         return pair;
      }

      public static _Pair attribute(string assoc)
      {
         return list(_Symbol.define("@"), _Symbol.define(assoc));
      }
   }

   public partial class _Symbol
   {
      public readonly static _Symbol AND = new _Symbol("and");
      public readonly static _Symbol EQ = new _Symbol("=");

      private _Symbol()
      {
      }

      private _Symbol(string name)
      {
         this.name = name;
      }

      public static _Symbol define(string name)
      {
         return new _Symbol(name);
      }
   }

   public partial class _Expression
   {
      private _Expression()
      {
      }

      public _Expression(string text)
      {
         this.text = text;
      }
   }

   public partial class _TransferObject
   {
      public object findValue(string name)
      {
         for (int i = 0, n = this._keys.Length; i != n; ++i)
         {
            if (this._keys[i] == name)
            {
               return this._values[i];
            }
         }

         return null;
      }
   }
};
