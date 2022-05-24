--with Optional_Type;
generic
   Max : Positive;
   type Element_Type is private;
package BoundedQueues is
   pragma Preelaborate;
   pragma Pure;
   Underflow    : exception;
   Overflow     : exception;
   Inconsistent : exception;

   --Type Optional stolen and modified from wiki
   --wiki https://en.wikipedia.org/wiki/Option_type#Ada
   type Optional (Has_Element : Boolean := False) is record
      case Has_Element is
         when False =>
            null;
         when True =>
            Element : Element_Type;
      end case;
   end record;

   type QueueContent is array (Natural range <>) of Optional;
   type Queue is record
      First   : Natural range 0 .. (Max - 1) := 0;
      Size    : Natural range 0 .. Max       := 0;
      content : QueueContent (0 .. (Max - 1));
   end record;
   --with
   --  Predicate =>
   --  ((First in content'Range) and then (Size in content'Range)
   --   and then
   --   ((First = 0 and Size <= 1) or ((First + Size - 1) in content'Range))
   --   and then
   --   ((for all I in content'First .. First - 1 =>
   --       not content (I).Has_Element) and
   --    (for all I in First .. (First + Size) - 1 =>
   --       content (I).Has_Element) and
   --    (for all I in (First + Size) .. content'Last =>
   --       not content (I).Has_Element)));

   function Consistent (Q : in Queue) return Boolean;

   -- with
   --  Annotate => (GNATprove, Terminating),
   --  Post     =>
   --  (if Consistent'Result then
   --     ((Q.First in Q.content'Range) and
   --      (Q.Size = 0 or Q.First + Q.Size - 1 in Q.content'Range) and
   --      ((for all I in Q.content'First .. Q.First - 1 =>
   --          not Q.content (I).Has_Element) and
   --       (for all I in Q.First .. (Q.First + Q.Size) - 1 =>
   --          Q.content (I).Has_Element) and
   --       (for all I in (Q.First + Q.Size) .. Q.content'Last =>
   --          not Q.content (I).Has_Element)))
   --       else True);

   --  (if (Q.First = 0 and Q.Size = 0) then
   --     (for all I in Q.content'Range =>
   --        not Q.content(I).Has_Element)
   --   else
   --     ((for all I in Q.content'First .. (Q.First-1) =>
   --         not Q.content(I).Has_Element) and
   --      (for all I in Q.First .. (GetLast (Q)) =>
   --         Q.content(I).Has_Element) and
   --      (for all I in GetLast (Q)+1 .. Q.content'Last =>
   --         not Q.content(I).Has_Element))));

   function GetContent (Q : in Queue) return QueueContent with
      Global => null,
      Post   => GetContent'Result = Q.content;
   function GetFirst (Q : in Queue) return Natural with
      Global => null,
      Post   => GetFirst'Result = Q.First;
   function GetSize (Q : Queue) return Natural with
      Global => null,
      Pre    => Consistent (Q)
      and then (GetFirst (Q) <= Max and GetContent (Q)'Length = Max),
      Post => Consistent (Q) and GetSize'Result >= 0 and
      GetSize'Result <= Max and GetSize'Result = Q.Size and
      (for all I in GetFirst (Q) .. (GetFirst (Q) + Q.Size - 1) =>
         GetContent (Q) (I).Has_Element);
   function GetLast (Q : Queue) return Natural with
      Global => null,
      Pre    => Consistent (Q) and then GetSize (Q) + GetFirst (Q) <= Max,
      Post   => Consistent (Q)
      and then
      (if GetSize (Q) = 0 then
         (GetLast'Result = GetFirst (Q) and
          not GetContent (Q) (GetLast'Result).Has_Element)
       else
         (if GetSize (Q) = 1 then
            (GetLast'Result = GetFirst (Q) and
             GetContent (Q) (GetLast'Result).Has_Element)
          else GetLast'Result = GetFirst (Q) + (GetSize (Q) - 1)
            and then GetContent (Q) (GetLast'Result).Has_Element));

   function Make return Queue with
      Global => null,
      Post   => Consistent (Make'Result) and IsEmpty (Make'Result) and
      Make'Result.First = Make'Result.content'First;
      --(for all I in GetContent (Make'Result)'Range =>
      --  not GetContent (Make'Result) (I).Has_Element);
      --pragma Assume(Consistent(Make'Result));

   function IsEmpty (Q : in Queue) return Boolean with
      Global => null,
      Pre    => Consistent (Q),
      Post   =>
      (if IsEmpty'Result then
         (GetSize (Q) = 0 and GetLast (Q) = GetFirst (Q) and
          (for all I in GetContent (Q)'Range =>
             not GetContent (Q) (I).Has_Element))
       else
         (GetSize (Q) > 0 and
          (GetLast (Q) > GetFirst (Q) or (GetSize (Q) = 1)) and
          (for all J in GetFirst (Q) .. GetLast (Q) =>
             GetContent (Q) (J).Has_Element)));
   function IsFull (Q : in Queue) return Boolean with
      Pre  => Consistent (Q),
      Post =>
      (if IsFull'Result then
         (GetLast (Q) = Max - 1) and
         (IsEmpty (Q) or
          (for all I in GetFirst (Q) .. GetContent (Q)'Last =>
             GetContent (Q) (I).Has_Element))
       else GetLast (Q) < Max - 1);
   procedure Pop (Q : in out Queue; e : out Element_Type) with
      Global  => null,
      Depends => (Q => Q, e => Q),
      Pre     => Consistent (Q)
      and then (not IsEmpty (Q) and (GetContent (Q) (Q.First).Has_Element)),
      Post => Consistent (Q) and
      (GetSize (Q) = GetSize (Q'Old) - 1 and
       (Q.First = Q'Old.First + 1 or Q.First = Q.content'Last) and
       e = GetContent (Q'Old) (GetFirst (Q'Old)).Element);
   procedure Push (Q : in out Queue; e : in Element_Type) with
      Global  => null,
      Depends => (Q => (Q, e)),
      Pre     => Consistent (Q)
      and then
      ((not IsFull (Q))),-- and then (GetSize (Q) + GetFirst (Q) < Max)),
      Post => Consistent (Q) and GetContent (Q) (GetLast (Q)).Element = e
      and then (GetSize (Q) = GetSize (Q'Old) + 1 and (not IsEmpty (Q)));

   type Element_Array is array (Natural range <>) of Element_Type;
   function GetElements (Q : in Queue) return Element_Array with
      Global => null,
      Pre    => Consistent (Q) and then (not IsEmpty (Q)),

     Post =>
          (((GetElements'Result'First <= GetElements'Result'Last)
      and
      (for all I in GetFirst(Q)..GetLast(Q) =>
            (for some J in GetElements'Result'Range =>
               (GetElements'Result (J) = GetContent (Q) (I).Element)))) or GetElements'Result'Length = 0) ;

      --  (for all I in GetElements'Result'Range =>
      --     (for some J in Q.content'Range =>
      --        (if (GetContent (Q) (J).Has_Element) then
      --           (GetElements'Result (I) = GetContent (Q) (J).Element))));
end BoundedQueues;
